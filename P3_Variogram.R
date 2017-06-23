###################################################################################
#                                                                                 #
#    The R-NCMPs package has been developed by the ET-NCMP.                       #
#    P3_Variogram_Dec2016.R                                                       #
#                                                                                 #
#    This program calculates the variogram for 5 different indices.               #
#    For further details please refer to the User Manual.                         #
#                                                                                 #
#    Programmers:                                                                 #
#    Megan Hartwell, McMaster University, Canada                                  #
#    Lucie Vincent, Environment and Climate Change Canada                         #
#    December 2016                                                                #
#    Modified by Simon Grainger, Bureau of Meteorology, Australia                 #
#    February 2017 - Cleaned up, code is more "R-like"                            #
#                                                                                 #
###################################################################################

# The use of the data.table package imposes requirements on the version of
# R and data.table - test for this and advise the user
# If package data.table is missing, packageVersion() will generate an error and stop

if (getRversion() < "3.0.0") {
  warning("R Version < 3.0.0 is not supported by this code - upgrade is recommended",call.=FALSE)
}
if (packageVersion("data.table") < "1.9.6") {
  warning("Package 'data.table' Version < 1.9.6 is not tested - upgrade is recommended",call.=FALSE)
}
suppressPackageStartupMessages(library(data.table))

source("Support.R")

###################################################################################
#    Gathers input info from the user                                             #
# First hard-wire key parameters or limits for user input                         #
# This allows user to easily change parameters for their own purposes             #
# - it is impossible to guarantee that NCMP output will be consistent across NMHS #
# Allow a broader range of periods for generating the variograms                  #
# Have maximum distances for binning temperature and precipitation indices,       #
# which will be further constrained by the station separation for small regions   #
# The upper limit seems reasonable, as the shelf is usually reached at a much     #
# shorter distance. But for small countries/regions the fixing of the shelf variance
# will scale the functional form. Kriging constrains the variogram function,      #
# but is it necessary to limit the shelf distance to the domain?                  #
                                                                                  #
ylo <- 1900L                              # earliest possible year for variograms
yhi <- as.POSIXlt(Sys.time())$year + 1899L  # latest possible year == current - 1

DmaxT <- 3000                 # maximum separation (km) for temperature indices
DmaxP <- 2000                 # maximum separation (km) for precipitation indices
w <- 20                       # bin width (km)

cat("Calculating Variograms for all stations with computed indices.",
 "Can change this by editing the file 'A2_Indices/P2_Station_List.txt'.",sep="\n")

inquiry <- function(ylo=1950L,yhi=2020L) {

  x <- 0L                                        # using fixed number of stations
  y1 <- 0L
  yr <- as.POSIXlt(Sys.time())$year + 1900L                        # current year
  y1l <- min(ylo,yr) %/% 10L * 10L + 1L         # modify limit to start of decade
  y2h <- min(yhi,yr)
  yex <- min(y2h-1L,max(1981L,y1l))
  mess <- paste("\n(between ",y1l," and ",y2h-1L,",ex. ",yex,"): ",sep="")
  while (is.na(y1) || y1 < y1l || y1 >= y2h) {
    cat("Enter beginning year to calculate variogram")
    y1 <- readline(mess)
    y1 <- suppressWarnings(as.integer(y1))
  }

  y2 <- 0L
  yex <- max(y1+1L,min(1981L,y2h))
  mess <- paste("\n(between ",y1+1L," and ",y2h,",ex. ",yex,"): ",sep="")
  while (is.na(y2) || y2 <= y1 || y2 > y2h) {
    cat("Enter ending year to calculate variogram")
    y2 <- readline(mess)
    y2 <- suppressWarnings(as.integer(y2))
  }

# 30 minutes for 100 stations would be a VERY slow PC
  cat("Please note that this program is heavily computational.",
      "It may require 30 minutes for one index for 100 stations.",
      "For NCMP 1, Monthly Mean Temperature Anomaly, enter 1.",
      "For NCMP 2, Monthly Total Precipitation Anomaly Normalized, enter 2.",
      "For NCMP 2, Monthly Total Precipitation Anomaly, enter 3.",
      "For NCMP 3, Standardized Precipitation Index, enter 4.",
      "For NCMP 4, Percentage of Warm Days, enter 5.",
      "For NCMP 4, Percentage of Warm Nights, enter 6.",
      "For NCMP 5, Percentage of Cold Days, enter 7.",
      "For NCMP 5, Percentage of Cold Nights, enter 8.",sep="\n")

  ne <- NA_integer_
  while (is.na(ne) || ne < 0L || ne > 8L) {
    ne <- readline("\nEnter the desired NCMP number (between 1 and 8, or 0 for all): ")
    ne <- suppressWarnings(as.integer(ne))
  }

  c(x,y1,y2,ne)
}

# Get the user input and set to local variables
if (interactive()) a <- inquiry(ylo,yhi)
nyb <- a[2]
nye <- a[3]
ne <- a[4]

#    User input collected. Done!
###################################################################################

###################################################################################
#    Creates directories for output files                                         #
# Directories for input NCMP indices - using consistent approach                  #

ncmpn <- c(1L,2L,2L,3L,4L,4L,5L,5L)
folder <- "A2_Indices"
folder2 <- paste("NCMP",ncmpn,sep="")
folder3 <- c("Monthly_Mean_Temp_Anom","Monthly_Total_Prec_Anom_Norm","Monthly_Total_Prec_Anom",
       "Standard_Prec_Index","Warm_Days","Warm_Nights","Cold_Days","Cold_Nights")
ele <- c("TMA","PrAn","PrA","SPI","TX90p","TN90p","TX10p","TN10p") # NCMP index element
dirs <- file.path(folder,folder2,folder3)                # Will add separator "/"

folder <- "A3_Variogram"
filet <- paste(folder,"/NCMP_",ele,"_Graphs.pdf",sep="")          # output graphs
filev <- paste(folder,"/NCMP_",ele,"_Variogram.csv",sep="")   # output variograms
dir.create(folder,showWarnings=FALSE)                          # create directory

#    Directories created. Done!
###################################################################################

###################################################################################
#    Read the station list - use the modified list and all stations               #

files <- read.table("A2_Indices/P2_Station_List.txt",header=TRUE,stringsAsFactors=FALSE)
Station <- files[,"Station"]
nstn <- nrow(files)

#    Read station list. Done!
###################################################################################

###################################################################################
#    Calculate Distances:                                                         #
# Vectorised calculation - generate lat/long for all pairs of stations            #
# The use of cosine distance will generate warnings                               #
# - these can be ignored but need to reset the diagonal (same station) to zero    #

lat1 <- matrix(files[,"Lat"],nrow=nstn,ncol=nstn)
long1 <- matrix(files[,"Long"],nrow=nstn,ncol=nstn)
lat2 <- matrix(files[,"Lat"],nrow=nstn,ncol=nstn,byrow=TRUE)
long2 <- matrix(files[,"Long"],nrow=nstn,ncol=nstn,byrow=TRUE)
D <- distance(lat1,long1,lat2,long2)                     # vectorised calculation
diag(D) <- 0

D <- round(D,2)                                                    # Round matrix
Dmax1 <- round(max(D),-2)  # Maximum distance between stations (rounded to 100km)

#    Finished calculating distances. Done!
###################################################################################

# Output column names (now standard across CSV files)
cnames <- c(month.name,"Annual")

# Reset loop index for doing all indices

if (ne == 0L) ix <- 1:8 else ix <- ne
for (ne in ix) {                                                 # loop for indices

# Constrain distances and define bins accordingly
# Currently setting last bin distance as half width past Dmax

  if (is.element(ncmpn[ne],2:3)) Dmax2 <- DmaxP else Dmax2 <- DmaxT
  Dmax <- min(Dmax1,Dmax2)

  nbin <- as.integer(ceiling(Dmax/w))              # Rounding not actually required
  bin <- (1:nbin)*w                                # Max dist of each bin
  Dl <- bin+w/2                                    # Centre of bin

# Determine which bin each station pair belongs to
  L <- pmin(as.integer(floor(D/w))+1L,nbin)
  dim(L) <- c(nstn,nstn)

###################################################################################
#    Read index data for all stations:                                            #
# Instead of assign/get, define list of tables for each station                   #
                                                                                  #
  cat("Reading data for index",ne,fill=TRUE)                                      #
  NCMP.stn <- vector("list",nstn)                       # Empty list for stations #
  name1 <- paste(dirs[ne],"/",Station,"_",ele[ne],".csv",sep="")                  #
  for (i in 1:nstn) { # 1:nstn                                                    #
    I1 <- read.csv(file=name1[i],header=TRUE,na.strings="-99.9")      # read data #
    if (ne == 2L) I1[,2:14] <- I1[,2:14]/100                                      #
    NCMP.stn[[i]] <- I1                                 # put data into list slot #
  }                                                                # End stn loop #
                                                                                  #
#    Finished reading input data. Done!                                           #
###################################################################################

###################################################################################
#   Calculate differences in indices and assign into appropriate bin              #
# Now doing this outside months for simplicity and speed                          #
# Was storing all station pair differences anyway, so memory is not a factor      #
# Now utilising data.table, supposed to be faster and more efficient              #
# Start by setting up data.tables for each bin                                    #

  Idum <- as.data.table(I1)[0]                                 # dummy data.table
  Y <- vector("list",nbin)                                  # Empty list for bins
  for (i in 1:nbin) Y[[i]] <- Idum                # Empty data.table for each bin

  cat("\t\t Calculating Differences",fill=TRUE)        # Write update in terminal
  yrs <- nyb:nye                                                # vector of years
  cxnames <- paste(cnames,"x",sep=".")
  cynames <- paste(cnames,"y",sep=".")
  for (i in 1:(nstn-1)) {                        # loop through pairs of stations
    I1 <- data.table(NCMP.stn[[i]],key="Year")
    for (j in (i+1):nstn) {
      I12 <- merge(I1,NCMP.stn[[j]])              # Only retain overlapping years
      I12v <- I12[,.SD[is.element(Year,yrs)]]           # Cut to variogram period
      I12vd <- I12v[,cxnames,with=FALSE]-I12v[,cynames,with=FALSE]   # Difference
      names(I12vd) <- cnames                    # consistency with bin data.table
      Lij <- L[i,j]
      Y[[Lij]] <- rbind(Y[[Lij]],cbind(I12v[,.(Year)],I12vd))    # Add to bin Lij
    }                                                                # End j loop
    }                                                                # End i loop
  nrec <- sapply(Y,nrow)                          # Number of records in each bin 

#    Finished calculating differences in indices. Done!                           #
###################################################################################

# Set up Variogram output table for all months
# This is the information that will be written to file for kriging

  X <- data.frame(Month=cnames,Function=NA_character_,
     n=NA_real_,r=NA_real_,s=NA_real_,"Mean Sq Err"=NA_real_,
     check.names=FALSE,stringsAsFactors=FALSE)
  Graph <- c("Gaussian","Exponential","Spherical")  # Which variogram function to use

# Set specifications for graphs
# These will be plotted on a single page
  pdf(filet[ne])
  par(mfrow=c(5,3),mar=c(3,4,1,0),oma=c(1,1,1,1),cex=0.5,mgp=c(1.5,0.4,0),tcl=-0.2)

  for (nm in 1:13) { #1:13
    cat(nm,"\t",cnames[nm],fill=TRUE)                     # Write month in terminal

###################################################################################
#   Calculate data points of the variogram, and plot it
# No need to do this if do not have any data in the bin, but account for missing

    cat("\t\t Fitting Variogram",fill=TRUE)            # Write update in terminal
    Bl <- rep(NA,nbin)                                # Empty vector for bin avgs
    for (k in which(nrec > 0L)) {                   # Loop through non-empty bins
      b2 <- Y[[k]][,nm+1,with=FALSE]^2            # Data^2 for this bin and month
      Bl[k] <- mean(b2,na.rm=TRUE)                        # assign into vector Bl
    }                                                              # End bin loop
    Bl <- round(Bl,3)                                           # round Bl (why?)

    plot(Dl,Bl,xlab="Distance",ylab="Diff in Index",col="Blue")  # Plot Dl and Bl
    title(cnames[nm],line=0.5)

#    Finished plotting variogram. Done!
###################################################################################

###################################################################################
#    Fit functional variogram to plot
# If there are a lot of pairs in the last bin, should this be the shelf variance?
# What are the consequences of fixing shelf variance against fitting?
# Why are different start values of "a" used, and what is the impact of this?
# Why is fitting the Spherical function excluded from annual data?

    s1 <- mean(Bl[as.integer(.6*nbin):as.integer(.8*nbin)],na.rm=TRUE)#Find avg of 60%-80%
    s2 <- mean(Bl[as.integer(.8*nbin):nbin],na.rm=TRUE) #Find avg of last 20% of bins
    s0 <- max(s1,s2)

# nls fits Bl to function(Dl,a,b,s) by changing parameters a and b and fixing s

    mod1 <- nls(Bl~Gaussian(Dl,a,b,c),start=list(a=0.01,b=600,c=s0),
       control=nls.control(warnOnly=TRUE,maxiter=100))
    cmod1 <- coef(mod1)

    mod2 <- nls(Bl~Exponential(Dl,a,b,c),start=list(a=0.01,b=400,c=s0),
       control=nls.control(warnOnly=TRUE,maxiter=100))
    cmod2 <- coef(mod2)

    mod3 <- nls(Bl~Spherical(Dl,a,b,c),start=list(a=0.01,b=800,c=s0),
       control=nls.control(warnOnly=TRUE,maxiter=100))
    cmod3 <- coef(mod3)

#copy fitted coefficients to n,r and s
    n <- c(cmod1[1],cmod2[1],cmod3[1])
    r <- c(cmod1[2],cmod2[2],cmod3[2])
    s <- c(cmod1[3],cmod2[3],cmod3[3])

# Calculate mean squared error for each fit
    E <- rep(Inf,3)
    E[1] <- sum((Bl - Gaussian(Dl,n[1],r[1],s[1]))^2,na.rm=TRUE)
    E[2] <- sum((Bl - Exponential(Dl,n[2],r[2],s[2]))^2,na.rm=TRUE)
    E[3] <- sum((Bl - Spherical(Dl,n[3],r[3],s[3]))^2,na.rm=TRUE)

# Keep name and parameters of best fit
    k <- which.min(E)
    X[nm,"Function"] <- Graph[k]
    X[nm,"n"] <- n[k]
    X[nm,"r"] <- r[k]
    X[nm,"s"] <- s[k]
    X[nm,"Mean Sq Err"] <- E[k]

#plot all fitted variograms
    curve(get(Graph[1])(x,n[1],r[1],s[1]),col="green",add=TRUE)
    curve(get(Graph[2])(x,n[2],r[2],s[2]),col="green",add=TRUE)
    curve(get(Graph[3])(x,n[3],r[3],s[3]),col="green",add=TRUE)
#highlight best fit in red
    curve(get(Graph[k])(x,n[k],r[k],s[k]),col="red",add=TRUE)

#    Fit functional variogram. Done!
###################################################################################

  }                                                       #Ends loop for month (nm)

  write.csv(X,file=filev[ne],row.names=FALSE)
  dev.off()                                                # plotting specification
}                                                     # Ends loop for indices (ne)

dy <- date()
mess <- c(paste("These Variograms are calculated using the period",nyb,"to",nye),
          paste("Calculation was completed at",dy))
writeLines(mess,file.path(folder,"Variogram_period.txt"))
cat("Variograms Done!",fill=TRUE)
