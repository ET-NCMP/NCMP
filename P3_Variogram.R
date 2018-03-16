###################################################################################
#                                                                                 #
#    The R-NCMPs package has been developed by the ET-NCMP.                       #
#    P3_Variogram.R                                                               #
#                                                                                 #
#    This program calculates the variogram for 8 different indices.               #
#    For further details please refer to the User Manual.                         #
#                                                                                 #
#    Programmers:                                                                 #
#    Megan Hartwell, McMaster University, Canada                                  #
#    Lucie Vincent, Environment and Climate Change Canada                         #
#    December 2016                                                                #
#    Modified by Simon Grainger, Bureau of Meteorology, Australia                 #
#    February 2017 - Cleaned up, code is more "R-like"                            #
#    Modified by John Kennedy, Met Office, UK                                     #
#    June 2017 - overhauled to make some decisions more consistent and added      #
#    precip anomaly as one of the variables that can be variogrammed. Moved       #
#    function definitions to a support script                                     #
#                                                                                 #
###################################################################################

cat("***** P3_Variogram.R *****",fill=TRUE)

# Suppress warning messages
# - these should be related to the version of R used to build the package on PC, and can be ignored

op <- options(warn=-1)
suppressPackageStartupMessages(library(data.table))
source("Support_Variogram.R")
source("Support_Configuration.R")
cat("Successfully loaded packages",fill=TRUE)

# An effective check of whether can run the code is to load the configuration file
# If it does not exist, the relevant script has not been successfully run

clist.P2 <- read_configuration("P2")

###################################################################################
# Set variables with key thresholds for estimating variogram functions            #
# These are values which have been determined by the ET-NCMP for the purposes of  #
# generating NCMPs and may differ from other standards or guidlines               #
# There is currently no minimum threshold for the period to estimate the          #
# variogram, but testing indicates that short sample periods are problematic      #
# The ET-NCMP recommendation is to use the climatological period                  #
# ***** DO NOT CHANGE THE VALUE OF THESE VARIABLES *****                          #
###################################################################################

stnhi <- clist.P2$nstn  # maximum number of stations
yrlo <- 1900L            # earliest possible year for variograms
yrhi <- as.POSIXlt(Sys.time())$year + 1899L  # latest possible year == current - 1
yvlo <- clist.P2$nyb    # recommended start year of variogram period
yvhi <- clist.P2$nye    # recommended end year of variogram period
DmaxT <- 3000           # maximum separation (km) for temperature indices
DmaxP <- 2000           # maximum separation (km) for precipitation indices
w <- 20                 # bin width (km)

###################################################################################
#    Gathers input info from the user                                             #
# For clarity, no longer do this as a separate function                           #
###################################################################################

cat("Calculating Variograms from Station monthly series for selected NCMP indices",fill=TRUE)

# Number of stations to process
# Can exclude stations at the end of the list, but arbitrary selection requires
# editing of 'P2_Station_List.txt'
# Still suppressing warning messages - about converting strings to integer
	
cat("Can either use the first 'n' stations processed by 'P2_Indices.R' or all of them",fill=TRUE)
nstn <- NA_integer_
mess <- paste("\nbetween 1 and ",stnhi,", or 0 for all - recommended =",stnhi,": ")
while (is.na(nstn) || nstn < 0L || nstn > stnhi) {
  cat("Enter the number of stations to use")
  nstn <- readline(mess)
  nstn <- as.integer(nstn)
}
if (nstn == 0L) nstn <- stnhi

# Generate limits for variogram period
# Allow the full range of years but effectively recommend the climatological period
# Without a minimum threshold, the user can generate contradictory messages

cat("The ET-NCMP recommends a variogram period of",yvlo,"-",yvhi,fill=TRUE)
yr1 <- yrlo
yr2 <- yrhi
nyb <- 0L
mess <- paste("\nbetween",yr1,"and",yr2,"- recommended =",yvlo,": ")
while (is.na(nyb) || nyb < yr1 || nyb > yr2) {
  cat("Enter beginning year for variogram period")
  nyb <- readline(mess)
  nyb <- as.integer(nyb)
}

yr1 <- nyb
yr2 <- yrhi
nye <- 0L
mess <- paste("\nbetween",yr1,"and",yr2,"- recommended =",yvhi,": ")
while (is.na(nye) || nye < yr1 || nye > yr2) {
  cat("Enter ending year for variogram period")
  nye <- readline(mess)
  nye <- as.integer(nye)
}

# Which diagnostic to compute
# Removed the option to process all diagnostics in one run of the script

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
while (is.na(ne) || ne < 1L || ne > 8L) {
  ne <- readline("\nEnter the desired NCMP number (between 1 and 8) : ")
  ne <- as.integer(ne)
}
cat("User input collected",fill=TRUE)

# Turn warnings back on, but print immediately
options(warn=1)

###################################################################################
#    Creates directories for output files                                         #
# Directories for input NCMP indices - using consistent approach                  #
###################################################################################

ncmpn <- c(1L,2L,2L,3L,4L,4L,5L,5L)
folder <- "A2_Indices"
folder2 <- paste("NCMP",ncmpn,sep="")
folder3 <- c("Monthly_Mean_Temp_Anom","Monthly_Total_Prec_Anom_Norm","Monthly_Total_Prec_Anom",
       "Standard_Prec_Index","Warm_Days","Warm_Nights","Cold_Days","Cold_Nights")
ele <- c("TMA","PrAn","PrA","SPI","TX90p","TN90p","TX10p","TN10p") # NCMP index element
dirs <- file.path(folder,folder2,folder3)  # adds separator "/"

# Output variograms

folder <- "A3_Variogram"
filet <- file.path(folder,paste("NCMP",ele,"Graphs.pdf",sep="_"))     # output graphs
filev <- file.path(folder,paste("NCMP",ele,"Variogram.csv",sep="_"))  # output variograms
dir.create(folder,showWarnings=FALSE)                          # create directory

# Output column names (now standard across CSV files)
cnames <- c(month.name,"Annual")

###################################################################################
#    Read the modified station list                                               #
###################################################################################
# With the number of stations already set, extract the subset to use

namex <- file.path("A2_Indices","P2_Station_List.txt")
files <- read.table(namex,header=TRUE,stringsAsFactors=FALSE)
Station <- files[1:nstn,"Station"]
olats <- files[1:nstn,"Lat"]
olons <- files[1:nstn,"Long"]

###################################################################################
#    Calculate Distances:                                                         #
# Vectorised calculation - generate lat/long for all pairs of stations            #
# The use of cosine distance will generate warnings                               #
# - these can be ignored but need to reset the diagonal (same station) to zero    #
###################################################################################

lat1 <- matrix(olats,nrow=nstn,ncol=nstn)
long1 <- matrix(olons,nrow=nstn,ncol=nstn)
lat2 <- matrix(olats,nrow=nstn,ncol=nstn,byrow=TRUE)
long2 <- matrix(olons,nrow=nstn,ncol=nstn,byrow=TRUE)
D <- distance(lat1,long1,lat2,long2)
diag(D) <- 0
D <- round(D,2)
cat("Calculated distances between stations",fill=TRUE)

# Constrain distances and define bins accordingly

Dmax1 <- round(max(D),-2)  # Maximum distance between stations (rounded to 100km)
if (is.element(ncmpn[ne],2:3)) Dmax2 <- DmaxP else Dmax2 <- DmaxT
Dmax <- min(Dmax1,Dmax2)

# Currently setting last bin distance as half width past Dmax

nbin <- as.integer(ceiling(Dmax/w))  # Rounding not actually required
bin <- (1:nbin)*w                    # Max dist of each bin
Dl <- bin+w/2                        # Centre of bin

# Determine which bin each station pair belongs to

L <- pmin(as.integer(floor(D/w))+1L,nbin)
dim(L) <- c(nstn,nstn)

###################################################################################
#    Read index data for all stations:                                            #
###################################################################################
# Converting PrAn from percentage back to ratio
# This also apparently works for PrA (ne == 3), although it would be preferable
# if there was something less arbitrary 

NCMP.stn <- vector("list",nstn)
namex <- paste(dirs[ne],"/",Station,"_",ele[ne],".csv",sep="")
for (i in 1:nstn) {
  I1 <- read.csv(file=namex[i],header=TRUE,na.strings="-99.9")
  if (ne == 2L) I1[,2:14] <- I1[,2:14]/100
  if (ne == 3L) I1[,2:14] <- I1[,2:14]/100 # scale PrA so that matrix inversion is clean
  NCMP.stn[[i]] <- I1
}
cat("Finished reading data for",ele[ne],fill=TRUE)

###################################################################################
#   Calculate differences in indices and assign into appropriate bin              #
# Now doing this outside months for simplicity and speed                          #
# Was storing all station pair differences anyway, so memory is not a factor      #
# Now utilising data.table, supposed to be faster and more efficient              #
###################################################################################
# Set up variables for using data.table with the station pair loop

yrs <- nyb:nye
cxnames <- paste(cnames,"x",sep=".")  # column names for first station
cynames <- paste(cnames,"y",sep=".")  # column names for second station
Y <- vector("list",nbin)              # empty list for bins
Idum <- as.data.table(I1)[0]          # dummy data.table
for (i in 1:nbin) Y[[i]] <- Idum      # empty data.table for each bin

# Loop through pairs of stations
# For each pair, retain overlapping years only, then cut to variogram period
# Can then calculate differences and add to the appropriate bin

for (i in 1:(nstn-1)) {
  I1 <- data.table(NCMP.stn[[i]],key="Year")
  for (j in (i+1):nstn) {
    I12 <- merge(I1,NCMP.stn[[j]])
    I12v <- I12[,.SD[is.element(Year,yrs)]]
    I12vd <- I12v[,cxnames,with=FALSE]-I12v[,cynames,with=FALSE]
    names(I12vd) <- cnames  # consistency with bin data.table
    Y[[L[i,j]]] <- rbind(Y[[L[i,j]]],cbind(I12v[,.(Year)],I12vd))
  }
} 
nrec <- sapply(Y,nrow)  # Number of records in each bin 
cat("Finished calculating station differences",fill=TRUE)

# Set up Variogram output table for all months
# This is the information that will be written to file for kriging

X <- data.frame(Month=cnames,Function=NA_character_,
       n=NA_real_,r=NA_real_,s=NA_real_,"Mean Sq Err"=NA_real_,
       check.names=FALSE,stringsAsFactors=FALSE)
Graph <- c("Gaussian","Exponential","Spherical")  # Which variogram function to use

###################################################################################
# Begins loop over months                                                         #
###################################################################################
# Set specifications for graphs
# These will be plotted on a single page - have modified the margins

pdf(filet[ne])
#par(mfrow=c(5,3),mar=c(3,4,1,0),oma=c(1,1,1,1),cex=0.5,mgp=c(1.5,0.4,0),tcl=-0.2)
par(mfrow=c(5,3),mar=c(2.5,4,1.5,0)+0.1,cex=0.5,mgp=c(1.5,0.4,0),tcl=-0.2)

for (nm in 1:13) {
  cat(nm,"\t",cnames[nm],fill=TRUE)

###################################################################################
# Calculate mean of binned values for variogram fitting                           #
###################################################################################
# No need to do this if do not have any data in the bin, but account for missing values
# Have commented out rounding of the mean binned value

  Bl <- rep(NA,nbin)  # empty vector for bin avgs     
  for (k in which(nrec > 0L)) {
    b2 <- Y[[k]][,nm+1,with=FALSE]^2   # Data^2 for this bin and month
    Bl[k] <- mean(b2,na.rm=TRUE)       # assign into vector Bl
  }
#  Bl <- round(Bl,3)

###################################################################################
# Fit functional variogram to plot                                                #
# There are a number of options which could still be considered at this point     #
# However, testing has found that the major stability issues are                  #
# too few years (< 20?) or too few (< 10?) stations                               #
# The current configuration solves for n, r and s from plausible start values     #
# and apply an iterative loop to set the maximum range <= Dmax                    #
###################################################################################

  cat("Fitting Variogram",fill=TRUE)

# Set up start values for each parameter
# s0 == the highest of the mean value of 60-80 and 80-100 percentile bins
# TEST: Define n0 from binned data - mean over 0-20 percentile bins
# TEST: r0 == 0.5*maximum range (within 250 - 1000 km) - same for each station

  n0 <- max(0.01,0.95*mean(Bl[1:as.integer(.2*nbin)],na.rm=TRUE)) 
  s1 <- mean(Bl[as.integer(.6*nbin):(as.integer(.8*nbin)-1L)],na.rm=TRUE)
  s2 <- mean(Bl[as.integer(.8*nbin):nbin],na.rm=TRUE) 
  s0 <- max(s1,s2)
  r0 <- max(250,min(1000,0.5*Dmax))
  clist <- list(n=n0,r=r0,s=s0)

#  clist <- list(n=0.01,r=600,s=s0)
  mod1 <- nls(Bl~Gaussian(Dl,n,r,s),start=clist,
    control=nls.control(warnOnly=TRUE,maxiter=100))
  cmod1 <- coef(mod1)

# clist$r <- 400
  mod2 <- nls(Bl~Exponential(Dl,n,r,s),start=clist,
    control=nls.control(warnOnly=TRUE,maxiter=100))
  cmod2 <- coef(mod2)

# clist$r <- 800
  mod3 <- nls(Bl~Spherical(Dl,n,r,s),start=clist,
    control=nls.control(warnOnly=TRUE,maxiter=100))
  cmod3 <- coef(mod3)

# Copy fitted coefficients to n,r and s

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

# Plot binned data and best fit variogram
# Commented out curves for all fitted variograms

  plot(Dl,Bl,xlab="Distance",ylab="Diff in Index",col="Blue")  # Plot Dl and Bl
  title(cnames[nm],line=0.5)
#  curve(get(Graph[1])(x,n[1],r[1],s[1]),col="green",add=TRUE)
#  curve(get(Graph[2])(x,n[2],r[2],s[2]),col="green",add=TRUE)
#  curve(get(Graph[3])(x,n[3],r[3],s[3]),col="green",add=TRUE)
# Highlight best fit in red
  curve(get(Graph[k])(x,n[k],r[k],s[k]),col="red",add=TRUE)
}
dev.off()  # Close PDF file

###################################################################################
# End loop of months                                                              #
###################################################################################
  
###################################################################################
#    Write best fit variogram information                                         #
###################################################################################

cat("Writing results",fill=TRUE)
write.csv(X,file=filev[ne],row.names=FALSE)

# It is probably useful to retain a "configuration" file for variograms

namex <- file.path(folder,"P3_Configuration.txt")
dy <- date()
desc <- c("Date of processing","Number of stations",
  "Start of variogram period","End of variogram period")
mess <- paste(desc,c(dy,nstn,nyb,nye),sep=" = ")  # Variables are converted to strings
writeLines(mess,con=namex)

cat("Variograms Done!",fill=TRUE)
options(op)
