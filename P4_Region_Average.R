###################################################################################
#                                                                                 #
#    The R-NCMPs package has been developed by the ET-NCMP.                       #
#    P4_Region_Average.R                                                          #
#                                                                                 #
#    This program calculates the regional average of 8 different indices.         #
#    For further details please refer to the User Manual.                         #
#                                                                                 #
#    Programmers:                                                                 #
#    Megan Hartwell, McMaster University, Canada                                  #
#    Lucie Vincent, Environment and Climate Change Canada                         #
#    December 2016                                                                #
#    Modified by Simon Grainger, Bureau of Meteorology, Australia                 #
#    February 2017 - Cleaned up, code is more "R-like"                            #
#    Modified by John Kennedy, Met Office, UK                                     #
#    June 2017 - move function defs to support sript, added precip anomaly        #
###################################################################################

cat("***** P4_Region_Average.R *****",fill=TRUE)

# Suppress warning messages
# - these should be related to the version of R used to build the package on PC, and can be ignored

op <- options(warn=-1)
suppressPackageStartupMessages(library(maptools))
data(wrld_simpl)
source("Support_Variogram.R")
source("Support_Configuration.R")
cat("Successfully loaded packages",fill=TRUE)

# An effective check of whether can run the code is to load the configuration file
# If it does not exist, the relevant script has not been successfully run

clist.P2 <- read_configuration("P2")

###################################################################################
# Set variables with key thresholds for region averages                           #
# These are values which have been determined by the ET-NCMP for the purposes of  #
# generating NCMPs and may differ from other standards or guidlines               #
# ***** DO NOT CHANGE THE VALUE OF THESE VARIABLES *****                          #
###################################################################################

stnhi <- clist.P2$nstn  # maximum number of stations
yrlo <- 1900L           # earliest possible year for analysis period
yrhi <- as.POSIXlt(Sys.time())$year + 1899L # latest possible year == current - 1
ytlo <- 1950L           # suggested start year for regional average period
DmaxT <- 3000           # maximum separation (km) for temperature indices
DmaxP <- 2000           # maximum separation (km) for precipitation indices

###################################################################################
#    Gathers input info from the user                                             #
# Check if a configuration file has been created, and if the user wants to use it #
# This enables once-only definition of region and analysis period                 #
# For clarity, no longer do this as a separate function                           #
###################################################################################
# Still suppressing warning messages - about converting strings to numeric

cat("Calculate Country or Regional Averages from NCMP station indices",fill=TRUE)

folder <- "A4_Region_Average"
dir.create(folder,showWarnings=FALSE)

namex <- file.path(folder,"P4_Configuration.txt")
if (file.exists(namex)) {
  icheck <- NA_integer_
  while (is.na(icheck) || icheck < 0L || icheck > 1L) {
    icheck <- readline("\nUse existing configuration file? (0 = No, 1 = Yes) : ")
    icheck <- as.integer(icheck)
  }
} else {
  icheck <- 0L
}

# If have, and want to use, the configuration file, read this in now
# Extract the required variables from the returned list

if (icheck == 1L) {
  clist <- read_configuration("P4")
  nstn <- clist$nstn
  nyb <- clist$nyb
  nye <- clist$nye
  tcode <- clist$tcode
  tname <- clist$tname
  res <- clist$res
  uncode2 <- clist$uncode2
} else {

# Otherwise need to generate this from scratch
# Can exclude stations at the end of the list, but arbitrary selection requires
# editing of 'P2_Station_List.txt'
# Still suppressing warning messages - about converting strings to integer

  cat("\nCan either use the first 'n' stations processed by 'P2_Indices.R' or all of them",fill=TRUE)
  nstn <- NA_integer_
  mess <- paste("\nbetween 1 and ",stnhi,", or 0 for all - recommended =",stnhi,": ")
  while (is.na(nstn) || nstn < 0L || nstn > stnhi) {
    cat("Enter the number of stations to use")
    nstn <- readline(mess)
    nstn <- as.integer(nstn)
    }
  if (nstn == 0L) nstn <- stnhi

# Allow a broad range of analysis periods, but suggest running up to last year

  mess <- paste("\nbetween",yrlo,"and",yrhi,"- suggested =",ytlo,": ")  
  nyb <- 0L
  while (is.na(nyb) || nyb < yrlo || nyb > yrhi) {
    cat("\nEnter beginning year for region average")
    nyb <- readline(mess)
    nyb <- as.integer(nyb)
  }

  mess <- paste("\nbetween",nyb,"and",yrhi,"- suggested =",yrhi,": ")  
  nye <- 0L
  while (is.na(nye) || nye < nyb || nyb > yrhi) {
    cat("Enter ending year for region average")
    nye <- readline(mess)
    nye <- as.integer(nye)
  }

# Get name of country or region to calculate for
# The easiest identifier is the ISO 3166-1 3 letter code, but internally use the UN code
# Currently not considering the region and subregion numeric codes  
  
  cat("\nEnter one of the following options for a Country or Region:",
      "\tISO 3166-1 three (3) letter Country code",
      "\tUN digital Country code or 0 for regional average",
      "\tSee 'www.nationsonline.org/oneworld/countrycodes.htm'",sep="\n")

# The first check generates a valid ISO 3166-1 code

  tcode <- NA_character_

  while (!is.element(tcode,wrld_simpl@data[,"ISO3"]) && !is.element(tcode,c("REG"))) {
      tcode <- readline("\nEnter Country or Region to process: ")

      if (tcode == 0L) {
          tcode = "REG"
          uncode2 = "0"
          tname = "Region"
      } else {
          UNcode <- as.integer(tcode)
          if (!is.na(UNcode)) {
     	      print("Numeric code entered")
              ix <- which(is.element(wrld_simpl@data[,"UN"],UNcode))
              if (length(ix) == 1L) tcode   <- levels(wrld_simpl@data[,"ISO3"])[wrld_simpl@data[ix,"ISO3"]]
	      if (length(ix) == 1L) uncode2 <- wrld_simpl@data[ix,"UN"]
	      if (length(ix) == 1L) tname   <- levels(wrld_simpl@data[,"NAME"])[wrld_simpl@data[ix,"NAME"]]
          } else {
	      print("AlphaNumeric code entered")
              ix <- which(is.element(wrld_simpl@data[,"ISO3"],tcode))
              if (length(ix) == 1L) tcode   <- levels(wrld_simpl@data[,"ISO3"])[wrld_simpl@data[ix,"ISO3"]]
	      if (length(ix) == 1L) uncode2 <- wrld_simpl@data[ix,"UN"]
	      if (length(ix) == 1L) tname   <- levels(wrld_simpl@data[,"NAME"])[wrld_simpl@data[ix,"NAME"]]
          }     
      }   
  }

  cat("Current Country/Region name =",tname,fill=TRUE)
  tname2 <- readline("\nType new name or press 'Enter' to retain : ")
  if (nchar(tname2) > 0L) tname <- tname2
  tname <- gsub("[[:space:]]|[[:punct:]]","_",tname)  # Replace punctuation and spaces
  
# Select possible grid resolutions - limit to actual options
# It is not clear if a recommendation can be given
# This text will be changed to include approximate km

  gvals <- c(0.1,0.25,0.5,1,2)
  gs <- paste(gvals,collapse=", ")  
  cat("\nChoose a grid spacing for the interpolation.",
      paste("Allowed grid spacings are",gs,"degrees."),
      "1 degree corresponds to around 100km.",
      "Smaller grid spacings will give a more accurate result",
      "but will run more slowly.",sep="\n")
  mess <- paste("\nEnter the desired grid resolution",gs,": ")
  res <- NA_real_ 
  while (!is.element(res,gvals)) {
    res <- readline(mess)
    res <- as.numeric(res)
  }

# Can now write the configuration file  
  
  dy <- date()
  desc <- c("Date of processing","Number of stations",
    "Start of analysis period","End of analysis period",
    "ISO 3166-1 Country code","Country/Region name","Grid resolution",
    "UN Code number")
  mess <- paste(desc,c(dy,nstn,nyb,nye,tcode,tname,res,uncode2),sep=" = ")
  writeLines(mess,con=namex)
}

# Which diagnostic to compute
# Removed the option to process all diagnostics in one run of the script

cat("\nPlease note that this program is heavily computational.",
    "It may require one hour for one index for 100 stations.",
    "For NCMP 1, Monthly Mean Temperature Anomaly, enter 1.",
    "For NCMP 2, Monthly Total Precipitation Anomaly Normalized, enter 2.",
    "For NCMP 2, Monthly Total Precipitation Anomaly , enter 3.",
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

###################################################################################
#    Creates directories for output files                                         #
###################################################################################
# Directories for input NCMP indices - using consistent approach

ncmpn <- c(1L,2L,2L,3L,4L,4L,5L,5L)
folder <- "A2_Indices"
folder2 <- paste("NCMP",ncmpn,sep="")
folder3 <- c("Monthly_Mean_Temp_Anom","Monthly_Total_Prec_Anom_Norm","Monthly_Total_Prec_Anom",
             "Standard_Prec_Index","Warm_Days","Warm_Nights","Cold_Days","Cold_Nights")
ele <- c("TMA","PrAn","PrA","SPI","TX90p","TN90p","TX10p","TN10p")  # NCMP index element
idirs <- file.path(folder,folder2,folder3)  # adds separator "/"

# Input variograms

filev <- file.path("A3_Variogram",paste("NCMP_",ele,"_Variogram.csv",sep=""))

# Output folders

folder <- "A4_Region_Average"
folder2 <- c("Grid_Squares","Region_Annual")
dirs <- file.path(folder,folder2)
for (dname in dirs) dir.create(dname,showWarnings=FALSE,recursive=TRUE)

# Set a place holder file name for the grid squares
# - will replace with the actual month name 'cnames' (and update if looping over indices)

filez <- file.path(folder,paste(tname,ele,"Region_Avg.csv",sep="_"))
filea <- file.path(folder,folder2[2],paste(tname,ele,"Avg",res,"Annual.csv",sep="_"))
filer <- file.path(folder,folder2[2],paste(tname,ele,"Avg",res,"Annual_Rank.csv",sep="_"))
fileg <- file.path(folder,folder2[1],paste(tname,ele[ne],nyb:nye,"MONTH.csv",sep="_"))
cnames <- c(month.name,"Annual")

# New user input to make output of gridded data optional
# These would be more compact if used netCDF files (or similar)

cat("\nGrid square data files may use large amounts of disk space",fill=TRUE)
igrid <- NA_integer_
while (is.na(igrid) || igrid < 0L || igrid > 1L) {
  igrid <- readline("\nWrite grid square files? (0 = No, 1 = Yes) : ")
  igrid <- as.integer(igrid)
}
cat("Thank you. User input collected",fill=TRUE)

# Turn warnings back on, but print immediately
options(warn=1)

###################################################################################
#    Read the modified station list                                               #
###################################################################################
# With the number of stations already set, extract the subset to use

files <- read.table("A2_Indices/P2_Station_List.txt",header=TRUE,stringsAsFactors=FALSE)
Station <- files[1:nstn,"Station"]
olats <- files[1:nstn,"Lat"]
olons <- files[1:nstn,"Long"]

###################################################################################
#    Set up grid squares                                                          #
# This will be different as round(x,0) rounds rather than truncates towards zero  #
# The land grid point check requires that longitudes are in the range -180 to 180 #
# Have enforced this in the modified station file,                                #
# but need to consider the case that they cross the dateline (ie Pacific Ocean)   #
###################################################################################

latmin <- floor(min(olats))   # min station latitude, rounded down
latmax <- ceiling(max(olats)) # max station latitude, rounded up
latr <- latmax-latmin         # range of latitudes
lngmin <- floor(min(olons))   # min station longitude, rounded down
lngmax <- ceiling(max(olons)) # max station longitude, rounded up
lngr <- lngmax-lngmin         # range of longitudes
if (lngr > 180) {
  lngr <- 360-lngr
  lngmin <- lngmax
}

# Set size of squares in degrees (now user-set)

slng <- res
slat <- res
xlng <- as.integer(ceiling(lngr/slng))  # All station longitudes
xlat <- as.integer(ceiling(latr/slat))  # All station latitudes
gridsq <- xlng*xlat  # Number of grid squares

# Get latitude and longitude for all grid squares
# This requires repeating the individual values

lata <- latmin+slat*(0:xlat)                   # vector of latitude edges
lnga <- lngmin+slng*(0:xlng)                   # vector of longitude edges
latn <- rep(lata[-(xlat+1)],times=xlng)        # min lat of each grid square
latx <- rep(lata[-1],times=xlng)               # max lat of each grid square
lngn <- rep(lnga[-(xlng+1)],each=xlat)         # min long of each grid square
lngx <- rep(lnga[-1],each=xlat)                # max long of each grid square
latc <- rep(lata[-(xlat+1)]+slat/2,times=xlng) # centre lat of each grid square
lngc <- rep(lnga[-(xlng+1)]+slng/2,each=xlat)  # centre long of each grid square

d1 <- distance(latn,lngn,latn,lngx)  # length of southern edge of each grid square
d2 <- distance(latx,lngn,latx,lngx)  # length of northern edge of each grid square
d3 <- distance(latn,lngc,latx,lngc)  # height of each grid square
A <- (d1+d2)/2*d3                    # area (km^2) of each grid square

Dsq <- cbind(Grid=1:gridsq,latn,latx,lngn,lngx,Lat=latc,Long=lngc,Area=A)

# Find which grid square each station is in
# This may be useful as a backup if no land grid squares are found
# Need to allow for crossing the Dateline - should work but not tested
# Vectorised the calculation, although this is harder to follow

olons[olons < lngmin] <- olons[olons < lngmin]+360
ilat <- lapply(olats,function(x) which(Dsq[,2] <= x & Dsq[,3] > x))
ilon <- lapply(olons,function(x) which(Dsq[,4] <= x & Dsq[,5] > x))
c1 <- mapply(intersect,ilat,ilon)
if (!is.vector(c1,mode="integer"))
  {stop("Error calculating grid squares for stations",call.=FALSE)}
cat("Grid squares established for bounding region",fill=TRUE)

###################################################################################
#    Find which grid squares are on land and in our country                       #
# This has now been tested for generating a regional average                      #
# Consider extending this to ensure that include grid squares with stations       #
# - this may make more sense for regions, for example                             #
###################################################################################
# Convert centre of grid squares to a SpatialPoints object
# Reset longitude range to -180 to 180 if have crossed the Dateline

lngc[lngc >= 180] <- lngc[lngc >= 180]-360
L <- cbind(lngc,latc)
pts <- SpatialPoints(L,proj4string=CRS(proj4string(wrld_simpl)))

# Which grid points are inside a land map database, and what are their UN codes?
# Country sqs == land point and matching UN code
# Region sqs == any land points within grid box

country <- over(pts,wrld_simpl)$UN
land <- !is.na(country)
if (tcode == "REG") {
  sqs <- land
} else {
  sqs <- (land & country == uncode2)
}
cat("Established land grid squares for",tname,fill=TRUE)

###################################################################################
#    Calculate Distances:                                                         #
###################################################################################
# Vectorised calculation - generate lat/long for all pairs of stations
# The use of cosine distance will generate warnings
# - these can be ignored but need to reset the diagonal (same station) to zero

lat1 <- matrix(olats,nrow=nstn,ncol=nstn)
long1 <- matrix(olons,nrow=nstn,ncol=nstn)
lat2 <- matrix(olats,nrow=nstn,ncol=nstn,byrow=TRUE)
long2 <- matrix(olons,nrow=nstn,ncol=nstn,byrow=TRUE)
D <- distance(lat1,long1,lat2,long2)  # vectorised calculation
diag(D) <- 0
D <- round(D,2)  # Round distances to 2dp

# Distances between stations and grid square centres
# No checking whether a station location is on a centre - would also give a warning

lat1 <- matrix(olats,nrow=nstn,ncol=gridsq)
long1 <- matrix(olons,nrow=nstn,ncol=gridsq)
lat2 <- matrix(Dsq[,6],nrow=nstn,ncol=gridsq,byrow=TRUE)
long2 <- matrix(Dsq[,7],nrow=nstn,ncol=gridsq,byrow=TRUE)
Dio <- distance(lat1,long1,lat2,long2)
Dio <- round(Dio,2)
cat("Calculated distances between stations and to grid square centres",fill=TRUE)

# Maximum distance of 2000 km for precipiation indices, 3000 km for temperature
# Reduced further by actual maximum separation for small networks,
# but what about large areas, including "regions"?
# Use this to restrict distance between stations and to grid squares
# Is this the most appropriate way to limit the weighting for remote stations?

Dmax1 <- round(max(D),-2)  # Maximum distance between stations (rounded to 100km)
if (is.element(ncmpn[ne],2:3)) Dmax2 <- DmaxP else Dmax2 <- DmaxT

Dmax <- min(Dmax1,Dmax2)
Dn <- pmin(D,Dmax)
Dion <- pmin(Dio,Dmax)

nyrs <- nye-nyb+1L                                     # variable number of years
NCMP <- matrix(NA,nrow=nyrs,ncol=13)   # empty matrix for region avg, dim nyrsx12
NT <- matrix(0L,nrow=nyrs,ncol=13)                     # empty matrix dim nyrsx12

###################################################################################
#    Read variogram:                                                              #
###################################################################################
var <- read.csv(file=filev[ne],header=TRUE,stringsAsFactors=FALSE)

###################################################################################
#    Read index data for all stations:                                            #
# Instead of assign/get, define list of tables for each station                   #
###################################################################################
# Converting PrAn from percentage back to ratio

NCMP.stn <- vector("list",nstn)
namex <- paste(idirs[ne],"/",Station,"_",ele[ne],".csv",sep="")
for (i in 1:nstn) {
  I1 <- read.csv(file=namex[i],header=TRUE,na.strings="-99.9")
  if (ne == 2L) I1[,2:14] <- I1[,2:14]/100
  if (ne == 3L) I1[,2:14] <- I1[,2:14]/100  #scale to ensure matrix inversion behaves well
  NCMP.stn[[i]] <- I1
  }
cat("Finished reading data for",ele[ne],fill=TRUE)

###################################################################################
# Begins loop over months                                                         #
###################################################################################

  for (nm in 1:13) {
    cat(nm,"\t",cnames[nm],fill=TRUE)
	namex <- sub("MONTH",cnames[nm],fileg)  # Grid square file names for this month

###################################################################################
# Generate variograms for this month for all years                                #
# Should not result in NA - if it did would end up with all NA upon inversion     #
# However, taking the approach of dropping entries for missing obs,               #
# so matrix inversion is still required for each yr/mo                            #
###################################################################################

    cat("\t\t Generating variogram",fill=TRUE)

# Copy the variogram best fit function and parameters from input table

    s1 <- var[nm,"s"]
    n1 <- var[nm,"n"]
    r1 <- var[nm,"r"]
    f <- get(var[nm,"Function"])

# Calculate inter-station variogram values and set diagonaly to nugget
# diag(C0) <- 0 # original code set diagonal to zero.

    C0 <- f(Dn,n1,r1,s1)
    diag(C0) <- n1

# Calculate station-to-grid square variogram values
# Keep only the valid grid squares for our country or region

    F <- rbind(f(Dion,n1,r1,s1),rep(1,gridsq))
    F <- F[,sqs]

###################################################################################
# Loop over years                                                                 #
###################################################################################

    cat("\t\t Calculating Region Average",fill=TRUE)
    for (ny in nyb:nye) {

###################################################################################
#    Fill vector of index values for all stns for set yr/mo                       #
###################################################################################
# For each station extract the value for this year

      z <- ny-nyb+1            # index of year plus month
      d <- c(rep(NA,nstn),1)   # Empty vector for index, with last entry set to 1
      for (i in 1:nstn) {
        I1 <- NCMP.stn[[i]]    # Copy of table for this index
        iz <- match(ny,I1[,1]) # Check for correct year within table
        if (!is.na(iz)) d[i] <- I1[iz,nm+1]  # Copy if year is present
      }

###################################################################################
#    Calculates region average of set yr/mo                                       #
# Do this only using where have valid observations, and only if > 0               #
###################################################################################

      iobs <- which(!is.na(d))     # Index of contributing stations + end row
      ntot <- length(iobs)-1L      # Count of number of contributing stations
      NT[z,nm] <- ntot
      if (ntot > 0L) {             # Have contributing stations
        C <- matrix(0,nrow=ntot+1,ncol=ntot+1)  # 0 == value for last row/col
        C[1:ntot,1:ntot] <- C0[iobs[1:ntot],iobs[1:ntot]]  # variogram of valid
        C[1:ntot,ntot+1] <- 1                              # set last col to 1
        C[ntot+1,1:ntot] <- 1                              # set bottom row to 1
        Cinv <- solve(C)                                   # invert C

# Calculate value for each valid grid square and then area-weighted average
# It seems here that there is effectively no weight for each grid square, and
# hence all (sqs) grid squares will have a non-missing value
# However kriging can generate an error estimate (not calculated here)

        Ix <- t(d[iobs]) %*% Cinv %*% F[iobs,]     # Matrix multiplication
        if (ne == 2L) Ix <- Ix*100 # If prec anom ratio (and prec anom?) multiply by 100
        if (ne == 3L) Ix <- Ix*100 # scale PrA so that matrix inversion works
        NCMP[z,nm] <- weighted.mean(Ix,Dsq[sqs,8]) # Find area average - no NA
      } else {                     # No contributing stations
        Ix <- rep(NA,length(sqs))  # Set all grid sqs to missing
	    NCMP[z,nm] <- NA   # Make it clear regional av is also missing
      }

#    Region average calculated. Done!
# If requested, write the grid square values to CSV file

      if (igrid == 1L) {
          namex <- sub("MONTH",cnames[nm],fileg[z])
          X <- cbind(Dsq[sqs,c(1,6:8)],Index=t(Ix))
       	  write.csv(X,file=namex,row.names=FALSE,na="-99.9")
      }
    }
  }

###################################################################################
# End loop of years (inner) and months (outer)                                    #
###################################################################################

###################################################################################
#    Write region average                                                         #
# Writes the annual value as month == 13                                          #
# This is the format given in the Guidance and used by P5/P7                      #
# But useful to also write annual series separately - chronological and ranked    #
# (The latter was done in P5_Trends_Graphs.R but makes more sense here)           #
###################################################################################

cat("Writing results",fill=TRUE)
X <- data.frame(Year=rep(nyb:nye,each=13),Month=rep(1:13,times=nyrs),
       Index=as.vector(t(NCMP)),"No of Stns"=as.vector(t(NT)),check.names=FALSE)
write.csv(X,file=filez[ne],row.names=FALSE,na="-99.9")

# Round extracted annual values to 3dp (the original file retains the full precision)
# Rank is from highest to lowest, with missing values (no valid stations) put last

iz <- which(X[,'Month'] == 13L)
Xann <- X[iz,-2]
Rank <- order(Xann[,"Index"],decreasing=TRUE)
Xann[,"Index"] <- round(Xann[,"Index"],3)
write.csv(Xann,       file=filea[ne],row.names=FALSE,na="-99.900")
write.csv(Xann[Rank,],file=filer[ne],row.names=FALSE,na="-99.900")

cat("Region Averages completed for",ele[ne],fill=TRUE)
options(op)
