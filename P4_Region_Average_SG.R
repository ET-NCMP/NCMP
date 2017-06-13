###################################################################################
#                                                                                 #
#    The R-NCMPs package has been developed by the ET-NCMP.                       #
#    P4_Region_Average_Dec2016.R                                                  #
#                                                                                 #
#    This program calculates the regional average of 7 different indices.         #
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

suppressPackageStartupMessages(library(maptools))
data(wrld_simpl)

###################################################################################
#    Gathers input info from the user                                             #
                                                                                  #
inquiry <- function() {                                                           #
                                                                                  #
  cat("The average is computed for a region (eg Caribbean) or for a",             #
      "country (eg Canada).",fill=TRUE)                                           #
                                                                                  #
  ty <- NA_integer_                                                               #
  while (is.na(ty) || ty < 0L || ty > 1L) {                                       #
    ty <- readline("\nEnter 0 for a country average or 1 for a regional average: ")
    ty <- suppressWarnings(as.integer(ty)) }                                      #
                                                                                  #
# The wrld_simpl table has other information which could determine the country    #
# (or possibly region) to calculate the average for                               # 
  if (ty == 0L) {                                                                 #
    UNcode <- NA_integer_                                                         #
    while (is.na(UNcode) || UNcode < 1L || UNcode > 999L) {                       #
      cat("Enter the numeric 3 digit UN code for your country.",                   #
          "See 'www.nationsonline.org/oneworld/countrycodes.htm'",sep="\n")       #
      UNcode <- readline("\n(ex. 124 for Canada): ")                              #
      UNcode <- suppressWarnings(as.integer(UNcode)) }                            #
  } else {                                                                        #
    UNcode <- 0L                                                                  #
  }                                                                               #
                                                                                  #
  cat("Calculating Variograms for all stations with computed indices.",           #
      "Can change this by editing the file 'A2_Indices/P2_Station_List.txt'.",sep="\n")
                                                                                  #
  x <- 0L                                                                         #
                                                                                  #
# Could plausibly have different years for variogram and regional average,        #
# in which case should consider allowing a different year range                   #
  y1 <- 0L                                                                        #
  while (is.na(y1) || y1 < 1950L || y1 > 2010L) {                                 #
    cat("Enter beginning year for region average")                                #
    y1 <- readline("\n(between 1950 and 2010, ex. 1950): ")                       #
    y1 <- suppressWarnings(as.integer(y1)) }                                      #
                                                                                  #
  y2 <- 0L                                                                        #
  while (is.na(y2) || y2 < 2000L || y2 > 2020L) {                                 #
  cat("Enter ending year for region average")                                     #
    y2 <- readline("\n(between 2000 and 2020, ex. 2015): ")                       #
    y2 <- suppressWarnings(as.integer(y2)) }                                      #
                                                                                  #
# 1 hour for 100 stations would be a VERY slow PC                                 #
  cat("Please note that this program is heavily computational.",                  #
      "It may require one hour for one index for 100 stations.",                  #					  #
      "For NCMP 1, Monthly Mean Temperature Anomaly, enter 1.",                   #
      "For NCMP 2, Monthly Total Precipitation Anomaly Normalized, enter 2.",     #
      "For NCMP 3, Standardized Precipitation Index, enter 3.",                   #
      "For NCMP 4, Percentage of Warm Days, enter 4.",                            #
      "For NCMP 4, Percentage of Warm Nights, enter 5.",                          #
      "For NCMP 5, Percentage of Cold Days, enter 6.",                            #
      "For NCMP 5, Percentage of Cold Nights, enter 7.",sep="\n")                 #
                                                                                  #
  ne <- NA_integer_                                                               #
  while (is.na(ne) || ne < 0L || ne > 7L) {                                       #
    ne <- readline("\nEnter the desired NCMP number (between 1 and 7, or 0 for all): ")
    ne <- suppressWarnings(as.integer(ne)) }                                      #
                                                                                  #
  c(ty,UNcode,x,y1,y2,ne) }                                                       #
                                                                                  #
#    User input collected. Done!                                                  #
###################################################################################

if (interactive()) a <- inquiry()        # ask if interactive call function inquiry
#a <- c(0L,36L,0L,1950L,2015L,1L)
type <- a[1]
UNcode <- a[2]
nstn <- a[3]
nyb <- a[4]
nye <- a[5]
ne <- a[6]

###################################################################################
#    Creates directories for output files                                         #
# Directories for input NCMP indices - using consistent approach                  #
                                                                                  #
ncmpn <- c(1L,2L,3L,4L,4L,5L,5L)                                                  #
folder <- "A2_Indices"                                                            #
folder2 <- paste("NCMP",ncmpn,sep="")                                             #
folder3 <- c("Monthly_Mean_Temp_Anom","Monthly_Total_Prec_Anom_Norm",             #
       "Standard_Prec_Index","Warm_Days","Warm_Nights","Cold_Days","Cold_Nights") #
ele <- c("TMA","PrAn","SPI","TX90p","TN90p","TX10p","TN10p") # NCMP index element #
dirs <- file.path(folder,folder2,folder3)                # Will add separator "/" #
                                                                                  #
# Input variograms                                                                #
                                                                                  #
folder <- "A3_Variogram"                                                          #
filev <- paste(folder,"/NCMP_",ele,"_Variogram.csv",sep="")                      #
                                                                                  #
folder <- "A4_Region_Average"                                                     #
folder2 <- "A1_Grid_Squares"                                                      #
filez <- paste(folder,"/NCMP_",ele,"_Region_Avg.csv",sep="") # output region avgs #
dir.create(file.path(folder,folder2),showWarnings=FALSE,recursive=TRUE)           #
                                                                                  #
#    Directories created. Done!                                                   #
###################################################################################

###################################################################################
#    Read the station list - use the modified list and all stations               #
                                                                                  #
files <- read.table("A2_Indices/P2_Station_List.txt",header=TRUE,stringsAsFactors=FALSE)
Station <- files[,"Station"]                                                      #
nstn <- nrow(files)                                                               #
                                                                                  #
#    Read station list. Done!                                                     #
###################################################################################

###################################################################################
#    Function to calulcate distance from lat and long                             #
                                                                                  #
distance <- function (lat1,long1,lat2,long2) {  # arguments lat1,long1,lat2,long2 #
  x1 <- lat1*pi/180                                          # convert to radians #
  x2 <- lat2*pi/180                                                               #
  y1 <- long1*pi/180                                                              #
  y2 <- long2*pi/180                                                              #
  d1 <- sin(x1)*sin(x2)                                                           #
  d2 <- cos(x1)*cos(x2)*cos(y1-y2)                                                #
  acos(pmin(pmax(d1+d2,-1.0),1.0))*6371.009 } # acos is arccos, 6371.009 (km) is WGS84 uniform sphere  # 
                                                                                  #
#    Distance calculated. Done!                                                   #
###################################################################################

###################################################################################
#    Functions to fit variogram                                                   #
# These are standard (but not all) kriging variogram functions                    #
                                                                                  #
Gaussian <- function (x,n,r,s) {                        # parameters from program #
  (s-n)*(1-exp(-(x)^2/(r^2)))+n }                                      # function #
                                                                                  #
Exponential <- function (x,n,r,s) {                     # parameters from program #
  (s-n)*(1-exp(-x/r))+n }                                              # function #
                                                                                  #
Spherical <- function (x,n,r,s) {                       # parameters from program #
  ifelse(x<=r,(s-n)*(3*x/(2*r)-x^3/(2*r^3))+n,s) }                     # function #
                                                                                  #
#    Variogram function calculated. Done!                                         #
###################################################################################

###################################################################################
#    Set up grid squares                                                          #
# This will be different as round(x,0) rounds rather than truncates towards zero  #
# The land grid point check requires that longitudes are in the range -180 to 180 #
# Have enforced this in the modified station file,                                #
# but need to consider the case that they cross the dateline (ie Pacific Ocean)   #
                                                                                  #
olats <- files[,"Lat"]                                                            #
olons <- files[,"Long"]                                                           #
latmin <- floor(min(olats))                 # min station latitude, rounded down  #
latmax <- ceiling(max(olats))               # max station latitude, rounded up    #
latr <- latmax-latmin                       # range of latitudes                  #
lngmin <- floor(min(olons))                 # min station longitude, rounded down #
lngmax <- ceiling(max(olons))               # max station longitude, rounded up   #
lngr <- lngmax-lngmin                       # range of longitudes                 #
if (lngr > 180) {                                                                 #
  lngr <- 360-lngr                                                                #
  lngmin <- lngmax }                                                              #
                                                                                  #
#Option 1: set box size so that have > 100 squares                                #
#z <- round(latr/lngr,1)               # ratio of lat/long to create square boxes #
#xlng <- round(sqrt(100/z),0)                        # number of boxes across lon #
#xlat <- as.integer(z*xlng+1)                        # number of boxes across lat #
#slng <- round(lngr/xlng,1)                             # length of each long box #
#slat <- round(latr/xlat,1)                              # length of each lat box #
                                                                                  #
#Option 2: set 2 deg x 2 deg squares                                              #
slng <- 2.0                                                                       #
slat <- 2.0                                                                       #
xlng <- as.integer(ceiling(lngr/slng))                       # cover all obs lons #
xlat <- as.integer(ceiling(latr/slat))                       # cover all obs lats #
gridsq <- xlng*xlat                                      # number of grid squares #
                                                                                  #
# Get latitude and longitude for all grid squares                                 #
# This requires repeating the individual values                                   #
                                                                                  #
lata <- latmin+slat*(0:xlat)                   # vector of latitude edges         #
lnga <- lngmin+slng*(0:xlng)                   # vector of longitude edges        #
latn <- rep(lata[-(xlat+1)],times=xlng)        # min lat of each grid sq          #
latx <- rep(lata[-1],times=xlng)               # max lat of each grid sq          #
lngn <- rep(lnga[-(xlng+1)],each=xlat)         # min long of each grid sq         #
lngx <- rep(lnga[-1],each=xlat)                # max long of each grid sq         #
latc <- rep(lata[-(xlat+1)]+slat/2,times=xlng) # centre lat of each grid sq       #
lngc <- rep(lnga[-(xlng+1)]+slng/2,each=xlat)  # centre long of each grid sq      #
                                                                                  #
d1 <- distance(latn,lngn,latn,lngx)  # length of southern edge of each grid sq    #
d2 <- distance(latx,lngn,latx,lngx)  # length of northern edge of each grid sq    #
d3 <- distance(latn,lngc,latx,lngc)  # height of each grid sq                     #
A <- (d1+d2)/2*d3                    # area (km^2) of each grid sq                #
                                                                                  #
Dsq <- cbind(Grid=1:gridsq,latn,latx,lngn,lngx,Lat=latc,Long=lngc,Area=A)         #
                                                                                  #
#    Grid squares established. Done!                                              #
###################################################################################

###################################################################################
#    Find which grid square each station is in                                    #
# This is useful as a backup if the check for land points fails to find any       #
# Need to allow for crossing the Dateline - should work but not tested            #
# Attempting to vectorise the approach, but this may not be very clear            #
                                                                                  #
olons[olons < lngmin] <- olons[olons < lngmin]+360                                #
ilat <- lapply(olats,function(x) which(Dsq[,2] <= x & Dsq[,3] > x))               #
ilon <- lapply(olons,function(x) which(Dsq[,4] <= x & Dsq[,5] > x))               #
c1 <- mapply(intersect,ilat,ilon)                                                 #
if (!is.vector(c1,mode="integer")) stop("Error calculating grid squares")         #
                                                                                  #
#    Grid squares established. Done!                                              #
###################################################################################

###################################################################################
#    Find which grid squares are on land and in our country                       #
# Reset range to -180 to 180 if have crossed the Dateline                         #
                                                                                  #
lngc[lngc >= 180] <- lngc[lngc >= 180]-360                                        #
L <- cbind(lngc,latc)                            # long/lat of centre of grid sqs #
pts <- SpatialPoints(L,proj4string=CRS(proj4string(wrld_simpl)))                  #
                                                # convert to SpatialPoints object #
                                                                                  #
# May want to extend this by grid squares where stations are located              #
# For a region it makes more sense to calcuate ONLY where stations are located    #
# Consider implications of UNcode == 0 for type == region

country <- over(pts,wrld_simpl)$UN        # vector of country codes of our points #
land <- !is.na(country)  # any point where grid sq centre is within land polygons #
cntry <- (land & country == UNcode)               # select squares in our country #
if (type == 0L) sqs <- cntry else sqs <- land      # this was the wrong way round #
                                                                                  #
#    Grid squares on land marked. Done!                                           #
###################################################################################

###################################################################################
#    Calculate Distances:                                                         #
# Vectorised calculation - generate lat/long for all pairs of stations            #
# The use of cosine distance will generate warnings                               #
# - these can be ignored but need to reset the diagonal (same station) to zero    #
                                                                                  #
lat1 <- matrix(olats,nrow=nstn,ncol=nstn)                                         #
long1 <- matrix(olons,nrow=nstn,ncol=nstn)                                        #
lat2 <- matrix(olats,nrow=nstn,ncol=nstn,byrow=TRUE)                              #
long2 <- matrix(olons,nrow=nstn,ncol=nstn,byrow=TRUE)                             #
D <- distance(lat1,long1,lat2,long2)                     # vectorised calculation #
diag(D) <- 0                                                                      #
                                                                                  #
D <- round(D,2)                                                    # Round matrix #
Dmax1 <- round(max(D),-2)  # Maximum distance between stations (rounded to 100km) #
                                                                                  #
# Repeat for all pairs of stations and grid square centres                        #
                                                                                  #
lat1 <- matrix(olats,nrow=nstn,ncol=gridsq)                                       #
long1 <- matrix(olons,nrow=nstn,ncol=gridsq)                                      #
lat2 <- matrix(Dsq[,6],nrow=nstn,ncol=gridsq,byrow=TRUE)                          #
long2 <- matrix(Dsq[,7],nrow=nstn,ncol=gridsq,byrow=TRUE)                         #
Dio <- distance(lat1,long1,lat2,long2)                                            #
                                                                                  #
Dio <- round(Dio,2)                                                # Round matrix #
                                                                                  #
#    Finished calculating distances. Done!                                        #
###################################################################################

# Output column names (now standard across CSV files)
cnames <- c(month.name,"Annual")

# Reset loop index for doing all indices

if (ne == 0L) ix <- 1:7 else ix <- ne
for (ne in ix) {                                                 # loop for indices

# Maximum distance of 2000km for precipiation indices, 3000km for temperature
# Reduced further by actual maximum separation for small networks,
# but what about large areas, including "regions"?
# Use this to restrict distance between stations and to grid squares
# Is this the most appropriate way to limit the weighting for remote stations?

  if (ne == 2L || ne == 3L) Dmax2 <- 2000 else Dmax2 <- 3000
  Dmax <- min(Dmax1,Dmax2)
  Dn <- pmin(D,Dmax)
  Dion <- pmin(Dio,Dmax)

  nyrs <- nye-nyb+1L                                     # variable number of years
  NCMP <- matrix(NA,nrow=nyrs,ncol=13)   # empty matrix for region avg, dim nyrsx12
  NT <- matrix(0L,nrow=nyrs,ncol=13)                     # empty matrix dim nyrsx12

###################################################################################
#    Read variogram:                                                              #
                                                                                  #
  cat("Reading data for index",ne,fill=TRUE)                                      #
  var <- read.csv(file=filev[ne],header=TRUE,stringsAsFactors=FALSE)              #
                                                                                  #
#    Finished reading variogram. Done!                                            #
###################################################################################

###################################################################################
#    Read index data for all stations:                                            #
# Instead of assign/get, define list of tables for each station                   #
                                                                                  #
  NCMP.stn <- vector("list",nstn)                       # Empty list for stations #
  file1 <- paste(dirs[ne],"/",Station,"_",ele[ne],".csv",sep="")                  #
  for (i in 1:nstn) { # 1:nstn                                                    #
    I1 <- read.csv(file=file1[i],header=TRUE,na.strings="-99.9")      # read data #
    if (ne == 2L) I1[,2:14] <- I1[,2:14]/100                                      #
    NCMP.stn[[i]] <- I1                                 # put data into list slot #
  }                                                                # End stn loop #
                                                                                  #
#    Finished reading input data. Done!                                           #
###################################################################################

  for (nm in 1:13) { # 1:13                                       # loop for months
    cat(nm,"\t",cnames[nm],fill=TRUE)                     # Write month in terminal

###################################################################################
# Generate variograms for this month for all years                                #
# Should not result in NA - if it did would end up with all NA upon inversion     #
# However, taking the approach of dropping entries for missing obs,               #
# so matrix inversion is still required for each yr/mo                            #
                                                                                  #
    s1 <- var[nm,"s"]                                                             #
    n1 <- var[nm,"n"]                                                             #
    r1 <- var[nm,"r"]                                                             #
    f <- get(var[nm,"Function"])                              # Best fit function #
                                                                                  #
    C0 <- f(Dn,n1,r1,s1)                    # variogram for dist between stations #
    diag(C0) <- 0                                           # zero along diagonal #
#    write.csv(C,file=paste("C_",cnames[nm],".csv",sep=""))                       #
##    k <- rbind(cbind(D > Dmax,FALSE),FALSE)                                     #
##    Cinv[k] <- NA                                                               #
#    Cinv[is.na(Cinv)] <- 0                                   # replace NA with 0 #
                                                                                  #
    F <- rbind(f(Dion,n1,r1,s1),rep(1,gridsq))     # variogram to dist to grid sq #
    F <- F[,sqs]                    # keep only grid sqs in our country or region #
#    write.csv(F,file=paste("F_",cnames[nm],".csv",sep=""))                       #
#    F[is.na(F)] <- 0                                         # replace NA with 0 #
###################################################################################

    fileg <- file.path(folder,folder2,paste("NCMP_",ele[ne],nyb:nye,cnames[nm],".csv",sep=""))
    cat("\t\t Calculating Region Average",fill=TRUE)     # Write update in terminal
    for (ny in nyb:nye) {                                      # loop through years

###################################################################################
#    Fill vector of index values for all stns for set yr/mo                       #
                                                                                  #
      z <- ny-nyb+1                              # index of year we're working in #
      d <- c(rep(NA,nstn),1)   # Empty vector for index, with last entry set to 1 #
      for (i in 1:nstn) {                                     # for each station: #
        I1 <- NCMP.stn[[i]]                                 # get table for index #
        iz <- match(ny,I1[,1])                                 # for correct year #
        if (!is.na(iz)) d[i] <- I1[iz,nm+1]             # copy if year is present #
      }                                                            # End stn loop #
#      if (ny == nyb) write.csv(d,file=paste("d_",cnames[nm],".csv",sep=""))      #
                                                                                  #
#    Vector of indices. Done!                                                     #
###################################################################################

###################################################################################
#    Calculates region average of set yr/mo                                       #
# Do this only using where have valid observations, and only if > 0               #
                                                                                  #
      iobs <- which(!is.na(d))         # index of contributing stations + end row #
      ntot <- length(iobs)-1L          # count of number of contributing stations #
      NT[z,nm] <- ntot                                                            #
      if (ntot > 0L) {                               # have contributing stations # 
        C <- matrix(0,nrow=ntot+1,ncol=ntot+1)      # 0 == value for last row/col #
        C[1:ntot,1:ntot] <- C0[iobs[1:ntot],iobs[1:ntot]]  # variogram of valid   #
        C[1:ntot,ntot+1] <- 1                              # set last col to 1    #
        C[ntot+1,1:ntot] <- 1                              # set bottom row to 1  #
        Cinv <- solve(C)                                   # invert C             #
                                                                                  #
# Calculate value for each valid grid square and then area-weighted average       #
# It seems here that there is effectively no weight for each grid square, and     #
# hence all (sqs) grid squares will have a non-missing value                      #
# However kriging can generate an error estimate (not calculated here)            #
                                                                                  #
        Ix <- t(d[iobs]) %*% Cinv %*% F[iobs,]            # matrix multiplication #
        if (ne == 2L) Ix <- Ix*100  # if prec, multiply by 100 (correct for file) #
        Q <- weighted.mean(Ix,Dsq[sqs,8])   # find area-dependant average - no NA #
        NCMP[z,nm] <- Q                         # Place region avg in NCMP matrix #
      } else {                                         # no contributing stations #
        Ix <- rep(NA,length(sqs))                   # set all grid sqs to missing #
	NCMP[z,nm] <- NA }            # make it clear regional av is also missing #
                                                                                  #
      X <- cbind(Dsq[sqs,c(1,6:8)],Index=t(Ix))           # matrix of grid sq values #
      write.csv(X,file=fileg[z],row.names=FALSE,na="-99.9")     # write into file #
                                                                                  #
#    Region average calculated. Done!                                             #
###################################################################################

    }                                                      #Ends loop for year (ny)
  }                                                       #Ends loop for month (nm)

###################################################################################
#    Write region average                                                         #
# This writes the annual value as month == 13, but the trend code depends         #
# (currently) on this structure                                                   #
                                                                                  #
  cat("\t Writing results",fill=TRUE)                  # Write update in terminal #
  X <- data.frame(Year=rep(nyb:nye,each=13),Month=rep(1:13,times=nyrs),           #
                  Index=as.vector(t(NCMP)),"No of Stns"=as.vector(t(NT)),check.names=FALSE)             #
  write.csv(X,file=filez[ne],row.names=FALSE,na="-99.9")        # write into file #
                                                                                  #
#    Finished writing. Done!                                                      #
###################################################################################

}                                                      # Ends loop for indices (ne)

dy <- date()
mess <- paste("These Regional averages are calculated using the period",nyb,"to",nye,"on date",dy)
writeLines(mess,file.path(folder,"Region_Avg_period.txt"))
cat("Region Averages Done!",fill=TRUE)                   # Write update in terminal
