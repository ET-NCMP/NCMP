###################################################################################
#                                                                                 #
#    The R-NCMPs package has been developed by the ET-NCMP.                       #
#    P5_Trends_Graphs.R                                                           #
#                                                                                 #
#    This program reads the indices calculated previously and calculates the      #
#    trend for the period specified by the user. It produces a map of each of     #
#    these trends by station.For further details please refer to the User Manual  #
#                                                                                 #
#    Programmers:                                                                 #
#    Megan Hartwell, McMaster University, Canada                                  #
#    Lucie Vincent, Environment and Climate Change Canada                         #
#    December 2016                                                                #
#    Modified by Simon Grainger, Bureau of Meteorology, Australia                 #
#    December 2017 - Fixed bugs introduced in February 2017 version               #
#                                                                                 #
###################################################################################

cat("***** P5_Trends_Graphs.R *****",fill=TRUE)

# The mapdata package is used for the high resolution world map, but the
# basic "world" database in maps seems equivalent to the wrld_simpl object in maptools
# Suppress warning messages
# - these should be related to the version of R used to build the package on PC, and can be ignored

op <- options(warn=-1)
library(maps)
library(zyp)
source("Support_Configuration.R")
cat("Successfully loaded packages",fill=TRUE)

# An effective check of whether can run the code is to load the configuration file
# If it does not exist, the relevant script has not been successfully run

clist.P2 <- read_configuration("P2")
clist.P4 <- read_configuration("P4")

###################################################################################
# Set variables with key thresholds for calculating and plotting trends           #
# These are values which have been determined by the ET-NCMP for the purposes of  #
# generating NCMPs and may differ from other standards or guidlines               #
# ***** DO NOT CHANGE THE VALUE OF THESE VARIABLES *****                          #
###################################################################################

stnhi <- clist.P2$nstn  # maximum number of stations
ylo <- 1900L            # earliest possible year for station trends
yhi <- as.POSIXlt(Sys.time())$year + 1899L # latest possible year == current - 1
yalo <- clist.P4$nyb    # start year of region average period
yahi <- clist.P4$nye    # end year of region average period
tthresh <- 11L          # number of years required to calculated a trend

# Lower trend threshold if the analysis period is too short
# Hopefully this avoids an infinite loop when defining the trend periods

nyrs <- yahi-yalo+1L
if (nyrs < tthresh) {
  warning("Region Average period is too short for reliable trends - resetting threshold",call.=FALSE)
  tthresh <- nyrs
}

###################################################################################
#    Gathers input info from the user                                             #
# For clarity, no longer do this as a separate function                           #
###################################################################################

cat("Calculate and Station and Region average trends",
    "Note that only annual series and trends are plotted for Station indices",sep="\n")

# Number of stations to process
# Can exclude stations at the end of the list, but arbitrary selection requires
# editing of 'P2_Station_List.txt'
# Still suppressing warning messages - about converting strings to integer
	
cat("\nCan either use the first 'n' stations processed by 'P2_Indices.R' or all of them",fill=TRUE)
nstn <- NA_integer_
mess <- paste("\nbetween 1 and ",stnhi,", or 0 for all - recommended =",clist.P4$nstn,": ")
while (is.na(nstn) || nstn < 0L || nstn > stnhi) {
  cat("Enter the number of stations to use")
  nstn <- readline(mess)
  nstn <- as.integer(nstn)
  }
if (nstn == 0L) nstn <- stnhi

# For region average trends the constraint is analysis period
# Need to modify this by the minimum years threshold (as per climatologies)
# (and confirm there is no infinite loop)

yr1 <- yalo
yr2 <- yahi - tthresh + 1L
nyba <- 0L
mess <- paste("\nbetween",yr1,"and",yr2,"- recommended =",yalo,": ")
while (is.na(nyba) || nyba < yr1 || nyba > yr2) {
  cat("\nEnter beginning year for region average trend period")
  nyba <- readline(mess)
  nyba <- as.integer(nyba)
}

yr1 <- nyba + tthresh - 1L
yr2 <- yahi
nyea <- 0L
mess <- paste("\nbetween",yr1,"and",yr2,"- recommended =",yahi,": ")
while (is.na(nyea) || nyea < yr1 || nyea > yr2) {
  cat("Enter ending year for region average trend period")
  nyea <- readline(mess)
  nyea <- as.integer(nyea)
}
#nyra <- nyea-nyba+1L

# For plotting station trends, allow the full range of years
# but recommend the current region average trend period

yr1 <- ylo
yr2 <- yhi - tthresh + 1L
nyb <- 0L
mess <- paste("\nbetween",yr1,"and",yr2,"- recommended =",nyba,": ")
while (is.na(nyb) || nyb < yr1 || nyb > yr2) {
  cat("\nEnter beginning year for station trend period")
  nyb <- readline(mess)
  nyb <- as.integer(nyb)
}

yr1 <- nyb + tthresh - 1L
yr2 <- yhi
nye <- 0L
mess <- paste("\nbetween",yr1,"and",yr2,"- recommended =",nyea,": ")
while (is.na(nye) || nye < yr1 || nye > yr2) {
  cat("Enter ending year for station trend period")
  nye <- readline(mess)
  nye <- as.integer(nye)
}
#nyrs <- nye-nyb+1L

cat("Thank you. User input collected",fill=TRUE)

# Turn warnings back on, but print immediately
options(warn=1)

###################################################################################
#    Creates directories for output files                                         #
###################################################################################
# Have moved (ranked) annual region averages extraction to P4_Region_Average.R
# Allow for different trend periods

folder <- "A5_Trends_Graphs"
folder2 <- c("Graphs_Annual_Station","Maps_Annual_Station","Graphs_Annual_Region")
nybt <- c(nyb,nyb,nyba)
nyet <- c(nye,nye,nyea)
odirs <- file.path(folder,paste(folder2,nybt,nyet,sep="_"))  # adds separator "/"
for (dname in odirs) dir.create(dname,showWarnings=FALSE,recursive=TRUE)
cat("Directories successfully created",fill=TRUE)

###################################################################################
# Set up variables for the 16 station indices to analyse                          #
###################################################################################

ncmpn <- c(1L,2L,3L,6L,5L,4L,5L,4L,6L,6L,6L,6L,1L,2L,2L,2L)
folder <- "A2_Indices"
folder2 <- paste("NCMP",ncmpn,sep="")
folder3 <- c("Monthly_Mean_Temp_Anom","Monthly_Total_Prec_Anom_Norm",
         "Standard_Prec_Index","Extreme_Prec",
         "Cold_Days","Warm_Days",
         "Cold_Nights","Warm_Nights",
         "Extreme_Cold_Day","Extreme_Warm_Day",
         "Extreme_Cold_Night","Extreme_Warm_Night",
         "Monthly_Mean_Temp","Monthly_Total_Prec",
         "Monthly_Total_Prec_Anom","Monthly_Total_Prec_Ratio")
dirs <- file.path(folder,folder2,folder3)  # adds separator "/"

# Element relating to NCMP Index

ele2 <- c("TMA","PrAn","SPI","RXday1","TX10p","TX90p","TN10p","TN90p",
         "TXn","TXx","TNn","TNx","TM","Pr","PrA","PrR")

# Descriptive title relating to NCMP Index

ele3 <- c("Annual Mean Temp Anom (TMA)","Ann Tot Prec Anom Norm (PrAn)",
         "Standard Prec Index (SPI)","Extreme Prec (RXday1)",
         "Cold Days (# of tmax < 10 perc)","Warm Days (# of tmax > 90 perc)",
         "Cold Nights (# of tmin < 10 perc)","Warm Nights (# of tmin > 90 perc)",
         "Extreme Cold Day (TXn)","Extreme Warm Day (TXx)",
         "Extreme Cold Night (TNn)","Extreme Warm Night (TNx)",
         "Annual Mean Temp (TM)","Annual Total Prec (Pr)",
         "Annual Total Prec Anom (PrAn)","Annual Total Prec Ratio (PrR)")

ntrend <- length(ncmpn)  # number of indices to analyse
iext <- (ncmpn == 6L)    # which indices are extremes indices

###################################################################################
#    Read the modified station list                                               #
###################################################################################
# With the number of stations already set, just need to extract the names

namex <- file.path("A2_Indices","P2_Station_List.txt")
files <- read.table(namex,header=TRUE,stringsAsFactors=FALSE)
Station <- files[1:nstn,"Station"]

# Output table of trend values

X <- data.frame(Station,
        NA_real_,"?",NA_real_,"?",NA_real_,"?",NA_real_,"?",
        NA_real_,"?",NA_real_,"?",NA_real_,"?",NA_real_,"?",
        NA_real_,"?",NA_real_,"?",NA_real_,"?",NA_real_,"?",
        NA_real_,"?",NA_real_,"?",NA_real_,"?",NA_real_,"?",stringsAsFactors=FALSE)
names(X) <- c("Station",as.vector(t(matrix(c(ele2,paste(ele2,"S",sep="_")),ncol=2))))

###################################################################################
# Information for plots                                                           #
# Currently only plotting the first 12, but will add the remainder                #
# Also determining the range of the of the monthly means from the annual range    #
###################################################################################

ymin <- c(-4,-50,-4,NA,rep( 0,4),rep(NA,8))
ymax <- c( 4, 50, 4,NA,rep(25,4),rep(NA,8))
ylabel <- c("deg C","%","no units","mm",rep("%",4),rep("deg C",5),"mm","mm","%")

# Titles of graphs - use descriptive form in 'ele3'

title <- paste("(",letters[1:ntrend],") ",gsub("_"," ",ele3),sep="")

# Output PDF file names by station

namep <- file.path(odirs[1],paste(Station,"_Annual.pdf",sep=""))

sq <- seq(from=2,to=ntrend*2,by=2) # sequence of even numbers

###################################################################################
# Begins loop for station trends                                                  #
###################################################################################

cat("Plotting annual series by station",fill=TRUE)
for (i in 1:nstn) {
  cat("\t",i,"\t",Station[i],fill=TRUE)

# Open PDF for plots
  
  pdf(namep[i],width=7,height=5)
  par(mfrow=c(2,2),mar=c(3,3,2,0.5)+0.1,mgp=c(2,0.5,0),tcl=-0.25,las=1)

###################################################################################
# Begins loop for indices                                                         #
###################################################################################

  trend <- rep(NA,ntrend)  # store annual estimated trend
  pval <- rep(NA,ntrend)   # store annual estimated p-value
  namex <- file.path(dirs,paste(Station[i],"_",ele2,".csv",sep=""))
  for (j in 1:ntrend) {

# Read in data - extremes indices require deleting the last row
# (The intention is to remove this row when creating in P2_Indices.R)

    In <- read.csv(namex[j],header=TRUE,stringsAsFactors=FALSE,na.strings="-99.9")
    if (iext[j]) {
      In <- In[-nrow(In),]
      In[,'Year'] <- as.integer(In[,'Year'])  # Year also needs to be converted
    }

# Calculate annual trends, using the Zhang method, if have enough data
# The commented out code is for linear least squares trend

    ref <- (In[,'Year'] >= nyb & In[,'Year'] <= nye)  # Select trend years
    if (sum(!is.na(In[ref,'Annual'])) >= tthresh) {
      q <- zyp.trend.vector(In[ref,'Annual'],In[ref,'Year'],method='zhang')
      trend[j] <- q['trendp']         # Extract trend over period to vector
      pval[j] <- q['sig']             # Extract p-value to vector
#      lq <- lm(Annual ~ Year,In,ref)  # Linear best fit
#      q <- coef(summary(lq))          # Extract coefficients
#      trend[j] <- q[2,1]*nyrs         # Trend per year times number of years
#      pval[j] <- q[2,4]
    }

# Plot annual values for index (currently the first 12 only)
# Allow for dynamic range for any variable by setting the default value to NA
# RX1day is a special case - lower limit is zero, upper is set to 50 above

    if (j <= 12L) {
      if (!is.na(ymin[j])) {
        ylim <- c(ymin[j],ymax[j])
      } else if (ele2[j] == 'RXday1') {
        ylim <- c(0,ceiling(max(In[ref,'Annual'],na.rm=TRUE)/50)*50)
      } else {
        yrng <- range(In[ref,'Annual'],na.rm=TRUE)
        ylim <- c(floor(yrng[1]),ceiling(yrng[2]))  # expanded to nearest integer
      }
      plot(In[ref,'Year'],In[ref,'Annual'],xlab="Year",ylab=ylabel[j],main=title[j],
           col="Blue",type="l",ylim=ylim,yaxs="i")

# Only plot trend line if it has been estimated

      if (!is.na(trend[j])) {
        abline(q[c('intercept','trend')],col="Red",lty=2)
#        abline(q[,1],col="Red",lty=2)
      }
    }
  }

###################################################################################
# Ends loop for calculating trends for indices                                    #
###################################################################################
# Close PDF file
 
  dev.off()

# Copy trend and p-value into output table
# Currently converting p-value into "y"/"n" significance flag - should this be changed?
 
  X[i,sq] <- round(trend,2)
  X[i,sq+1] <- ifelse(is.na(pval),"?",ifelse(pval < 0.05,"y","n"))
}

###################################################################################
# Ends loop for stations                                                          #
###################################################################################

###################################################################################
#    Write station annual trends (over period) and significance to file           #
# Consider separating as per Region Averages (and now Count Records)              #
###################################################################################

cat("Writing station annual trends",fill=TRUE)
filet <- file.path(folder,paste("stn","Trends",nyb,nye,"Annual.csv",sep="_"))
write.csv(X,file=filet,row.names=FALSE,na="-99.90") 

###################################################################################
# Map annual trends by station                                                    #
###################################################################################
# Add station lat/lon back into trends

Dt <- cbind(files[1:nstn,],X[,-1])

# Note that have enforced -180 <= longitudes <= 180 in modified station file
# to work generally with the maps package and the Greenwich Meridian
# But this means tackling the Dateline - can use the "world2" map here

lonrng <- range(Dt[,3])
if (lonrng[2] - lonrng[1] > 180) {
  ind <- which(Dt[,3] < 0)      # should be at least 1
  Dt[ind,3] <- Dt[ind,3] + 360  # needed since world2 map has range 0-360
  xlim <- c(ceiling(lonrng[2]),floor(lonrng[1])+360)
  wmap <- "world2"
} else {
  xlim <- c(floor(lonrng[1]),ceiling(lonrng[2]))
  wmap <- "world"
}
latrng <- range(Dt[,2])
ylim <- c(floor(latrng[1]),ceiling(latrng[2]))

# Names of maps produced - why JPEG and not PDF as per other graphs?
mapf <- file.path(odirs[2],paste("Map_",c("Stns",ele2),"_Annual.jpg",sep=""))

# Prec normalised anomaly (2) is %
# Need to check/set the units for the last 4 indices

uts <- c("Deg C","%","no units","mm",rep(c("%","Deg C",NA),each=4))

###################################################################################
#    Functions for size colour and direction of triangles for mapping             #
# These are all vectorised, so should be able to call once for each graph         #
# Set up size divisions and colours for triangles by index                        #
# Missing values => NA => no triangle                                             #
###################################################################################
# Are the symbol colours being reconsidered?
# The 'multiple' for the last 4 indices will need to be tested

multiple <- c(1,10,1,10,3,3,3,3,2,2,2,2,1,50,50,10)    
colup <- c("red",rep("darkgreen",3),rep("red",9),rep("darkgreen",3))
coldown <- ifelse(colup == "red","blue","tan4")  # consistently match colup

Size <- function(x)      # input: trend, output: size with limit (seems to work)
  {pmin(trunc(abs(x)/multiple[i])+1,3)}

Type <- function(x)      # input: trend, output: symbol triangle up/down
  {ifelse(x >= 0,24,25)}

Colour <- function(x,i)  # input: trend, index, output colour
  {ifelse(x >= 0,colup[i],coldown[i])}

Back <- function(x,y,i)  # input: trend, signif, index, output: fill if signif
  {ifelse(y == 'y',Colour(x,i),"white")} 

###################################################################################
#    Map Stations                                                                 #
# Now using the standard resolution world map instead of high resolution          #
###################################################################################

cat("Mapping annual trends by station",fill=TRUE)

# Plot the station locations

jpeg(mapf[1])
map(wmap,xlim=xlim,ylim=ylim,col="gray80",fill=TRUE)  # grey fill of continents
points(Dt[,3],Dt[,2],pch=16,col="Red",cex=1.0)        # dots of lat/long (check size)
title(main="Stn locations")                           # map title
dev.off()

# Changed to plot all (16) indices - need to test the last 4
# The plot titles may need modification

for (i in 1:ntrend) {
  cat("\t",ele2[i],fill=TRUE)
  S <- Size(Dt[,2*i+2])               # Size of all triangles
  Ty <- Type(Dt[,2*i+2])              # Type (up/down) of all triangles
  Cr <- Colour(Dt[,2*i+2],i)          # Colour of all triangles
  Bg <- Back(Dt[,2*i+2],Dt[,2*i+3],i) # Fill of all triangles (if significant)

  jpeg(mapf[i+1])
  map(wmap,xlim=xlim,ylim=ylim,col="gray80",fill=TRUE)
  title(main=title[i])
  points(Dt[,3],Dt[,2],pch=Ty,col=Cr,bg=Bg,cex=0.4*S+0.9,lwd=2)

# Legend only shows the filled triangles
# Consider the text size 'cex'

  S <- 0.4*c(2:0,0:2)+0.9
  Cr <- rep(c(colup[i],coldown[i]),each=3)
  Ty <- rep(24:25,each=3)
  n1 <- c(Inf,2:-2)
  n2 <- c(2:-2,-Inf)
  lT <- paste(n2*multiple[i],"< x <",n1*multiple[i])
  legend("topleft",legend=lT,title=uts[i],pch=Ty,col=Cr,pt.cex=S,cex=0.8,pt.bg=Cr)
  dev.off()
}

###################################################################################
# Calculate monthly trends for regional averages                                  #
###################################################################################

# Element relating to NCMP Index
	
ele <- c("TMA","PrAn","PrA","SPI","TX90p","TN90p","TX10p","TN10p")

# Names of input regional average files

tname <- clist.P4$tname
filez <- file.path("A4_Region_Average",paste(tname,ele,"Region_Avg.csv",sep="_"))

# Names of output regional average graphs

namep <- file.path(odirs[3],paste(tname,ele,"Annual.pdf",sep="_"))

# Titles of graphs

title <- c("Mean Temperature Anom (TMA)","Total Precipitation Anom Norm (PrAn)",
         "Total Precipitation Anom (PrA)","Standard Prec Index (SPI)",
         "% Warm Days (tmax > 90th percentile)","% Warm Nights (tmin > 90th percentile)",
         "% Cold Days (tmax < 10th percentile)","% Cold Nights (tmin < 10th percentile)")

# Range for graphs - need to allow the Prec Anom (at least) to vary
# (although the issue might be with the Prec Anom estimation)

ymin <- c(-4,-50,NA,-2,rep( 0,4))
ymax <- c( 4, 50,NA, 2,rep(25,4))
ylabel <- c("deg C","%","mm","no units",rep("%",4)) # Labels for y axis

# Output data.frame for trend and significance
# Changed column names for consistency with other output tables

X <- data.frame(title,
        NA_real_,"?",NA_real_,"?",NA_real_,"?",NA_real_,"?",
        NA_real_,"?",NA_real_,"?",NA_real_,"?",NA_real_,"?",
        NA_real_,"?",NA_real_,"?",NA_real_,"?",NA_real_,"?",
        NA_real_,"?",stringsAsFactors=FALSE)
cnames <- c(month.name,"Annual")
names(X) <- c("NCMP",as.vector(t(matrix(c(cnames,paste(cnames,"S",sep="_")),ncol=2))))

sq <- seq(from=2,to=26,by=2) # sequence of even numbers

###################################################################################
# Begins loop for indices                                                         #
###################################################################################

cat("Calculate and plot monthly trends for region",fill=TRUE)
for (i in 1:8) {
  cat("\t",ele[i],fill=TRUE)

# Read in regional average data for all months for this index
# Skip if this has not yet been calculated

  if (!file.exists(filez[i])) {
    cat("Regional average has not yet been calculated - skipping",fill=TRUE)
	next
  }
  In <- read.csv(file=filez[i],header=TRUE,na.strings="-99.9",check.names=FALSE)

  trend <- rep(NA,13)  # store monthly estimated trend over period
  pval <- rep(NA,13)   # store monthly estimated p-value

# Calculations for trends by month
# Although have enforced a minimum period for trend analysis,
# allow for missing values generated by no station data

  for (j in 1:13) {
    ref <- which(In[,'Year'] >= nyba & In[,'Year'] <= nyea & In[,'Month'] == j)
    if (sum(!is.na(In[ref,'Index'])) >= tthresh) {
      q <- zyp.trend.vector(In[ref,'Index'],In[ref,'Year'],method='zhang')
      trend[j] <- q['trendp']         # Extract trend over period to vector
      pval[j] <- q['sig']             # Extract p-value to vector
#      lq <- lm(Index ~ Year,In,ref)   # Linear best fit
#      q <- coef(summary(lq))          # Extract coefficients
#      trend[j] <- q[2,1]*nyra         # Trend per year times number of years
#      pval[j] <- q[2,4]
    }
  }

# Copy trend and p-value into output table
# Currently converting p-value into "y"/"n" significance flag - should this be changed?
 
  X[i,sq] <- round(trend,2)
  X[i,sq+1] <- ifelse(is.na(pval),"?",ifelse(pval < 0.05,"y","n"))

# Plot annual series and trend
# These are preserved as the last index in the loop (j == 13)

  pdf(namep[i])
  if (!is.na(ymin[i])) {
    ylim <- c(ymin[i],ymax[i])
  } else {
    yrng <- range(In[ref,'Index'],na.rm=TRUE)
    ylim <- c(floor(yrng[1]),ceiling(yrng[2]))
  }
  plot(In[ref,'Year'],In[ref,'Index'],type="l",col="Blue",ylim=ylim,yaxs="i",
    xlab="Year",ylab=ylabel[i],main=title[i],las=1)
  if (!is.na(trend[j])) {  # Add trend line if calculated
    abline(q[c("intercept","trend")],col="Red",lty=2)
#    abline(q[,1],col="Red",lty=2)
  }
  dev.off()
}

###################################################################################
# Ends loop for calculating and plotting regional average trends                  #
###################################################################################

###################################################################################
#    Write regional average trends (over period) and significance to file         #
###################################################################################

cat("Writing regional average monthly trends",fill=TRUE)
filet <- file.path(folder,paste(tname,"Trends",nyba,nyea,"Region_Avg.csv",sep="_"))
write.csv(X,file=filet,row.names=FALSE,na="-99.90")

cat("Calculations and maps done!",fill=TRUE)
options(op)
