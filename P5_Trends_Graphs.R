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
#
# The mapdata package is used for the high resolution world map, but the
# basic "world" database in maps seems equivalent to the wrld_simpl object in maptools

library(maps)
#library(mapdata)
library(zyp)

###################################################################################
#    Gathers input info from the user                                             #
                                                                                  #
inquiry <- function() {                                                           #
                                                                                  #
  x <- NA_integer_                                                                #
  while (is.na(x) || x < 0L || x > 200L) {                                        #
    x <- readline("\nEnter the number of stations (between 1 and 200, or 0 for all): ")
    x <- suppressWarnings(as.integer(x))                                          #
  }                                                                               #
                                                                                  #
# Start and end years can be a subset of the indices and/or regional average      #
# But do not see why cannot allow a larger year range                             #
  y1 <- 0L                                                                        #
  while (is.na(y1) || y1 < 1950L || y1 > 2010L) {                                 #
    cat("Enter beginning year to calculate trends")                               #
    y1 <- readline("\n(between 1950 and 2010, ex. 1950): ")                       #
    y1 <- suppressWarnings(as.integer(y1))                                        #
  }                                                                               #
                                                                                  #
  y2 <- 0L                                                                        #
  y2l <- y1+10L                        # need at least 11 years to estimate trend #
  y2h <- as.POSIXlt(Sys.time())$year + 1899L                # last completed year #
  yex <- max(y2l,min(2015L,y2h))                                                  #
  mess <- paste("\n(between ",y2l," and ",y2h,",ex. ",yex,"): ",sep="")           #
  while (is.na(y2) || y2 < y2l || y2 > y2h) {                                     #
  cat("Enter ending year to calculate trends")                                    #
    y2 <- readline(mess)                                                          #
    y2 <- suppressWarnings(as.integer(y2))                                        #
  }                                                                               #
                                                                                  #
  c(x,y1,y2)                                                                      #
}                                                                                 #
                                                                                  #
#    User input collected. Done!                                                  #
###################################################################################

if (interactive()) a <- inquiry()        # ask if interactive call function inquiry
#a <- c(0L,1950L,2015L)
nstn <- a[1]
nyb <- a[2]
nye <- a[3]
nyrs <- nye-nyb+1L

###################################################################################
#    Creates directories for output files                                         #
                                                                                  #
folder <- "A5_Trends_Graphs"                                                      #
folder2 <- c("Graphs_Annual_Station","Maps_Annual_Station","Graphs_Annual_Region",#
             "Ranks_Annual_Region")                                               #
dirs <- file.path(folder,paste(folder2,nyb,nye,sep="_"))     # adds separator "/" #
for (dname in dirs) dir.create(dname,showWarnings=FALSE,recursive=TRUE)           #
                                                                                  #
#    Directories created. Done!                                                   #
###################################################################################

###################################################################################
#    Reads the station list                                                       #
files <- read.table("A2_Indices/P2_Station_List.txt",header=TRUE,stringsAsFactors=FALSE)
Station <- files[,"Station"]                                                      #
nstn <- nrow(files)                                                               #
                                                                                  #
#    Read station list. Done!                                                     #
###################################################################################

# Construct elements for the 16 indices to graph

ncmpn <- c(1L,2L,3L,6L,5L,4L,5L,4L,6L,6L,6L,6L,1L,2L,2L,2L)
folderi <- "A2_Indices"
folder2 <- paste("NCMP",ncmpn,sep="")
ele <- c("Monthly_Mean_Temp_Anom","Monthly_Total_Prec_Anom_Norm",
         "Standard_Prec_Index","Extreme_Prec",
         "Cold_Days","Warm_Days",
         "Cold_Nights","Warm_Nights",
         "Extreme_Cold_Day","Extreme_Warm_Day",
         "Extreme_Cold_Night","Extreme_Warm_Night",
         "Monthly_Mean_Temp","Monthly_Total_Prec",
         "Monthly_Total_Prec_Anom","Monthly_Total_Prec_Ratio")
ele2 <- c("TMA","PrAn","SPI","RXday1","TX10p","TX90p","TN10p","TN90p",
         "TXn","TXx","TNn","TNx","TM","Pr","PrA","PrR")
iext <- (ncmpn == 6L) # which are extremes indices

# Output table of trend values
X <- data.frame(Station[1:nstn],
        NA_real_,"?",NA_real_,"?",NA_real_,"?",NA_real_,"?",
        NA_real_,"?",NA_real_,"?",NA_real_,"?",NA_real_,"?",
        NA_real_,"?",NA_real_,"?",NA_real_,"?",NA_real_,"?",
        NA_real_,"?",NA_real_,"?",NA_real_,"?",NA_real_,"?",stringsAsFactors=FALSE)
names(X) <- c("Station",as.vector(t(matrix(c(ele2,paste(ele2,"S",sep="_")),ncol=2))))

# Information for plots - have 16 elements but only plotting the first 12
# Also determining the range of the of the monthly means from the annual range
ymin <- c(-4,-50,-4,NA,rep( 0,4),rep(NA,8))
ymax <- c( 4, 50, 4,NA,rep(25,4),rep(NA,8))
ylabel <- c("deg C","%","no units","mm",rep("%",4),rep("deg C",5),"mm","mm","%")

# Titles of graphs
title <- paste("(",letters[1:16],") ",gsub("_"," ",ele),sep="")

# Output PDF file names by station
namep <- file.path(dirs[1],paste(Station,"_Annual.pdf",sep=""))

sq <- seq(from=2,to=32,by=2) # sequence of even numbers

# Begins loop for reading data files and doing calculations

cat("Analysis by station",fill=TRUE)
for (i in 1:nstn) {         # 1:nstn
  cat("\t",i,"\t",Station[i],fill=TRUE)

###################################################################################
#    Calculate the trend for each element                                         #
                                                                                  #
  trend <- rep(NA,16)                          # empty vector to fill with trends #
  pval <- rep(NA,16)                         # empty vector to fill with p-values #
  namex <- file.path(folderi,folder2,ele,paste(Station[i],"_",ele2,".csv",sep=""))#
                                                                                  #
  pdf(namep[i],width=7,height=5)                                                  #
  par(mfrow=c(2,2),mar=c(3,3,2,0.5)+0.1,mgp=c(2,0.5,0),tcl=-0.25,las=1)           #
  for (j in 1:16) {                                                               #
                                                                                  #
# Read in data - extremes indices require deleting the last row                   #
# Will only be plotting annual values                                             #
                                                                                  #
    In <- read.csv(namex[j],header=TRUE,stringsAsFactors=FALSE,na.strings="-99.9")#
    if (iext[j]) {                                                                #
      In <- In[-nrow(In),]                                                        #
      In[,'Year'] <- as.integer(In[,'Year'])         # Year needs to be converted #
    }                                                                             #
    ref <- (In[,'Year'] >= nyb & In[,'Year'] <= nye)           # Select ref years #
                                                                                  #
# Allow for dynamic range for any variable by setting the default value to NA     #
# RX1day is a special case - lower limit is zero, upper is set to 50 above        #
                                                                                  #
    if (j <= 12L) {                                                               #
      if (!is.na(ymin[j])) {                                                      #
        ylim <- c(ymin[j],ymax[j])                                                #
      } else if (ele2[j] == 'RXday1') {                                           #
        ylim <- c(0,ceiling(max(In[ref,'Annual'],na.rm=TRUE)/50)*50)              #
      } else {                                                                    #
        yrng <- range(In[ref,'Annual'],na.rm=TRUE)                                #
        ylim <- c(floor(yrng[1]),ceiling(yrng[2]))  # expanded to nearest integer #
      }                                                                           #
      plot(In[ref,'Year'],In[ref,'Annual'],xlab="Year",ylab=ylabel[j],main=title[j],
           col="Blue",type="l",ylim=ylim,yaxs="i")                                #
    }                                                                             #
                                                                                  #
    if (sum(!is.na(In[ref,'Annual'])) > 10L) {        # check if have enough data #
      q <- zyp.trend.vector(In[ref,'Annual'],In[ref,'Year'],method='zhang')       #
      trend[j] <- q['trendp']               # extract trend over period to vector #
      pval[j] <- q['sig']                             # extract p-value to vector #
#      lq <- lm(Annual ~ Year,In,ref)                            # Linear best fit #
#      q <- coef(summary(lq))                               # Extract coefficients #
#      trend[j] <- q[2,1]*nyrs              # trend per year times number of years #
#      pval[j] <- q[2,4]                                                           #
      if (j <= 12L) {                                                             #
        abline(q[c('intercept','trend')],col="Red",lty=2) # plot trend line on graph #
#        abline(q[,1],col="Red",lty=2)                  # plot trend line on graph #
      }                                                                           #
    }                                                                             #
  } # Ends loop for each element                                                  #
  dev.off()                                                      # close PDF file #
                                                                                  #
  X[i,sq] <- round(trend,2)                           # put trend into data frame #
  X[i,sq+1] <- ifelse(is.na(pval),"?",ifelse(pval < 0.05,"y","n"))  # signif flag #
                                                                                  #
#    Calculated the trend and significance of each element                        #
###################################################################################

}  # Ends loop for stations

###################################################################################
#    Write trends and significance - including absolute monthly values            #
# Consider separating as per Region Averages (and now Count Records)              #
                                                                                  #
filet <- file.path(folder,paste("Trends_Annual_stn_",nyb,"_",nye,".csv",sep=""))  #
write.csv(X,file=filet,row.names=FALSE,na="-99.90")            # write data frame #
                                                                                  #
#    Finished writing trends. Done!                                               #
###################################################################################

cat("Mapping by station",fill=TRUE)

# Add station lat/lon back into trends
# Note that have enforced -180 <= longitudes <= 180 in modified station file
# to work generally with the maps package and the Greenwich Meridian
# But this means tackling the Dateline - can use the "world2" map here

Dt <- cbind(files[1:nstn,],X[,-1])

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
mapf <- file.path(dirs[2],paste("Map_",c("Stns",ele2),"_Annual.jpg",sep=""))

# Prec normalised anomaly (2) is %
uts <- c("Deg C","%","no units","mm",rep(c("%","Deg C",NA),each=4))

###################################################################################
#    Functions for size colour and direction of triangles for mapping             #
# These are all vectorised, so should be able to call once for each graph         #
# Set up size divisions and colours for triangles by index                        #
# Missing values => NA => no triangle                                             #
                                                                                  #
multiple <- c(1,10,1,10,3,3,3,3,2,2,2,2,1,50,50,10)                               #     
colup <- c("red",rep("darkgreen",3),rep("red",9),rep("darkgreen",3))              #
coldown <- ifelse(colup == "red","blue","tan4")        # consistently match colup #
                                                                                  #
Size <- function(x)                # input: trend, output: size with limit (TEST) #
  {pmin(trunc(abs(x)/multiple[i])+1,3)}                                           # 
                                                                                  #
Type <- function(x)               # input: trend, output: symbol triangle up/down #
  {ifelse(x >= 0,24,25)}                                                          #
                                                                                  #
Colour <- function(x,i)                      # input: trend, index, output colour #
  {ifelse(x >= 0,colup[i],coldown[i])}                                            #
                                                                                  #
Back <- function(x,y,i)     # input: trend, signif, index, output: fill if signif #
  {ifelse(y == 'y',Colour(x,i),"white")}                                          #
                                                                                  #
#    Functions for mapping!                                                       #
###################################################################################

###################################################################################
#    Map Stations                                                                 #
# Now using the standard resolution world map instead of high resolution          #
                                                                                  #
cat("\t Stns",fill=TRUE)                               # write update in terminal #
jpeg(mapf[1])                                                                     #
map(wmap,xlim=xlim,ylim=ylim,col="gray80",fill=TRUE)                  # grey fill #
points(Dt[,3],Dt[,2],pch=16,col="Red",cex=1.0)             # add dots of lat/long #
title(main="Stn locations")                                           # map title #
dev.off()                                                             # close map #
                                                                                  #
for (i in 1:12) {                                                                 #
  cat("\t",ele2[i],fill=TRUE)                          # write update in terminal #
  S <- Size(Dt[,2*i+2])                                   # size of all triangles #
  Ty <- Type(Dt[,2*i+2])                        # type (up/down) of all triangles #
  Cr <- Colour(Dt[,2*i+2],i)                            # colour of all triangles #
  Bg <- Back(Dt[,2*i+2],Dt[,2*i+3],i)         # fill of all triangles (if signif) #
                                                                                  #
  jpeg(mapf[i+1])                                                                 #
  map(wmap,xlim=xlim,ylim=ylim,col="gray80",fill=TRUE)                            #
  title(main=title[i])                                                            #
  points(Dt[,3],Dt[,2],pch=Ty,col=Cr,bg=Bg,cex=0.4*S+0.9,lwd=2)                   #
                                                                                  #
# Legend only shows the filled triangles                                          #
  S <- 0.4*c(2:0,0:2)+0.9                                                         #
  Cr <- rep(c(colup[i],coldown[i]),each=3)                                        #
  Ty <- rep(24:25,each=3)                                                         #
  n1 <- c(Inf,2:-2)                                                               #
  n2 <- c(2:-2,-Inf)                                                              #
  lT <- paste(n2*multiple[i],"< x <",n1*multiple[i])                              #
# Smaller/larger cex = 0.8/1.0:                                                   #
  legend("topleft",legend=lT,title=uts[i],pch=Ty,col=Cr,pt.cex=S,cex=0.8,pt.bg=Cr)#
  dev.off()                                                           # close map #
}                                                                                 #
                                                                                  #
#   Finished mapping stations!                                                    #
###################################################################################

cat("Analysis by region",fill=TRUE)

# Element relating to NCMP Index	
ele <- c("TMA","PrAn","PrA","SPI","TX90p","TN90p","TX10p","TN10p")

# Names of input regional average files
folderz <- "A4_Region_Average"
filez <- file.path(folderz,paste("NCMP",ele,"Region_Avg.csv",sep="_"))

# Names of output regional average graphs and ranks
namep <- file.path(dirs[3],paste("NCMP_",ele,"_Annual.pdf",sep=""))
namex <- file.path(dirs[4],paste("NCMP_",ele,"_Annual_Rank.csv",sep=""))

# Titles of graphs
title <- c("Mean Temp Anom","Prec Anom Norm","Prec Anom","Standardized Prec Index",
           "Warm Days","Warm Nights","Cold Days","Cold Nights")

# Range for graphs - need to allow the Prec Anom (at least) to vary
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

for (i in 1:8) {                                           # Loop for elements
  cat("\t",ele[i],fill=TRUE)

###################################################################################
#    Calculations for trends by month                                             #
# Not checking that have long enough period to estimate the trend                 #
# Is there much difference between ZYP trends and linear least squares regression?#
                                                                                  #
  trend <- rep(NA,13)                          # empty vector to fill with trends #
  pval <- rep(NA,13)                         # empty vector to fill with p-values #
                                                                                  #
  In <- read.csv(file=filez[i],header=TRUE,na.strings="-99.9",check.names=FALSE)  #
  for (j in 1:13) {                              # Loop through months and annual #
    ref <- which(In[,'Year'] >= nyb & In[,'Year'] <= nye & In[,'Month'] == j)     #
    q <- zyp.trend.vector(In[ref,'Index'],In[ref,'Year'],method="zhang")          #
    trend[j] <- q['trendp']                 # extract trend over period to vector #
    pval[j] <- q['sig']                               # extract p-value to vector #
#    lq <- lm(Index ~ Year,In,ref)                               # Linear best fit #
#    q <- coef(summary(lq))                                 # Extract coefficients #
#    trend[j] <- q[2,1]*nyrs                # trend per year times number of years #
#    pval[j] <- q[2,4]                                                             #
  }                                                                               #
                                                                                  #
  X[i,sq] <- round(trend,2)                           # put trend into data frame #
  X[i,sq+1] <- ifelse(pval < 0.05,"y","n")      # put signif flag into data frame #
                                                                                  #
#    Monthly trends calculated and written. Done!                                 #
###################################################################################

###################################################################################
#    Examine annual values to graph and rank.                                     #
# These are preserved as the last index in the loop (j == 13)                     #
                                                                                  #
  pdf(namep[i])                                                       # Open plot #
  if (!is.na(ymin[i])) {                                                          #
    ylim <- c(ymin[i],ymax[i])                                                    #
  } else {                                                                        #
    yrng <- range(In[ref,'Index'],na.rm=TRUE)                                     #
    ylim <- c(floor(yrng[1]),ceiling(yrng[2]))                                    #
  }                                                                               #
  plot(In[ref,'Year'],In[ref,'Index'],type="l",col="Blue",ylim=ylim,yaxs="i",     #
    xlab="Year",ylab=ylabel[i],main=title[i],las=1)                               #
                                                                                  #
  abline(q[c("intercept","trend")],col="Red",lty=2)              # Add trend line #
#  abline(q[,1],col="Red",lty=2)                                                   #
  dev.off()                                                          # Close plot #
                                                                                  #
  Rank <- order(In[ref,"Index"],decreasing=TRUE)    # missing values are put last #
  In[ref,"Index"] <- round(In[ref,"Index"],3)  # round to 3dp for writing to file #
  write.csv(In[ref[Rank],-2],file=namex[i],row.names=FALSE,na="-99.900")          #
                                                                                  #
#    Finished graphing/ranking annual values. Done!                               #
###################################################################################

}                                                                # End element loop

###################################################################################
#    Write regional average trends and significance with months in columns        #
                                                                                  #
filet <- file.path(folder,paste("Trends_Region_",nyb,"_",nye,".csv",sep=""))      #
write.csv(X,file=filet,row.names=FALSE,na="-99.90")                # write trends #
                                                                                  #
#    Finished writing trends. Done!                                               #
###################################################################################

cat("Calculations and maps done!",fill=TRUE)
