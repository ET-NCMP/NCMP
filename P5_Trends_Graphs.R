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
#    December 2016 - Cleaned up, code is more "R-like"                            #
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
  x <- suppressWarnings(as.integer(x)) }                                          #
                                                                                  #
# Start and end years can be a subset of the indices and/or regional average      #
# But do not see why cannot allow a larger year range                             #
  y1 <- 0L                                                                        #
  while (is.na(y1) || y1 < 1950L || y1 > 2010L) {                                 #
    cat("Enter beginning year to calculate trends")                               #
    y1 <- readline("\n(between 1950 and 2010, ex. 1950): ")                       #
    y1 <- suppressWarnings(as.integer(y1)) }                                      #
                                                                                  #
  y2 <- 0L                                                                        #
  while (is.na(y2) || y2 < 2000L || y2 > 2020L) {                                 #
  cat("Enter ending year to calculate trends")                                    #
    y2 <- readline("\n(between 2000 and 2020, ex. 2015): ")                       #
    y2 <- suppressWarnings(as.integer(y2)) }                                      #
                                                                                  #
  c(x,y1,y2) }                                                                    #
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
dstr <- paste("A5_Trends_Graphs/",c("Graphs_by_stn","Maps","Graphs_by_reg","Rank_by_Reg"),
              "_",nyb,"_",nye,"/",sep="")                                         #
dir.create("A5_Trends_Graphs",showWarnings=FALSE)                                 #
dir.create(dstr[1],showWarnings=FALSE)                                            #
dir.create(dstr[2],showWarnings=FALSE)                                            #
dir.create(dstr[3],showWarnings=FALSE)                                            #
dir.create(dstr[4],showWarnings=FALSE)                                            #
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
# file.path function will deal with the path separator

ncmpn <- c(1L,2L,3L,6L,5L,4L,5L,4L,6L,6L,6L,6L,1L,2L,2L,2L)
folder <- "A2_Indices"
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

# Information for plots
ylabel <- c("deg C","%","no units","mm",rep("%",4),rep("deg C",5),"mm","mm","%")
ymin <- c(-4,-40,-4,rep(0,5))
ymax <- c(4,40,4,300,rep(30,4))
mstr <- paste(letters[1:16],") ",ele2,sep="")

# Output PDF file names by station
namep <- paste(dstr[1],Station,".pdf",sep="")

# Begins loop for reading data files and doing calculations

cat("Analysis by station",fill=TRUE)
for (i in 1:nstn) {         # 1:nstn
  cat("\t",i,"\t",Station[i],fill=TRUE)

###################################################################################
#    Calculate the trend for each element                                         #
                                                                                  #
  sq <- seq(from=2,to=32,by=2)                         # sequence of even numbers #
  trend <- rep(NA,16)                          # empty vector to fill with trends #
  pval <- rep(NA,16)                         # empty vector to fill with p-values #
  namex <- file.path(folder,folder2,ele,paste(Station[i],"_",ele2,".csv",sep="")) #
                                                                                  #
  pdf(namep[i])                                                                   #
  par(mfrow=c(2,2))                                                               #
  for (j in 1:16) {    # 1:16                                                     #
                                                                                  #
# Read in data - extremes indices require deleting the last row                   #
# Will only be plotting annual values                                             #
                                                                                  #
    tm <- read.csv(namex[j],header=TRUE,stringsAsFactors=FALSE,na.strings="-99.9")#
    if (iext[j]) {                                                                #
      tm <- tm[-nrow(tm),]                                                        #
      tm[,'Year'] <- as.integer(tm[,'Year'])         # Year needs to be converted #
    }                                                                             #
    ref <- (tm[,'Year'] >= nyb & tm[,'Year'] <= nye)           # Select ref years #
                                                                                  #
    if (j <= 8L) {                                                                #
      y1 <- ymin[j]                                                               #
      y2 <- ymax[j]                                                               #
    } else {  # y1/2 not used for j > 12 (monthly T,Pr)                           #
      y1 <- as.integer(min(tm[ref,'Annual'],na.rm=TRUE))-3L                      #
      y2 <- y1+25L  # is this a reasonable upper limit?                           #
    }                                                                             #
                                                                                  #
    if (j <= 12L) {                                                               #
      plot(tm[ref,'Year'],tm[ref,'Annual'],xlab="",ylab=ylabel[j],main=mstr[j],   #
           col="Blue",type="l",ylim=c(y1,y2),yaxs='i')                            #
    }                                                                             #
                                                                                  #
    if (sum(!is.na(tm[ref,'Annual'])) > 10L) {       # check if have enough data #
      q <- zyp.trend.vector(tm[ref,'Annual'],tm[ref,'Year'],method='zhang')       #
      trend[j] <- q['trendp']               # extract trend over period to vector #
      pval[j] <- q['sig']                             # extract p-value to vector #
      if (j <= 12L) {                                                             #
        sl <- q['trend']                    # extract slope (trend per unit time) #
        b <- q['intercept']                                   # extract intercept #
        abline(b,sl,col="Red")                         # plot trend line on graph #
      }                                                                           #
    }                                                                             #
  } # Ends loop for each element                                                  #
  dev.off()                                                      # close PDF file #
                                                                                  #
  X[i,sq] <- round(trend,2)                           # put trend into data frame #
  X[i,sq+1] <- ifelse(pval < 0.05,"y","n")      # put signif flag into data frame #
  is.na(X[i,sq+1]) <- "?"                   # reset if do not have trend estimate #
                                                                                  #
#    Calculated the trend and significance of each element                        #
###################################################################################

}  # Ends loop for stations

###################################################################################
#    Write trends and significance - including absolute monthly values            #
                                                                                  #
name <- paste("A5_Trends_Graphs/Trends_by_stn_",nyb,"_",nye,".csv",sep="")        #
write.csv(X,file=name,row.names=FALSE,na="-99.9")              # write data frame #
                                                                                  #
#    Finished writing trends. Done!                                               #
###################################################################################

cat("Mapping by station",fill=TRUE)

# This has the same issues as defining the region for averaging (P4)
# Need to fix this in a consistent fashion

Dt <- cbind(Station[1:nstn],files[1:nstn,2:3],T[,-1])
x1 <- as.integer(min(Dt[,3]))-1
x2 <- as.integer(max(Dt[,3]))+2
y1 <- as.integer(min(Dt[,2]))-1
y2 <- as.integer(max(Dt[,2]))+2

# Names of maps produced - why JPEG and not PDF as per other graphs?
mapf <- paste(dstr[2],"Map",sprintf("%2.2i",0:16),"_",c("Stns",ele2),".jpg",sep="")

# Prec normalised anomaly (2) is %
uts <- c("Deg C","%","no units","mm",rep("%",4),rep("Deg C",4))

###################################################################################
#    Functions for size colour and direction of triangles for mapping             #
# These are all vectorised, so should be able to call once for each graph         #
# Set up size divisions and colours for triangles by index                        #
# Missing values => NA => no triangle                                             #
                                                                                  #
multiple <- c(1,10,1,3,3,3,3,3,2,2,2,2,1,50,50,10)                                #     
colup <- c("red",rep("darkgreen",3),rep("red",9),rep("darkgreen",3))              #
coldown <- c("blue",rep("tan4",3),rep("blue",9),rep("tan4",3))                    #
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
Bg <- function(x,y,i)       # input: trend, signif, index, output: fill if signif #
  {ifelse(y == 'y',Colour(x,i),"white")}                                          #
                                                                                  #
#    Functions for mapping!                                                       #
###################################################################################

###################################################################################
#    Map Stations                                                                 #
# Now using the standard resolution world map instead of high resolution          #
                                                                                  #
cat("\t Map 1",fill=TRUE)                              # write update in terminal #
jpeg(mapf[1])                                                                     #
map("world",xlim=c(x1,x2),ylim=c(y1,y2),col="gray80",fill=TRUE)       # grey fill #
points(Dt[,3],Dt[,2],pch=16,col="red",cex=1.0)             # add dots of lat/long #
dev.off()                                                             # close map #
                                                                                  #
for (i in 1:12) {                                                                 #
  cat("\t Map",i+1,fill=TRUE)                          # write update in terminal #
  S <- Size(Dt[,2*i+2])                                   # size of all triangles #
  Ty <- Type(Dt[,2*i+2])                        # type (up/down) of all triangles #
  Cr <- Colour(Dt[,2*i+2],i)                            # colour of all triangles #
  Bg <- Bg(Dt[,2*i+2],Dt[,2*i+2],i)           # fill of all triangles (if signif) #
                                                                                  #
  jpeg(mapf[i+1])                                                                 #
  map("world",xlim=c(x1,x2),ylim=c(y1,y2),col="gray80",fill=TRUE)     # grey fill #
  points(Dt[,3],Dt[,2],pch=Ty,col=Cr,bg=Bg,cex=0.4*S+0.9,lwd=3.5)  # smaller lwd? #
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
ele <- c("TMA","PrAn","SPI","TX90p","TN90p","TX10p","TN10p")

# Names of input regional average files
filez <- paste("A4_Region_Average/NCMP",ele,"Region_Avg.csv",sep="_")

# Names of output regional average graphs and ranks
namep <- paste(dstr[3],"NCMP_",ele,".pdf",sep="")
namex <- paste(dstr[4],"NCMP_",ele,".csv",sep="")

# Titles of graphs
title <- c("Mean Temp Anom","Prec Anom Norm","Standardized Prec Index",
           "Warm Days","Warm Nights","Cold Days","Cold Nights")

ymin <- c(-5,-20,-2,rep( 0,4))                                        # Fix y range
ymax <- c( 5, 20, 2,rep(30,4))
ylabel <- c("deg C","%","no units",rep("%",4))                  # Labels for y axis

# Output data.frame for trend and significance
# Changed column names for consistency with other output tables

X <- data.frame(title,
        NA_real_,"?",NA_real_,"?",NA_real_,"?",NA_real_,"?",
        NA_real_,"?",NA_real_,"?",NA_real_,"?",NA_real_,"?",
        NA_real_,"?",NA_real_,"?",NA_real_,"?",NA_real_,"?",
        NA_real_,"?",stringsAsFactors=FALSE)
cnames <- c(month.name,"Annual")
names(X) <- c("NCMP",as.vector(t(matrix(c(cnames,paste(cnames,"S",sep="_")),ncol=2))))

for (i in 1:7) { #1:7                                           # Loop for elements
  cat("\t NCMP",i,fill=TRUE)

###################################################################################
#    Calculations for trends by month                                             #
# Not checking that have long enough period to estimate the trend                 #
                                                                                  #
  sq <- seq(from=2,to=24,by=2)                         # sequence of even numbers #
  trend <- rep(NA,13)                          # empty vector to fill with trends #
  pval <- rep(NA,13)                         # empty vector to fill with p-values #
                                                                                  #
  I <- read.csv(file=filez[i],header=TRUE,stringsAsFactors=FALSE,na.strings="-99.9")
  for (j in 1:13) {                              # Loop through months and annual #
    ref <- (I[,'Year'] >= nyb & I[,'Year'] <= nye & I[,'Month'] == j)             #
    q <- zyp.trend.vector(I[ref,'Index'],I[ref,'Year'],method="zhang") # fit trend#
    trend[j] <- q['trendp']                 # extract trend over period to vector #
    pval[j] <- q['sig']                               # extract p-value to vector #
#    q <- lm(I[ref,'Index'] ~ I[ref,'Year'])                    # Linear best fit #
#    cq <- coef(summary(q))                                # Extract coefficients #
#    trend[j] <- cq[2,1]*nyrs                                                     #
#    pval[j] <- cq[2,4]                                                           #
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
  plot(I[ref,'Year'],I[ref,'Index'],xlab="",ylab=ylabel[i],main=title[i],         #
        col="Blue",type="l",ylim=c(ymin[i],ymax[i]),yaxs="i")  # Graph ann values #
  sl <- q["trend"]                          # extract slope (trend per unit time) #
  b <- q["intercept"]                                         # extract intercept #
  abline(b,sl,col="Red",lty=2)                                   # Add trend line #
  dev.off()                                                          # Close plot #
                                                                                  #
  Rank <- order(I[ref,"Index"],decreasing=TRUE)     # missing values are put last #
  I[ref,"Index"] <- round(I[ref,"Index"],2)    # round to 2dp for writing to file #
  write.csv(I[ref[Rank],-2],file=namex[i],row.names=FALSE,na="-99.9") # Write rank#
                                                                                  #
#    Finished graphing/ranking annual values. Done!                               #
###################################################################################

}                                                                # End element loop

###################################################################################
#    Write regional average trends and significance with months in columns        #
                                                                                  #
name <- paste("A5_Trends_Graphs/Trends_by_reg_",nyb,"_",nye,".csv",sep="")        #
write.csv(X,file=name,row.names=FALSE,na="-99.9")                  # write trends #
                                                                                  #
#    Finished writing trends. Done!                                               #
###################################################################################

cat("Calculations and maps done!",fill=TRUE)
