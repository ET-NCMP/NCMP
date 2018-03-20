
###################################################################################
#                                                                                 #
#    The R-NCMPs package has been developed by the ET-NCMP.                       #
#    P1_Quality_Control.R                                                         #
#                                                                                 #
#    This program does an elementary quality control on temp and prec data to     #
#    look for outliers. Temperature outliers are qualified by the number of       #
#    standard deviations from the mean; if the maximum is less than the           #
#    minimum; if temp is outside the user defined limits. Prec outliers are       #
#    qualified by their value exceeding a set level or being negative.            #
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
suppressPackageStartupMessages(library(data.table))                  # call library

###################################################################################
#    Gathers input info from the user                                             #
                                                                                  #
inquiry <- function() {                   # start function, defined before called #
# \n is escape sequence for new line - does this have a locale dependency?        #
                                                                                  #
  x <- NA_real_                                                                   #
  while (is.na(x) || x < 0 || x > 200) {      # is x numeric and >= 1 and <= 200? #
    x <- readline("\nEnter the number of stations to be used (between 1 and 200, or 0 for all): ")
    x <- suppressWarnings(as.numeric(x)) }                                        #
# Allow for numeric rather than integer values - this requires a decimal point    #
# and the easiest way is to do the conversion and suppress the warning            #
                                                                                  #
  uptx <- NA_real_                                                                #
  while (is.na(uptx) || uptx < -65 || uptx > 55) {                                #
    cat("Enter value (Celsius) to identify upper limit for maximum temperature")  #
    uptx <- readline("\n(between -65 and 55, example 40): ")                      #
    uptx <- suppressWarnings(as.numeric(uptx)) }                                  #
                                                                                  #
  lotx <- NA_real_                                                                #
  while (is.na(lotx) || lotx < -65 || lotx > 55) {                                #
    cat("Enter value (Celsius) to identify lower limit for maximum temperature")  #
    lotx <- readline("\n(between -65 and 55, example -30): ")                     #
    lotx <- suppressWarnings(as.numeric(lotx)) }                                  #
                                                                                  #
  uptn <- NA_real_                                                                #
  while (is.na(uptn) || uptn < -65 || uptn > 50) {                                #
    cat("Enter value (Celsius) to identify upper limit for minimum temperature")  #
    uptn <- readline("\n(between -65 and 50, example 25): ")                      #
    uptn <- suppressWarnings(as.numeric(uptn)) }                                  #
                                                                                  #
  lotn <- NA_real_                                                                #
  while (is.na(lotn) || lotn < -65 || lotn > 50) {                                #
    cat("Enter value (Celsius) to identify lower limit for minimum temperature")  #
    lotn <- readline("\n(between -65 and 50, example -55): ")                     #
    lotn <- suppressWarnings(as.numeric(lotn)) }                                  #
                                                                                  #
  pr <- -1                                                                        #
  while (is.na(pr) || pr < 100 || pr > 500) {   # is 500 mm a reasonable extreme? #
    cat("Enter value (mm) to identify daily precipitation outlier")               #
    pr <- readline("\n(between 100 and 500, example 300): ")                      #
    pr <- suppressWarnings(as.numeric(pr)) }                                      #
                                                                                  #
  ts <- 0                                                                         #
  while (is.na(ts) || ts < 3 || ts > 7) {                                         #
    cat("Enter number of standard deviations to identify daily")                  #
    ts <- readline("\ntemperature outlier (between 3 and 7, example 6): ")        #
    ts <- suppressWarnings(as.numeric(ts)) }                                      #
                                                                                  #
  c(x,uptx,lotx,uptn,lotn,pr,ts) } # combine variables                            #
# Do not need explicit return at the end of a function                            #
                                                                                  #
#    User input collected. Done!                                                  #
###################################################################################

if (interactive()) a <- inquiry()        # ask if interactive call function inquiry
#a <- c(112,40,-10,20,-30,500,5)                                  # use for testing
nstn <- as.integer(a[1])                        # truncates after any decimal point
txup <- a[2]
txlo <- a[3]
tnup <- a[4]
tnlo <- a[5]
pmax <- a[6]
tsd  <- a[7]

###################################################################################
#    Create directory for output files and write header                           #
# An alternative is to generate data.frames for each diagnostic and write once    #
# but these might get large if have many QC records                               #
                                                                                  #
folder <- "A1_Quality_Control"                                                    #
ele <- c("Pr_Outliers","TX_Outliers1","TX_Outliers2",                             #
         "TN_Outliers1","TN_Outliers2","TX_less_than_TN")                         #
filet <- file.path(folder,paste("A1_",ele,".csv",sep=""))                         #
dir.create(folder,showWarnings=FALSE)                                             #
                                                                                  #
c1 <- c('"Station","Year","Mo","Day","Prec","PrLow","PrHigh","Error"',            #
        '"Station","Year","Mo","Day","Tx","TxLow","TxHigh","Error"',              #
        '"Station","Year","Mo","Day","Tx","TxLow","TxHigh","Error"',              #
        '"Station","Year","Mo","Day","Tn","TnLow","TnHigh","Error"',              #
        '"Station","Year","Mo","Day","Tn","TnLow","TnHigh","Error"',              #
        '"Station","Year","Mo","Day","Tn","Tx","Error"')                          #
for (i in 1:6) writeLines(c1[i],filet[i])                                         #
                                                                                  #
#    Write header. Done!                                                          #
###################################################################################

###################################################################################
#    Reads the station list                                                       #
# Can set everything up in a data.frame (!= array) in one call to read.table      #
# Rather than insisting that Station names be 23 character with "_" padding,      #
# do this here and use internally for all subsequent files                        #
                                                                                  #
files <- read.table("P0_Station_List.txt",header=FALSE,stringsAsFactors=FALSE,    #
    col.names=c("FileName","Lat","Long"))                                         #
file1 <- file.path("A0_Input_Data",files[,"FileName"])     # file names with path #
Station <- gsub("[[:space:]]|[[:punct:]]|txt|$","",files[,"FileName"])            #
if (nstn == 0L) nstn <- nrow(files) else nstn <- min(nrow(files),nstn)            #
Sout <- rep("_______________________",nstn)            # padding to 23 characters #
substring(Sout,1L,23L) <- Station[1:nstn] # replace "_" with station name up to 23#
                                                                                  #
#    Read station list. Done!                                                     #
###################################################################################

# Output files for each station - not currently created
#namex <- paste(folder,"/",Sout,".csv",sep="")

# Begins loop for reading data files and doing calculations
cat("\n") 
for (i in 1:nstn) {
cat(i,"\t",Station[i],fill=TRUE)  # text to stdout: i=1, Station name, separate with tab

# Read station's data
# Consider allowing CSV files - could then set missing to blank rather than fixed value
# However, since the format of the data file is specified, can use additional arguments
# to read.table to speed it up and reduce memory usage
# Not good practice (but works) to use "data" as a variable name

data <- read.table(file1[i],header=FALSE,na.strings="-99.9",comment.char="",
    colClasses=c("integer","integer","integer","numeric","numeric","numeric"),
    col.names=c("Year","Mo","Day","Prec","Tx","Tn"))

###################################################################################
#    Calculate the daily mean and SD of Tx & Tn                                   #
                                                                                  #
data <- as.data.table(data)                         # convert array to data table #
Means <- data[,.(TxM=mean(Tx,na.rm=TRUE),TnM=mean(Tn,na.rm=TRUE),                 #
              TxSD=min(sd(Tx,na.rm=TRUE),5),TnSD=min(sd(Tn,na.rm=TRUE),5)),       #
              by=.(Mo,Day)]                                                       #
            # Calculate mean and standard deviation by day and month of tx and tn #
#      This is very noisy - synthetic data uses a harmonic fit to these estimates #
                                                                                  #
#Writes data1 matrix with valid range of max and min temps for each day           #
Ref1 <- Means[,.(Mo,Day,TxMax=TxM+tsd*TxSD,TxMin=TxM-tsd*TxSD,                    #
          TnMax=TnM+tsd*TnSD,TnMin=TnM-tsd*TnSD)]              # TxMax is new col #
Ref1 <- round(Ref1,1)                                                             #
                                                                                  #
# Merge back to aid checking                                                      #
# Should not need to reorder columns, but do need the rows in date order          #
data1 <- merge(data,Ref1,by=c("Mo","Day"))                                        #
data1 <- data1[order(data1$Year,data1$Mo,data1$Day),]                             #
data1 <- cbind(Station=Sout[i],data1,Error="none" )           # add extra columns #
n <- nrow(data1)                                                # number of dates #
                                                                                  #
#    Calculated the mean and SD of Tx and Tn. Done!                               #
###################################################################################

###################################################################################
#    Searching for outliers in Pr, Tx and Tn                                      #
                                                                                  #
Eflag <- matrix(FALSE,nrow=n,ncol=11)     # logical matrix for each outlier check #
Eflag[,1] <- (data1[,.(Prec)] > pmax)     # Large prec                            #
Eflag[,2] <- (data1[,.(Prec)] < 0)        # negative prec                         #
Eflag[,3] <- (data1[,.(Tx)] > txup)       # Tmax > absolute high threshold        #
Eflag[,4] <- (data1[,.(Tx)] < txlo)       # Tmax < absolute low threshold         #
Eflag[,5] <- (data1[,.(Tn)] > tnup)       # Tmin > absolute high threshold        #
Eflag[,6] <- (data1[,.(Tn)] < tnlo)       # Tmin < absolute low threshold         #
Eflag[,7] <- (data1[,.(Tx)] > data1[,.(TxMax)]) # Tmax > SD high threshold        #
Eflag[,8] <- (data1[,.(Tx)] < data1[,.(TxMin)]) # Tmax < SD low threshold         #
Eflag[,9] <- (data1[,.(Tn)] > data1[,.(TnMax)]) # Tmin > SD high threshold        #
Eflag[,10] <- (data1[,.(Tn)] < data1[,.(TnMin)]) # Tmin < SD low threshold        #
Eflag[,11] <- (data1[,.(Tn)] > data1[,.(Tx)])   # Tmin > Tmax                     #
Eflag[is.na(Eflag)] <- FALSE                    # replace NA with FALSE           #
                                                                                  #
# Append any detected QC issues to relevant CSV file - keeping the header         #
# Precipitation outliers                                                          #
                                                                                  #
ierr <- (Eflag[,1] | Eflag[,2])                                                   #
if (any(ierr)) {                                                                  #
   data2 <- cbind(data1,PrMin=0,PrMax=pmax)                                       #
   if (any(Eflag[,1])) data2[Eflag[,1],"Error"] <- paste("Prec above",pmax,"mm")  #
   if (any(Eflag[,2])) data2[Eflag[,2],"Error"] <- "Negative Prec"                #
   write.table(data2[ierr,.(Station,Year,Mo,Day,Prec,PrMin,PrMax,Error)],         #
       file=filet[1],append=TRUE,sep=",",row.names=FALSE,col.names=FALSE) }       #
                                                                                  #
# Maximum outliers (1)                                                            #
                                                                                  #
ierr <- (Eflag[,3] | Eflag[,4])                                                   #
if (any(ierr)) {                                                                  #
   data2 <- cbind(data1,TMin=txlo,TMax=txup)                                      #
   if (any(Eflag[,3])) data2[Eflag[,3],"Error"] <- paste("Max temp above",txup,"C")
   if (any(Eflag[,4])) data2[Eflag[,4],"Error"] <- paste("Max temp below",txlo,"C")
   write.table(data2[ierr,.(Station,Year,Mo,Day,Tx,TMin,TMax,Error)],             #
       file=filet[2],append=TRUE,sep=",",row.names=FALSE,col.names=FALSE) }       #
                                                                                  #
# Maximum outliers (2)                                                            #
                                                                                  #
ierr <- (Eflag[,7] | Eflag[,8])                                                   #
if (any(ierr)) {                                                                  #
   if (any(Eflag[,7])) data1[Eflag[,7],"Error"] <- paste("Max temp above",tsd,"SD")
   if (any(Eflag[,8])) data1[Eflag[,8],"Error"] <- paste("Max temp below",tsd,"SD")
   write.table(data1[ierr,.(Station,Year,Mo,Day,Tx,TxMin,TxMax,Error)],           #
       file=filet[3],append=TRUE,sep=",",row.names=FALSE,col.names=FALSE) }       #
                                                                                  #
# Minimum outliers (1)                                                            #
                                                                                  #
ierr <- (Eflag[,5] | Eflag[,6])                                                   #
if (any(ierr)) {                                                                  #
   data2 <- cbind(data1,TMin=tnlo,TMax=tnup)                                      #
   if (any(Eflag[,5])) data2[Eflag[,5],"Error"] <- paste("Min temp above",tnup,"C")
   if (any(Eflag[,6])) data2[Eflag[,6],"Error"] <- paste("Min temp below",tnlo,"C")
   write.table(data2[ierr,.(Station,Year,Mo,Day,Tn,TMin,TMax,Error)],             #
       file=filet[4],append=TRUE,sep=",",row.names=FALSE,col.names=FALSE) }       #
                                                                                  #
# Minimum outliers (2)                                                            #
                                                                                  #
ierr <- (Eflag[,9] | Eflag[,10])                                                  #
if (any(ierr)) {                                                                  #
   if (any(Eflag[,9])) data1[Eflag[,9],"Error"] <- paste("Min temp above",tsd,"SD")
   if (any(Eflag[,10])) data1[Eflag[,10],"Error"] <- paste("Min temp below",tsd,"SD")
   write.table(data1[ierr,.(Station,Year,Mo,Day,Tn,TnMin,TnMax,Error)],           #
       file=filet[5],append=TRUE,sep=",",row.names=FALSE,col.names=FALSE) }       #
                                                                                  #
# Maximum < Minimum                                                               #
                                                                                  #
ierr <- Eflag[,11]                                                                #
if (any(ierr)) {                                                                  #
   data1[ierr,"Error"] <- "Max lower than Min"                                    #
   write.table(data1[ierr,.(Station,Year,Mo,Day,Tn,Tx,Error)],                    #
       file=filet[6],append=TRUE,sep=",",row.names=FALSE,col.names=FALSE) }       #
                                                                                  #
#    Found and wrote all outliers. Done!                                          #
###################################################################################

}                                                                # End station loop
cat("Quality control done!",fill=TRUE)
