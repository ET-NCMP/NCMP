
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

cat("***** P1_Quality_Control.R *****",fill=TRUE)
xx <- getRversion()
if (xx < "3.2.2") {
  cat("R Version",xx,"has not been tested with this code - upgrade is suggested",fill=TRUE)
}
xx <- find.package("data.table",quiet=TRUE)  # Will cause an error for R < 2.13.0
if (length(xx) == 0L) {
  stop("Package 'data.table' is not installed - require Version >= 1.9.6",call.=FALSE)
} else if (packageVersion("data.table") < "1.9.6") {
  cat("Package 'data.table' Version < 1.9.6 has not been tested with this code - upgrade is suggested",fill=TRUE)
}

# Suppress warning messages
# - these should be related to the version of R used to build the package on PC, and can be ignored

op <- options(warn=-1)
suppressPackageStartupMessages(library(data.table))
cat("Successfully loaded packages",
    "Quality Control for daily station time series",
    "Note that output is indicative only - it is not used when calculating NCMP indices",sep='\n')

###################################################################################
# Set variables with key thresholds for Quality Control measures                  #
# These are considered to be suggestions by the ET-NCMP                           #
# Advanced users can consider different limits suitable for their data            #
# Values for each check are stored as (lower,higher,example)                      #
###################################################################################
# Currently have same thresholds for temperature, but different examples

stnhi <- 200L  # maximum number of stations
TXThi <- c(-65,55,40)    # upper limit for Tx outlier
TXTlo <- c(-65,55,-30)   # lower limit for Tx outlier
TNThi <- c(-65,55,25)    # upper limit for Tn outlier
TNTlo <- c(-65,55,-55)   # lower limit for Tn outlier
PrThi <- c(100,500,300)  # upper limit for Pr outlier
TSDThi <- c(3,7,6)       # upper limit for temperarure standard deviation outlier
TdSDhi <- 5              # upper limit for sample daily temperature standard deviation

###################################################################################
#    Gathers input info from the user                                             #
# For clarity, no longer do this as a separate function                           #
###################################################################################
# Number of stations to process
# Still suppressing warning messages - about converting strings to integer

nstn <- NA_integer_
mess <- paste("\nbetween 1 and ",stnhi,", or 0 for all - recommended = all : ")
while (is.na(nstn) || nstn < 0L || nstn > stnhi) {
  cat("\nEnter the number of stations to use")
  nstn <- readline(mess)
  nstn <- as.integer(nstn)
}

# Upper and lower limits for each variable

txup <- NA_real_
mess <- paste("\nbetween ",TXThi[1],"and",TXThi[2],"- example =",TXThi[3],": ")
while (is.na(txup) || txup < TXThi[1] || txup > TXThi[2]) {
  cat("Enter value (Celsius) to identify upper limit for maximum temperature")
  txup <- readline(mess)
  txup <- as.numeric(txup)
}

txlo <- NA_real_
mess <- paste("\nbetween ",TXTlo[1],"and",TXTlo[2],"- example =",TXTlo[3],": ")
while (is.na(txlo) || txlo < TXTlo[1] || txlo > TXTlo[2]) {
  cat("Enter value (Celsius) to identify lower limit for maximum temperature")
  txlo <- readline(mess)
  txlo <- as.numeric(txlo)
}

tnup <- NA_real_
mess <- paste("\nbetween ",TNThi[1],"and",TNThi[2],"- example =",TNThi[3],": ")
while (is.na(tnup) || tnup < TNThi[1] || tnup > TNThi[2]) {
  cat("Enter value (Celsius) to identify upper limit for minimum temperature")
  tnup <- readline(mess)
  tnup <- as.numeric(tnup)
}

tnlo <- NA_real_
mess <- paste("\nbetween ",TNTlo[1],"and",TNTlo[2],"- example =",TNTlo[3],": ")
while (is.na(tnlo) || tnlo < TNTlo[1] || tnlo > TNTlo[2]) {
  cat("Enter value (Celsius) to identify lower limit for minimum temperature")
  tnlo <- readline(mess)
  tnlo <- as.numeric(tnlo)
}
  
prup <- NA_real_
mess <- paste("\nbetween ",PrThi[1],"and",PrThi[2],"- example =",PrThi[3],": ")
while (is.na(prup) || prup < PrThi[1] || prup > PrThi[2]) {
  cat("Enter value (mm) to identify daily precipitation outlier")
  prup <- readline(mess)
  prup <- as.numeric(prup)
}

# Daily temperature standard deviation  
  
tsd <- NA_real_
mess <- paste("\nbetween ",TSDThi[1],"and",TSDThi[2],"- example =",TSDThi[3],": ")
while (is.na(tsd) || tsd < TSDThi[1] || tsd > TSDThi[2]) {
  cat("Enter number of standard deviations to identify daily temperature outlier")
  tsd <- readline(mess)
  tsd <- as.numeric(tsd)
}
cat("Thank you. User input collected",fill=TRUE)

# Turn warnings back on, but print immediately
options(warn=1)

###################################################################################
#    Create directory for output files and write header                           #
# An alternative is to generate data.frames for each diagnostic and write once    #
# but these might get large if have many QC records                               #
###################################################################################

folder <- "A1_Quality_Control"
ele <- c("Pr_Outliers","TX_Outliers1","TX_Outliers2",
         "TN_Outliers1","TN_Outliers2","TX_less_than_TN")
filet <- file.path(folder,paste("A1_",ele,".csv",sep=""))
dir.create(folder,showWarnings=FALSE)
cat("Directory successfully created",fill=TRUE)

c1 <- c('"Station","Year","Mo","Day","Prec","PrLow","PrHigh","Error"',
        '"Station","Year","Mo","Day","Tx","TxLow","TxHigh","Error"',
        '"Station","Year","Mo","Day","Tx","TxLow","TxHigh","Error"',
        '"Station","Year","Mo","Day","Tn","TnLow","TnHigh","Error"',
        '"Station","Year","Mo","Day","Tn","TnLow","TnHigh","Error"',
        '"Station","Year","Mo","Day","Tn","Tx","Error"')
for (i in 1:6) writeLines(c1[i],filet[i])
cat("Output file headers successfully written",fill=TRUE)

###################################################################################
#    Reads the station list                                                       #
# Can set everything up in a data.frame (!= array) in one call to read.table      #
# Rather than insisting that Station names be 23 character with "_" padding,      #
# do this internally - although this is only used here in the QC files            #
###################################################################################

files <- read.table("P0_Station_List.txt",header=FALSE,stringsAsFactors=FALSE,
    col.names=c("FileName","Lat","Long"))
file1 <- file.path("A0_Input_Data",files[,"FileName"])  # file names with path

# Get number of stations, subject to the upper limit

nstns <- nrow(files)
if (nstn == 0L) nstn <- min(nstns,stnhi)
if (nstn > nstns) {
  warning("Requested ",nstn," stations but only ",nstns," available - resetting",call.=FALSE)
  nstn <- nstns
} else if (nstn < nstns) {
  cat("Processing first",nstn,"of",nstns,"stations in file",fill=TRUE)
}

# Remove non-standard characters from input station name and fixed at
# 23 characters, appeding with "_" where required
# This formatting may change in future versions of the code

Station <- gsub("[[:space:]]|[[:punct:]]|txt|$","",files[,"FileName"])
Sout <- rep("_______________________",nstn)
substring(Sout,1L,23L) <- Station[1:nstn]
cat("Sucessfully read in station list",fill=TRUE)

###################################################################################
# Begins loop for reading data files and doing calculations                       #
###################################################################################

for (i in 1:nstn) {
cat(i,"\t",Station[i],fill=TRUE)  # text to stdout: i=1, Station name, separate with tab

# Read station data
# Consider allowing CSV files - could then set missing to blank rather than fixed value
# However, since the format of the data file is specified, can use additional arguments
# to read.table() to speed it up and reduce memory usage
# Not good practice (but works) to use "data" as a variable name

data <- read.table(file1[i],header=FALSE,na.strings="-99.9",comment.char="",
    colClasses=c("integer","integer","integer","numeric","numeric","numeric"),
    col.names=c("Year","Mo","Day","Prec","Tx","Tn"))
data <- as.data.table(data)  # convert data.frame to data.table object

###################################################################################
#    Calculate the daily mean and SD of Tx & Tn                                   #
# The use of daily mean and SD is very noisy - might want to consider alternatives#
###################################################################################
# Note that the upper limit on the daily standard deviation is *not* a constraint
# on the subsequent check for temperature outliers

Means <- data[,.(TxM=mean(Tx,na.rm=TRUE),TnM=mean(Tn,na.rm=TRUE),
              TxSD=min(sd(Tx,na.rm=TRUE),TdSDhi),TnSD=min(sd(Tn,na.rm=TRUE),TdSDhi)),
              by=.(Mo,Day)]

# Writes data1 matrix with valid range of max and min temps for each day

Ref1 <- Means[,.(Mo,Day,TxMax=TxM+tsd*TxSD,TxMin=TxM-tsd*TxSD,
          TnMax=TnM+tsd*TnSD,TnMin=TnM-tsd*TnSD)]
Ref1 <- round(Ref1,1)

# Merge back to aid checking
# This requires re-ordering as merge() sorts by character, not integer

data1 <- merge(data,Ref1,by=c("Mo","Day"))
data1 <- data1[order(data1$Year,data1$Mo,data1$Day),]
data1 <- cbind(Station=Sout[i],data1,Error="none" )

cat("Daily mean and SD successfully calculated",fill=TRUE)

###################################################################################
#    Searching for outliers in Pr, Tx and Tn                                      #
###################################################################################

n <- nrow(data1)  # number of dates
Eflag <- matrix(FALSE,nrow=n,ncol=11)     # logical matrix for each outlier check
Eflag[,1] <- (data1[,.(Prec)] > prup)     # Large prec
Eflag[,2] <- (data1[,.(Prec)] < 0)        # negative prec
Eflag[,3] <- (data1[,.(Tx)] > txup)       # Tmax > absolute high threshold
Eflag[,4] <- (data1[,.(Tx)] < txlo)       # Tmax < absolute low threshold
Eflag[,5] <- (data1[,.(Tn)] > tnup)       # Tmin > absolute high threshold
Eflag[,6] <- (data1[,.(Tn)] < tnlo)       # Tmin < absolute low threshold
Eflag[,7] <- (data1[,.(Tx)] > data1[,.(TxMax)]) # Tmax > SD high threshold
Eflag[,8] <- (data1[,.(Tx)] < data1[,.(TxMin)]) # Tmax < SD low threshold
Eflag[,9] <- (data1[,.(Tn)] > data1[,.(TnMax)]) # Tmin > SD high threshold
Eflag[,10] <- (data1[,.(Tn)] < data1[,.(TnMin)]) # Tmin < SD low threshold
Eflag[,11] <- (data1[,.(Tn)] > data1[,.(Tx)])   # Tmin > Tmax
Eflag[is.na(Eflag)] <- FALSE  # replace NA with FALSE

# Append any detected QC issues to relevant CSV file - keeping the header
# Precipitation outliers

ierr <- (Eflag[,1] | Eflag[,2])
if (any(ierr)) {
   data2 <- cbind(data1,PrMin=0,PrMax=prup)
   if (any(Eflag[,1])) data2[Eflag[,1],"Error"] <- paste("Prec above",prup,"mm")
   if (any(Eflag[,2])) data2[Eflag[,2],"Error"] <- "Negative Prec"
   write.table(data2[ierr,.(Station,Year,Mo,Day,Prec,PrMin,PrMax,Error)],
       file=filet[1],append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)
}

# Maximum outliers (1)

ierr <- (Eflag[,3] | Eflag[,4])
if (any(ierr)) {
   data2 <- cbind(data1,TMin=txlo,TMax=txup)
   if (any(Eflag[,3])) data2[Eflag[,3],"Error"] <- paste("Max temp above",txup,"C")
   if (any(Eflag[,4])) data2[Eflag[,4],"Error"] <- paste("Max temp below",txlo,"C")
   write.table(data2[ierr,.(Station,Year,Mo,Day,Tx,TMin,TMax,Error)],
       file=filet[2],append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)
}

# Maximum outliers (2)

ierr <- (Eflag[,7] | Eflag[,8])
if (any(ierr)) {
   if (any(Eflag[,7])) data1[Eflag[,7],"Error"] <- paste("Max temp above",tsd,"SD")
   if (any(Eflag[,8])) data1[Eflag[,8],"Error"] <- paste("Max temp below",tsd,"SD")
   write.table(data1[ierr,.(Station,Year,Mo,Day,Tx,TxMin,TxMax,Error)],
       file=filet[3],append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)
}

# Minimum outliers (1)

ierr <- (Eflag[,5] | Eflag[,6])
if (any(ierr)) {
   data2 <- cbind(data1,TMin=tnlo,TMax=tnup)
   if (any(Eflag[,5])) data2[Eflag[,5],"Error"] <- paste("Min temp above",tnup,"C")
   if (any(Eflag[,6])) data2[Eflag[,6],"Error"] <- paste("Min temp below",tnlo,"C")
   write.table(data2[ierr,.(Station,Year,Mo,Day,Tn,TMin,TMax,Error)],
       file=filet[4],append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)
}

# Minimum outliers (2)

ierr <- (Eflag[,9] | Eflag[,10])
if (any(ierr)) {
   if (any(Eflag[,9])) data1[Eflag[,9],"Error"] <- paste("Min temp above",tsd,"SD")
   if (any(Eflag[,10])) data1[Eflag[,10],"Error"] <- paste("Min temp below",tsd,"SD")
   write.table(data1[ierr,.(Station,Year,Mo,Day,Tn,TnMin,TnMax,Error)],
       file=filet[5],append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)
}

# Maximum < Minimum

ierr <- Eflag[,11]
if (any(ierr)) {
   data1[ierr,"Error"] <- "Max lower than Min"
   write.table(data1[ierr,.(Station,Year,Mo,Day,Tn,Tx,Error)],
       file=filet[6],append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)
}

cat("Check for outliers completed",fill=TRUE)

}

###################################################################################
# Ends loop for stations                                                          #
###################################################################################

cat("Quality control done!",fill=TRUE)
options(op)
