###################################################################################
#                                                                                 #
#    The R-NCMPs package has been developed by the ET-NCMP.                       #
#    P7_Summary.R                                                                 #
#                                                                                 #
#    This program assembles the NCMPs to be sent to WMO.                          #
#                                                                                 #
#    Programmer:                                                                  #
#    Megan Hartwell, McMaster University, Canada                                  #
#    Lucie Vincent, Environment and Climate Change Canada                         #
#    December 2016                                                                #
#    Modified by Justin Peter, Bureau of Meteorology, Australia                   #
#    December 2017                                                                #
#    Added Count Records by Simon Grainger, Bureau of Meteorology, Australia      #
#    March 2018                                                                   #
###################################################################################

cat("***** P7_Summary.R *****",fill=TRUE)

# Since using merge(), consider whether to load the 'data.table' package,
# although the penalty is probably nominal in this script
# An effective check of whether can run the code is to load the configuration file
# If it does not exist, the relevant script has not been successfully run

source("Support_configuration.R")
clist.P2 <- read_configuration("P2")
clist.P4 <- read_configuration("P4")

###################################################################################
#    Gathers input info from the user                                             #
# For clarity, no longer do this as a separate function                           #
###################################################################################
# Suppressing warning messages - about converting strings to integer

op <- options(warn=-1)	
cat("Generate the Summary file as given in the WMO ET-NCMP Guidance",fill=TRUE)

# The period for the Summary file is limited by the Region Average period,
# although the count records can use a different one

ylo <- clist.P4$nyb
yhi <- clist.P4$nye

cat("The ET-NCMP recommends a Summary file period of",ylo,"-",yhi,fill=TRUE)
nyb <- 0L
mess <- paste("\nbetween",ylo,"and",yhi,"- recommended =",ylo,": ")
while (is.na(nyb) || nyb < ylo || nyb > yhi) {
  cat("Enter beginning year for Summary file period")
  nyb <- readline(mess)
  nyb <- as.integer(nyb)
}

nye <- 0L
mess <- paste("\nbetween",nyb,"and",yhi,"- recommended =",yhi,": ")
while (is.na(nye) || nye < nyb || nye > yhi) {
  cat("Enter ending year for count period")
  nye <- readline(mess)
  nye <- as.integer(nye)
}
nyrs <- nye-nyb+1L
cat("User input collected",fill=TRUE)

# Turn warnings back on, but print immediately
options(warn=1)

###################################################################################
# Information for indices to process                                              #
# Note that the Guidance only requires the extreme warm day and cold nights       #
# - i.e. TX90p, TN10p Region averages and TXx, TNn and RXday1 absolute records    #
# Make the others optional here, along with PrA as it is not clear whether the    #
# Region average estimation is reliable for that variable                         #
###################################################################################
# Names of input regional average files
	
tname <- clist.P4$tname
elez <- c("TMA","PrAn","PrA","SPI","TX90p","TN90p","TX10p","TN10p")
folder <- "A4_Region_Average"
# *** Hack for testing - should be tname
filez <- file.path(folder,paste("NCMP",elez,"Region_Avg.csv",sep="_"))
optionalz <- c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE,TRUE,FALSE)

# Names of input count record files

eler <- c("TXx","TXn","TNx","TNn","RXday1")
folder <- "A6_Count_Records"
filer <- file.path(folder,paste(tname,eler,"Count_Record.csv",sep="_"))
optionalr <- c(FALSE,TRUE,TRUE,FALSE,FALSE)

# Concatenate these into a single variable to enable a single loop

ele <- c(elez,eler)
namex <- c(filez,filer)
optional <- c(optionalz,optionalr)

# Initialise storage for output tables
# Use dummy tables for merging each diagnostic

X <- data.frame(Year=rep(nyb:nye,each=13),Month=rep(1:13,times=nyrs))
X0 <- X
nrec <- nrow(X)
Xdum <- data.frame(X0,Index=rep(NA_real_,nrec),Count=rep(0L,nrec))

###################################################################################
# Begins loop over diagnostic files                                               #
###################################################################################

cat("Extracting Region Average and absolute Count Records",fill=TRUE)
for (ne in seq_along(ele)) {

# Set column names for this diagnostic
# These are consistent with P4 and P6 files but not the Guidance
# This may need to change, as it is arguable that TX90p and TXx are confusing
# (although these names are ultimately based on the WMO ETCCDI definitions)

  if (ne <= 8L) {
    desc <- "Region Average"
	vname1 <- "Index"
  } else {
    desc <- "Count Record"
	vname1 <- "Count"
  }
  vnames <- paste(ele[ne],c(vname1,"No of Stns"))

# Check whether diagnostic has been calculated
# If not, fill with the dummy table
# Note that P6_Count_Records.R currently processes all 5 diagnostics at once

  if (!file.exists(namex[ne])) {
    cat(desc,"has not been calculated for",ele[ne],fill=TRUE)
	if (!optional[ne]) {
	  cat("Summary file will be filled with missing values",fill=TRUE)
	  names(Xdum)[3:4] <- vnames
	  X <- merge(X,Xdum,by=c("Year","Month"))
	  next
	}
  }

# Ask if wish to skip the "optional" diagnostics

  if (optional[ne]) {
    cat(desc,ele[ne],"is optional for Summary file",fill=TRUE)
	icheck <- readline("\nDo you wish to skip this diagnostic? (y/n) : ")
	if (icheck == 'y') next
  }

# Read in diagnostic, and extract the requested years
# The Guidance implies rounding to 2dp, but the annual extraction is to 3dp
# Allowing for extra columns, e.g. planned station index of record counts

  In <- read.csv(file=namex[ne],header=TRUE,na.strings="-99.9",check.names=FALSE)
  names(In)[3:4] <- vnames
  if (ne <= 8L) In[,3] <- round(In[,3],3)
  X <- merge(X,In[,1:4],by=c("Year","Month"),all.x=TRUE)
}

###################################################################################
# Ends loop over diagnostic files                                                 #
###################################################################################
# While by default merge() will sort the output rows, it does so as a character vector
# Thus the months are in the order 1,10,11,12,13,2,...,9 (in an English locale)
# Put back into the expected chronological order

ind <- order(X[,"Year"],X[,"Month"])

cat("Writing Summary file",fill=TRUE)

# Create a header for the Summary file
# This consists of key metadata used in the analysis
# The idea of a header, and combining the QC and homogenisation flags, differs from
# the format in the Guidance, but no information has been lost
# (unless one can do homogenisation *without* Quality Control, or these vary in time)

desc <- c("Total number of Stations",
          "Climatological period start","Climatological period end",
          "Temperature Quality Control level","Precipitation Quality Control level",
          "Region Average grid resolution","Version")
vals <- c(clist.P2$nstn,clist.P2$nyb,clist.P2$nye,clist.P2$QCT,clist.P2$QCPr,
          clist.P4$res,2.0)
mess <- paste(desc,vals,sep=" = ")

# Write header and table to single file - generates a warning which can be ignored
# File has been tested as directly readable by Office 2016 Excel

namex <- paste(tname,nyb,nye,"Summary.csv",sep="_")
writeLines(mess,con=namex)
suppressWarnings(write.table(X[ind,],file=namex,append=TRUE,row.names=FALSE,sep=","))

cat("Summary file completed!",fill=TRUE)
options(op)
