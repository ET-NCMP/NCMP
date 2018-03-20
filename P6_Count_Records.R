###################################################################################
#                                                                                 #
#    The R-NCMPs package has been developed by the ET-NCMP.                       #
#    P6_Count_Records.R                                                           #
#                                                                                 #
#    This program counts the number of records for 5 different indices.           #
#    For further details please refer to the User Manual.                         #
#                                                                                 #
#    Programmers:                                                                 #
#    Megan Hartwell, McMaster University, Canada                                  #
#    Lucie Vincent, Environment and Climate Change Canada                         #
#    December 2016                                                                #
#    Modified by Simon Grainger, Bureau of Meteorology, Australia                 #
#    December 2017                                                                #
#                                                                                 #
###################################################################################

cat("***** P6_Count_Records.R *****",fill=TRUE)

# An effective check of whether can run the code is to load the configuration file
# If it does not exist, the relevant script has not been successfully run

source("Support_configuration.R")
clist.P2 <- read_configuration("P2")
clist.P4 <- read_configuration("P4")

###################################################################################
# Set variables with key thresholds for estimating variogram functions            #
# These are values which have been determined by the ET-NCMP for the purposes of  #
# generating NCMPs and may differ from other standards or guidelines              #
# The ET-NCMP recommendation is to use the region average period                  #
# ***** DO NOT CHANGE THE VALUE OF THESE VARIABLES *****                          #
###################################################################################

stnhi <- clist.P2$nstn  # maximum number of stations
ylo <- 1900L            # earliest possible year for count records
yhi <- as.POSIXlt(Sys.time())$year + 1899L  # latest possible year == current - 1
yclo <- clist.P4$nyb    # recommended start year of count record period
ychi <- clist.P4$nye    # recommended end year of count record period
nthresh <- 30L          # minimum number of years for valid station record

###################################################################################
#    Gathers input info from the user                                             #
# For clarity, no longer do this as a separate function                           #
###################################################################################

cat("Count the number of station temperature and precipitation records set each month",
    "Output both the count of absolute and cumulative (over years) records",sep="\n")

# Number of stations to process
# Can exclude stations at the end of the list, but arbitrary selection requires
# editing of 'P2_Station_List.txt'
# Suppressing warning messages - about converting strings to integer

op <- options(warn=-1)	
cat("Can either use the first 'n' stations processed by 'P2_Indices.R' or all of them",fill=TRUE)
nstn <- NA_integer_
mess <- paste("\nbetween 1 and ",stnhi,", or 0 for all - recommended =",clist.P4$nstn,": ")
while (is.na(nstn) || nstn < 0L || nstn > stnhi) {
  cat("Enter the number of stations to use")
  nstn <- readline(mess)
  nstn <- as.integer(nstn)
  }
if (nstn == 0L) nstn <- stnhi

# For counting station records, allow the full range of years
# but recommend the current region average period
# Adjust for the minimum station length - should be OK

cat("The ET-NCMP recommends a count record period of",yclo,"-",ychi,fill=TRUE)
yr1 <- ylo
yr2 <- yhi - nthresh + 1L
nyb <- 0L
mess <- paste("\nbetween",yr1,"and",yr2,"- recommended =",yclo,": ")
while (is.na(nyb) || nyb < yr1 || nyb > yr2) {
  cat("Enter beginning year for count record period")
  nyb <- readline(mess)
  nyb <- as.integer(nyb)
}

yr1 <- nyb + nthresh - 1L
yr2 <- yhi
nye <- 0L
mess <- paste("\nbetween",yr1,"and",yr2,"- recommended =",ychi,": ")
while (is.na(nye) || nye < yr1 || nye > yr2) {
  cat("Enter ending year for count period")
  nye <- readline(mess)
  nye <- as.integer(nye)
}
nyrs <- nye-nyb+1L
cat("User input collected",fill=TRUE)

# Turn warnings back on, but print immediately
options(warn=1)

###################################################################################
# Set up variables for the 5 station extremes to count                            #
###################################################################################
# For looping, do minimum extremes first, then the maxima

ncmpn <- c(6L,6L,6L,6L,6L)
folder <- "A2_Indices"
folder2 <- paste("NCMP",ncmpn,sep="")
folder3 <- c("Extreme_Cold_Day","Extreme_Cold_Night",
             "Extreme_Warm_Day","Extreme_Warm_Night","Extreme_Prec")
dirs <- file.path(folder,folder2,folder3)  # adds separator "/"

# Element relating to NCMP Index

ele <- c("TXn","TNn","TXx","TNx","RXday1")

# Output file names
# The Guidance indicates that this should be one file, and not TXn or TNx

folder <- "A6_Count_Records"
tname <- clist.P4$tname
filer <- file.path(folder,paste(tname,ele,"Count_Record.csv",sep="_"))
filec <- file.path(folder,paste(tname,ele,"Count_Record_Accum.csv",sep="_"))
dir.create(folder,showWarnings=FALSE)

###################################################################################
#    Read the modified station list                                               #
###################################################################################
# With the number of stations already set, extract the subset to use

namex <- file.path("A2_Indices","P2_Station_List.txt")
files <- read.table(namex,header=TRUE,stringsAsFactors=FALSE)
Station <- files[1:nstn,"Station"]

###################################################################################
# Begins loop over NCMP indices                                                   #
###################################################################################

Yrc <- nyb:nye  # vector of record count period years
cYrc <- as.character(Yrc)
cnames <- c(month.name,"Annual")  # only used internally

for (ne in 1:5) {
  cat("\t",ele[ne],fill=TRUE)

# Initialise counter arrays for this index
# Using the years for dimension names makes it easier to match periods

  Icount <- matrix(0L,nyrs,13L,dimnames=list(Yrc,cnames))
  Irec <- matrix(0L,nyrs,13L,dimnames=list(Yrc,cnames))
  Irecc <- matrix(0L,nyrs,13L,dimnames=list(Yrc,cnames))

###################################################################################
# Begins loop over stations		                                                  #
###################################################################################

  namex <- file.path(dirs[ne],paste(Station,"_",ele[ne],".csv",sep=""))
  for (i in 1:nstn) {
#    cat(i,"\t",Station[i],fill=TRUE)

###################################################################################
#    Read index data for all stations:                                            #
# These are formatted as year by row, with months + annual extreme,               #
# with an extra row at the bottom for the station extreme value                   #
# Within P2_Indices.R, the extreme values were found by applying which.max/min()  #
# This is has the effect of producing a NULL value when a month is missing,       #
# causing the data.table to collapse missing years - an issue which has to        #
# addressed when trying to determine whether have 30 years of station data        #
# Another problem with the way that the records are generated is that the         #
# missing days threshold is not applied, i.e. set extreme if have one day in month#
# A significant rewrite of P2_Indices.R is required to pad out missing years      #
# with missing extremes - which would likely fix several other issues             #
###################################################################################

    I1 <- read.csv(namex[i],header=TRUE,stringsAsFactors=FALSE,
	  na.strings="-99.9",row.names=1)
    I1 <- I1[-nrow(I1),]  # remove bottom row

# For now, skip if less than 30 years, and print message if first 30 years is incomplete
# Have to allow for missing years in the table as indicated above

    nyr <- nrow(I1)
    if (nyr < nthresh) {
      cat("Less than",nthresh,"years at",Station[i],"- skipping",fill=TRUE)
      next
    }

    cYr <- rownames(I1)                 # station years as character vector
    Yr <- as.integer(cYr)               # station years as integer vector
    y1 <- Yr[1]                         # first year of station
    y2 <- y1+nthresh-1L                 # last year of initial period
    Irs <- which(is.element(Yr,y1:y2))  # indices of initial period
    if (length(Irs) < nthresh)
	  {cat("Less than",nthresh,"contiguous years at start of",Station[i],fill=TRUE)}

# Which station years are in the reporting period?
# Consequently, increment counter for stations reporting - logical F/T to integer 0/1

    Ir <- is.element(Yr,Yrc)
    Icount[cYr[Ir],] <- Icount[cYr[Ir],] + !is.na(I1[Ir,])

# Find accumulation of record min/max depending on the NCMP index
# cummin/max requires NA to be reset to +/-Inf to work as intended

    if (ne <= 2L) {
      I1[is.na(I1)] <- +Inf
      Ival <- sapply(I1,cummin)
    } else {
      I1[is.na(I1)] <- -Inf
      Ival <- sapply(I1,cummax)
    }

# Having done this, can extract the index (from the start year) of
# each cumulative record from the sum of the run lengths
# The need to append to the time series generates a spurious "record" after
# the end of the data, but this is able to be ignored when converting to years

    Irl <- apply(Ival,2,function(x) cumsum(c(1L,rle(x)$lengths)))

# For each month, convert to the year of the record, subject to not more
# than one cumulative record in the first 30 years
# Also save the year of the absolute (non-cumulative) record

    ilen <- sapply(Irl,length)  # number of records for each month (+ 1 spurious)
    for (j in 1:13) {
      jmax <- max(which(Yr[Irl[[j]]] <= y2))  # index of last record in start period
      jrec <- which(is.element(Yr[Irl[[j]]],Yrc) & 1:ilen[j] >= jmax)  # index of valid records
	  jlen <- length(jrec)
      if (jlen > 0L) {               # have at least one valid record for this month
	    jcYr <- cYr[Irl[[j]][jrec]]  # year of valid records, as character
	    Irecc[jcYr,j] <- Irecc[jcYr,j] + 1L
        Irec[jcYr[jlen],j] <- Irec[jcYr[jlen],j] + 1L
	  }
    }

# Ends loop over months

  }
  
###################################################################################
# Ends loop over stations                                                         #
###################################################################################

###################################################################################
#    Write record counts                                                          #
###################################################################################
# This is now in a format consistent with the Region Averages
# However the separate files and column names are not consistent with the Guidance

  cat("Writing results",fill=TRUE)
  X <- data.frame(Year=rep(Yrc,each=13),Month=rep(1:13,times=nyrs),
       Count=as.vector(t(Irec)),"No of Stns"=as.vector(t(Icount)),check.names=FALSE)
  write.csv(X,file=filer[ne],row.names=FALSE)
 
  X[,"Count"] <- as.vector(t(Irecc))  # Replace absolute count with accumulated
  write.csv(X,file=filec[ne],row.names=FALSE)
}

###################################################################################
# Ends loop over indices                                                          #
###################################################################################
# It is probably useful to retain a "configuration" file for counting records

namex <- file.path(folder,"P6_Configuration.txt")
dy <- date()
desc <- c("Date of processing","Number of stations",
  "Start of count record period","End of count record period")
mess <- paste(desc,c(dy,nstn,nyb,nye),sep=" = ")  # Variables are converted to strings
writeLines(mess,con=namex)

cat("Finished counting record stations!",fill=TRUE)
options(op)
