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
  y1 <- 0L                                                                        #
  while (is.na(y1) || y1 < 1950L || y1 > 2010L) {                                 #
    cat("Enter beginning year for counts of records")                             #
    y1 <- readline("\n(between 1950 and 2010, ex. 1950): ")                       #
    y1 <- suppressWarnings(as.integer(y1))                                        #
  }                                                                               #
                                                                                  #
# Allow counting of records for a single year - might help with appending         #
  y2 <- 0L                                                                        #
  y2l <- y1                                                                       #  
  y2h <- as.POSIXlt(Sys.time())$year + 1899L                # last completed year #
  yex <- max(y2l,min(2015L,y2h))                                                  #
  mess <- paste("\n(between ",y2l," and ",y2h,",ex. ",yex,"): ",sep="")           #
  while (is.na(y2) || y2 < y2l || y2 > y2h) {                                     #
    cat("Enter ending year for counts of records")                                #
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
#a <- c(112L,1950L,2015L)
nstn <- a[1]
nyb  <- a[2]
nye <- a[3]
nyrs <- nye-nyb+1L
Yrc <- nyb:nye

nthresh <- 30L  # number of years for valid station record

###################################################################################
#    Reads the station list                                                       #
                                                                                  #
files <- read.table("A2_Indices/P2_Station_List.txt",header=TRUE,stringsAsFactors=FALSE)
Station <- files[,"Station"]
if (nstn == 0L) nstn <- nrow(files) else nstn <- min(nrow(files),nstn)            #
                                                                                  #
#    Read station list. Done!                                                     #
###################################################################################

# Name of input indices files
# Re-ordered to do extreme minima followed by extreme maxima
namex <- c("A2_Indices/NCMP6/Extreme_Cold_Day/",
           "A2_Indices/NCMP6/Extreme_Cold_Night/",
           "A2_Indices/NCMP6/Extreme_Warm_Day/",
           "A2_Indices/NCMP6/Extreme_Warm_Night/",
           "A2_Indices/NCMP6/Extreme_Prec/")

# Element of indices to process
ele <- c("TXn","TNn","TXx","TNx","RXday1")

# Output file names
folder <- "A6_Count_Records"
filec <- paste(folder,"/NCMP_",ele,"_Count_Rec.csv",sep="")
dir.create(folder,showWarnings=FALSE)

# Dimension names - these are of type character

cYrc <- as.character(Yrc)
cnames <- c(month.name,"Annual")
vnames <- c("Record count","Total stations") # not yet consistent with Regional Average files

for (ne in seq_along(ele)) {  # Loop index based on length of elements

###################################################################################
#    Read index data for all stations:                                            #
# These are formatted as year by row, with months + annual extreme,               #
# with an extra row at the bottom for the station extreme value                   #
# Utilise this format internally but write to same format as A4 Regional Average  #
# Using the years for dimension names makes it easier to match periods            #
                                                                                  #
  cat("Reading data for element",ele[ne],fill=TRUE)                               #
  Icount <- array(0L,c(nyrs,13L,2L),list(Yrc,cnames,vnames))                      #
                                                                                  #
  name1 <- paste(namex[ne],Station,"_",ele[ne],".csv",sep="")    # all file names #
  for (i in 1:nstn) {                                                  # Stn loop #
#    cat(i,"\t",Station[i],fill=TRUE)                                             #
    I1 <- read.csv(name1[i],header=TRUE,stringsAsFactors=FALSE,na.strings="-99.9",#
                   row.names=1)                                    # read in data #
    I1 <- I1[-nrow(I1),]                                      # remove bottom row #
                                                                                  #
# The extremes indices estimatation by climdex.pcic will remove years where       #
# no monthly values can be calculated. This contrasts with other manipulations    #
# of data.table where missing data is padded out by NA                            #
# The preferred solution is to ensure that P2_Indices.R pads out missing years    #
# with NA - regardless of the source. More work is required to achieve that       #
# For now, skip if less than 30 years, and warn if first 30 years is incomplete   #
# And have to allow for those missing years                                       #
                                                                                  #
    nyr <- nrow(I1)                                                               #
    if (nyr < nthresh) {                                                          #
      cat("Less than",nthresh,"years at",Station[i],"- skipping",fill=TRUE)       #
      next                                                                        #
    }                                                                             #
                                                                                  #
    cYr <- rownames(I1)                     # years of data - as "character" type #
    Yr <- as.integer(cYr)                                                         #
    y1 <- Yr[1]                                              # First year of data #
    y2 <- y1+nthresh-1L                             # last year of initial period #
    Irs <- which(is.element(Yr,y1:y2))                  # Indices of start period #
    if (length(Irs) < nthresh) {                                                  #
       cat("Less than",nthresh,"contiguous years at start of",Station[i],fill=TRUE)
    }                                                                             #
                                                                                  #
# Which station years are in the reporting period?                                #
# Consequently, increment counter for stations reporting - logical F/T to integer 0/1
                                                                                  #
    Ir <- is.element(Yr,Yrc)                                                      #
    Icount[cYr[Ir],,2] <- Icount[cYr[Ir],,2] + !is.na(I1[Ir,])                    #
                                                                                  #
# Find accumulation of record min/max depending on the NCMP index                 #
# cummin/max requires NA to be reset to +/-Inf to work as intended                #
# Having done this, can extract the year of the record by the index of the        #
# run length of each record                                                       #
                                                                                  #
    if (ne <= 2L) {                                                               #
      I1[is.na(I1)] <- +Inf                                                       #
      Ival <- sapply(I1,cummin)                                                   #
    } else {                                                                      #
      I1[is.na(I1)] <- -Inf                                                       # 
      Ival <- sapply(I1,cummax)                                                   #
    }                                                                             #
    Irl <- apply(Ival,2,function(x) cumsum(c(1L,rle(x)$lengths)))                 #
                                                                                  #
# Drop all records except the most extreme (the last occurance) in the first 30 years
# and the last "record" - this occurs from the need to extend the series by one   #
# Increment the counter of any remaining years of record by one                   #
                                                                                  #
    ilen <- sapply(Irl,length)-1L                                                 #
    for (j in 1:13) {                                                # month loop #
      Irec <- Irl[[j]][1:ilen[j]]                # drops the spurious last record #
      imax <- max(which(Yr[Irec] <= y2))          # last record in initial period #
      cRec <- cYr[Irec[imax:ilen[j]]]                                             #
      ind <- which(is.element(cRec,cYrc))                                         #
      if (length(ind) > 0L) Icount[cRec[ind],j,1] <- Icount[cRec[ind],j,1] + 1L   #
    }                                                            # End month loop #
  }                                                                # End stn loop #
                                                                                  #
###################################################################################
#    Write record counts                                                          #
# Use a format similar to the Regional Average files                              #
# This requires permuting the internal counter array                              #
#                                                                                 #
  cat("\t Writing results",fill=TRUE)                  # Write update in terminal #
  Icount1 <- matrix(aperm(Icount,c(2,1,3)),nyrs*13L,2L)                           #
  X <- data.frame(Year=rep(Yrc,each=13),Month=rep(1:13,times=nyrs),Icount1)       #
  names(X)[3:4] <- vnames                                                         #
  write.csv(X,file=filec[ne],row.names=FALSE)                                     #
                                                                                  #
#    Finished counting station records. Done!                                     #
###################################################################################

} # End index loop 

dy <- date()
mess <- paste("These Record Counts are calculated using the period",nyb,"to",nye,"on date",dy)
writeLines(mess,file.path(folder,"Count_Rec_period.txt"))
cat("Finished counting record stations!",fill=TRUE)
