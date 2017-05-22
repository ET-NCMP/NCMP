###################################################################################
#                                                                                 #
#    The R-NCMPs package has been developed by the ET-NCMP.                       #
#    P6_Count_Records_Dec2016.R                                                   #
#                                                                                 #
#    This program counts the number of records for 5 different indices.           #
#    For further details please refer to the User Manual.                         #
#                                                                                 #
#    Programmers:                                                                 #
#    Megan Hartwell, McMaster University, Canada                                  #
#    Lucie Vincent, Environment and Climate Change Canada                         #
#    December 2016                                                                #
#    Modified by Simon Grainger, Bureau of Meteorology, Australia                 #
#    December 2016 - Cleaned up, code is more "R-like"                            #
#                                                                                 #
###################################################################################

dir.create("A6_Count_Records",showWarnings=FALSE)          # create directories

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
  y2 <- 0L                                                                        #
  while (is.na(y2) || y2 < 2000L || y2 > 2020L) {                                 #
    cat("Enter ending year for counts of records ")                               #
    y2 <- readline("\n(between 2000 and 2020, ex. 2015): ")                       #
    y2 <- suppressWarnings(as.integer(y2))                                        #
  }                                                                               #
                                                                                  #
  c(x,y1,y2) }                                                                    #
                                                                                  #
#    User input collected. Done!                                                  #
###################################################################################

if (interactive()) a <- inquiry()        # ask if interactive call function inquiry
#a <- c(112L,1950L,2015L)
nstn <- a[1]
nyb  <- a[2]
nye <- a[3]
nyrs <- nye-nyb+1L

###################################################################################
#    Reads the station list                                                       #
                                                                                  #
files <- read.table("P0_Station_List.txt",header=FALSE,stringsAsFactors=FALSE,    #
    col.names=c("FileName","Lat","Long"))                                         #
Station <- sub("\\.txt|$","",files[,"FileName"])       # Only strip ".txt" at end #
if (nstn == 0L) nstn <- nrow(files) else nstn <- min(nrow(files),nstn)            #
                                                                                  #
#    Read station list. Done!                                                     #
###################################################################################

# Have re-ordered to do record high extreme warm, and record low extreme cold
# Name of input indices files
namex <- c("A2_Indices/NCMP6/Extreme_Cold_Day/",
           "A2_Indices/NCMP6/Extreme_Cold_Night/",
           "A2_Indices/NCMP6/Extreme_Warm_Day/",
           "A2_Indices/NCMP6/Extreme_Warm_Night/",
           "A2_Indices/NCMP6/Extreme_Prec/")

# Element of indices to process
ele <- c("TXn","TNn","TXx","TNx","RXday1")

# data.frame for output file
# Like the input station files, and unlike the NCMP2 indices, each row is a month
# Output names have been slightly modified (#stns is stations with > 30 years at that time)

X <- data.frame(Year=rep(nyb:nye,each=13),Month=rep(1:13,times=nyrs),t(rep(0L,10)))
names(X) <- c("Year","Month",
              as.vector(t(sapply(c("#rec","#stns"), function(x) paste(ele,x,sep="-"))))

for (ne in 1:5) {  # 1:5                                         # loop for indices

###################################################################################
#    Read index data for all stations:                                            #
# Utilise Year as first column as row names - helps with placing current station  #
# within the full period but note that years are now character not integer        #
# Define empty matrices for record and station counts                             #
                                                                                  #
  Irec <- matrix(0L,nrow=13L,ncol=nyrs,dimnames=list(nyb:nye,NULL))               #
  Icount <- Irec                                          # copy empty data frame #
  name1 <- paste(namex[ne],Station,"_",ele[ne],".csv",sep="")    # all file names #
                                                                                  #
  cat("Reading data for index",ne,fill=TRUE)                                      #
  for (i in 1:nstn) { # 1:nstn                                                    #
    cat(i,"\t",Station[i],fill=TRUE)                                              #
    I1 <- read.csv(name1[i],header=TRUE,stringsAsFactors=FALSE,na.strings="-99.9",#
                   row.names=1)                                    # read in data #
    I1 <- I1[-nrow(I1),]                                      # remove bottom row #
                                                                                  #
# There is no assumption that the station years are contiguous - allow for this   #
# Skip station if not enough valid years, allowing for different cases            #
                                                                                  #
    Yr <- as.integer(rownames(I1))                                # years of data #
    y1 <- Yr[1]                                              # First year of data #
# Option 1: 30 years from beginning of data                                       #
    ic <- (Yr-y1 >= 30L)                                                          #
# Option 2: 30 years from start year                                              #
#    ic <- (Yr-nyb >= 30L)                                                        #
    if (nrow(I1) <= 30L || !any(ic) || !any(is.element(Yr[ic],nyb:nye))) next     #
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
    ilen <- sapply(Irl,length)-1L      # number of times record set in each month #
                                                                                  #
# Convert the list of indices to matrix of when records were set,                 #
# and valid index values to a station count (when > 30 years and not missing)     #
                                                                                  #
    Srec <- array(0L,dim(I1),dimnames(I1))                                        #
    for (j in 1:13) {                                            # for each month #
      indx <- Irl[[j]][1:ilen[j]]                # index of when records were set #
      Srec[indx,j] <- ifelse(ic[indx],1L,0L)    # record if > 30 years from start #
    }                                                                             #
    Scount <- array(0L,dim(I1),dimnames(I1))                                      #
    Scount[ic,] <- sapply(I1[ic,],function(x) ifelse(is.finite(x),1L,0L))         #
                                                                                  #
# Add station counts to total counts for this NCMP index                          #
                                                                                  #
    cyr <- rownames(I1)                                       # year as character #
    indx <- is.element(Yr,nyb:nye)                   # Station years within range #
    Irec[cyr[indx],] <- Irec[cyr[indx],]+Srec[cyr[indx],]     # reference by year #
    Icount[cyr[indx],] <- Icount[cyr[indx],]+Scount[cyr[indx,]]                   #
  }                                                                # End stn loop #
                                                                                  #
# Copy counts for this index to output data.frame                                 #
                                                                                  #
  X[,2*i+1] <- as.vector(t(Irec))                                                 #
  X[,2*i+2] <- as.vector(t(Icount))                                               #
                                                                                  #
#    Finished counting station records. Done!                                     #
###################################################################################

} # End index loop 

write.csv(X,file="A6_Count_Records/Records.csv",row.names=FALSE)
cat("Finished counting record stations!",fill=TRUE)
