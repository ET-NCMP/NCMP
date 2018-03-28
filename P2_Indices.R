###################################################################################
#                                                                                 #
#    The R-NCMPs package has been developed by the ET-NCMP.                       #
#    P2_Indices.R                                                                 #
#                                                                                 #
#    This program calculates 21 indices defined as the 6 NCMPs by ET-NCMP.        #
#    For further details please refer to the User Manual.                         #
#                                                                                 #
#    Programmers:                                                                 #
#    Megan Hartwell, McMaster University, Canada                                  #
#    Lucie Vincent, Environment and Climate Change Canada                         #
#    December 2016                                                                #
#    Modified by Simon Grainger, Bureau of Meteorology, Australia                 #
#    February 2017 - Cleaned up, code is more "R-like"                            #
#    Modified by John Kennedy, Met Office, UK                                     #
#    June 2017 - removed commented out code                                       #
#    Modified by Simon Grainger, Bureau of Meteorology, Australia                 #
#    March 2018 - Improved handling of missing values, zero climatological rain   #
#                                                                                 #
###################################################################################

# Primary check for version of R and key package dependencies
# A re-check of the dependencies indicates that R-NCMP will not work in versions
# of R earlier than 3.0.0. It *should* work for all subsequent versions, but has
# has only been properly tested for R >= 3.2.2
# Also trying to improve diagnostic messages for testing the key dependencies
# 'data.table' and 'climdex.pcic' - this uses find.package() available in >= R 2.13.0
# The existence of package dependencies is implied by these tests
# This should be done as a preliminary script

cat("***** P2_Indices.R *****",fill=TRUE)
xx <- getRversion()
if (xx < "3.0.0") {
  stop("R Version < 3.0.0 cannot execute this code - upgrade is required",call.=FALSE)
} else if (xx >= "3.0.0" && xx < "3.2.2") {
  cat("R Version",xx,"has not been tested with this code - upgrade is suggested",fill=TRUE)
}
xx <- find.package("data.table",quiet=TRUE)
if (length(xx) == 0L) {
  stop("Package 'data.table' is not installed - require Version >= 1.9.6",call.=FALSE)
} else if (packageVersion("data.table") < "1.9.6") {
  stop("Package 'data.table' Version >= 1.9.6 is required - upgrade is required",call.=FALSE)
}
xx <- find.package("climdex.pcic",quiet=TRUE)
if (length(xx) == 0L) {
  stop("Package 'climdex.pcic' is not installed - require Version >= 1.0-3",call.=FALSE)
} else if (packageVersion("climdex.pcic") < "1.0.3") {
  stop("Package 'climdex.pcic' Version >= 1.0-3 is required - upgrade is required",call.=FALSE)
}

# Can now safely load the data.table and climdex.pcic packages (and their dependencies)
# Suppress warning messages
# - these should be related to the version of R used to build the package on PC, and can be ignored

op <- options(warn=-1)
suppressPackageStartupMessages(library(data.table))
library(climdex.pcic)
source("Support_Indices.R")
cat("Successfully loaded packages",fill=TRUE)

###################################################################################
# Set variables with key thresholds for climatological reference period           #
# and for the number of missing days within a month and a year                    #
# These are values which have been determined by the ET-NCMP for the purposes of  #
# generating NCMPs and may differ from other standards or guidlines               #
# ***** DO NOT CHANGE THE VALUE OF THESE VARIABLES *****                          #
###################################################################################

stnhi <- 200L  # maximum number of stations
yrlo <- 1900L  # earliest possible year for reference period
yrhi <- as.POSIXlt(Sys.time())$year + 1899L # latest possible year == current - 1
yclo <- 1981L  # recommended start year for climatological period (WMO-1201, 2017)
ychi <- 2010L  # recommended end year for climatological period (WMO-1201, 2017)
cthresh <- 20L # number of years required for calculating a valid climatology
missm <- 10L   # No. of missing days allowable for valid month (WMO-1201, 2017)
missa <- 36L   # No. of missing days allowable for valid year (no standard)

###################################################################################
#    Gathers input info from the user                                             #
# Use this to generate a configuration file for later use, notably P7             #
# For clarity, no longer do this as a separate function                           #
###################################################################################

cat("Generate NCMP monthly and annual station indices from daily data",fill=TRUE)

# Number of stations to process
# Still suppressing warning messages - about converting strings to integer

cat("Processes either all stations in 'P0_Station_List.txt', or the first 'n' stations",
    "\nIn both cases a maximum of",stnhi,"stations will be processed") 
mess <- paste("\nEnter the number of stations (between 1 and ",stnhi,", or 0 for all) : ",sep="")
nstn <- NA_integer_
while (is.na(nstn) || nstn < 0L || nstn > stnhi) {
  nstn <- readline(mess)
  nstn <- as.integer(nstn)
}

# Obtain quality control flags for temperature and rainfall data for Summary
# The single 0/1/2 differs slightly from that defined in the Guidance Annex

cat("To what level have the input data been quality controlled?:","0 = No Quality Control",
    "1 = Quality Control","2 = Quality Control plus homogenisation",sep="\n")
mess <- "\nEnter Quality Control level value for station daily temperature : "
QCT <- NA_integer_
while (is.na(QCT) || QCT < 0L || QCT > 2L) {
  QCT <- readline(mess)
  QCT <- as.integer(QCT)
}

mess <- "\nEnter Quality Control level value for station daily precipitation : "
QCPr <- NA_integer_
while (is.na(QCPr) || QCPr < 0L || QCPr > 2L) {
  QCPr <- readline(mess)
  QCPr <- as.integer(QCPr)
}
		
# Generate limits for climatological period, and enforce the mininmum length
# This checking should not be generating contradictory messages

cat("The WMO and ET-NCMP recommends a climatological period of",yclo,"-",ychi,fill=TRUE)
yr1 <- yrlo
yr2 <- yrhi - cthresh + 1L
nybr <- 0L
mess <- paste("\nbetween",yr1,"and",yr2,"- recommended =",yclo,": ")
while (is.na(nybr) || nybr < yr1 || nybr > yr2) {
  cat("Enter beginning year for climatological period")
  nybr <- readline(mess)
  nybr <- as.integer(nybr)
}

yr1 <- nybr + cthresh - 1L
yr2 <- yrhi
nyer <- 0L
mess <- paste("\nbetween",yr1,"and",yr2,"- recommended =",ychi,": ")
while (is.na(nyer) || nyer < yr1 || nyer > yr2) {
  cat("Enter ending year for climatological period")
  nyer <- readline(mess)
  nyer <- as.integer(nyer)
}
cat("Thank you. User input collected",fill=TRUE)

# Turn warnings back on, but print immediately
options(warn=1)

###################################################################################
#    Creates directories for output files                                         #
# Rather than do this with individual variable names, use vectors to show how     #
# a consistent approach is taken to structure and naming conventions              #
###################################################################################

ncmpn <- c(1L,1L,2L,2L,2L,2L,3L,4L,4L,5L,5L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L)
folder <- "A2_Indices"
folder2 <- paste("NCMP",ncmpn,sep="")
folder3 <- c("Monthly_Mean_Temp","Monthly_Mean_Temp_Anom",
             "Monthly_Total_Prec_Anom","Monthly_Total_Prec_Anom_Norm",
             "Monthly_Total_Prec","Monthly_Total_Prec_Ratio","Standard_Prec_Index",
             "Warm_Days","Warm_Nights","Cold_Days","Cold_Nights",
             "Extreme_Prec_Date","Extreme_Prec",
             "Extreme_Warm_Day_Date","Extreme_Warm_Day",
             "Extreme_Warm_Night_Date","Extreme_Warm_Night",
             "Extreme_Cold_Day_Date","Extreme_Cold_Day",
             "Extreme_Cold_Night_Date","Extreme_Cold_Night")
ele <- c("TM","TMA","PrA","PrAn","Pr","PrR","SPI",
         "TX90p","TN90p","TX10p","TN10p","RXday1_date","RXday1",
	     "TXx_date","TXx","TNx_date","TNx","TXn_date","TXn","TNn_date","TNn")

dirs <- file.path(folder,folder2,folder3)  # adds separator "/"
for (dname in dirs) dir.create(dname,showWarnings=FALSE,recursive=TRUE)
cat("Directories successfully created",fill=TRUE)

###################################################################################
#    Reads the station list                                                       #
# Can set everything up in a data.frame (!= array) in one call to read.table      #
# Rather than insisting that Station names be 23 character with "_" padding,      #
# do this here and use internally for all subsequent files                        #
# Assuming here that the files are in a readable format - it is planned to        #
# formally check this in a pre-processing script before P1_Quality_Control.R      #
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

# For internal use also require longitudes in the range -180 to 180
# This is for generating the land points for regional average, and for map plots

olats <- files[1:nstn,"Lat"]
olons <- files[1:nstn,"Long"]
olons[olons >= 180] <- olons[olons >= 180]-360
X <- data.frame(Station=Sout,Lat=olats,Long=olons,stringsAsFactors=FALSE)
cat("Sucessfully read in station list",fill=TRUE)

###################################################################################
# Set up miscellaneous variables                                                  #
###################################################################################

max.miss <- c(monthly=missm,annual=missa)  # for climdexInput.raw

# Output column names (now standard across CSV files)
cnames <- c("Year",month.name,"Annual")

# Fixed days per month
Days <- c(31L,28L,31L,30L,31L,30L,31L,31L,30L,31L,30L,31L)

###################################################################################
# Begins loop for reading data files and doing calculations                       #
###################################################################################
# For testing purposes, can modify the indices to loop over (normally process all)

for (i in 1:nstn) {
cat(i,"\t",Station[i],fill=TRUE)

# Output file names for all indices for this station
namex <- paste(dirs,"/",Sout[i],"_",ele,".csv",sep="")

# Read station data
# Since have specified the format, apply additional arguments to read.table()
# to significantly speed up the reading and reduce the internal memory used
# Consider allowing CSV formats in future versions of the code
# Not good practice (but works) to use "data" as a variable name

data <- read.table(file1[i],header=FALSE,na.strings="-99.9",comment.char="",
    colClasses=c("integer","integer","integer","numeric","numeric","numeric"),
    col.names=c("Year","Mo","Day","Prec","Tx","Tn"))

###################################################################################
#    Find mean temperature and monthly values                                     #
###################################################################################

Tm <- (data[,"Tx"] + data[,"Tn"])/2.0
data <- data.table(data,Tm)

# Copy for creating monthly values
data1 <- data
data1$Prec[data1$Prec < 1] <- 0  # replace prec less than 1 with 0

# Calculate monthly values where missing days are below the threshold
# Would be desirable to allow for leap years, but this would be rather harder
# as need to account for partial months and years 

Month <- data1[,.(
     Pr=ifelse(Days[Mo]-sum(!is.na(Prec)) > missm,NA_real_,sum(Prec,na.rm=TRUE)),
     Tm=ifelse(Days[Mo]-sum(!is.na(Tm)) > missm,NA_real_,mean(Tm,na.rm=TRUE))),by=.(Year,Mo)]

# Calculate annual value where missing days are below the threshold
Year <- data1[,.(
       Pr.Y=ifelse(365L-sum(!is.na(Prec)) > missa,NA_real_,sum(Prec,na.rm=TRUE)),
       Tm.Y=ifelse(365L-sum(!is.na(Tm)) > missa,NA_real_,mean(Tm,na.rm=TRUE))),by=Year]

# Shape monthly values into table by year
# This now explictly uses data.table::dcast >= 1.9.6

Month.Y <- dcast(Month,Year~Mo,value.var=c("Pr","Tm"),sep=".",fill=NA)
Month.Y <- merge(Month.Y,Year,by="Year")  # No need to sort (in all locales?)
Month.Y <- round(Month.Y,2)  # round table to 2 digits

# Having dcast multiple variables, split them back up for further processing
# This seems to be the only way to evaluate the correct column names

colp <- c("Year",paste("Pr",c(1:12,"Y"),sep="."))
Prec <- Month.Y[,colp,with=FALSE]
colnames(Prec) <- cnames

colt <- c("Year",paste("Tm",c(1:12,"Y"),sep="."))
Temp <- Month.Y[,colt,with=FALSE]
colnames(Temp) <- cnames

# Write output files

write.csv(Prec,file=namex[5],row.names=FALSE,na="-99.9")
write.csv(Temp,file=namex[1],row.names=FALSE,na="-99.9")
cat("Calculated and written monthly and annual totals",fill=TRUE)

###################################################################################
#    Calculate climatology for mean temp and avg prec                             #
# Save swapping by using R internal representation of missing values == NA(_real_)#
# Require 'cthresh' years (recommend == 20) for non-missing value                 #
###################################################################################

ref <- (Prec$Year >= nybr & Prec$Year <= nyer)
Clim.T <- ifelse(colSums(!is.na(Temp[ref,])) >= cthresh,colMeans(Temp[ref,],na.rm=TRUE),NA_real_)
Clim.P <- ifelse(colSums(!is.na(Prec[ref,])) >= cthresh,colMeans(Prec[ref,],na.rm=TRUE),NA_real_)
cat("Calculated monthly and annual climatologies",fill=TRUE)

###################################################################################
#    Fill zeroes in precipitation climatology                                     #
# Use full period to calculate an extended precipitation climatology              #
# and fill zeroes in the shorter standard precipitation climatology if needed.    #
# if it is still zero, set it to 0.1 mm                                           #
###################################################################################
ref_ext <- (Prec$Year >= 0 & Prec$Year <= 9999)
Clim_ext.P <- ifelse(colSums(!is.na(Prec[ref_ext,])) >= cthresh,colMeans(Prec[ref_ext,],na.rm=TRUE),NA_real_)
for (mn in 2:13) {
   if (!is.na(Clim.P[mn])) {
      if (Clim.P[mn] == 0) { 
         cat("Replacing zero in climatology period with average for full series",fill=TRUE)
         Clim.P[mn] <- Clim_ext.P[mn]
         if (Clim.P[mn] == 0) { Clim.P[mn] <- 0.1 }
      }
   }
}


###################################################################################
#    Calculate the temperature anomaly for mean temp                              #
# Subtract Climatology from each row                                              #
# Note that NA, column names etc are taken care of, but leave Year unchanged      #
###################################################################################
# Utilises R vector recycling, but need to transpose Temp to do this

Clim.T[1] <- 0
Temp.Anom <- round(t(t(Temp) - Clim.T),2)

write.csv(Temp.Anom,file=namex[2],row.names=FALSE,na="-99.9")

###################################################################################
#    Calculate the precipitation ratios and anomalies                             #
# This is a bit more difficult to deal with the Year column                       #
###################################################################################

Clim.P[1] <- 100
Prec.Rat <- round(t(t(Prec)/Clim.P*100),1)
Clim.P[1] <- 0
Prec.Anom <- round(t(t(Prec) - Clim.P),1)
Prec.Nor.Anom <- round(t((t(Prec)-Clim.P)/Clim.P*100),1)
Prec.Nor.Anom[,"Year"] <- Prec.Anom[,"Year"]

write.csv(Prec.Rat,file=namex[6],row.names=FALSE,na="-99.9")
write.csv(Prec.Anom,file=namex[3],row.names=FALSE,na="-99.9") 
write.csv(Prec.Nor.Anom,file=namex[4],row.names=FALSE,na="-99.9")
cat("Calculated and written precipitation anomalies and ratios",fill=TRUE)


###################################################################################
#    Calculate the standardized precpitation index                                #
###################################################################################
# Currrently replacing zeroes with NA to get the correct log values
# But this results in zero monthly rainfall with missing SPI - probably not right
# The PZero variable suggests that this can be better handled
# Also can investigate definitions suitable for arid climates

nyrs <- nrow(Prec)            # get dim of Prec; Prec monthly total when daily >=1
Prec2 <- Prec[,-1,with=FALSE] # Exclude Year column
PZero <- colSums(Prec2==0,na.rm=TRUE)/nyrs # fraction of 0 values, by counting
Prec2[Prec2 == 0] <- NA

A <- log(colMeans(Prec2,na.rm=TRUE))-colMeans(log(Prec2),na.rm=TRUE)
Alpha <- ((1+sqrt(1+4*A/3))/(4*A))          # Find alpha parameter for each month
Beta <- colMeans(Prec2,na.rm=TRUE)/Alpha    # Find  beta parameter for each month

Gamma.Prob <- apply(Prec2,1,pgamma,shape=Alpha,scale=Beta)   # Implicit over rows
Prob <- PZero+(1-PZero)*Gamma.Prob         # normalize with prob of zero; bet 0-1
SPI <- round(qnorm(Prob,mean=0,sd=1),1) # input prob into normal quantile func bet -3-3
SPI <- cbind(Prec[,.(Year)],t(SPI))

write.csv(SPI,file=namex[7],row.names=FALSE,na="-99.9")
cat("Calculated and written SPI",fill=TRUE)

###################################################################################
#    Calculate the daily 10th and 90th % of Tx & Tn                               #
# Due to a probable error in climdex.pcic:::percent.days.op.threshold when have   #
# less than the base period, bypass the wrappers and use a modified,              #
# and hopefully correct, calculation of monthly and annual percentages            #
###################################################################################

dd <- data[,paste(Year,Mo,Day,sep="-")]    # YYYY-MM-DD as string from data.table
D1 <- as.PCICt(dd,cal="gregorian")         # String represented as Gregorian date
D <- climdexInput.raw(tmax=data[,Tx],tmin=data[,Tn],prec=data[,Prec],
                      tmax.dates=D1,tmin.dates=D1,prec.dates=D1,
                      base.range=c(nybr,nyer),max.missing.days=max.miss)
dd <- strsplit(levels(D@date.factors$monthly),"-")    # have padded partial years
Year <- as.integer(sapply(dd,"[[",1))
Mo <- as.integer(sapply(dd,"[[",2))

# This setup allows for looping over the percentile variables
# 1 = Warm days, 2 = Warm nights, 3 = Cold days, 4 = Cold nights

for (ix in 8:11) {
  ne <- ele[ix]

# Calculate monthly and annual percentages for this diagnostic - round to 2dp

  NCMP45.M <- round(percent.days.op.threshold.mod(D,"monthly",ne),2)
  NCMP45.Y <- round(percent.days.op.threshold.mod(D,"annual",ne),2)

# Combine monthly and annual values by casting monthly table into years

  NCMP45 <- data.table(Year,Mo,NCMP45.M)
  NCMP45 <- dcast(NCMP45,Year~Mo,value.var="NCMP45.M")
  NCMP45 <- cbind(NCMP45,NCMP45.Y)

# Write to file with standardised column names

  colnames(NCMP45) <- cnames
  write.csv(NCMP45,file=namex[ix],row.names=FALSE,na="-99.9")
}
cat("Calulated and written percentile indices: Warm/Cold Day/Night",fill=TRUE)


###################################################################################
#    Calculate the Highest and Lowest Values Tx, Tn and RX1                       #
# This is known to cause problems with extended periods of missing data,          #
# and currently prevents the use of precipitation-only stations                   #
# This is probably caused by which.max/min() returning integer(0)                 #
# when no data is valid for that month/year                                       #
# This also defines monthly extreme if *any* value is present, as opposed to      #
# applying missing value limits for monthly mean/totals                           #
# One alternative is to use climdex.pcic, although extracting day of month        #
# then becomes more difficult                                                     #
# Another alternative is calculate these alongside the basic monthly indices      #
# in the data.table object - although that might get a bit messy                  #
###################################################################################

# Retain only the Day and value in each row (or add dateMD/DY if used)
# If extreme monthly precipitation is zero, sensible to set day index to missing

RX1X <- data[,.SD[which.max(Prec),.(Day,Prec)],by=.(Year,Mo)]
RX1X[,Day := ifelse(RX1X[,Prec] == 0,NA,RX1X[,Day])]
TxXX <- data[,.SD[which.max(Tx),.(Day,Tx)],by=.(Year,Mo)]
TnXX <- data[,.SD[which.max(Tn),.(Day,Tn)],by=.(Year,Mo)]
TxNN <- data[,.SD[which.min(Tx),.(Day,Tx)],by=.(Year,Mo)]
TnNN <- data[,.SD[which.min(Tn),.(Day,Tn)],by=.(Year,Mo)]

# If no data, set all index values to missing - use a dummy table for all cases
# This does not appear to be working as intended, given the observed issues

yrs <- range(data[,Year])
dummy <- data.table(Year=c(yrs[1]:yrs[2],"xxxx"),t(rep(NA,13)))

# By putting the 5 records into a list, it is possible to loop over them
# However, must distinguish between max and min records, and make sure that the
# order lines up with the indices as defined by "ele"

listVX <- list(RX1X,TxXX,TnXX,TxNN,TnNN)
rn <- c("HiPr","HiTx","HiTn","LoTx","LoTn")       # row names for end of table
colV <- c("Year",paste("Var",1:12,sep="_"),"Var") # Std column names for variable
colD <- c("Year",paste("Day",1:12,sep="_")) # Standard column names for day index

for (ne in 1:5) {
  ix <- ne*2L + 10L          # offset into "namex" for file names
  VarX <- listVX[[ne]]       # copy of records table
  colnames(VarX)[4] <- "Var" # standard data column name - works in data.table
  if (nrow(VarX) == 0L) {
    dummy[nrow(dummy),1] <- rn[ne]
    Var <- dummy
    Var.D <- dummy
  } else {

# Extract the extreme values for each month and year
# This should give the actual years for which extreme indices were calculated
# Also extract month and value for absolute extreme (ie over all days in data)

    if (ne <= 3L) {
      MVarX <- VarX[,.SD[which.max(Var)],by=.(Mo)]
      YVarX <- VarX[,.SD[which.max(Var)],by=.(Year)]
      Var.MY <- as.numeric(VarX[,.SD[which.max(Var),.(Mo,Var)]])
    } else {
      MVarX <- VarX[,.SD[which.min(Var)],by=.(Mo)]
      YVarX <- VarX[,.SD[which.min(Var)],by=.(Year)]
      Var.MY <- as.numeric(VarX[,.SD[which.min(Var),.(Mo,Var)]])
    }

# Combine monthly and annual records - do need to worry about the table order

    MVarX <- MVarX[order(Mo)]
    VarX.D <- dcast(VarX,Year~Mo,value.var=c("Day","Var"),fill=NA)
    VarX.all <- merge(VarX.D,YVarX,by="Year")

# Separate extreme value and day index
# Day index "Annual" value is the month of extreme value

    Var <- VarX.all[,colV,with=FALSE]
    Var.D <- VarX.all[,colD,with=FALSE]
    Var.D$Annual <- month.abb[as.matrix(VarX.all[,.(Mo)])]

# Add monthly row to output: year of extreme to index, value to data
# This is somewhat murky - and putting month/extreme of extreme in "annual"

    xx <- c(rn[ne],as.list(MVarX[,Var]),Var.MY[2])
    Var <- rbind(Var,xx)
    xx <- c(rn[ne],as.list(MVarX[,Year]),month.abb[Var.MY[1]])
    Var.D <- rbind(Var.D,xx)
  }

# Write extreme and day index to file

  colnames(Var) <- cnames
  colnames(Var.D) <- cnames
  write.csv(Var,file=namex[ix+1L],row.names=FALSE,na="-99.9")
  write.csv(Var.D,file=namex[ix],row.names=FALSE,na="-99")    # was missing == 99
}
cat("Calculated and written extremes",fill=TRUE)

}

###################################################################################
# Ends loop for stations                                                          #
###################################################################################
# Write the modified station table and configuration file
# Do this last so that know have generated all station indices

namex <- file.path(folder,c("P2_Station_List.txt","P2_Configuration.txt"))
write.table(X,file=namex[1],row.names=FALSE)

dy <- date()
desc <- c("Date of processing","Number of stations",
  "Start of climatological period","End of climatological period",
  "Daily Temperature Quality Control level","Daily Precipitation Quality Control Level")
mess <- paste(desc,c(dy,nstn,nybr,nyer,QCT,QCPr),sep=" = ")  # Variables are converted to strings
writeLines(mess,con=namex[2])

cat("Indices done!",fill=TRUE)
options(op)
