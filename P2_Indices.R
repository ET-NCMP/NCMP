###################################################################################
#                                                                                 #
#    The R-NCMPs package has been developed by the ET-NCMP.                       #
#    P2_Indices_Dec2016.R                                                         #
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
#                                                                                 #
###################################################################################

# The use of the data.table package imposes requirements on the version of
# R and data.table - test for this and advise the user
# If package data.table is missing, packageVersion() will generate an error and stop

if (getRversion() < "3.0.0") {
  warning("R Version < 3.0.0 is not supported by this code - upgrade is recommended",call.=FALSE)
  library(reshape2,warn.conflicts=FALSE)
}
if (packageVersion("data.table") < "1.9.6") {
  stop("Package 'data.table' Version >= 1.9.6 is required - upgrade is necessary",call.=FALSE)
}
suppressPackageStartupMessages(library(data.table))
library(climdex.pcic)

source("P2_Indices_extra.R")

###################################################################################
#    Gathers input info from the user                                             #
# First hard-wire key parameters or limits for user input                         #
# This allows user to easily change parameters for their own purposes             #
# - it is impossible to guarantee that NCMP output will be consistent across NMHS #
# The previous values for missing data are typical for extremes indices, possibly #
# where have long duration and counts                                             #
# However, this seems too strict, and the WMO 2009 recommendations for climate    #
# normals, plus a looser threshold for missing days in annual, seems much more    #
# reasonable for the indices calculated here                                      #
# However, rainfall should notionally have a zero days threshold, or certainty    #
# that the missing data are accumulated - cannot know the latter here             #
# The low climatology percentage is helpful for ACORN-SAT (should be 50%)         #
# and is suggested by WMO 1989 allowing decadal means as provisional              #
# CRUTem also estimates climate normals for << 30 years of data                   #
# The WMO 2009 standard is 80% valid years for a 30 year period                   #
# Apply start/end of decades to the hard-wired limits                             #
                                                                                  #
stnhi <- 200L                                        # maximum number of stations #
yrlo <- 1900L                       # earliest possible year for reference period #
yrhi <- as.POSIXlt(Sys.time())$year + 1899L # latest possible year == current - 1 #
                                                                                  #
cper <- 33L                          # Percentage of years valid for climatology  #
missm <- 10L   # No. of missing days allowable for valid month (WMO 2009) (was 6) #
missa <- 36L   # No. of missing days in year (was 18 == 5%, no apparent standard) #
                                                                                  #
inquiry <- function(stnhi=200L,yrlo=1961L,yrhi=2020L) {                           #
                                                                                  #
  mess <- paste("\nEnter the number of stations (between 1 and ",stnhi,", or 0 for all): ",sep="")
  x <- NA_integer_                                                                #
  while (is.na(x) || x < 0L || x > stnhi) {                                       #
    x <- readline(mess)                                                           #
    x <- suppressWarnings(as.integer(x))                                          #
  }                                                                               #
                                                                                  #
  cat("The WMO and NCMP Expert Team recommends a base period of 1981-2010.",fill=TRUE)
                                                                                  #
  y1l <- yrlo %/% 10L * 10L + 1L                                                  #
  y2h <- yrhi %/% 10L * 10L                                                       #
  mess <- paste("\n(between",y1l,"and",y2h-9L,"ex. 1981): ")                      #
  y1 <- 0L                                                                        #
  while (is.na(y1) || y1 < y1l || y1 > y2h-9L) {                                  #
    cat("Enter beginning year for base period")                                   #
    y1 <- readline(mess)                                                          #
    y1 <- suppressWarnings(as.integer(y1))                                        #
  }                                                                               #
                                                                                  #
  mess <- paste("\n(between",y1l+9L,"and",y2h,"ex. 2010): ")                      #
  y2 <- 0L                                                                        #
  while (is.na(y2) || y2 < y1l+9L || y2 > y2h || y2 < y1) {                       #
    cat("Enter ending year for base period")                                      #
    y2 <- readline(mess)                                                          #
    y2 <- suppressWarnings(as.integer(y2))                                        #
  }                                                                               #
                                                                                  #
  c(x,y1,y2)                                                                      #
}                                                                                 #
                                                                                  #
# Get the user input and set to local variables                                   #
if (interactive()) a <- inquiry(stnhi,yrlo,yrhi)                                  #
#a <- c(0L,1961L,1990L)                                                           #
nstn <- a[1]                                                                      #
nybr <- a[2]                                                                      #
nyer <- a[3]                                                                      #
                                                                                  #
cthresh <- (cper*(nyer-nybr+1L)) %/% 100L      # no. of valid years for climatol. #
max.miss <- c(monthly=missm,annual=missa)                  # for climdexInput.raw #
                                                                                  #
#    User input collected. Done!                                                  #
###################################################################################

###################################################################################
#    Creates directories for output files                                         #
# Rather than do this with individual variable names, use vectors to show how     #
# a consistent approach is taken to structure and naming conventions              #
                                                                                  #
ncmpn <- c(1L,1L,2L,2L,2L,2L,3L,4L,4L,5L,5L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L)        #
folder <- "A2_Indices"                                                            #
folder2 <- paste("NCMP",ncmpn,sep="")                                             #
folder3 <- c("Monthly_Mean_Temp","Monthly_Mean_Temp_Anom",                        #
             "Monthly_Total_Prec_Anom","Monthly_Total_Prec_Anom_Norm",            #
             "Monthly_Total_Prec","Monthly_Total_Prec_Ratio","Standard_Prec_Index",
             "Warm_Days","Warm_Nights","Cold_Days","Cold_Nights",                 #
             "Extreme_Prec_Date","Extreme_Prec",                                  #
             "Extreme_Warm_Day_Date","Extreme_Warm_Day",                          #
             "Extreme_Warm_Night_Date","Extreme_Warm_Night",                      #
             "Extreme_Cold_Day_Date","Extreme_Cold_Day",                          #
             "Extreme_Cold_Night_Date","Extreme_Cold_Night")                      #
ele <- c("TM","TMA","PrA","PrAn","Pr","PrR","SPI",                                #
         "TX90p","TN90p","TX10p","TN10p","RXday1_date","RXday1",                  #
	   "TXx_date","TXx","TNx_date","TNx","TXn_date","TXn","TNn_date","TNn")     #
dirs <- file.path(folder,folder2,folder3)                # Will add separator "/" #
for (dname in dirs) dir.create(dname,showWarnings=FALSE,recursive=TRUE)           #
                                                                                  #
#    Directories created. Done!                                                   #
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
# For internal use also require longitudes in the range -180 to 180               #
# This is for generating the land points for regional average, and for map plots  #
                                                                                  #
olats <- files[1:nstn,"Lat"]                                                      #
olons <- files[1:nstn,"Long"]                                                     #
olons[olons >= 180] <- olons[olons >= 180]-360                                    #
                                                                                  #
X <- data.frame(Station=Sout,Lat=olats,Long=olons,stringsAsFactors=FALSE)         #
write.table(X,file=file.path(folder,"P2_Station_List.txt"),row.names=FALSE)       #
                                                                                  #
#    Read station list. Done!                                                     #
###################################################################################

# Output column names (now standard across CSV files)
cnames <- c("Year",month.name,"Annual")

# Fixed days per month
Days <- c(31L,28L,31L,30L,31L,30L,31L,31L,30L,31L,30L,31L)

# Begins loop for reading data files and doing calculations
cat("\n")
for (i in 1:nstn) {                                           # use i=1 for testing
cat(i,"\t",Station[i],fill=TRUE)

# Output file names for all indices for this station

namex <- paste(dirs,"/",Sout[i],"_",ele,".csv",sep="")

# Read station's data
# Consider allowing CSV files - could then set missing to blank rather than fixed value
# Not good practice (but works) to use "data" as a variable name

data <- read.table(file1[i],header=FALSE,na.strings="-99.9",
    col.names=c("Year","Mo","Day","Prec","Tx","Tn"))

###################################################################################
#    Find mean temperature and monthly values                                     #
                                                                                  #
Tm <- (data[,"Tx"] + data[,"Tn"])/2.0                                 # Mean temp #
data <- data.table(data,Tm)                                                       #
# Copy for creating monthly values                                                #
data1 <- data                                                                     #
data1$Prec[data1$Prec < 1] <- 0                 # replace prec less than 1 with 0 #
                                                                                  #
# Calculate monthly values where missing days are below the threshold             #
# Would be desirable to allow for leap years, but this would be rather harder     #
# as need to account for partial months and years                                 #
Month <- data1[,.(                                                                #
     Pr=ifelse(Days[Mo]-sum(!is.na(Prec)) > missm,NA_real_,sum(Prec,na.rm=TRUE)), #
     Tm=ifelse(Days[Mo]-sum(!is.na(Tm)) > missm,NA_real_,mean(Tm,na.rm=TRUE))),by=.(Year,Mo)]
                                                                                  #
# Calculate yearly value - if months missing result will be NA (as per WMO 2009)  #
#Year <- Month[,.(Pr.Y=sum(Pr),Tm.Y=mean(Tm),by=Year]                             #
                                                                                  #
# Calculate annual value where missing days are below the threshold               #
Year <- data1[,.(                                                                 #
       Pr.Y=ifelse(365L-sum(!is.na(Prec)) > missa,NA_real_,sum(Prec,na.rm=TRUE)), #
       Tm.Y=ifelse(365L-sum(!is.na(Tm)) > missa,NA_real_,mean(Tm,na.rm=TRUE))),by=Year]                              #
                                                                                  #
# Shape monthly values into table by year                                         #
# This now explictly uses data.table::dcast >= 1.9.6                              #
# Has NOT been tested on data.table >= 1.10.0, or R > 3.2.2                       #
                                                                                  #
Month.Y <- dcast(Month,Year~Mo,value.var=c("Pr","Tm"),sep=".",fill=NA)            #
Month.Y <- merge(Month.Y,Year,by="Year")                                          #
Month.Y <- round(Month.Y,2)                             # round table to 2 digits #
                                                                                  #
# Having dcast multiple variables, split them back up for further processing      #
# This seems to be the only way to evaluate the correct column names              #
                                                                                  #
colp <- c("Year",paste("Pr",c(1:12,"Y"),sep="."))                                 #
Prec <- Month.Y[,colp,with=FALSE]                                                 #
colnames(Prec) <- cnames                                                          #
                                                                                  #
colt <- c("Year",paste("Tm",c(1:12,"Y"),sep="."))                                 #
Temp <- Month.Y[,colt,with=FALSE]                                                 #
colnames(Temp) <- cnames                                                          #
                                                                                  #
# Write output files                                                              #
write.csv(Prec,file=namex[5],row.names=FALSE,na="-99.9")                          #
write.csv(Temp,file=namex[1],row.names=FALSE,na="-99.9")                          #
                                                                                  #
#    This writes the monthly and annual prec and temp data. Done!                 #
###################################################################################

###################################################################################
#    Calculate climatology for mean temp and avg prec                             #
# Save swapping by using R internal representation of missing values == NA(_real_)#
# Previously had hard-wired < 7 years missing => 30 years and 80% valid           #
                                                                                  #
ref <- (Prec$Year >= nybr & Prec$Year <= nyer)                                    #
Clim.T <- ifelse(colSums(!is.na(Temp[ref,])) >= cthresh,colMeans(Temp[ref,],na.rm=TRUE),NA_real_)
Clim.P <- ifelse(colSums(!is.na(Prec[ref,])) >= cthresh,colMeans(Prec[ref,],na.rm=TRUE),NA_real_)
                                                                                  #
#    This calculates the climatology for each month. Done!                        #
###################################################################################

###################################################################################
#    Calculate the temperature anomaly for mean temp                              #
                                                                                  #
# Subtract Climatology from each row                                              #
# Note that NA, column names etc are taken care of, but leave Year unchanged      #
# Utilises R vector recycling, but need to transpose Temp to do this              #
                                                                                  #
Clim.T[1] <- 0                                                                    #
Temp.Anom <- round(t(t(Temp) - Clim.T),2)                                         #
                                                                                  #
write.csv(Temp.Anom,file=namex[2],row.names=FALSE,na="-99.9")                     #
                                                                                  #
#    Calculated and wrote the temperature anomaly. Done!                          #
###################################################################################

###################################################################################
#    Calculate the precipitation ratios and anomalies                             #
                                                                                  #
# This is a bit more difficult to deal with the Year column                       #
                                                                                  #
Clim.P[1] <- 100                                                                  #
Prec.Rat <- round(t(t(Prec)/Clim.P*100),1)                                        #
Clim.P[1] <- 0                                                                    #
Prec.Anom <- round(t(t(Prec) - Clim.P),1)                                         #
Prec.Nor.Anom <- round(t((t(Prec)-Clim.P)/Clim.P*100),1)                          #
Prec.Nor.Anom[,"Year"] <- Prec.Anom[,"Year"]                                      #
                                                                                  #
write.csv(Prec.Rat,file=namex[6],row.names=FALSE,na="-99.9")                      #
write.csv(Prec.Anom,file=namex[3],row.names=FALSE,na="-99.9")                     #
write.csv(Prec.Nor.Anom,file=namex[4],row.names=FALSE,na="-99.9")                 #
                                                                                  #
#    Calculated and wrote the precipitation ratios and anomaly. Done!             #
###################################################################################

###################################################################################
#    Calculate the standardized precpitation index                                #
                                                                                  #
nyrs <- nrow(Prec)                # get dim of Prec; Prec mon values when dly >=1 #
Prec2 <- Prec[,-1,with=FALSE]          # Exclude Year column from Prec data.table #
PZero <- colSums(Prec2==0,na.rm=TRUE)/nyrs          # Find percentage of 0 values #
                                             # Prec2==0 : if prec=0, count values #
Prec2[Prec2 == 0] <- NA      # Replace zeroes with NA to exclude from calculation #
                                                                                  #
A <- log(colMeans(Prec2,na.rm=TRUE))-colMeans(log(Prec2),na.rm=TRUE)              #
Alpha <- ((1+sqrt(1+4*A/3))/(4*A))          # Find alpha parameter for each month #
Beta <- colMeans(Prec2,na.rm=TRUE)/Alpha    # Find  beta parameter for each month #
                                                                                  #
Gamma.Prob <- apply(Prec2,1,pgamma,shape=Alpha,scale=Beta)   # Implicit over rows #
Prob <- PZero+(1-PZero)*Gamma.Prob         # normalize with prob of zero; bet 0-1 #
SPI <- round(qnorm(Prob,mean=0,sd=1),1) # input prob into normal quantile func bet -3-3
SPI <- cbind(Prec[,.(Year)],t(SPI))                                               #
                                                                                  #
write.csv(SPI,file=namex[7],row.names=FALSE,na="-99.9")                           #
                                                                                  #
#    Calculated and wrote the SPI. Done!                                          #
###################################################################################

###################################################################################
#    Calculate the daily 10th and 90th % of Tx & Tn                               #
# Due to a probable error in climdex.pcic:::percent.days.op.threshold when have   #
# less than the base period, bypass the wrappers and use a modified,              #
# and hopefully correct, calculation of monthly and annual percentages            #
                                                                                  #
dd <- data[,paste(Year,Mo,Day,sep="-")]    # YYYY-MM-DD as string from data.table #
D1 <- as.PCICt(dd,cal="gregorian")         # String represented as Gregorian date #
D <- climdexInput.raw(tmax=data[,Tx],tmin=data[,Tn],prec=data[,Prec],             #
                      tmax.dates=D1,tmin.dates=D1,prec.dates=D1,                  #
                      base.range=c(nybr,nyer),max.missing.days=max.miss)          #
dd <- strsplit(levels(D@date.factors$monthly),"-")    # have padded partial years #
Year <- as.integer(sapply(dd,"[[",1))                                             #
Mo <- as.integer(sapply(dd,"[[",2))                                               #
                                                                                  #
# This setup allows for looping over the percentile variables                     #
# 1 = Warm days, 2 = Warm nights, 3 = Cold days, 4 = Cold nights                  #
                                                                                  #
for (ix in 8:11) { # loop over elements                                           #
  ne <- ele[ix]                                                                   #
                                                                                  #
# Calculate monthly and annual percentages for this diagnostic - round to 2dp     #
                                                                                  #
  NCMP45.M <- round(percent.days.op.threshold.mod(D,"monthly",ne),2)              #
  NCMP45.Y <- round(percent.days.op.threshold.mod(D,"annual",ne),2)               #
                                                                                  #
# Merge monthly and annual values by casting monthly table into years             #
                                                                                  #
  NCMP45 <- data.table(Year,Mo,NCMP45.M)                                          #
  NCMP45 <- dcast(NCMP45,Year~Mo,value.var="NCMP45.M")                            #
  NCMP45 <- cbind(NCMP45,NCMP45.Y)                                                #
                                                                                  #
# Write to file with standardised column names                                    #
                                                                                  #
  colnames(NCMP45) <- cnames                                                      #
  write.csv(NCMP45,file=namex[ix],row.names=FALSE,na="-99.9")                     #
} # End loop over indices                                                         #
                                                                                  #
#  Calculated and wrote percentage of warm/cold days/nights. Done!                #
###################################################################################

###################################################################################
#    Calculate the Highest and Lowest Values Tx, Tn and RX1                       #
                                                                                  #
# The maximum/mininum values can be extracted from climdex.pcic,                  #
# but the day of the month would be harder                                        #
# Retain only the Day and value in each row (or add dateMD/DY if used)            #
# If extreme monthly precipitation is zero, sensible to set day index to missing  #
                                                                                  #
RX1X <- data[,.SD[which.max(Prec),.(Day,Prec)],by=.(Year,Mo)]                     #
RX1X[,Day := ifelse(RX1X[,Prec] == 0,NA,RX1X[,Day])]                              #
TxXX <- data[,.SD[which.max(Tx),.(Day,Tx)],by=.(Year,Mo)]                         #
TnXX <- data[,.SD[which.max(Tn),.(Day,Tn)],by=.(Year,Mo)]                         #
TxNN <- data[,.SD[which.min(Tx),.(Day,Tx)],by=.(Year,Mo)]                         #
TnNN <- data[,.SD[which.min(Tn),.(Day,Tn)],by=.(Year,Mo)]                         #
                                                                                  #
# Apparently need to allow for the case of no data for each of Prec, Tx and Tn    #
# However, have not previously assumed this - why?                                #
# If no data, set all index values to missing - use a dummy table for all cases   #
                                                                                  #
yrs <- range(data[,Year])                                                         #
dummy <- data.table(Year=c(yrs[1]:yrs[2],"xxxx"),t(rep(NA,13)))                   #
                                                                                  #
# By putting the 5 records into a list, it is possible to loop over thems         #
# However, must distinguish between max and min records, and make sure that the   #
# order lines up with the indices as defined by "ele"                             #
                                                                                  #
listVX <- list(RX1X,TxXX,TnXX,TxNN,TnNN)                                          #
rn <- c("HiPr","HiTx","HiTn","LoTx","LoTn")          # row names for end of table #
colV <- c("Year",paste("Var",1:12,sep="_"),"Var") # Std column names for variable #
colD <- c("Year",paste("Day",1:12,sep="_")) # Standard column names for day index #
                                                                                  #
for (ne in 1:5) { # Loop over records                                             #
  ix <- ne*2L + 10L                          # offset into "namex" for file names #
  VarX <- listVX[[ne]]                                    # copy of records table #
  colnames(VarX)[4] <- "Var"    # standard data column name - works in data.table #
  if (nrow(RX1X) == 0L) {                                                         #
    dummy[nrow(dummy),1] <- rn[ne]                                                #
    Var <- dummy                                                                  #
    Var.D <- dummy                                                                #
  } else {                                                                        #
                                                                                  #
# Extract the extreme values for each month and year                              #
# This should give the actual years for which extreme indices were calculated     # 
# Also extract month and value for absolute extreme (ie over all days in data)    #
                                                                                  #
    if (ne <= 3L) {                                                               #
      MVarX <- VarX[,.SD[which.max(Var)],by=.(Mo)]                                #
      YVarX <- VarX[,.SD[which.max(Var)],by=.(Year)]                              #
      Var.MY <- as.numeric(VarX[,.SD[which.max(Var),.(Mo,Var)]])                  #
    } else {                                                                      #
      MVarX <- VarX[,.SD[which.min(Var)],by=.(Mo)]                                #
      YVarX <- VarX[,.SD[which.min(Var)],by=.(Year)]                              #
      Var.MY <- as.numeric(VarX[,.SD[which.min(Var),.(Mo,Var)]])                  #
    }                                                                             #
                                                                                  #
# Combine monthly and annual records - do need to worry about the table order     #
                                                                                  #
    MVarX <- MVarX[order(Mo)]                                                     #
    VarX.D <- dcast(VarX,Year~Mo,value.var=c("Day","Var"),fill=NA)                #
    VarX.all <- merge(VarX.D,YVarX,by="Year")                                     #
                                                                                  #
# Separate extreme value and day index                                            #
# Day index "Annual" value is the month of extreme value                          #
                                                                                  #
    Var <- VarX.all[,colV,with=FALSE]                                             #
    Var.D <- VarX.all[,colD,with=FALSE]                                           #
    Var.D$Annual <- month.abb[as.matrix(VarX.all[,.(Mo)])]                        #
                                                                                  #
# Add monthly row to output: year of extreme to index, value to data              #
# This is somewhat murky - and putting month/extreme of extreme in "annual"       #
                                                                                  #
    xx <- c(rn[ne],as.list(MVarX[,Var]),Var.MY[2])                                #
    Var <- rbind(Var,xx)                                                          #
    xx <- c(rn[ne],as.list(MVarX[,Year]),month.abb[Var.MY[1]])                    #
    Var.D <- rbind(Var.D,xx)                                                      #
  } # Ends if (no data)                                                           #
                                                                                  #
# Write extreme and day index to file                                             #
                                                                                  #
  colnames(Var) <- cnames                                                         #
  colnames(Var.D) <- cnames                                                       #
  write.csv(Var,file=namex[ix+1L],row.names=FALSE,na="-99.9")                     #
  write.csv(Var.D,file=namex[ix],row.names=FALSE,na="-99")    # was missing == 99 #
} # Ends loop for indices                                                         #
                                                                                  #
#  Calculated and wrote extreme values and dates. Done!                           #
###################################################################################

} #Ends loop for stations

dy <- date()
mess <- c(paste("These indices are calculated using the reference period",nybr,"to",nyer),
          paste("with threshold of at least",cthresh,"valid years"),
          paste("Calculation was completed at",dy))
writeLines(mess,file.path(folder,"Reference_period.txt"))
cat("Indices done!",fill=TRUE)
