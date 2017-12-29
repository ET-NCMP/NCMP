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
#                                                                                 #
###################################################################################

###################################################################################
#    Read and write the NCMPs time series in the same file                        #
#                                                                                 #

#Extract all csv files from the directory
folder <- "A4_Region_Average"
files <- list.files(folder,pattern="*.csv",full.names=TRUE)
ne <- length(files)

#Read all csv files - first three columns only and place into a list
data <- lapply(files, function(file) read.csv(file,header=TRUE,na.strings="-99.9")[,1:3])

#Extract the first three columns from each data file
#for (i in 1:ne){
#  data[[i]] <- data[[i]][,1:3]
#}

#Place all the names in a vector and assign to the data
#nms <- character(ne)
nsp <- strsplit(basename(files),"_")
for (i in 1:ne) {
  names(data[[i]])[3] <- nsp[[i]][2]
}

#Merge all the files - unless base::merge is overwritten this should work
df <- Reduce(merge,data)

#Apply rounding - except to year and month
df[,-(1:2)] <- round(df[,-(1:2)],3)

#Order the data frame
df <- df[order(df$Year,df$Month),]

#Write the output summary file
folder <- "A7_Summary"
dir.create(folder,showWarnings=FALSE)              

files<-paste(folder,"/A1_Summary1_check.csv",sep="")
write.csv(df,file=files,row.names=FALSE,na="-99.900")

##                                                                                 #
##    Read and write the NCMPs time series: done!                                  #
####################################################################################
#
message("Done! \n")


#file1=read.table("A4_Region_Average/NCMP_TMA_Region_Avg.csv",header=T,sep=",")
#file1=file1[,1:3]    
#names(file1)=c("Year","Month","TMA")                                   
#
#file2=read.table("A4_Region_Average/NCMP_PrAn_Region_Avg.csv",header=T,sep=",")   
#file2=file2[,1:3]
#names(file2)=c("Year","Month","PrAn")                                      
#
#file3=read.table("A4_Region_Average/NCMP_PrA_Region_Avg.csv",header=T,sep=",")   
#file3=file3[,1:3]
#names(file3)=c("Year","Month","PrA")                                      
#
#file4=read.table("A4_Region_Average/NCMP_SPI_Region_Avg.csv",header=T,sep=",")
#file4=file4[,1:3]    
#names(file4)=c("Year","Month","SPI")                                   
#
#file5=read.table("A4_Region_Average/NCMP_TX10p_Region_Avg.csv",header=T,sep=",")   
#file5=file5[,1:3]
#names(file5)=c("Year","Month","TX10p")                                      
#
#file6=read.table("A4_Region_Average/NCMP_TX90p_Region_Avg.csv",header=T,sep=",")   
#file6=file6[,1:3]
#names(file6)=c("Year","Month","TX90p")                                      
#
#file7=read.table("A4_Region_Average/NCMP_TN10p_Region_Avg.csv",header=T,sep=",")
#file7=file7[,1:3]    
#names(file7)=c("Year","Month","TN10p")                                   
#
#file8=read.table("A4_Region_Average/NCMP_TN90p_Region_Avg.csv",header=T,sep=",")
#file8=file8[,1:3]    
#names(file8)=c("Year","Month","TN90p")                                   
#
#
#
#data1=merge(file1,file2,by=c(1,2))
#data1=data1[order(data1$Year,data1$Month),] 
#
#data2=merge(data1,file3,by=c(1,2))
#data2=data2[order(data2$Year,data2$Month),] 
#
#data3=merge(data2,file4,by=c(1,2))
#data3=data3[order(data3$Year,data3$Month),] 
#
#data4=merge(data3,file5,by=c(1,2))
#data4=data4[order(data4$Year,data4$Month),] 
#
#data5=merge(data4,file6,by=c(1,2))
#data5=data5[order(data5$Year,data5$Month),] 
#
#data6=merge(data5,file7,by=c(1,2))
#data6=data6[order(data6$Year,data6$Month),] 
#
#data7=merge(data6,file8,by=c(1,2))
#data7=data7[order(data6$Year,data7$Month),] 
#
##data5=round(data5,3)
#data7=round(data7,3)
#
#dir.create("A7_Summary",showWarnings=FALSE)              
##names(data5)=c("Year","Month","TMA","PrAn","PrA","SPI","TX90p","TN10p")                            
#names(data7)=c("Year","Month","TMA","PrAn","PrA","SPI","TX10p","TX90p","TN10p","TN90p")                            
##write.table(data5,file="A7_Summary/A1_Summary1.csv",sep=",",row.names=FALSE)
#write.table(data7,file="A7_Summary/A1_Summary1.csv",sep=",",row.names=FALSE)
#
##                                                                                 #
##    Read and write the NCMPs time series: done!                                  #
####################################################################################
#
#message("Done! \n")
