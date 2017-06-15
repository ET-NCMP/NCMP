###################################################################################
#                                                                                 #
#    The R-NCMPs package has been developed by the ET-NCMP.                       #
#    P7_Summary_Dec2016.R                                                         #
#                                                                                 #
#    This program assembles the NCMPs to be sent to WMO.                          #
#                                                                                 #
#    Programmer:                                                                  #
#    Megan Hartwell, McMaster University, Canada                                  #
#    Lucie Vincent, Environment and Climate Change Canada                         #
#    December 2016                                                                #
#                                                                                 #
###################################################################################

###################################################################################
#    Gathers input info from the user                                             #
                                                                                  #
inquiry <- function() {                                                           #
                                                                                  #
  y1=0                                                                            #
  while (is.na(y1) | y1<1950 | y1>2010) {                                         #
    message("\n","Enter beginning year of the NCMPs ")                            #
    message("(between 1950 and 2010, ex 1950): ")                                 #
    y1 <- readline("")                                                            #
    y1 <- ifelse(grepl("\\D",y1),0,as.integer(y1)) }                              #
                                                                                  #
  y2=0                                                                            #
  while (is.na(y2) | y2<2000 | y2>2020) {                                         #
    message("\n","Enter ending year tof the NCMPs ")                              #
    message("(between 2000 and 2020, ex 2015): ")                                 #
    y2 <- readline("")                                                            #
    y2 <- ifelse(grepl("\\D",y2),0,as.integer(y2)) }                              #
                                                                                  #
  a=c(y1,y2)                                                                      #
return(a)  }                                                                      #
                                                                                  #
#    User input collected. Done!                                                  #
###################################################################################

#if (interactive()) a=inquiry()
a=c(1950,2015)
nbeg=as.integer(a[1])
nend=as.integer(a[2])
nyrs=nend-nbeg+1
#print(nyrs)

###################################################################################
#    Read and write the NCMPs time series in the same file                        #
#                                                                                 #
file1=read.table("A4_Region_Average/NCMP_TMA_Region_Avg.csv",header=T,sep=",")
file1=file1[,1:3]    
names(file1)=c("Year","Month","TMA")                                   
file2=read.table("A4_Region_Average/NCMP_PrAn_Region_Avg.csv",header=T,sep=",")   
file2=file2[,1:3]
names(file2)=c("Year","Month","PrAn")                                      
file3=read.table("A4_Region_Average/NCMP_SPI_Region_Avg.csv",header=T,sep=",")
file3=file3[,1:3]    
names(file3)=c("Year","Month","SPI")                                   
file4=read.table("A4_Region_Average/NCMP_TX90p_Region_Avg.csv",header=T,sep=",")   
file4=file4[,1:3]
names(file4)=c("Year","Month","TX90p")                                      
file5=read.table("A4_Region_Average/NCMP_TX10p_Region_Avg.csv",header=T,sep=",")
file5=file5[,1:3]    
names(file5)=c("Year","Month","TX10p")                                   
file6=read.table("A4_Region_Average/NCMP_TN90p_Region_Avg.csv",header=T,sep=",")   
file6=file6[,1:3]
names(file6)=c("Year","Month","TN90p")                                      
file7=read.table("A4_Region_Average/NCMP_TN10p_Region_Avg.csv",header=T,sep=",")
file7=file7[,1:3]    
names(file7)=c("Year","Month","TN10p")                                   

data1=merge(file1,file2,by=c(1,2))
data1=data1[order(data1$Year,data1$Month),] 
data2=merge(data1,file3,by=c(1,2))
data2=data2[order(data2$Year,data2$Month),] 
data3=merge(data2,file4,by=c(1,2))
data3=data3[order(data3$Year,data3$Month),] 
data4=merge(data3,file5,by=c(1,2))
data4=data4[order(data4$Year,data4$Month),] 
data5=merge(data4,file6,by=c(1,2))
data5=data5[order(data5$Year,data5$Month),] 
data6=merge(data5,file7,by=c(1,2))
data6=data6[order(data6$Year,data6$Month),] 
data6=round(data6,2)

dir.create("A7_Summary",showWarnings=FALSE)              
names(data6)=c("Year","Month","TMA","PrAn","SPI","TX90p","TX10p","TN90p","TN10p")                            
write.table(data6,file="A7_Summary/A1_Summary1.csv",sep=",",row.names=FALSE)
#                                                                                 #
#    Read and write the NCMPs time series: done!                                  #
###################################################################################

###################################################################################
#    Read and write the resquested year and rank                                  #
#                                                                                 #
#data1=read.table("A5_Trends_Graphs/Rank_by_reg_1950_2015/NCMP_TMA.csv",header=T,sep=",")
#odata1=subset(data1,data1[,1]==2015)
#if (nrow(odata1)==1) write.table(odata1,file="A7_Summary/A1_Summary2.csv",sep=",",row.names=FALSE)
#                                                                                 #
#    Read and write the resquested year and rank:done!                            #
###################################################################################

message("Done! \n")
