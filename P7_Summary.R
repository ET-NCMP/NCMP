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

file5=read.table("A4_Region_Average/NCMP_TN10p_Region_Avg.csv",header=T,sep=",")
file5=file5[,1:3]    
names(file5)=c("Year","Month","TN10p")                                   

data1=merge(file1,file2,by=c(1,2))
data1=data1[order(data1$Year,data1$Month),] 
data2=merge(data1,file3,by=c(1,2))
data2=data2[order(data2$Year,data2$Month),] 
data3=merge(data2,file4,by=c(1,2))
data3=data3[order(data3$Year,data3$Month),] 
data4=merge(data3,file5,by=c(1,2))
data4=data4[order(data4$Year,data4$Month),] 

data4=round(data4,3)

dir.create("A7_Summary",showWarnings=FALSE)              
names(data4)=c("Year","Month","TMA","PrAn","SPI","TX90p","TN10p")                            
write.table(data4,file="A7_Summary/A1_Summary1.csv",sep=",",row.names=FALSE)

#                                                                                 #
#    Read and write the NCMPs time series: done!                                  #
###################################################################################

message("Done! \n")
