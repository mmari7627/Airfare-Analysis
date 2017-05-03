#installing xslx package
install.packages("xlsx")
library(xlsx) #loading the xlsx package

#setting working directory
#if the files are in same directory where your Rstudio is then this step is not necessary
setwd("/Users/homanaren/Documents/CS504-DATA_SCIENCE/Assignments/DataCollection_Assignment2/")

#reading the airfare file in xslx format
AirfareReportQ1<-read.xlsx("DomesticAirfare_2015_Q1.xlsx",sheetIndex = 1,header=TRUE)
AirfareReportQ2<-read.xlsx("DomesticAirfare_2015_Q2.xlsx",sheetIndex = 1,header=TRUE)
AirfareReportQ3<-read.xlsx("DomesticAirfare2015_Q3.xlsx",sheetIndex = 1,header=TRUE)
AirfareReportQ4<-read.xlsx("DomesticAirfare_2015_Q4.xlsx",sheetIndex = 1,header=TRUE)

#to check the content of the airfare data for different quarters
AirfareReportQ1
AirfareReportQ2
AirfareReportQ3
AirfareReportQ4

#combining all data into single excel file
write.table(AirfareReportQ1, "AirfareReport.xlsx", col.names=TRUE, row.names = FALSE, sep=",")
write.table(AirfareReportQ2, "AirfareReport.xlsx", col.names=FALSE, row.names = FALSE, sep = ",", append=TRUE)
write.table(AirfareReportQ3, "AirfareReport.xlsx", col.names=FALSE, row.names = FALSE, sep = ",", append=TRUE)
write.table(AirfareReportQ4, "AirfareReport.xlsx", col.names=FALSE, row.names = FALSE, sep = ",", append=TRUE)
write.xlsx(rbind(AirfareReportQ1, AirfareReportQ2, AirfareReportQ3, AirfareReportQ4), "AirfareReport.xlsx", row.names = FALSE)

#reading the combined excel file
AirfareReport<-read.xlsx("AirfareReport.xlsx",sheetIndex = 1,header=TRUE)

save(AirfareReport, file = "Airfare_report.rda")

#summary of the airfare data
summary(AirfareReport)

#filtering data for flights from Albany 
AlbanyData <- AirfareReport[AirfareReport$airport_1=='ALB',]
AlbanyData
save(AlbanyData, file = "AlbanyData.rda")

#loading library lattice which is required for barcharts
library(lattice)
#Attaching AlbanyData dataframe
attach(AlbanyData)
#quarterly plot of fare vs to_airport for albany City
barchart(fare~airport_2,data = AlbanyData,groups=quarter)
#detaching the AlbanyData dataframe
detach(AlbanyData)

#summary of Albany dataset
summary(AlbanyData)

#to get minimum airfare city from Albany
AlbanyData[AlbanyData$fare==min(AlbanyData$fare),]
AlbanyData[AlbanyData$airport_2==AlbanyData$airport_2[AlbanyData$fare==min(AlbanyData$fare)],]

#histogram for airfare from Albany city 
hist(AlbanyData$fare)
hist(AlbanyData$fare_lg)
hist(AlbanyData$fare_low)

#installing psych package to draw histograms for fare variables
install.packages("psych")
library(psych) #loading library
multi.hist(AlbanyData[7:9])

pairs(AlbanyData[7:9])
