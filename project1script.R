# Set up directory
getwd()
setwd("/Users/luisnunez/Desktop/Desktop/SPRING 2022/analytics/projects/project1")
getwd()
# Read in file
stationData <- read.table(file="CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.tsv",sep ="\t",quote="",header=TRUE)
# Double check that the file is loaded
file.info("CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.tsv")
summary(stationData)
dim(stationData)
head(stationData)
# I want to isolate the UIC-Halsted and O'Hare Airport stops as well as Loyola because I, a commuter, am interested in seeing if there is a similar ussage in the Loyola stop
threeStops <- subset(stationData,stationname=="UIC-Halsted" | stationname=="Loyola" | stationname=="O'Hare Airport")
# Double check that the correct number of rows are included.
summary(threeStops)
dim(threeStops)
head(threeStops)
# Write to a file.
write.csv(threeStops,"three-stops.csv",row.names=FALSE)
threeStopTbl <- read.table(file="three-stops.csv",sep =",",header=TRUE)
# Double check
summary(threeStopTbl)
dim(threeStopTbl)
head(threeStopTbl)
# install.packages("lubridate")
# Use lubridate
library(lubridate)
# Change the date format into a yyyy-mm-dd format
threeStopTbl$Date <- mdy(threeStopTbl$date)
threeStopTbl
uicHalsted <- subset(threeStopTbl,stationname=="UIC-Halsted")
ggplot(uicHalsted, aes(x=Date,y=rides)) + geom_col(width=.8) + scale_x_date(date_labels ="%m-%d-%Y", date_breaks="2 years", date_minor_breaks="1 year" ) 
