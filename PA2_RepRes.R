
setwd("D:/0 Online Courses/Coursera/Data Science Specialization - John Hopkins School of Public Health/05 Reproducible Research/WD")

#reading the data
stormdata <- read.csv("NOAA data/repdata-data-StormData (2).csv", na.strings = "NA")

#extracting Variables to be used in the study
stormdata2 <- stormdata[,c("BGN_DATE","EVTYPE","MAG","FATALITIES","INJURIES","PROPDMG", "PROPDMGEXP","CROPDMG","CROPDMGEXP")]

#converting the BGN_DATE variable to date format
stormdata2$BGN_DATE <- as.Date(stormdata2$BGN_DATE, format = "%m/%d/%Y %H:%M:%S")

df1 <- as.data.frame(stormdata2)

summary(df1)
str(df1)

#first time the event was recorded in the database
library(plyr)
startdate <- ddply(df1, EVTYPE, summarise, min = min(BGN_DATE, na.rm = TRUE))

summary(startdate)
df2 <- subset(df1, BGN_DATE >= "2000-01-01")
summary(df2)

#duplicating variables
df2$CROPDMGEXP2 <- df2$CROPDMGEXP
df2$PROPDMGEXP2 <- df2$PROPDMGEXP

#recoding the notations to its numerical value
df2$CROPDMGEXP2 <- as.numeric(recode(df2$CROPDMGEXP2, "'' = 1; 'B' = 1000000000; 'K' = 1000; 'M' = 1000000"))

df2$PROPDMGEXP2 <- as.numeric(recode(df2$PROPDMGEXP2, "'' = 1; 'B' = 1000000000; 'K' = 1000; 'M' = 1000000"))

#multiplying the two variables to calculate for total 
df2$CROPDMG2 <- df2$CROPDMG * df2$CROPDMGEXP2
df2$PROPDMG2 <- df2$PROPDMG * df2$PROPDMGEXP2
totalCropDamage <- ddply(df2, "EVTYPE", summarise, total = sum(CROPDMG2, na.rm = TRUE))
totalPropDamage <- ddply(df2, "EVTYPE", summarise, total = sum(PROPDMG2, na.rm = TRUE))
totalInjuries <- ddply(df2, "EVTYPE", summarise, total = sum(INJURIES, na.rm = TRUE))
totalFatalities <- ddply(df2, "EVTYPE", summarise, total = sum(FATALITIES, na.rm = TRUE))
library(plyr)
#joining
damages <- join(totalCropDamage,totalPropDamage, by = "EVTYPE")

#renaming
names(damages) <- c("EVTYPE","totalC","totalP")

#adding values of Crop and Property Damages and saving it to a new variable
damages$totalDamages <- damages$totalC + damages$totalP

#sorting
totalFatalities <- totalFatalities[order(totalFatalities$total, decreasing = TRUE),]
totalInjuries <- totalFatalities[order(totalInjuries$total, decreasing = TRUE),]
damages_sort <- damages[order(damages$totalDamages, decreasing = TRUE),]

#top 10
fatalities <- totalFatalities[1:10,]
injuries <- totalInjuries[1:10,]
damages1 <- damages_sort[1:10,3]
```

fatalities
injuries
damages1
