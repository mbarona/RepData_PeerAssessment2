# Storm and Weather Events with the Greatest Impact to Population Health and Economy

## Synopsis
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

In this report we aim to explore the National Oceanic and Atmospheric Administration (NOAA) storm database and determine which types of events, across the United States poses the most harmful effects with respect to population health and has the greatest economic consequences.

## Loading and Processing the Raw Data
We used the Storm Data from US NOAA, it contains characteristics of major storms and weather events in the US, as well as estimates of any fatalities, injuries and property damage.

First step, setting the directory and downloading the data.

I presented two options on downloading and reading the data, in my case, having a slow internet connection requires downloading data into working directory than reading it from there but I also presented an alternative code, for purposes of reproducibility

Option 1: Ideal option of downloading the data and unzipping the file using the R.utils package


```r
#if (!file.exists("stormdata.csv.bz2")) { download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",destfile = "stormdata.csv.bz2", method = "curl")}

#require("R.utils")

#bunzip2("stormdata.csv.bz2") #unzipping downloaded data
```

Option 2: Due to limitations of a slow internet connection, I've opted to download the data in the working directory.


```r
#reading the data
stormdata <- read.csv("NOAA data/repdata-data-StormData (2).csv", na.strings = "NA")

#extracting Variables to be used in the study
stormdata2 <- stormdata[,c("BGN_DATE","EVTYPE","MAG","FATALITIES","INJURIES","PROPDMG", "PROPDMGEXP","CROPDMG","CROPDMGEXP")]

#converting the BGN_DATE variable to date format
stormdata2$BGN_DATE <- as.Date(stormdata2$BGN_DATE, format = "%m/%d/%Y %H:%M:%S")
```

Getting summaries and structure of the data

```r
df1 <- as.data.frame(stormdata2)

summary(df1)
```

```
##     BGN_DATE                        EVTYPE            MAG         
##  Min.   :1950-01-03   HAIL             :288661   Min.   :    0.0  
##  1st Qu.:1995-04-20   TSTM WIND        :219940   1st Qu.:    0.0  
##  Median :2002-03-18   THUNDERSTORM WIND: 82563   Median :   50.0  
##  Mean   :1998-12-27   TORNADO          : 60652   Mean   :   46.9  
##  3rd Qu.:2007-07-28   FLASH FLOOD      : 54277   3rd Qu.:   75.0  
##  Max.   :2011-11-30   FLOOD            : 25326   Max.   :22000.0  
##                       (Other)          :170878                    
##    FATALITIES          INJURIES            PROPDMG          PROPDMGEXP    
##  Min.   :  0.0000   Min.   :   0.0000   Min.   :   0.00          :465934  
##  1st Qu.:  0.0000   1st Qu.:   0.0000   1st Qu.:   0.00   K      :424665  
##  Median :  0.0000   Median :   0.0000   Median :   0.00   M      : 11330  
##  Mean   :  0.0168   Mean   :   0.1557   Mean   :  12.06   0      :   216  
##  3rd Qu.:  0.0000   3rd Qu.:   0.0000   3rd Qu.:   0.50   B      :    40  
##  Max.   :583.0000   Max.   :1700.0000   Max.   :5000.00   5      :    28  
##                                                           (Other):    84  
##     CROPDMG          CROPDMGEXP    
##  Min.   :  0.000          :618413  
##  1st Qu.:  0.000   K      :281832  
##  Median :  0.000   M      :  1994  
##  Mean   :  1.527   k      :    21  
##  3rd Qu.:  0.000   0      :    19  
##  Max.   :990.000   B      :     9  
##                    (Other):     9
```

```r
str(df1)
```

```
## 'data.frame':	902297 obs. of  9 variables:
##  $ BGN_DATE  : Date, format: "1950-04-18" "1950-04-18" ...
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
```

```r
#first time the event was recorded in the database
require(plyr)
```

```
## Loading required package: plyr
```

```r
startdate <- ddply(df1, "EVTYPE", summarise, min = min(BGN_DATE, na.rm = TRUE))

summary(startdate)
```

```
##                    EVTYPE         min            
##     HIGH SURF ADVISORY:  1   Min.   :1950-01-03  
##   COASTAL FLOOD       :  1   1st Qu.:1994-07-02  
##   FLASH FLOOD         :  1   Median :1995-07-02  
##   LIGHTNING           :  1   Mean   :1995-11-13  
##   TSTM WIND           :  1   3rd Qu.:1996-09-20  
##   TSTM WIND (G45)     :  1   Max.   :2007-03-08  
##  (Other)              :979
```
Since the results show that most of the events, were only taken in to account or was recorded at the latter part of the 90s, I decided to only take into account the data from year 2000 onwards.

Subsetting the data (this also narrows down the EXP notations to more uniform range which aids to the next step of the study)


```r
df2 <- subset(df1, BGN_DATE >= "2000-01-01")
summary(df2)
```

```
##     BGN_DATE                        EVTYPE            MAG          
##  Min.   :2000-01-01   HAIL             :165719   Min.   :    0.00  
##  1st Qu.:2003-07-09   TSTM WIND        : 85007   1st Qu.:    0.00  
##  Median :2006-09-03   THUNDERSTORM WIND: 81402   Median :   50.00  
##  Mean   :2006-07-10   FLASH FLOOD      : 40585   Mean   :   44.57  
##  3rd Qu.:2009-06-21   FLOOD            : 19961   3rd Qu.:   70.00  
##  Max.   :2011-11-30   TORNADO          : 17687   Max.   :22000.00  
##                       (Other)          :112802                     
##    FATALITIES           INJURIES           PROPDMG          PROPDMGEXP    
##  Min.   :  0.00000   Min.   :0.00e+00   Min.   :   0.00   K      :328461  
##  1st Qu.:  0.00000   1st Qu.:0.00e+00   1st Qu.:   0.00          :189121  
##  Median :  0.00000   Median :0.00e+00   Median :   0.00   M      :  5551  
##  Mean   :  0.01146   Mean   :6.72e-02   Mean   :  11.41   B      :    29  
##  3rd Qu.:  0.00000   3rd Qu.:0.00e+00   3rd Qu.:   1.00   0      :     1  
##  Max.   :158.00000   Max.   :1.15e+03   Max.   :5000.00   -      :     0  
##                                                           (Other):     0  
##     CROPDMG          CROPDMGEXP    
##  Min.   :  0.000   K      :271351  
##  1st Qu.:  0.000          :250613  
##  Median :  0.000   M      :  1195  
##  Mean   :  1.708   B      :     4  
##  3rd Qu.:  0.000   ?      :     0  
##  Max.   :990.000   0      :     0  
##                    (Other):     0
```

Duplicating the CROPDMGEXP and PROPDMGEXP, these duplicates will be recoded to their exponential/numerical equivalent to be used as multiplier to compute for the total amount of damages.


```r
#duplicating variables
df2$CROPDMGEXP2 <- df2$CROPDMGEXP
df2$PROPDMGEXP2 <- df2$PROPDMGEXP

require(car)
```

```
## Loading required package: car
```

```r
#recoding the notations to its numerical value
df2$CROPDMGEXP2 <- as.numeric(recode(df2$CROPDMGEXP2, "'' = 1; 'B' = 1000000000; 'K' = 1000; 'M' = 1000000"))

df2$PROPDMGEXP2 <- as.numeric(recode(df2$PROPDMGEXP2, "'' = 1; 'B' = 1000000000; 'K' = 1000; 'M' = 1000000"))

#multiplying the two variables to calculate for total 
df2$CROPDMG2 <- df2$CROPDMG * df2$CROPDMGEXP2
df2$PROPDMG2 <- df2$PROPDMG * df2$PROPDMGEXP2
```

Computing for the total Fatalities, Injuries and Damages for each event type.

```r
totalCropDamage <- ddply(df2, "EVTYPE", summarise, total = sum(CROPDMG2, na.rm = TRUE))
totalPropDamage <- ddply(df2, "EVTYPE", summarise, total = sum(PROPDMG2, na.rm = TRUE))
totalInjuries <- ddply(df2, "EVTYPE", summarise, total = sum(INJURIES, na.rm = TRUE))
totalFatalities <- ddply(df2, "EVTYPE", summarise, total = sum(FATALITIES, na.rm = TRUE))
```

Combining the total crop and property damages

```r
library(plyr)
#joining
damages <- join(totalCropDamage,totalPropDamage, by = "EVTYPE")

#renaming
names(damages) <- c("EVTYPE","totalC","totalP")

#adding values of Crop and Property Damages and saving it to a new variable
damages$totalDamages <- damages$totalC + damages$totalP
```

Sorting the data and extracting the top 10 results

```r
#sorting
totalFatalities <- totalFatalities[order(totalFatalities$total, decreasing = TRUE),]
totalInjuries <- totalFatalities[order(totalInjuries$total, decreasing = TRUE),]
damages_sort <- damages[order(damages$totalDamages, decreasing = TRUE),]

#top 10
fatalities <- totalFatalities[1:10,]
injuries <- totalInjuries[1:10,]
damages1 <- damages_sort[1:10,]
```

## Results

### Top 10 Storms and Weather Events in the United States with the Most Harmful Effects to Population Health

**No. of Fatalities - Top 10 Event Types with Highest No. of Fatalities**

```r
fatalities
```

```
##                EVTYPE total
## 152           TORNADO  1193
## 36     EXCESSIVE HEAT  1013
## 45        FLASH FLOOD   600
## 93          LIGHTNING   466
## 123       RIP CURRENT   340
## 46              FLOOD   266
## 67               HEAT   231
## 10          AVALANCHE   179
## 78          HIGH WIND   131
## 148 THUNDERSTORM WIND   130
```

**No. of Injuries - Top 10 Event Types with Highest No. of Injuries**

```r
injuries
```

```
##                       EVTYPE total
## 132                    SMOKE     0
## 98  MARINE THUNDERSTORM WIND    10
## 48                    FREEZE     0
## 137             SNOW SHOWERS     0
## 128     SEVERE THUNDERSTORMS     0
## 22     CSTL FLOODING/EROSION     0
## 2                FLASH FLOOD     0
## 178             VOLCANIC ASH     0
## 79                 HURRICANE     4
## 15                BRUSH FIRE     0
```


### Top 10 Storms and Weather Events in the United States with the Greatest Economic Consequences in terms of Crop and Property Damages

**Total Crop and Property Damages - Top 10 Highest Event Types with Highest Damages**

```r
damages1
```

```
##                EVTYPE    totalC    totalP totalDamages
## 45        FLASH FLOOD 265536.39 3007889.6    3273426.0
## 152           TORNADO 147431.22 2734612.5    2882043.8
## 148 THUNDERSTORM WIND 133658.00 2589295.0    2722953.0
## 156         TSTM WIND 107731.75 2435922.6    2543654.4
## 46              FLOOD 245744.46 2030015.7    2275760.1
## 64               HAIL 727981.34 1367349.8    2095331.1
## 93          LIGHTNING   2827.20 1187856.4    1190683.6
## 78          HIGH WIND  24345.86  744730.0     769075.9
## 191      WINTER STORM   1306.00  292575.5     293881.5
## 185          WILDFIRE   9019.80  252663.4     261683.2
```
