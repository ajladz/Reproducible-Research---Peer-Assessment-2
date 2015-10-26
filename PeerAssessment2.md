# Reproducible Research: Peer Assesment 2
 
## Impact of Weather Events on Public Health and the Economy of USA
 
 
### Synopsis
 
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

The basic goal of this assignment is to explore the NOAA Storm Database and answer some basic questions about severe weather events. 
The questions that will be adressed in this data analysis are:

1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful      with respect to population health?

2. Across the United States, which types of events have the greatest economic consequences?



### Data Processing

Downloading the Storm Data data set and loading it into R:



```r
if(!file.exists('data')){
        dir.create('data')
        fileUrl <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
        download.file(url = fileUrl, destfile ='./data/StormData.csv.bz2' )
}
StormData <- read.csv('./data/StormData.csv.bz2')
dim(StormData)
```

```
## [1] 902297     37
```

After looking through the data set and finiding the relevant information for this assignment I'll subset my data to make the analysis easier.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data <- select(StormData,BGN_DATE, EVTYPE, FATALITIES:CROPDMGEXP)
data <- tbl_df(data)

data$BGN_DATE <- as.Date(as.character(data$BGN_DATE), format = '%m/%d/%Y %H:%M:%S')
data$years <- as.numeric(format(data$BGN_DATE, "%Y"))
hist(data$years, breaks = 61)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

From https://www.ncdc.noaa.gov/stormevents/details.jsp we can see that

1. Tornado: From 1950 through 1954, only tornado events were recorded.

2. Tornado, Thunderstorm Wind and Hail: From 1955 through 1992, only tornado, thunderstorm wind and hail events were keyed from the paper publications into digital data. 
From 1993 to 1995, only tornado, thunderstorm wind and hail events have been extracted from  the Unformatted Text Files.

3. All Event Types (48 from Directive 10-1605): From 1996 to present, 48 event types are recorded as defined in NWS Directive 10-1605.  

From the histogram we can also see that the number of records obviously increases significantly with years.
Since complete records of weather events are available only for years from 1996 onwards and because using earlier data could produce skewed results, I'll use only these records for my analysis.


```r
data <- data[data$years >= 1996,]
nlevels(data$EVTYPE)
```

```
## [1] 985
```

```r
levels(data$PROPDMGEXP)
```

```
##  [1] ""  "-" "?" "+" "0" "1" "2" "3" "4" "5" "6" "7" "8" "B" "h" "H" "K"
## [18] "m" "M"
```

```r
levels(data$CROPDMGEXP)
```

```
## [1] ""  "?" "0" "2" "B" "k" "K" "m" "M"
```

As we can see from the  the output of levels() function some values from EVTYPE, PROPDMGEXP and CROPDMGEXP need to be modified in order to make them useful for our analysis.

PROPDMGEXP and CROPDMGEXP have multipliers as their values, where:

* H = Hundred 
* K = Thousand
* M = Million
* B = Billion

I'll assume that:

- 'b' == 'B'
- 'm' == 'M'
- 'k' == 'K'
- 'h' == 'H'

Signs like '-', '?', '+' are hard to translate into numbers and I'll assume that these are  typos and 
change their value to 0,  so that they wouldn't affect values in PROPDMG and CROPDMG.
As for numbers '0', 1', '2'etc. I'll leave their values unchanged. 


```r
data$PROPDMGEXP <- gsub('[[:punct:]]', '0', as.character(data$PROPDMGEXP))
data$PROPDMGEXP <- gsub('h', '2', as.character(data$PROPDMGEXP), ignore.case = TRUE)
data$PROPDMGEXP <- gsub('k', '3', as.character(data$PROPDMGEXP), ignore.case = TRUE)
data$PROPDMGEXP <- gsub('m', '6', as.character(data$PROPDMGEXP), ignore.case = TRUE)
data$PROPDMGEXP <- gsub('b', '9', as.character(data$PROPDMGEXP), ignore.case = TRUE)
data$PROPDMGEXP <- as.numeric(data$PROPDMGEXP)
```

We can see from levels(data$CROPDMGEXP) that there are no 'h' factor levels in CROPDMGEXP, so we'll leave that out.


```r
data$CROPDMGEXP <- gsub('[[:punct:]]', '0', as.character(data$CROPDMGEXP))
data$CROPDMGEXP <- gsub('k', '3', as.character(data$CROPDMGEXP), ignore.case = TRUE)
data$CROPDMGEXP <- gsub('m', '6', as.character(data$CROPDMGEXP), ignore.case = TRUE)
data$CROPDMGEXP <- gsub('b', '9', as.character(data$CROPDMGEXP), ignore.case = TRUE)
data$CROPDMGEXP <- as.numeric(data$CROPDMGEXP)
```

I'll turn off scientific notation:

```r
options(scipen = 1)
```

Calculating property and crop damage:


```r
data$PROPDMGEXP <- 10^data$PROPDMGEXP
data$CROPDMGEXP <- 10^data$CROPDMGEXP
data <- mutate(data, propdmg = PROPDMG * PROPDMGEXP, cropdmg = CROPDMG * CROPDMGEXP)
```

**Question 1**

Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health? 


```r
harmful <- 
        data %>%
        group_by(EVTYPE) %>%
        summarize(
                fatalities = sum(FATALITIES),
                injuries = sum(INJURIES) 
                ) %>%
        arrange(desc(fatalities), desc(injuries))

harmful
```

```
## Source: local data frame [516 x 3]
## 
##            EVTYPE fatalities injuries
##            (fctr)      (dbl)    (dbl)
## 1  EXCESSIVE HEAT       1797     6391
## 2         TORNADO       1511    20667
## 3     FLASH FLOOD        887     1674
## 4       LIGHTNING        651     4141
## 5           FLOOD        414     6758
## 6     RIP CURRENT        340      209
## 7       TSTM WIND        241     3629
## 8            HEAT        237     1222
## 9       HIGH WIND        235     1083
## 10      AVALANCHE        223      156
## ..            ...        ...      ...
```

Since there are 516 unique events in my subsets and Storm Data Event Table contains only 48 permitted Storm Data events I'll do some 'cleaning' to see if that changes anything in EVTYPE column. 


```r
library(stringr)
harmful$EVTYPE <- str_trim(harmful$EVTYPE, side = 'both')
harmful <- harmful[!grepl('summary', harmful$EVTYPE, ignore.case = TRUE),]
```

Since summary events don't provide any meaningful information about events or damage made by those events I removed it from EVTYPE vector.


```r
harmful$EVTYPE[grepl('tstm|thunderstorm', harmful$EVTYPE, ignore.case = TRUE)] <- 'THUNDERSTORM WIND'
harmful$EVTYPE[grepl('typhoon|hurricane', harmful$EVTYPE, ignore.case = TRUE)] <- 'HURRICANE(TYPHOON)'
harmful$EVTYPE[grepl('tornado', harmful$EVTYPE, ignore.case = TRUE)] <- 'TORNADO'
harmful$EVTYPE[grepl('dry', harmful$EVTYPE, ignore.case = TRUE)] <- 'DROUGHT'
harmful$EVTYPE[grepl('hail', harmful$EVTYPE, ignore.case = TRUE)] <- 'HAIL'
harmful$EVTYPE[grepl('blizzard', harmful$EVTYPE, ignore.case = TRUE)] <- 'BLIZZARD'
harmful$EVTYPE[grepl('urban flood|small stream flood|urban|small|minor flood', harmful$EVTYPE, ignore.case = TRUE)] <- 'FLOOD'
harmful$EVTYPE[grepl('flash|flash flood', harmful$EVTYPE, ignore.case = TRUE)] <- 'FLASH FLOOD'
harmful$EVTYPE[grepl('excessive heat|extreme heat|record heat|heat wave|unseasonably warm', harmful$EVTYPE, ignore.case = TRUE)] <- 'EXCESSIVE HEAT'
harmful$EVTYPE[grepl('lightning', harmful$EVTYPE, ignore.case = TRUE)] <- 'LIGHTNING'
harmful$EVTYPE[grepl('heavy snow', harmful$EVTYPE, ignore.case = TRUE)] <- 'HEAVY SNOW'
harmful$EVTYPE[grepl('winter storm', harmful$EVTYPE, ignore.case = TRUE)] <- 'WINTER STORM'
harmful$EVTYPE[grepl('high wind', harmful$EVTYPE, ignore.case = TRUE)] <- 'HIGH WIND'
harmful$EVTYPE[grepl('frost|freeze', harmful$EVTYPE, ignore.case = TRUE)] <- 'FROST/FREEZE'
harmful$EVTYPE[grepl('wild|wildfire|fire', harmful$EVTYPE, ignore.case = TRUE)] <- 'WILDFIRE'

n_distinct(harmful$EVTYPE)
```

```
## [1] 339
```

After cleaning up 'harmful' data frame, I am going to make two data frames 'fatal' and 'injury'.


```r
fatal <- harmful %>%
        group_by(EVTYPE) %>%
        summarize(fatalities = sum(fatalities, na.rm = TRUE)) %>%
        arrange(desc(fatalities))

injury <- harmful %>%
        group_by(EVTYPE) %>%
        summarize(injuries = sum(injuries, na.rm = TRUE)) %>%
        arrange(desc(injuries))
```

These data frames contain information about deaths and injuries caused by severe weather events in USA from 1995 until 2011.

**Question 2**

Across the United States, which types of events have the greatest economic consequences?


```r
damage <- data %>%
        group_by(EVTYPE) %>%
        summarize(propdmg = sum(propdmg, na.rm = TRUE),
                  cropdmg = sum(cropdmg, na.rm = TRUE)) %>%
        arrange(desc(propdmg), desc(cropdmg))

damage
```

```
## Source: local data frame [516 x 3]
## 
##               EVTYPE      propdmg    cropdmg
##               (fctr)        (dbl)      (dbl)
## 1              FLOOD 143944833550 4974778400
## 2  HURRICANE/TYPHOON  69305840000 2607872800
## 3        STORM SURGE  43193536000       5000
## 4            TORNADO  24616945710  283425010
## 5        FLASH FLOOD  15222203910 1334901700
## 6               HAIL  14595143420 2476029450
## 7          HURRICANE  11812819010 2741410000
## 8     TROPICAL STORM   7642475550  677711000
## 9          HIGH WIND   5247860360  633561300
## 10          WILDFIRE   4758667000  295472800
## ..               ...          ...        ...
```

There are 516 unique events in 'damage' data frame and only 48 permitted Storm Data events. I'l try to clean up EVTYPE column in the next step. 


```r
damage$EVTYPE <- str_trim(damage$EVTYPE, side = 'both')  # removing spaces from strings in EVTYPE vector
damage <- damage[!grepl('summary', damage$EVTYPE, ignore.case = TRUE),]
damage$EVTYPE[grepl('tornado', damage$EVTYPE, ignore.case = TRUE)] <- 'TORNADO'
damage$EVTYPE[grepl('wild|wildfire|fire', damage$EVTYPE, ignore.case = TRUE)] <- 'WILDFIRE'
damage$EVTYPE[grepl('typhoon|hurricane', damage$EVTYPE, ignore.case = TRUE)] <- 'HURRICANE(TYPHOON)'
damage$EVTYPE[grepl('urban flood|small stream flood|urban|small|minor flood|major flood|river flood|floods|stream flood|ice jam flooding|excessive wetness|wet conditions', damage$EVTYPE, ignore.case = TRUE)] <- 'FLOOD'
damage$EVTYPE[grepl('coastal flood|coastal flooding|cstl|coastal  flooding|coastal erosion', damage$EVTYPE, ignore.case = TRUE)] <- 'COASTAL FLOOD'
damage$EVTYPE[grepl('tstm|thunderstorm', damage$EVTYPE, ignore.case = TRUE)] <- 'THUNDERSTORM WIND'
damage$EVTYPE[grepl('hail', damage$EVTYPE, ignore.case = TRUE)] <- 'HAIL'
damage$EVTYPE[grepl('heavy snow|excessive snow', damage$EVTYPE, ignore.case = TRUE)] <- 'HEAVY SNOW'
damage$EVTYPE[grepl('high wind', damage$EVTYPE, ignore.case = TRUE)] <- 'HIGH WIND'
damage$EVTYPE[grepl('frost|freeze', damage$EVTYPE, ignore.case = TRUE)] <- 'FROST/FREEZE'
damage$EVTYPE[grepl('heavy rain|excessive rain|drizzle|heavy precipitation|unseasonable rain', damage$EVTYPE, ignore.case = TRUE)] <- 'HEAVY RAIN'
damage$EVTYPE[grepl('flash|flash flood', damage$EVTYPE, ignore.case = TRUE)] <- 'FLASH FLOOD'
damage$EVTYPE[grepl('excessive cold|wind chill|unseasonably cold|unseasonable cold|extreme windchill|extreme cold', damage$EVTYPE, ignore.case = TRUE)] <- 'EXCESSIVE COLD/WIND CHILL'

n_distinct(damage$EVTYPE)
```

```
## [1] 324
```

Data frames 'property' and ' 'crop' will contain information on property and crop damage.


```r
property <- damage %>%
        group_by(EVTYPE) %>%
        summarize(propdmg = sum(propdmg, na.rm = TRUE)) %>%
        arrange(desc(propdmg))


crop <- damage %>%
        group_by(EVTYPE) %>%
        summarize(cropdmg = sum(cropdmg, na.rm = TRUE)) %>%
        arrange(desc(cropdmg))
```




### Results

To show which severe weather events have the greatest impact on public health I'll extract top 10 events in both categories, that is, top 10 events that cause the greatest number of deaths and top 10 events that cause the greatest number of injuries.


```r
top_fatal <- fatal[1:10,]
top_injuries <- injury[1:10,]

library(grid)
library(gridExtra)
library(ggplot2)
f <- ggplot(data = top_fatal, aes(x = EVTYPE, y = fatalities)) + geom_bar(stat="identity", fill = 'pink') +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, colour ='black')) +
        labs(title = 'Severe Weather Events Causing Most Fatalities in USA (1995 - 2011)', x = 'Severe weather events', y = 'Number of fatalities') 

i <- ggplot(data = top_injuries, aes(x = EVTYPE, y = injuries)) + geom_bar(stat="identity", fill = 'pink') +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, colour ='black')) +
        labs(title = 'Severe Weather Events Causing Most Injuries in USA (1995 -2012)', x = 'Severe Weather events', y = 'Number of injuries') +
        ggtitle('Severe Weather Events Causing Most Injuries in USA (1995 -2012)' )

grid.arrange(f, i, ncol = 2)
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 

**Conclusion**

We can see from the graphs that the 3 most deadly severe weather events are:

1. **Excessive heat**
2. **Tornado**
3. **Flash flood**

Greatest number of injuries are caused by:

1. **Tornado**
2. **Flood**
3. **Excessive heat**




To show which severe weather events have the greatest economic consequences I'll extract top 10 events  that cause the greatest property damage and top 10 events that cause the greatest crop damage.


```r
top_prop <- property[1:10,]
top_crop <- crop[1:10,]

p <- ggplot(top_prop, aes(EVTYPE, propdmg)) + geom_bar(stat = 'identity', fill = 'pink') + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1, colour ='black')) +
        labs(title = 'Severe Weather Events Causing Greatest Property Damage', x = 'Severe weather events', y = 'Damage in Dollars')
        
c <- ggplot(top_crop, aes(EVTYPE, cropdmg)) + geom_bar(stat = 'identity', fill = 'pink') + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1, colour ='black')) +
        labs(title = 'Severe Weather Events Causing Greatest Crop Damage', x = 'Severe weather events', y = 'Damage in Dollars')

grid.arrange(p, c, ncol = 2)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 

**Conclusion**

When we look into economic impact of severe weather events, property damage is mostly caused by:

1. **Flood**
2. **Hurricane/Typhoon**
3. **Storm  surge**

Greatest crop damage is caused by:

1. **Draught**
2. **Hurricane/Typhoon**
3. **Flood**
