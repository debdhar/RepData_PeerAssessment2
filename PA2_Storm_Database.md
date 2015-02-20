# Reproducible Research: Peer Assessment 2
ddhar  
Wednesday, February 18, 2015  



--
Health and Economic Impact due to Severe Weather Events

Synopsis

Storm and other severe weather events can cause both public health and economic problems for communities and municipalities. Many such events can results in fatalities, injuries and property damage. Preventing such outcomes to the extent possible is a key concern. In this report, the aim is to analyze the impact of different weather events on public health and economy based on the storm database collected from the U.S. National Oceanic and Atmospheric Administration's (NOAA) from 1950 - 2011. The estimates of fatalities, injuries, property and crop damage will be used to decide which types of event are most harmful to the population health and economy.


Data Processing


Loading the data
--


```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.1.2
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.1.2
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
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(reshape2)
```

```
## Warning: package 'reshape2' was built under R version 3.1.2
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.2
```



```r
storm <- read.csv(bzfile("repdata_data_StormData.csv.bz2"))
```


--
The analysis will show the type of events that cause the most damages to the of population health and economy.For this only the following data sets are used:
 
 EVTYPE
 FATALITIES
 INJURIES
 PROPDMG
 PROPDMGEXP
 CROPDMG
 CROPDMGEXP
--


```r
## Load only the required Event Types

damageData <- storm[,c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

remove(storm)
```



--
Looking for all the unique values in PROPDMGEXP and CROPDMGEXP (K = thousands, M = millions and B = "billions""). 
--


```r
unique(damageData$PROPDMGEXP) 
```

```
##  [1] K M   B m + 0 5 6 ? 4 2 3 h 7 H - 1 8
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```

```r
unique(damageData$CROPDMGEXP)
```

```
## [1]   M K m B ? 0 k 2
## Levels:  ? 0 2 B k K m M
```



--
Retrieved values are replaced by their numerical value (K=1000, M=1000000, B=1000000000) and zeros with unknown values
--


```r
damageData$PROPDMGEXP <- as.character(damageData$PROPDMGEXP)
damageData$PROPDMGEXP = gsub("\\-|\\+|\\?|h|H|0","0",damageData$PROPDMGEXP)
damageData$PROPDMGEXP = gsub("k|K", "1000", damageData$PROPDMGEXP)
damageData$PROPDMGEXP = gsub("m|M", "1000000", damageData$PROPDMGEXP)
damageData$PROPDMGEXP = gsub("b|B", "1000000000", damageData$PROPDMGEXP)
damageData$PROPDMGEXP <- as.numeric(damageData$PROPDMGEXP)
damageData$PROPDMGEXP[is.na(damageData$PROPDMGEXP)] = 0

damageData$CROPDMGEXP <- as.character(damageData$CROPDMGEXP)
damageData$CROPDMGEXP = gsub("\\-|\\+|\\?|h|H|0","0",damageData$CROPDMGEXP)
damageData$CROPDMGEXP = gsub("k|K", "1000", damageData$CROPDMGEXP)
damageData$CROPDMGEXP = gsub("m|M", "1000000", damageData$CROPDMGEXP)
damageData$CROPDMGEXP = gsub("b|B", "1000000000", damageData$CROPDMGEXP)
damageData$CROPDMGEXP <- as.numeric(damageData$CROPDMGEXP)
damageData$CROPDMGEXP[is.na(damageData$CROPDMGEXP)] = 0

damageData <- mutate(damageData, Property = PROPDMG * PROPDMGEXP, Crops = CROPDMG * CROPDMGEXP)
```



--
Results

What events cause the most fatalities and injuries?
--


```r
harm <- summarize(group_by(damageData, EVTYPE), TOTAL.INJURIES = sum(INJURIES), TOTAL.FATALITIES = sum(FATALITIES))
harm <- arrange(harm, desc(TOTAL.FATALITIES, TOTAL.INJURIES))
harm <- head(harm, 10)
harm <- melt(harm, id.vars="EVTYPE")

plot <- ggplot(harm, aes(x=reorder(EVTYPE, -value), y=value, fill=variable))
plot <- plot + geom_bar(stat="identity")
plot <- plot + labs(x="Event Type", y="Fatalities and Injuries", title="Top 10 dangerous weather events in the US")
plot <- plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot
```

![](PA2_Storm_Database_files/figure-html/unnamed-chunk-11-1.png) 



--
What events cause the most economic damage?
--


```r
damage <- summarize(group_by(damageData, EVTYPE), TOTAL.PROPERTY = sum(Property), TOTAL.CROPS = sum(Crops))
damage <- arrange(damage, desc(TOTAL.PROPERTY))
damage <- head(damage, 10)
damage <- melt(damage, id.vars="EVTYPE")

plot <- ggplot(damage, aes(x=reorder(EVTYPE, -value), y=value / 1000000000, fill=variable))
plot <- plot + geom_bar(stat="identity") 
plot <- plot + labs(x="Event Type", y="Damages (in US billions)", title="Top 10 weather events that causing economic damages in the US") 
plot <- plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot
```

![](PA2_Storm_Database_files/figure-html/unnamed-chunk-13-1.png) 



--
Conclusion

As per the first graph, tornadoes are probably the most deadly weather event in the US, followed by excessive heat events.

As per the second graph, floods cause the most economic damages, followed by hurricanes/typhoons and torndaoes.
--
