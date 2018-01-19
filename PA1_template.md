# CourseProject1
Kelly  
January 19, 2018  




## What is the mean total number of steps taken per day?


```r
ActivityData <- read.csv(file="activity.csv",head=TRUE)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.3
```

```r
ActivityByDay <- ActivityData %>% group_by(date) %>% summarize(TotalSteps=sum(steps,na.rm=TRUE))

hist(ActivityByDay$TotalSteps)
```

![](PA1_template_files/figure-html/firstchunk-1.png)<!-- -->

```r
MeanByDay <- mean(ActivityByDay$TotalSteps)
MedianByDay <- median(ActivityByDay$TotalSteps)
```

The mean total steps taken per day is 9354.2295082 and the median total steps taken per day is 10395.

## What is the average daily activity pattern?

```r
ActivityByInterval <- ActivityData %>% group_by(interval) %>% summarize(MeanSteps=mean(steps,na.rm=TRUE))
plot(ActivityByInterval$interval, ActivityByInterval$MeanSteps, type ="l")
```

![](PA1_template_files/figure-html/secondchunk-1.png)<!-- -->

```r
MaxStepsInterval <- ActivityByInterval %>% filter(MeanSteps == max(MeanSteps))
```

The 5-minute interval with the most steps on average is 835.

## Imputing missing values


```r
MissingValues <- is.na(ActivityData)
TotalMissing <- sum(MissingValues)
ActivityDataCopy <- ActivityData

MissingValuesOneCol <- MissingValues[,1]
ActivityDataNAs <- ActivityDataCopy[MissingValuesOneCol,][,1:3]
ActivityDataGood <- ActivityDataCopy[!MissingValuesOneCol,][,1:3]

MergedDataNAs <- merge(ActivityDataNAs, ActivityByInterval, by = c("interval"), all.X = TRUE)
ReplacedData <- MergedDataNAs[,c(4,3,1)]

ColumnNames <- colnames(ActivityDataGood)
colnames(ReplacedData) <- ColumnNames
AllActivityDataReplacements <- rbind(ReplacedData, ActivityDataGood)

ActivityByDayNew <- AllActivityDataReplacements %>% group_by(date) %>% summarize(TotalStepsNew=sum(steps,na.rm=TRUE))

hist(ActivityByDayNew$TotalStepsNew)
```

![](PA1_template_files/figure-html/thirdchunk-1.png)<!-- -->

```r
MeanByDayNew <- mean(ActivityByDayNew$TotalStepsNew)
MedianByDayNew <- median(ActivityByDayNew$TotalStepsNew)
```

The total number of missing values in the dataset is 2304.

After replacing the missing values with the averages for that time interval from the other days, the mean total steps taken per day is 1.0766189\times 10^{4} and the median total steps taken per day is 1.0766189\times 10^{4}. Yes these values differ from the estimates before the missing values were replaced, and the new totals per day are higher. 


## Are there differences in activity patterns between weekdays and weekends?


```r
AllActivityDataReplacements$date <- as.Date(AllActivityDataReplacements$date, format="%Y-%m-%d")
AllActivityDataReplacements$dayofweek <- weekdays(AllActivityDataReplacements$date)

AllActivityDataReplacements$DayType <- ifelse(AllActivityDataReplacements$dayofweek == "Monday" | AllActivityDataReplacements$dayofweek == "Tuesday" | AllActivityDataReplacements$dayofweek == "Wednesday" | AllActivityDataReplacements$dayofweek == "Thursday" | AllActivityDataReplacements$dayofweek == "Friday","WeekDay","WeekEnd")
AllActivityDataReplacements$DayType <- as.factor(AllActivityDataReplacements$DayType)

AllActivityByIntervalDayType <- AllActivityDataReplacements %>% group_by(interval,DayType) %>% summarize(MeanSteps=mean(steps, na.rm=TRUE))

ggplot(AllActivityByIntervalDayType, aes(interval,MeanSteps))+geom_line()+facet_wrap(~DayType,nrow=2)
```

![](PA1_template_files/figure-html/fourth chunk-1.png)<!-- -->
