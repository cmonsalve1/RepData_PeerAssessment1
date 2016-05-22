---
title: 'Reproducibility Assignment: Project 1'
output: html_document
---

##Upload Data

setwd("~/Data Science/Reproducible Research/ProjectWeek2")

```r
dir()
```

```
## [1] "activity.csv"       "activity2.csv"      "figure"            
## [4] "PA1_template.html"  "PA1_template.md"    "PA1_template.Rmd"  
## [7] "PA1_template_files" "Project1.R"         "W2Project1.html"
```

```r
doc<-read.csv("activity.csv")
head(doc)
```

```
##   steps      date interval
## 1    NA 10/1/2012        0
## 2    NA 10/1/2012        5
## 3    NA 10/1/2012       10
## 4    NA 10/1/2012       15
## 5    NA 10/1/2012       20
## 6    NA 10/1/2012       25
```

```r
summary(doc$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```

##Histogram of the total number of steps taken each day


```r
stepDay<-tapply(doc$steps, doc$date, sum, na.rm=T)
hist(stepDay, breaks=10,xlab = "sum of steps per day", main = "histogram of steps per day")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-14-1.png" title="" alt="" width="672" />

## Mean Steps per day


```r
mean(stepDay) 
```

```
## [1] 9354.23
```

## Median Steps per day


```r
median(stepDay)
```

```
## [1] 10395
```

## Time series plot of the average number of steps taken


```r
mn_int <- tapply(doc$steps, doc$interval, mean, na.rm=T)
plot(mn_int ~ unique(doc$interval), type="l", xlab = "5-min interval")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-17-1.png" title="" alt="" width="672" />

## The 5-minute interval that, on average, contains the maximum number of steps


```r
mn_int[which.max(mn_int)]
```

```
##      835 
## 206.1698
```

## Code to describe and show a strategy for imputing missing data
### Percentage that is empty:


```r
mean(is.na(doc$steps))
```

```
## [1] 0.1311475
```

### Replace NA's under column "steps" with "average" steps in a 5-minute interval


```r
doc$date <- as.Date(doc$date,format = "%m/%d/%Y")
for(i in 1:ncol(doc)){
  doc[is.na(doc[,i]), i] <- mean(doc[,i], na.rm = TRUE)
}
head(doc)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

## Histogram of the total number of steps taken each day after missing values are imputed


```r
mn_int2 <- tapply(doc$steps, doc$date, sum, na.rm=T)
hist(mn_int2, breaks=8,xlab = "sum of steps per day", main = "histogram of steps per day")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-21-1.png" title="" alt="" width="672" />

## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
library(dplyr)
doc3 <- mutate(doc, day = ifelse(weekdays(doc$date)=="Sunday" | weekdays(doc$date)=="Saturday", "weekend", "weekday"))
doc3$day<-as.factor(doc3$day)
summary(doc3)
```

```
##      steps             date               interval           day       
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   weekday:12960  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   weekend: 4608  
##  Median :  0.00   Median :2012-10-31   Median :1177.5                  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5                  
##  3rd Qu.: 37.38   3rd Qu.:2012-11-15   3rd Qu.:1766.2                  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

### Group and summarise the data by the weekend and weekday factor


```r
library(lattice)
df_weekday <- NULL
df_weekend <- NULL
df_final <- NULL
activity2_weekend <- subset(doc3, doc3$day == "weekend")
activity2_weekday <- subset(doc3, doc3$day == "weekday")
mean_activity2_weekday <- tapply(activity2_weekday$steps, activity2_weekday$interval, mean)
mean_activity2_weekend <- tapply(activity2_weekend$steps, activity2_weekend$interval, mean)
df_weekday <- data.frame(interval = unique(activity2_weekday$interval), avg = as.numeric(mean_activity2_weekday), day = rep("weekday", length(mean_activity2_weekday)))
df_weekend <- data.frame(interval = unique(activity2_weekend$interval), avg = as.numeric(mean_activity2_weekend), day = rep("weekend", length(mean_activity2_weekend)))
df_final <- rbind(df_weekday, df_weekend)
xyplot(avg ~ interval | day, data = df_final, layout = c(1, 2), 
       type = "l", ylab = "Number of steps")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-23-1.png" title="" alt="" width="672" />

## Create .md and .html files


