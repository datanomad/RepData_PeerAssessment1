# Reproducible Research: Peer Assessment 1

This assignment makes use of data from a personal activity monitoring device. 
This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during 
the months of October and November, 2012 and include the number of steps taken 
in 5 minute intervals each day.


## Loading and preprocessing the data

The following code downloads the zip file into a temporary file, then extracts 
the text file and reads the data into a data frame called rawdata.

```r
url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp <- tempfile()
download.file(url, destfile = temp, method = "curl")
rawdata <- read.csv(unz(temp, "activity.csv"))
unlink(temp)
```

The raw data, stored in the data frame `rawdata`, looks like this:

```r
head(rawdata)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


For analysis, it will be more convenient to rearrange the data as a matrix of measurments (i.e. steps) by date (columns) and by interval (rows).
The data, in a format suitable for analysis, will be stored in a data frame called tidydata.

```r
tidysplit <- with(rawdata, split(steps, date))
tidy1 <- sapply(tidysplit, cbind)
tidydata <- as.data.frame(tidy1)
rownames(tidydata) <- levels(as.factor(rawdata$interval))
```

This is a sample of tidy data, stored in the data frame `tidydata`:

```r
head(tidydata[, c(4, 10, 32, 44, 52, 60)])
```

```
##    2012-10-04 2012-10-10 2012-11-01 2012-11-13 2012-11-21 2012-11-29
## 0          47         34         NA          0          0          0
## 5           0         18         NA          0          0          0
## 10          0          7         NA          0          0          0
## 15          0          0         NA          0          0          0
## 20          0          0         NA          0          0          0
## 25          0          0         NA          0          0          0
```


## What is mean total number of steps taken per day?

The following code creates a histogram of the total number of steps taken each
day. In the histogram, there is no bar for days in which there are no 
values recorded.  

**Note:** The raw data is used in this calculation, but with one transformation: the dates have been converted from a factor to a date.  

```r
tidy2 <- transform(rawdata, date = as.Date(rawdata$date, format = "%Y-%m-%d"))
library(ggplot2)
hist_1 <- ggplot(tidy2, aes(x = date)) + geom_histogram(binwidth = 1, aes(weight = steps)) + scale_x_date(breaks = "1 day", limits = c(as.Date(tidy2[which.min(tidy2[, 2]), 2]), as.Date(tidy2[which.max(tidy2[, 2]), 2]) + 1), expand = c(0 ,0)) + ylab("total steps") + xlab("date") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
hist_1
```

![plot of chunk hist_1](./PA1_template_files/figure-html/hist_1.png) 

The following code adds two horizontal lines intersecting the y axis at the **mean** (red solid line) and **median** (blue dotted line) total number of steps taken each day (i.e. the mean and median total number of steps taken each day, calculated across all days). **Note:** Days in which there were no valid measurements (i.e. only NA values) were excluded from these calculations.  

```r
mean1 <- mean(colSums(tidydata, na.rm = TRUE))
mean1
```

```
## [1] 9354
```

```r
median1 <- median(colSums(tidydata, na.rm = TRUE))
median1
```

```
## [1] 10395
```

```r
hist_1 + geom_hline(aes(yintercept = mean1), color = "red") + geom_hline(aes(yintercept = median1), color = "#0066ff", linetype="dashed") + annotate("text", x = as.Date(names(tidydata[2]), format = "%Y-%m-%d"), y = mean1 * 0.9, label = paste("mean: ", round(mean1, 0)), color = "red", hjust=0) + annotate("text", x = as.Date(names(tidydata[2]), format = "%Y-%m-%d"), y = median1 * 1.15, label = paste("median: ", round(median1, 0)), color = "#0066ff", hjust=0)
```

![plot of chunk mean_median_1](./PA1_template_files/figure-html/mean_median_1.png) 


## What is the average daily activity pattern?

The following code generates a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).  

```r
intnames <- rownames(tidydata)
avgsteps <- tapply(rawdata$steps, rawdata$interval, mean, na.rm = TRUE)
plot(intnames, avgsteps, type = "l", xlab = "5-minute interval", ylab = "average number of steps (across all days)")
```

![plot of chunk avg_steps_per_interval](./PA1_template_files/figure-html/avg_steps_per_interval.png) 

The following code identifies the name of the 5-minute interval containing the maximum average number of steps, calculated across all the days in the dataset.  

```r
names(which.max(avgsteps))
```

```
## [1] "835"
```


## Imputing missing values

The following code calculates the total number of missing values in the dataset.  

```r
sum(is.na(tidydata))
```

```
## [1] 2304
```

The following code creates new versions of `tidydata2` and `tidy2` where NA values have been replaced with the corresponding mean number of steps for the 5-minute interval, averaged across all days.  

```r
tidydata2 <- tidydata
for(r in 1:nrow(tidydata2)) {
    for(c in 1:ncol(tidydata2)) {
        if(is.na(tidydata2[r, c])==TRUE) {
            tidydata2[r, c] <- as.vector(avgsteps)[r]
        }
        else { next(c) }
    }
}

for(i in 1:length(unique(tidy2$interval))){
    tidy2[is.na(tidy2$steps) == TRUE & tidy2$interval == levels(as.factor(tidy2$interval))[i], "steps"] <- avgsteps[[i]]
}
```

This is a sample showing imputed values in `tidydata2` and `tidy2`:  

```
##    2012-10-04 2012-10-10 2012-11-01 2012-11-13 2012-11-21 2012-11-29
## 0          47         34    1.71698          0          0          0
## 5           0         18    0.33962          0          0          0
## 10          0          7    0.13208          0          0          0
## 15          0          0    0.15094          0          0          0
## 20          0          0    0.07547          0          0          0
## 25          0          0    2.09434          0          0          0
```

```
##     steps       date interval
## 1 1.71698 2012-10-01        0
## 2 0.33962 2012-10-01        5
## 3 0.13208 2012-10-01       10
## 4 0.15094 2012-10-01       15
## 5 0.07547 2012-10-01       20
## 6 2.09434 2012-10-01       25
```

A new histogram of the total number of steps taken each day (including the mean and median):  

```r
mean2 <- mean(colSums(tidydata2, na.rm = TRUE))
mean2
```

```
## [1] 10766
```

```r
median2 <- median(colSums(tidydata2, na.rm = TRUE))
median2
```

```
## [1] 10766
```

```r
hist_2 <- ggplot(tidy2, aes(x = date)) + geom_histogram(binwidth = 1, aes(weight = steps)) + scale_x_date(breaks = "1 day", limits = c(as.Date(tidy2[which.min(tidy2[, 2]), 2]), as.Date(tidy2[which.max(tidy2[, 2]), 2]) + 1), expand = c(0 ,0)) + ylab("total steps") + xlab("date") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) + geom_hline(aes(yintercept = mean2), color = "red") + geom_hline(aes(yintercept = round(median2, 0)), color = "#0066ff", linetype="dashed") + annotate("text", x = as.Date(names(tidydata2[2]), format = "%Y-%m-%d"), y = mean2 * 0.9, label = paste("mean: ", round(mean2, 0)), color = "red", hjust=0) + annotate("text", x = as.Date(names(tidydata2[2]), format = "%Y-%m-%d"), y = median2 * 1.15, label = paste("median: ", round(median2, 0)), color = "#0066ff", hjust=0)
hist_2
```

![plot of chunk hist_2](./PA1_template_files/figure-html/hist_2.png) 

The resulting histogram and the mean and median values are clearly different after imputing missing values.  

Empty bins in the histogram (i.e. days where all values were NA) had all values for all intervals replaced with mean values, increasing the frequency of "average day" instances, thus causing the mean and median lines to overlap.  


## Are there differences in activity patterns between weekdays and weekends?

The following code creates a new factor variable in the `tidy2` dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.  

```r
tidy2$wd <- weekdays(tidy2$date)
for(r in 1:nrow(tidy2)) {
    if(tidy2[r, 4]=="Saturday" | tidy2[r, 4]=="Sunday") { tidy2[r, 4] <- "weekend" }
    else { tidy2[r, 4] <- "weekday" }
}
tidy2 <- transform(tidy2, wd = factor(wd))
levels(tidy2$wd)
```

```
## [1] "weekday" "weekend"
```


The following code creates a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).  

(**Note:** I ran out of time and was not able to figure out why the first panel does not show a line)  

```r
intervals <- as.factor(tidy2$interval)
avgsteps2 <- tapply(tidy2$steps, tidy2$interval, mean, na.rm = TRUE)
library(lattice)
xyplot(avgsteps2 ~ intervals | wd, data = tidy2, xlab="5-minute intervals", ylab="Average number of steps", layout = c(1, 2), type = "l")
```

![plot of chunk plot_by_weekday](./PA1_template_files/figure-html/plot_by_weekday.png) 
