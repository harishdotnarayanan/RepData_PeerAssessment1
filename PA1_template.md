# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
        # reading from the zip file after unzipping
        activity <- read.csv(unzip("./activity.zip"))

        # re-ordering table to be more intuitive by placing
        # steps count as the last column
        activity <- activity[,c(2,3,1)]
        
        # changing data type of Date column to be... Date
        activity$date <- as.Date(activity$date)
```


## What is mean total number of steps taken per day?


```r
        # using dplyr to group by date and calculating 
        # total steps per date (ignoring NAs)
        daily_total_steps <- activity %>% 
                group_by(date) %>% 
                summarise(total_steps = sum(steps, na.rm = TRUE))

        # plotting histogram
        hist(daily_total_steps$total_steps, 
             main = "Histogram of total number of steps taken each day",
             xlab = "Total number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
        # calculating mean and medians for the total steps per date
        mean_total_steps <- mean(daily_total_steps$total_steps, na.rm = TRUE)
        median_total_steps <- median(daily_total_steps$total_steps, na.rm = TRUE)
```
The mean total number of steps taken per day is **9354.2295082**

The median total number of steps taken per day is **10395**


## What is the average daily activity pattern?


```r
        # using dplyr to group by interval and then calculating the 
        # average for each interval. This will be used later as well.
        mean_steps_per_interval <- activity %>% 
                group_by(interval) %>% 
                summarise(avg_step = mean(steps, na.rm = TRUE))

        # Plotting the average step count per interval
        plot(x = mean_steps_per_interval$interval, 
             y = mean_steps_per_interval$avg_step, 
             type = "l",
             xlab = "Interval",
             ylab = "Average number of steps",
             main = "Average step count per interval across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
        # Determining the maximum number of steps and in which interval this
        # was reached
        max_steps <- max(mean_steps_per_interval$avg_step)
        max_interval <- 
                mean_steps_per_interval[
                        mean_steps_per_interval$avg_step == max_steps,]$interval
```


Interval number **835** contains the highest average step count of **206.1698113**


## Imputing missing values


```r
        # Counting the number of rows of missing steps
        na_count <- sum(is.na(activity))
```

There are **2304** rows containing NA values


```r
        # saving the data to another variable
        act2 <- activity

        # filling up all the NA step counts with the Average Step count for that interval
        act2[is.na(act2$steps),]$steps <-
                mean_steps_per_interval[
                        mean_steps_per_interval$interval == 
                                                act2[is.na(act2$steps),]$interval,]$avg_step
        
        # Using dplyr to group by the date and calculate the 
        # new total steps taken per date
        new_daily_total_steps <- act2 %>% 
                group_by(date) %>% 
                summarise(total_steps = sum(steps, na.rm = TRUE))
        
        # Plotting the new daily step count
        hist(new_daily_total_steps$total_steps,
             main = "Histogram of NEW total number of steps taken each day",
             xlab = "NEW Total number of steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
        # Recalculating the new mean and median of total steps taken per day
        new_mean_total_steps <- mean(new_daily_total_steps$total_steps, na.rm = TRUE)
        new_median_total_steps <- median(new_daily_total_steps$total_steps, na.rm = TRUE)
```

The mean total number of steps taken per day is **9530.7244046**

The median total number of steps taken per day is **1.0439\times 10^{4}**

By imputing the missing (NA) step values, there is an increase in the mean and median of the total steps taken per day.

Increase in mean = **176.4948964**

Increase in median = **44**


## Are there differences in activity patterns between weekdays and weekends?


```r
        # Determining which days are weekends & weekdays and 
        # saving to new variable
        day <- weekdays(act2$date)
        act2 <- cbind(act2, 
                          day = ifelse(weekdays(act2$date) == "Saturday" |
                                               weekdays(act2$date) == "Sunday",
                                       "Weekend", "Weekday"))
        
        # Using dplyr to group based on day and interval
        # Then calculating the average steps per interval
        new_mean_steps_per_interval <- act2 %>% 
                group_by(day, interval) %>% 
                summarise(avg_step = mean(steps, na.rm = TRUE))
        
        # Plotting the Average steps per day over the interval range
        p <- ggplot(new_mean_steps_per_interval, aes(x = interval, y = avg_step))
        p <- p + geom_line()
        p <- p + facet_grid(day ~ .)
        p <- p + labs(title = "Average steps per day",
                      x = "Interval",
                      y = "Average step count")
        p
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
