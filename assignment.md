Reproducible Research: Peer Assessment 1
Loading and preprocessing the data

Show any code that is needed to

Load the data (i.e. read.csv())

Process/transform the data (if necessary) into a format suitable for your analysis

setwd("/Users/ZB/Documents/Coursera/tareas\ y\ controles/Reproducible\ Research/Course\ Project/")
source("ipak.R")
packages <- c("plyr", "lattice", "data.table", "httr", "ggplot2")
ipak(packages)
## Loading required package: plyr
## Warning: package 'plyr' was built under R version 3.1.3
## Loading required package: lattice
## Warning: package 'lattice' was built under R version 3.1.3
## Loading required package: data.table
## Warning: package 'data.table' was built under R version 3.1.3
## Loading required package: httr
## Loading required package: ggplot2
## Warning: package 'ggplot2' was built under R version 3.1.3
##       plyr    lattice data.table       httr    ggplot2 
##       TRUE       TRUE       TRUE       TRUE       TRUE
#EBImage is not available at CRAN and it is needed for plyr
#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")

direccion <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
data <- "data"
if(!file.exists(data)){
  dir.create(data)
} 
grafic <- "grafic" 
if(!file.exists(grafic)){
  dir.create(grafic)
}
zip <- paste(getwd(), "/activity.zip", sep = "")
if(!file.exists(zip)){
    download.file(direccion, zip, method="curl", mode="wb")
}
archivo <- paste(getwd(), "/data/activity.csv", sep = "")
if(!file.exists(archivo)){
    unzip(zip, list = FALSE, overwrite = FALSE, exdir = data)
}

activity <- read.table(file = archivo, header = TRUE, sep = ",")
What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

Make a histogram of the total number of steps taken each day

Calculate and report the mean and median total number of steps taken per day

activity$fechayhora <- as.POSIXct(with(activity, paste(date, paste(interval %/% 100, interval %% 100, sep=":"))),
    format="%Y-%m-%d %H:%M",tz="")

pasospordia <- setNames(aggregate(steps~as.Date(date), activity, sum, na.rm = TRUE), c("date","steps"))

xaxis <- seq(1, nrow(pasospordia), by = 6)

okscale <- list(x = list(rot = 45, cex = 1.0, labels = format(pasospordia$date, "%d-%b-%Y")[xaxis], at = xaxis))

barchart(date ~ steps, data = pasospordia, main = "steps per day", ylab = "steps", xlab = "date", scales = okscale, horizontal = F)


paste("mean:", mean(pasospordia$steps))
## [1] "mean: 10766.1886792453"
paste("median:", median(pasospordia$steps))
## [1] "median: 10765"
What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

seriedetiempo <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(seriedetiempo, type = "l", axes = F, xlab = "Time of the day", 
    ylab = "Average across all days provided a time", main = "Average number of steps taken", 
    col = "red")
axis(1,at=c(seq(0,2400,100),835), label = paste(c(seq(0,24,1),8),c(rep(":00",25),":40"),sep=""), pos = 0)
axis(2, at=c(seq(0,210,30),206.2), label = c(seq(0,210,30),206.2), pos = 0)
maximo <- which.max(seriedetiempo$steps)
segments(832, 0, 832, 206.2, col = "blue", lty = "dashed")
text(835,200, "max average of steps: (832,206.2)", col = "blue", adj = c(-.1, -.1))
segments(0, 206.2, 832, 206.2, col = "blue", lty = "dashed")


seriedetiempo [maximo, ]
##     interval    steps
## 104      835 206.1698
#which.max(seriedetiempo$steps)/12
paste(835, "is equivalent to 8.667 hours, this gives the maximum is reached at 8:40 am")
## [1] "835 is equivalent to 8.667 hours, this gives the maximum is reached at 8:40 am"
Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

paste("missing observations:", sum(is.na(activity$steps)))
## [1] "missing observations: 2304"
"missing observations can be replaced with the sample mean instead of replacing them by zero (the 1st won't bias the estimates while the 2nd will)"
## [1] "missing observations can be replaced with the sample mean instead of replacing them by zero (the 1st won't bias the estimates while the 2nd will)"
datanuevos <- activity
datanuevos[is.na(activity$steps), ]$steps <- mean(activity$steps)

datanuevos$fechayhora <- as.POSIXct(with(datanuevos, paste(date, paste(interval %/% 100, interval %% 100, sep=":"))),
    format="%Y-%m-%d %H:%M",tz="")

pasospordia2 <- setNames(aggregate(steps~as.Date(date), datanuevos, sum, na.rm = TRUE), c("date","steps"))

xaxis <- seq(1, nrow(pasospordia2), by = 6)

okscale2 <- list(x = list(rot = 45, cex = 1.0, labels = format(pasospordia2$date, "%d-%b-%Y")[xaxis], at = xaxis))

barchart(date ~ steps, data = pasospordia2, main = "steps per day", ylab = "steps", xlab = "date", scales = okscale2, horizontal = F)


paste("mean:", mean(pasospordia2$steps))
## [1] "mean: 10766.1886792453"
paste("median:", median(pasospordia2$steps))
## [1] "median: 10765"
paste("means difference:", mean(pasospordia2$steps)-mean(pasospordia$steps))
## [1] "means difference: 0"
paste("medians difference:", median(pasospordia2$steps)-median(pasospordia$steps))
## [1] "medians difference: 0"
Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data: Your plot will look different from the one above because you will be using the activity monitor data. Note that the above plot was made using the lattice system but you can make the same version of the plot using any plotting system you choose.

str(datanuevos)
## 'data.frame':    17568 obs. of  4 variables:
##  $ steps     : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date      : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval  : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ fechayhora: POSIXct, format: "2012-10-01 00:00:00" "2012-10-01 00:05:00" ...
datanuevos$date <- as.Date(datanuevos$date, "%Y-%m-%d")
datanuevos$day <- weekdays(datanuevos$date)
datanuevos$tipodia <- c("weekday")
for (i in 1:nrow(datanuevos)){
  if (datanuevos$day[i] == "Saturday" || datanuevos$day[i] == "Sunday"){
    datanuevos$tipodia[i] <- "weekend"
  }
}
datanuevos$tipodia <- as.factor(datanuevos$tipodia)
semana_o_finde <- aggregate(steps ~ interval+tipodia, datanuevos, mean)
qplot(interval, steps, data=semana_o_finde, geom=c("line"), xlab="5-min intervals", 
      ylab="steps mean", main="") + facet_wrap(~ tipodia, ncol=1)
