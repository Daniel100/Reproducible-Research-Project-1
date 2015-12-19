# TITLE ------------------------------------------------------------------------------------------
# Boesch Daniel
# 2015-12-13
# REQUIRED PACKAGES ------------------------------------------------------------------------------
  library(data.table)
  library(plyr)
  library(ggplot2)
  library(gridExtra)
  library(lubridate)
  library(lattice)

# LOADING THR RAW_DATA ----------------------------------------------------------------------------
  if( !file.exists("RAW_DATA") ){
    cat("< create the folder: RAW_DATA >")
    dir.create("RAW_DATA")
  }
  
  if( !file.exists("RAW_DATA/activity.csv") ){
    cat("< the data are loading >")
    fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    zipDATA <- "RAW_DATA/DATASET"
    download.file(fileURL, destfile = zipDATA, method = "curl")
    unzip(zipDATA, exdir = "RAW_DATA")
    rm(fileURL, zipDATA)
  }
  
  activity = fread("./RAW_DATA/activity.csv")

  
# DATA PREPROCESSING  ------------------------------------------------------------------------------
  # delete all uncomplete rows
  activity <- activity[ complete.cases(activity), ]
  
# MEAN OF TOTAL NUMBER OF STEPS PER DAY  ----------------------------------------------------------
  # What is mean total number of steps taken per day?
  
  # For this part of the assignment, you can ignore the missing values in the dataset.
  
  # Calculate the total number of steps taken per day
  
  # If you do not understand the difference between a histogram and a barplot, 
  # research the difference between them. Make a histogram of the total number of steps taken 
  # each day
  
  # Calculate and report the mean and median of the total number of steps taken per day
  
  activity_summarie = ddply(
                            activity, .(date), 
                            summarize, 
                            SUM_STEPS    = sum(steps)
                            )
  
# HISTORAM: TOTAL NUMBERS OF STEPS TAKEN PER DAY ------------------------------------------------
  p1 <- ggplot( activity_summarie, aes( x = SUM_STEPS ) )
  p1 <- p1 +  geom_histogram() + 
       xlab( "total number of steps per day" ) +
       ylab( "COUNT" ) +
       ggtitle( "total number of steps taken per day (without NA)" )
  
  print(p1) 
  
  png(filename = "./figures/plot1.png", width = 480, height = 480)
  p1
  dev.off()
  
  
  # mean:
  print(mean(activity_summarie$SUM_STEPS))
  
  # median:
  print(median(activity_summarie$SUM_STEPS))
  
  # sum:
  print(sum(activity_summarie$SUM_STEPS))
  
# AVERAGE DAILY ACTIVITY PATTERN ------------------------------------------------------------------
  # What is the average daily activity pattern?
  
  # Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average 
  # number of steps taken, averaged across all days (y-axis)
  
  # Which 5-minute interval, on average across all the days in the dataset, contains the maximum 
  # number of steps?
  
  activity_summarie2 = ddply(
    activity, 
    .(interval), 
    summarize, 
    MEAN_STEPS    = mean(steps)
  )
  
  p2 = ggplot(activity_summarie2, aes(interval, MEAN_STEPS)) + 
      geom_line(colour = "blue", size = 1) +
      xlab( "interval [5-minute]" ) +
      ylab( "AVERAGE STEPS" ) +
      ggtitle( "AVERAGE DAILY ACTIVITY PATTERN" )
  
  png(filename = "./figures/plot2.png", width = 480, height = 480)
  p2
  dev.off()
  
  
  
  activity_summarie2[which.max(activity_summarie2$MEAN_STEPS), ]
  p2
  
# IMPUTING MISSING VALUES -------------------------------------------------------------------------
  # Note that there are a number of days/intervals where there are missing values (coded as NA). 
  # The presence of missing days may introduce bias into some calculations or summaries of the data.
  
  # Calculate and report the total number of missing values in the dataset (i.e. the total number of 
  # rows with NAs)
  
  # Devise a strategy for filling in all of the missing values in the dataset. The strategy does not 
  # need to be sophisticated. For example, you could use the mean/median for that day, or the mean f
  # or that 5-minute interval, etc.
  
  # Create a new dataset that is equal to the original dataset but with the missing data filled in.
  
  # Make a histogram of the total number of steps taken each day and Calculate and report the 
  # mean and median total number of steps taken per day. Do these values differ from the estimates 
  # from the first part of the assignment? What is the impact of imputing missing data on the estimates 
  # of the total daily number of steps?
  
  # NUMBER OF MISSING VALUES
  activity2 = fread("./RAW_DATA/activity.csv")
  
  length(complete.cases(activity) == FALSE) # 2304 NA
  activity2[ !complete.cases(activity2), ]
  

  # DATA IMPUTATION: Replace NA through MEAN in this timeperiod
  imputValue = function(x){
    m = mean(x[ , 1], na.rm = TRUE)
    if( is.na(m) ){ m = 0 }
    x[is.na(x[ , 1]), 1] <- m
    return(x)
  }
  
  # NEW DATA THROUGH DATA IMPUTATION
  new_activity = ddply(activity2, .(interval), imputValue)
  
  activity_summarie3 = ddply(
    new_activity, .(date), 
    summarize, 
    SUM_STEPS    = sum(steps)
  )
  
  p3 <- ggplot( activity_summarie3, aes( x = SUM_STEPS ) )
  p3 <- p3 +  geom_histogram() + 
    xlab( "total number of steps per day" ) +
    ylab( "COUNT" ) +
    ggtitle( "total number of steps taken per day (with data imputation)" )
  
  png(filename = "./figures/plot3.png", width = 480, height = 480)
  p3
  dev.off()
  
  
  
  # mean:
  print(mean(activity_summarie3$SUM_STEPS))
  
  # median:
  print(median(activity_summarie3$SUM_STEPS))
  
  # sum:
  print(sum(activity_summarie3$SUM_STEPS))
  
  
  # DIFFERENCE
  grid.arrange(p1, p3, ncol=2)
  
  print(p31)
  
  png(filename = "./figures/plot31.png", width = 480, height = 480)
  p31
  dev.off()
  

# DIFFERENCES IN ACTIVITY PATTERNS BETWEEN WEEKDAYS AND WEEKENDS  ----
  new_activity$date <- ymd(new_activity$date)
  new_activity$weekday <- wday(new_activity$date, label = TRUE)
  new_activity$week <- ifelse((new_activity$weekday == "Sat" | new_activity$weekday == "Sun"), "weekday", "weekend")
  new_activity$week <- as.factor(new_activity$week)
  
  activity_summarie4 = ddply(
    new_activity, .(interval, week), 
    summarize, 
    MEAN_STEPS    = mean(steps)
  )
  
  p4 <-xyplot(MEAN_STEPS ~ interval | week,
              data = activity_summarie4,
              type = "l",
              col.line = "blue",
              layout = c(1,2)
            )
  
  png(filename = "./figures/plot4.png", width = 480, height = 480)
  p4
  dev.off()
  
  
  
  
  
  