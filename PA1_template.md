# PA1_template.Rmd
aimod62  
May 10, 2016  
The present data is extracted from a personal activity monitoring device which collects data at 5 minutes interval, on daily basis, of the activity of an anonymous individual carried through the month of October and November 2012. The allegedly aim is to find patterns of behavior destined to enhance performance in different areas of personal interest. Rcodes are included in the main output as indicated by the assignment prompt

##Libraries Used in the Present Assignment

```r
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(gtools)
library(reshape2)
library(zoo)
```


## Loading the Data
#### Rcode and Output

```r
dat <- read.csv("C:/Users/AstridIleana/Desktop/Reproducible Research/activity.csv", stringsAsFactors = FALSE)
# Getting acquainted with given data.
# head(dat, 5) not displayed
# tail(dat, 5) not displayed
# names(dat)   not displayed
# str(dat)     not displayed
summary(dat)
```

```
     steps            date              interval     
 Min.   :  0.00   Length:17568       Min.   :   0.0  
 1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
 Median :  0.00   Mode  :character   Median :1177.5  
 Mean   : 37.38                      Mean   :1177.5  
 3rd Qu.: 12.00                      3rd Qu.:1766.2  
 Max.   :806.00                      Max.   :2355.0  
 NA's   :2304                                        
```

## 1. What is mean total number of steps taken per day?
####Rcode and Output


```r
# Creating Dataframe
dat1 <- dat %>% mutate(Date = as.Date(date)) %>% select(-interval, -date) %>% 
  group_by(Date) %>% summarize(Total_per_Day = sum(steps))
head(dat1, 5)
```

```
Source: local data frame [5 x 2]

        Date Total_per_Day
      (date)         (int)
1 2012-10-01            NA
2 2012-10-02           126
3 2012-10-03         11352
4 2012-10-04         12116
5 2012-10-05         13294
```
####Rcode and Visualization

```r
## Creating necessary color palette 
colourCount <- length(unique(dat1$Date))
getPalette <- colorRampPalette(brewer.pal(11, "Spectral"))

p_dat1 <- ggplot(dat1, aes(Total_per_Day, fill= as.factor(Date)))+
  geom_histogram(binwidth = 1000)+
  ggtitle("Total Number of Steps Taken by Day") +
  xlab("Steps Taken from Jan 1, 2012 to Nov 11, 2012") +
  ylab("Frequency")+
  scale_fill_manual(values = getPalette(colourCount))+
  theme(legend.position = "bottom", plot.title = element_text(size = 15,
                                                              face = "bold"),
        axis.title = element_text(size = 10, face = "bold"))+
  guides(fill=guide_legend(nrow=3),guide_legend(title=NULL))
  
p_dat1
```

<img src="PA1_template_files/figure-html/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

#### Findings


Total number of steps taken per day seems approximately normally distributed, no evidence of remarkable skewness. 

The Dataset contains an important amount of missing data as evidenced by the plot. Tools to assess, if the subject was simply at rest or data was not properly collected, are not provided.

There is a visible pattern of increasing steps at certain days: however, no information is given to sustain any inference.

### 1.1 Calculate and report the mean and median of the total number of steps taken per day
####Rcode and Visualization


```r
total_mean <- mean(dat1$Total_per_Day, na.rm = TRUE)
total_median <- median(dat1$Total_per_Day, na.rm= TRUE)

mean_plot <- ggplot(dat1, aes(Total_per_Day)) +
  geom_density()+
  geom_vline(aes(xintercept= total_mean)) +
  ggtitle("Total Number of Steps Taken by Day\nMean") +
  theme(plot.title = element_text(size = 15, face = "bold"),
          axis.title = element_text(size = 10, face = "bold"))+
  annotate("text", x = 1750, y = 0.00009, label= "10766.18", color = "red")+
  guides(fill = FALSE)

median_plot <- ggplot(dat1, aes(Total_per_Day)) +
  geom_density()+
  geom_vline(aes(xintercept= total_median), linetype = "dashed", color = "red") +
  ggtitle("Total Number of Steps Taken by Day\nMedian") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"))+
  annotate("text", x=1750, y = 0.00009, label = "10765")+
  guides(fill = FALSE)

grid.arrange(mean_plot, median_plot, ncol = 2)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

Total mean per day: 10766.18

Total median per day: 10765


##2. What is the average daily activity pattern?
####Rcode, Output, and Visualization


```r
# 5 Minutes Interval
# Creating Dataframe
dat2 <- dat %>% mutate(Date = as.Date(date)) %>% 
  select(-date) %>% group_by(interval)%>% summarize(Average_Steps = mean(steps, na.rm = TRUE))
head(dat2, 5)
```

```
Source: local data frame [5 x 2]

  interval Average_Steps
     (int)         (dbl)
1        0     1.7169811
2        5     0.3396226
3       10     0.1320755
4       15     0.1509434
5       20     0.0754717
```

```r
plot_dat2 <- ggplot(dat2, aes(interval, Average_Steps, color = interval))+
  geom_line( size = 3, alpha = 0.7) +
  geom_point(colour = "red")+
  ggtitle("Average Daily Activity Pattern")+
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"))+
  annotate("rect", xmin = 750, xmax = 1000, ymin = 0, ymax = 210, alpha = 0.1, fill ="blue")
  
  plot_dat2
```

<img src="PA1_template_files/figure-html/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

###2.1 Which 5-minute interval, on average, contains the maximum number of steps? 


```r
max <- dat2[which.max(dat2$Average_Steps), ]
max #835      206.1698 
```

```
Source: local data frame [1 x 2]

  interval Average_Steps
     (int)         (dbl)
1      835      206.1698
```

The shaded area in the above plot portrays the average time where the highest amount of steps occurs. The maximum value takes place at the 835 interval at 206.1695 steps each five minutes which corresponds roughly to the last hour of the third day of the total period. No starting time is appointed to make sense of the obtained information.


## 3. Imputing missing values
####Rcode, Output, and Visualization

Before proceeding to fill the missing data in the set, it is recommended to assess if any pattern arises from the distributions of NAs.  Useful information could be hidden behind the phenomenon. Despite of not being required by the assignment, I have found rewarding to produce a Missingness plot. NAs appears at certain hours quite consistently. The subject of study might be at rest due to sleep or work, etc.  The daily mean or median previously obtained could be a good fill in such a case. 



```r
#Missing Data
sum(!complete.cases(dat))
```

```
[1] 2304
```

```r
dat_miss <- dat %>% select(-date) %>% group_by(interval) %>% is.na %>% melt()
head(dat_miss) 
```

```
  Var1  Var2 value
1    1 steps  TRUE
2    2 steps  TRUE
3    3 steps  TRUE
4    4 steps  TRUE
5    5 steps  TRUE
6    6 steps  TRUE
```

```r
p_dat_miss<- ggplot(dat_miss, aes(Var1, Var2)) +
  geom_raster(aes(fill = value)) +
  scale_fill_grey(name = "",
                  labels = c("Present","Missing"))+
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45, vjust=0.5)) + 
  labs(x = "Steps",
       y = "Intervals")+
  ggtitle("Missingness ") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"))
p_dat_miss
```

<img src="PA1_template_files/figure-html/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

### 3.1 Strategy for filling in all of the missing values in the dataset
####Rcode, Output, and Visualization


```r
# Filling NAs
dat_fill <- dat %>% group_by(date) %>% summarize(Total_per_Day = sum(steps)) %>%
  mutate(New_Total_per_Day = zoo::na.fill(Total_per_Day, total_mean), Date = as.Date(date))%>%
  select(-date,-Total_per_Day) 
head(dat_fill,5)
```

```
Source: local data frame [5 x 2]

  New_Total_per_Day       Date
              (dbl)     (date)
1          10766.19 2012-10-01
2            126.00 2012-10-02
3          11352.00 2012-10-03
4          12116.00 2012-10-04
5          13294.00 2012-10-05
```

```r
sum(!complete.cases(dat_fill)) # check if successfully filled
```

```
[1] 0
```

```r
## Plotting
## Creating palette 
colourCount1 <- length(unique(dat1$Date))
getPalette1 <- colorRampPalette(brewer.pal(11, "PuOr"))


p_dat_fill <- ggplot(dat_fill, aes(New_Total_per_Day, fill= as.factor(Date)))+
  geom_histogram(binwidth = 1000)+
  ggtitle("Total Number of Steps Taken by Day\nNAs filled") +
  xlab("Steps Taken from Jan 1, 2012 to Nov 11, 2012") +
  ylab("Frequency")+
  scale_fill_manual(values = getPalette1(colourCount1))+
  theme(legend.position = "bottom", plot.title = element_text(size = 15,
                                                              face = "bold"),
        axis.title = element_text(size = 10, face = "bold"))+
  guides(fill=guide_legend(nrow=3),guide_legend(title=NULL))

p_dat_fill
```

<img src="PA1_template_files/figure-html/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

### 3.2 Histogram of the total number of steps taken each day 
####Rcode, Output, and Visualization


```r
# Aggregate histogram

p_dat1C <- ggplot(dat1, aes(Total_per_Day, fill= as.factor(Date)))+
  geom_histogram(binwidth = 1000)+
  ggtitle("Total Number of Steps Taken by Day") +
  xlab("Steps Taken from Jan 1, 2012 to Nov 11, 2012") +
  ylab("Frequency")+
  scale_fill_manual(values = getPalette(colourCount))+
  theme(legend.position = "bottom", plot.title = element_text(size = 15,
                                                              face = "bold"),
        axis.title = element_text(size = 10, face = "bold"))+
  guides(fill = FALSE)

p_dat_fillC <- ggplot(dat_fill, aes(New_Total_per_Day, fill= as.factor(Date)))+
  geom_histogram(binwidth = 1000)+
  ggtitle("Total Number of Steps Taken by Day\nNAs filled") +
  xlab("Steps Taken from Jan 1, 2012 to Nov 11, 2012") +
  ylab("Frequency")+
  scale_fill_manual(values = getPalette1(colourCount1))+
  theme(legend.position = "bottom", plot.title = element_text(size = 15,
                                                              face = "bold"),
        axis.title = element_text(size = 10, face = "bold"))+
  guides(fill = FALSE)

grid.arrange(p_dat1C, p_dat_fillC, ncol=2)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />
Since the total mean previously obtained has been used to replace the missing values, it does not come as a surprise that the two distributions, not being exactly equal, resemble each other. 

### 3.3 Calculate and report the mean and median total number of steps taken per day
####Rcode, Output, and Visualization


```r
#Mean and Median 
Total_meanF <- mean(dat_fill$New_Total_per_Day)
total_medianF <- median(dat_fill$New_Total_per_Day)

mean_plotF <- ggplot(dat_fill, aes(New_Total_per_Day)) +
  geom_density()+
  geom_vline(aes(xintercept= Total_meanF)) +
  ggtitle("Total Number of Steps Taken by Day, Mean\nNAs filled") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"))+
  annotate("text", x = 1750, y = 0.00009, label= "Mean = 10766.18", color = "red")+
  guides(fill = FALSE)

median_plotF <- ggplot(dat_fill, aes(New_Total_per_Day)) +
  geom_density()+
  geom_vline(aes(xintercept= total_medianF), linetype = "dashed", color = "red") +
  ggtitle("Total Number of Steps Taken by Day, Median\nNAs filled") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"))+
  annotate("text", x=1750, y = 0.00009, label = "Median = 10766.18")+
  guides(fill = FALSE)

grid.arrange(mean_plot, median_plot, mean_plotF, median_plotF, ncol = 2)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

No major differences as displayed by the comparative enclosed figure given the filling strategy already discussed. The median reports a slightly change.



## 4. Are there differences in activity patterns between weekdays and weekends?
#### Rcode, Output, and Visualization


```r
#Creating Weekdays
## Intervalsummarize(Average_Steps = mean(steps, na.rm = TRUE))
mean_steps <- mean(dat$steps, na.rm = TRUE)

dat2_WD <- dat %>% mutate(new_steps = zoo::na.fill(steps,mean_steps),
                          Date = as.Date(date), Days_of_the_Week = weekdays(Date)) %>% 
  select(-date, -steps) %>% group_by(Days_of_the_Week, interval) %>% 
  summarize(Total_Steps_per_Day = sum(new_steps)) 

dat2_WD1 <- dat2_WD %>% mutate(weektype = factor(Days_of_the_Week %in% c("Monday", "Tuesday", "Wednesday","Thursday", "Friday"),  levels = c("TRUE", "FALSE"), 
                                        labels=c("WEEKDAY","WEEKEND")))
head(dat2_WD1) # check                              
```

```
Source: local data frame [6 x 4]
Groups: Days_of_the_Week [1]

  Days_of_the_Week interval Total_Steps_per_Day weektype
             (chr)    (int)               (dbl)   (fctr)
1           Friday        0             74.7652  WEEKDAY
2           Friday        5             74.7652  WEEKDAY
3           Friday       10             74.7652  WEEKDAY
4           Friday       15             74.7652  WEEKDAY
5           Friday       20             74.7652  WEEKDAY
6           Friday       25             74.7652  WEEKDAY
```

```r
p_dat2_WD1 <- ggplot(dat2_WD1, aes(log10(interval), log10(Total_Steps_per_Day), fill = weektype,
                                   color = Days_of_the_Week)) +
  geom_line(stat = "identity")+
  scale_colour_brewer(palette = "Oranges")+
  ggtitle("Weekdays vs Weekends\nActivity ") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"))+
  theme_dark()+
  facet_grid(weektype~.)

p_dat2_WD1
```

<img src="PA1_template_files/figure-html/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />
A logarithmic transformation has been applied to the data in order to enhance the visualization. In agreement with previous figures, the higher amount of steps taken seems to occur at the end of the day.
The figures evidence, as well, that less steps are taken during the weekend.

