library(zoo)
library(lubridate)
library(ggplot2)
#library(tidyverse)

#retrieve data from 'https://waterdata.usgs.gov/nwis/dv?referred_module=sw&site_no=13192200'
#read in data, look at structure of data
#need to look at column headings on website to connect to column headings in R
url<- 'https://waterdata.usgs.gov/nwis/dv?cb_00010=on&cb_00060=on&format=rdb&site_no=13192200&referred_module=sw&period=&begin_date=2012-10-01&end_date=2013-09-30'
url_fire <- 'https://waterdata.usgs.gov/nwis/dv?cb_00010=on&cb_00060=on&format=rdb&site_no=13192200&referred_module=sw&period=&begin_date=2013-10-01&end_date=2014-09-30'

df <- read.table(url, sep = '\t',header = TRUE)
df_fire <- read.table(url_fire, sep = '\t',header = TRUE)
str(df)
head(df)

#delete unneeded column headings
d <- df[-1, c(3, 10)]
d_fire <- df_fire[-1, c(3, 10)]

#rename column headings
names(d)
names(d) <- c('date', 'discharge')

names(d_fire)
names(d_fire) <- c('date', 'discharge')

#add columns for pre and post-fire
d$when <- 'Pre-Fire (2012-2013)'
d_fire$when <- 'Post-Fire (2013-2014)'
head(d)
head(d_fire)

#coerce date and discharge into useful object classes
d$date <- as.Date(d$date)
d$discharge <- as.numeric(levels(d$discharge))[d$discharge] 

d_fire$date <- as.Date(d_fire$date)
d_fire$discharge <- as.numeric(levels(d_fire$discharge))[d_fire$discharge] 
head(d_fire)

##############change years of d_fire to 2012 and 2013
#if (year(d_fire$date) == 2013){
#  year(d_fire$date) <- 2012
#} else {
#  year(d_fire$date) <- 2013
#}
#View(d_fire)

#year(d_fire$date)

###############
d$m_d <- as.Date(with(d, format(date, format="%m-%d")), format="%m-%d")
d_fire$m_d <- as.Date(with(d_fire, format(date, format="%m-%d")), format="%m-%d")
head(d_fire)

class(d$when)
#plot data
p <- ggplot(rbind(d,d_fire), aes(date, discharge, group = factor(when), colour = factor(when)))
p <- p + geom_line() 
p <- p + geom_vline(aes(xintercept = as.integer(as.Date("2013-08-09"))), col = "black", linetype = "dashed")
p <- p + theme_bw()
p <- p + scale_x_date(date_breaks = "1 month", date_labels =  "%b") 
p <- p + scale_y_continuous(breaks = seq(0, 2000, by = 250))
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p <- p + labs(colour = "Year", title = 'Discharge of South Fork Boise River at Neal Bridge',
              x = '', y = expression('Discharge (ft'^{3}*'/sec)'))
plot(p)

