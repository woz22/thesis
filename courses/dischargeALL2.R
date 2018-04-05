library(zoo)
library(lubridate)
library(ggplot2)
library(dplyr)
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
names(d) <- c('date', 'discharge_pre')

names(d_fire)
names(d_fire) <- c('date', 'discharge_post')
View(d_fire)

#coerce date and discharge into useful object classes
d$date <- as.Date(d$date)
d$discharge_pre <- as.numeric(levels(d$discharge_pre))[d$discharge_pre] 

d_fire$date <- as.Date(d_fire$date)
d_fire$discharge_post <- as.numeric(levels(d_fire$discharge_post))[d_fire$discharge_post] 

#create ID columns for both dataframes
d$ID <- seq.int(nrow(d))
d_fire$ID <- seq.int(nrow(d_fire))

data <- left_join(d, d_fire, by = 'ID')
View(data)

data <- data[,-4]
View(data)

#plot data
p <- ggplot(data, aes(x = date.x))
p <- p + geom_line(aes(y = discharge_pre, color = '#F8766D'))
p <- p + geom_line(aes(y = discharge_post, color = '#00BFC4'))
p <- p + scale_color_discrete(name = "", labels = c("Post-Fire (2013-14)","Pre-Fire (2012-13)"))
p <- p + theme_bw(base_size = 16)
p <- p + scale_x_date(date_breaks = "1 month", date_labels =  "%b") 
p <- p + scale_y_continuous(limits = c(0,2500), breaks = seq(0, 2500, by = 500), expand = c(0,0))
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18))
p <- p + labs(colour = "Year", title = 'Discharge of South Fork Boise River at Neal Bridge',
              x = '', y = expression('Discharge (ft'^{3}*'/sec)'))
plot(p)


###############anderson ranch dam
urldam<- 'https://waterdata.usgs.gov/nwis/dv?cb_00010=on&cb_00060=on&format=rdb&site_no=13190500&referred_module=sw&period=&begin_date=2012-10-01&end_date=2013-09-30'
url_firedam <- 'https://waterdata.usgs.gov/nwis/dv?cb_00010=on&cb_00060=on&format=rdb&site_no=13190500&referred_module=sw&period=&begin_date=2013-10-01&end_date=2014-09-30'

dfdam <- read.table(urldam, sep = '\t',header = TRUE)
df_firedam <- read.table(url_firedam, sep = '\t',header = TRUE)
str(dfdam)
head(df_firedam)

#delete unneeded column headings
ddam <- dfdam[-1, c(3, 4)]
d_firedam <- df_firedam[-1, c(3, 4)]

#rename column headings
names(ddam)
names(ddam) <- c('date', 'discharge_pre')

names(d_firedam)
names(d_firedam) <- c('date', 'discharge_post')
View(d_firedam)

#coerce date and discharge into useful object classes
ddam$date <- as.Date(ddam$date)
ddam$discharge_pre <- as.numeric(levels(ddam$discharge_pre))[ddam$discharge_pre] 

d_firedam$date <- as.Date(d_firedam$date)
d_firedam$discharge_post <- as.numeric(levels(d_firedam$discharge_post))[d_firedam$discharge_post] 

#create ID columns for both dataframes
ddam$ID <- seq.int(nrow(ddam))
d_firedam$ID <- seq.int(nrow(d_firedam))

datadam <- left_join(ddam, d_firedam, by = 'ID')
View(datadam)

datadam <- datadam[,-4]
View(datadam)

#plot data
p <- ggplot()
p <- p + geom_line(data = data, aes(x = date.x, y = discharge_pre, color = '#F8766D'))
p <- p + geom_line(data = data, aes(x = date.x, y = discharge_post, color = '#00BFC4'))
p <- p + geom_line(data = datadam, aes(x = date.x, y = discharge_pre, color = '#F8766D'), linetype = 'dashed')
p <- p + geom_line(data = datadam, aes(x = date.x, y = discharge_post, color = '#00BFC4'), linetype = 'dashed')
p <- p + scale_color_discrete(name = "", labels = c("Neal Bridge \nPost-Fire (2013-14)",
                                                    "Neal Bridge \nPre-Fire (2012-13)", 
                                                    "Anderson Ranch Dam \nPost-Fire (2013-14)",
                                                    "Anderson Ranch Dam \nPre-Fire (2012-13)"))
p <- p + theme_bw(base_size = 16)
p <- p + scale_x_date(date_breaks = "1 month", date_labels =  "%b") 
p <- p + scale_y_continuous(limits = c(0,2500), breaks = seq(0, 2500, by = 500), expand = c(0,0))
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18))
p <- p + labs(colour = "Year", title = 'Discharge of South Fork Boise River',
              x = '', y = expression('Discharge (ft'^{3}*'/sec)'))
plot(p)

linetype = 'dashed'


####Neal Bridge - Anderson Dam Discharge

#create unique names for discharge_pre and discharge_post for neal bridge (data dataframe) and anderson dam (datadam dataframe)
colnames(data) <- c('date', 'discharge_pre_neal', 'ID', 'discharge_post_neal')
colnames(datadam) <- c('date', 'discharge_pre_anderson', 'ID', 'discharge_post_anderson')


#diff = data frame with neal bridge pre- and post-fire discharge, anderson dam pre- and post-fire discharge, and the difference between the two
diff <- left_join(data, datadam, join_by = 'ID')

diff$diff_pre <- diff$discharge_pre_neal - diff$discharge_pre_anderson
diff$diff_post <- diff$discharge_post_neal - diff$discharge_post_anderson

#plot difference in discharge between anderson dam and neal bridge stream gages pre- and post-fire
p <- ggplot(data = diff, aes(x = date))
p <- p + geom_line(data = diff, aes(x = date, y = diff_pre, color = '#F8766D'))
p <- p + geom_line(data = diff, aes(x = date, y = diff_post, color = '#00BFC4'))
p <- p + geom_hline(yintercept = 0, linetype = 'dotted')
p <- p + scale_color_discrete(name = "", labels = c("Post-Fire", "Pre-Fire"))
p <- p + theme_bw(base_size = 16)
p <- p + scale_x_date(date_breaks = "1 month", date_labels =  "%b") 
p <- p + scale_y_continuous(limits = c(-200, 1000), breaks = seq(-200, 1000, by = 200), expand = c(0,0))
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18))
p <- p + labs(colour = "Year", title = 'Difference in Discharge Between Stream Gages', 
              subtitle = 'Neal Bridge - Anderson Dam',
              x = '', y = expression('Discharge (ft'^{3}*'/sec)'))
plot(p)

View(diff)
