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


