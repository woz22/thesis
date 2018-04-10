library(zoo)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
#library(tidyverse)

#retrieve data from 'https://waterdata.usgs.gov/nwis/dv?referred_module=sw&site_no=13192200'
#read in data, look at structure of data
#need to look at column headings on website to connect to column headings in R
url<- 'https://waterdata.usgs.gov/nwis/dv?cb_00010=on&cb_00060=on&format=rdb&site_no=13192200&referred_module=sw&period=&begin_date=2011-10-1&end_date=2012-09-30'
url_fire <- 'https://waterdata.usgs.gov/nwis/dv?cb_00010=on&cb_00060=on&format=rdb&site_no=13192200&referred_module=sw&period=&begin_date=2013-10-1&end_date=2014-09-30'

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

data <- data[,-4]


#Plot of Discharge of South Fork Boise River at Neal Bridge
pn <- ggplot(data, aes(x = date.x))
pn <- pn + geom_line(aes(y = discharge_pre, color = '#F8766D'))
pn <- pn + geom_line(aes(y = discharge_post, color = '#00BFC4'))
pn <- pn + scale_color_discrete(name = "", labels = c("Post-Fire (2013-14)","Pre-Fire (2011-12)"))
pn <- pn + theme_bw(base_size = 16)
pn <- pn + scale_x_date(date_breaks = "1 month", date_labels =  "%b") 
pn <- pn + scale_y_continuous(limits = c(0,6500), breaks = seq(0, 6500, by = 500), expand = c(0,0))
pn <- pn + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18))
pn <- pn + labs(colour = "Year", title = 'Discharge of South Fork Boise River', subtitle = 'Neal Bridge stream gage',
              x = '', y = expression('Discharge (ft'^{3}*'/sec)'))
plot(pn)


###############anderson ranch dam
urldam<- 'https://waterdata.usgs.gov/nwis/dv?cb_00010=on&cb_00060=on&format=rdb&site_no=13190500&referred_module=sw&period=&begin_date=2011-10-01&end_date=2012-09-30'
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

#Plot of Discharge of South Fork Boise River at Anderson Ranch
pa <- ggplot()
pa <- pa + geom_line(data = datadam, aes(x = date.x, y = discharge_pre, color = '#F8766D'))
pa <- pa + geom_line(data = datadam, aes(x = date.x, y = discharge_post, color = '#00BFC4'))
pa <- pa + scale_color_discrete(name = "", labels = c("Post-Fire (2013-14)",
                                                    "Pre-Fire (2012-13)"))
pa <- pa + theme_bw(base_size = 16)
pa <- pa + scale_x_date(date_breaks = "1 month", date_labels =  "%b") 
pa <- pa + scale_y_continuous(limits = c(0,6500), breaks = seq(0, 6500, by = 1000), expand = c(0,0))
pa <- pa + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18))
pa <- pa + labs(colour = "Year", title = 'Discharge of South Fork Boise River', subtitle = 'Anderson Dam stream gage',
              x = '', y = expression('Discharge (ft'^{3}*'/sec)'))
plot(pa)




pa <- ggplot()
pa <- pa + geom_line(data = datadam, aes(x = date.x, y = discharge_pre, color = '#F8766D'), linetype = 'dashed')
pa <- pa + geom_line(data = datadam, aes(x = date.x, y = discharge_post, color = '#00BFC4'), linetype = 'dashed')
pa <- pa + scale_color_discrete(name = "", labels = c("Post-Fire (2013-14)",
                                                      "Pre-Fire (2012-13)"))
pa <- pa + theme_bw(base_size = 16)
pa <- pa + scale_x_date(date_breaks = "1 month", date_labels =  "%b") 
pa <- pa + scale_y_continuous(limits = c(0,6500), breaks = seq(0, 6500, by = 1000), expand = c(0,0))
pa <- pa + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18))
pa <- pa + labs(colour = "Year", title = 'Discharge of South Fork Boise River', subtitle = 'Anderson Dam stream gage',
                x = '', y = expression('Discharge (ft'^{3}*'/sec)'))
plot(pa)

plot_grid(pn, pa)

#PLOT NEXT TO EACH OTHER
#Plot neal bridge without legend
 


#Plot of Both
p <- ggplot()
p <- p + geom_line(data = data, aes(x = date.x, y = discharge_pre, color = '#F8766D'))
p <- p + geom_line(data = data, aes(x = date.x, y = discharge_post, color = '#00BFC4'))
p <- p + geom_line(data = datadam, aes(x = date.x, y = discharge_pre, color = '#F8766D'), linetype = 'dashed')
p <- p + geom_line(data = datadam, aes(x = date.x, y = discharge_post, color = '#00BFC4'), linetype = 'dashed')
p <- p + scale_color_discrete(name = "", labels = c("Post-Fire (2013-14)",
                                                    "Pre-Fire (2011-12)"))
p <- p + theme_bw(base_size = 16)
p <- p + scale_x_date(date_breaks = "1 month", date_labels =  "%b") 
p <- p + scale_y_continuous(limits = c(0,6500), breaks = seq(0, 6500, by = 1000), expand = c(0,0))
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18))
p <- p + labs(colour = "Year", title = 'Discharge of South Fork Boise River',
              x = '', y = expression('Discharge (ft'^{3}*'/sec)'))
plot(p)

#######same plot 2nd try--use long form of data
colnames(data) <- c('date', 'discharge_pre_neal', 'ID', 'discharge_post_neal')
colnames(datadam) <- c('date', 'discharge_pre_anderson', 'ID', 'discharge_post_anderson')


#diff = data frame with neal bridge pre- and post-fire discharge, anderson dam pre- and post-fire discharge, and the difference between the two
diff <- left_join(data, datadam, join_by = 'ID')
colnames(diff)[1] <- 'date'

#longform
diff_long <- gather(diff, key = key , value = discharge, -c(date, ID))

#plot
p <- ggplot(data = diff_long, aes(x = date, y = discharge, color = key))

p <- p + geom_line(aes(linetype = key))
p <- p + scale_linetype_manual(values = c('solid',  'solid', 'dashed', 'dashed'))
p <- p + scale_color_manual(values = c('#F8766D', '#F8766D', '#00BFC4', '#00BFC4'))
#p <- p + geom_line(data = data, aes(x = date.x, y = discharge_post, color = '#00BFC4'))
#p <- p + geom_line(data = datadam, aes(x = date.x, y = discharge_pre, color = '#F8766D'))
#p <- p + geom_line(data = datadam, aes(x = date.x, y = discharge_post, color = '#00BFC4'))
p <- p + theme_bw(base_size = 16)
p <- p + scale_x_date(date_breaks = "1 month", date_labels =  "%b") 
p <- p + scale_y_continuous(limits = c(0,2500), breaks = seq(0, 2500, by = 500), expand = c(0,0))
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18))
p <- p + labs(colour = "Year", title = 'Discharge of South Fork Boise River',
              x = '', y = expression('Discharge (ft'^{3}*'/sec)'))
plot(p)

str(diff_long)
unique(diff_long$key)

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
p <- p + scale_y_continuous(limits = c(-300, 1200), breaks = seq(-200, 1200, by = 200), expand = c(0,0))
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18))
p <- p + labs(colour = "Year", title = 'Difference in Discharge Between Stream Gages', 
              subtitle = 'Neal Bridge - Anderson Dam',
              x = '', y = expression('Discharge (ft'^{3}*'/sec)'))
plot(p)

