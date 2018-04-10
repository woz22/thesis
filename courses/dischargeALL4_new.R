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

#delete unneeded column headings
d <- df[-1, c(3, 10)]
d_fire <- df_fire[-1, c(3, 10)]

#rename column headings
names(d)
names(d) <- c('date', 'discharge_pre')

names(d_fire)
names(d_fire) <- c('date', 'discharge_post')

#coerce date and discharge into useful object classes
d$date <- as.Date(d$date)
d$discharge_pre <- as.numeric(levels(d$discharge_pre))[d$discharge_pre] 

d_fire$date <- as.Date(d_fire$date)
d_fire$discharge_post <- as.numeric(levels(d_fire$discharge_post))[d_fire$discharge_post] 

#remove leap year extra day from dataframe d so that next step will work
d <- d[-152,]

#create ID columns for both dataframes
d$ID <- seq.int(nrow(d))
d_fire$ID <- seq.int(nrow(d_fire))

#join the two tables by the ID column
data <- left_join(d, d_fire, by = 'ID')

#remove second column of date values
data <- data[,-4]

#rename 'date.x' column 'date'
colnames(data)[1] <- 'date'

#gather discharge_pre and discharge_post into one column
data <- gather(data, key = time, value = discharge, -c(date, ID))

#Plot of Discharge of South Fork Boise River at Neal Bridge
pn <- ggplot(data, aes(x = date, y = discharge, color = time)) + geom_line()
pn <- pn + scale_color_discrete(name = "", labels = c('Post-Fire', 'Pre-Fire'))
pn <- pn + theme_bw(base_size = 16)
pn <- pn + scale_x_date(date_breaks = "1 month", date_labels =  "%b") 
pn <- pn + scale_y_continuous(limits = c(0,6500), breaks = seq(0, 6500, by = 1000), expand = c(0,0))
pn <- pn + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18))
pn <- pn + labs(title = 'Neal Bridge Stream Gage', 
                x = '', 
                y = expression('Discharge (ft'^{3}*'/sec)'))
plot(pn)


###############anderson ranch dam
urldam <- 'https://waterdata.usgs.gov/nwis/dv?cb_00010=on&cb_00060=on&format=rdb&site_no=13190500&referred_module=sw&period=&begin_date=2011-10-01&end_date=2012-09-30'
url_firedam <- 'https://waterdata.usgs.gov/nwis/dv?cb_00010=on&cb_00060=on&format=rdb&site_no=13190500&referred_module=sw&period=&begin_date=2013-10-01&end_date=2014-09-30'

dfdam <- read.table(urldam, sep = '\t',header = TRUE)
df_firedam <- read.table(url_firedam, sep = '\t',header = TRUE)

#delete unneeded column headings
ddam <- dfdam[-1, c(3, 4)]
d_firedam <- df_firedam[-1, c(3, 4)]

#rename column headings
names(ddam)
names(ddam) <- c('date', 'discharge_pre')

names(d_firedam)
names(d_firedam) <- c('date', 'discharge_post')

#coerce date and discharge into useful object classes
ddam$date <- as.Date(ddam$date)
ddam$discharge_pre <- as.numeric(levels(ddam$discharge_pre))[ddam$discharge_pre] 

d_firedam$date <- as.Date(d_firedam$date)
d_firedam$discharge_post <- as.numeric(levels(d_firedam$discharge_post))[d_firedam$discharge_post] 

#remove leap year extra day from dataframe d so that next step will work
ddam <- ddam[-152,]

#create ID columns for both dataframes
ddam$ID <- seq.int(nrow(ddam))
d_firedam$ID <- seq.int(nrow(d_firedam))

#join pre and post fire data frames by 'ID' column
datadam <- left_join(ddam, d_firedam, by = 'ID')

#remove redundant column of date values
datadam <- datadam[,-4]

#rename 'date.x' column 'date'
colnames(datadam)[1] <- 'date'

#gather discharge_pre and discharge_post into one column
datadam <- gather(datadam, key = time, value = discharge, -c(date, ID))

#Plot of Discharge of South Fork Boise River at Anderson Ranch
pa <- ggplot(data = datadam, aes(x = date, y = discharge, color = time))
pa <- pa + geom_line()
pa <- pa + scale_color_discrete(name = "", labels = c('Post-Fire', 'Pre-Fire'))
pa <- pa + theme_bw(base_size = 16)
pa <- pa + scale_x_date(date_breaks = "1 month", date_labels =  "%b") 
pa <- pa + scale_y_continuous(limits = c(0,6500), breaks = seq(0, 6500, by = 1000), expand = c(0,0))
pa <- pa + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18))
pa <- pa + labs(title = 'Anderson Dam Stream Gage', 
                x = '', 
                y = expression('Discharge (ft'^{3}*'/sec)'))
plot(pa)

###join data and plot both on same graph

#create site codes for data and datadam
data$site <- 'Neal Bridge'
datadam$site <- 'Anderson Dam'
datadam$time <- as.factor(datadam$time)
levels(datadam$time) <- c('Post-Fire','Pre-Fire')
data$time <- as.factor(data$time)
levels(data$time) <- c('Post-Fire', 'Pre-Fire')

#join dataframes
all <- rbind(data, datadam)
all$site <- as.factor(all$site)
all$site <- relevel(all$site, ref = "Neal Bridge")

#Plot of both stream gages on same graph
p <- ggplot(data = all, aes(x = date, y = discharge, color = time, linetype = site))
p <- p + geom_line()
p <- p + theme_bw(base_size = 16)
p <- p + scale_x_date(date_breaks = "1 month", date_labels =  "%b") 
p <- p + scale_y_continuous(limits = c(0,6500), breaks = seq(0, 6500, by = 1000), expand = c(0,0))
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), legend.title = element_blank())
p <- p + labs(colour = "Year", 
              title = 'Neal Bridge and Anderson Dam',
              x = '', 
              y = expression('Discharge (ft'^{3}*'/sec)'))
plot(p)

#faceted plot of stream gages
p <- ggplot(data = all, aes(x = date, y = discharge, color = time))
p <- p + geom_line()
p <- p + theme_bw(base_size = 16)
p <- p + scale_x_date(date_breaks = "1 month", date_labels =  "%b") 
p <- p + scale_y_continuous(limits = c(0,6500), breaks = seq(0, 6500, by = 1000), expand = c(0,0))
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), legend.title = element_blank())
p <- p + labs(colour = "Year",
              x = '', 
              y = expression('Discharge (ft'^{3}*'/sec)'))
p <- p + facet_wrap(~site)
plot(p)

#change values of 'site' column from Neal Bridge and Anderson Dam to nb and ad
data$site <- "nb"
datadam$site <- "ad"

#change values of 'time' column from Pre-Fire and Post-Fire to pre and post
levels(data$time) <- c('post', 'pre')
levels(datadam$time)<- c('post', 'pre')

#unite 'site' and 'time' columns
data_u <- unite(data, "type", c('site', 'time'))
datadam_u <- unite(datadam, "type", c('site', 'time'))


#change dataframes from long to wide form
data_wide <- spread(data_u, key = type, value = discharge)
datadam_wide <- spread(datadam_u, key = type, value = discharge)
diff <- left_join(data_wide, datadam_wide, join_by = 'ID')

diff$diff_pre <- diff$nb_pre - diff$ad_pre
diff$diff_post <- diff$nb_post - diff$ad_post


#should I change values of difference in discharge that are <0 to 0????
#which(diff$diff_pre < 0)
#which(diff$diff_post < 0)
#diff$diff_pre <- ifelse(diff$diff_pre < 0, 0, diff$diff_pre)
#diff$diff_post <- ifelse(diff$diff_post < 0, 0, diff$diff_post)

#put diff dataframe in long form
diff_long <- diff[, -c(3:6)]
diff_long <- gather(diff_long, key = time, value = discharge, -c('ID', 'date'))

#plot difference in discharge between anderson dam and neal bridge stream gages pre- and post-fire
p <- ggplot(data = diff_long, aes(x = date, y = discharge, color = time))
p <- p + geom_line()
p <- p + geom_hline(yintercept = 0, linetype = 'dotted')
p <- p + scale_color_discrete(name = "", labels = c("Post-Fire", "Pre-Fire"))
p <- p + theme_bw(base_size = 16)
p <- p + scale_x_date(date_breaks = "1 month", date_labels =  "%b") 
p <- p + scale_y_continuous(limits = c(-300, 1200), breaks = seq(-200, 1200, by = 200), expand = c(0,0))
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18))
p <- p + labs(colour = "Year", 
              title = 'Neal Bridge - Anderson Dam', 
              x = '', 
              y = expression('Discharge (ft'^{3}*'/sec)'))
plot(p)

#plot snow melt data on top of discharge data
snow <- read.csv('C:\\Users\\User\\Documents\\GitHub\\thesis\\courses\\snowmelt.csv')

#snow data starts on jan 1, but discharge data starts oct 1--add 92 to snow ID so that days match
snow$ID <- snow$Day + 92

#put snow df in long form
#snow <- gather(snow, key = melt_year, value = melt, -c(ID, Day))

#join snow and diff_long tables
overlay <- left_join(diff, snow, by = 'ID')

#separate
snowdate <- overlay[, -c(3:9)]

#make snowdate long from
snowdate_long <- gather(snowdate, key = melt_year, value = melt, -c(ID, date))

#plot snow and difference
p <- ggplot()
p <- p + geom_line(data = diff_long, aes(x = date, y = discharge, color = time))
p <- p + geom_point(data = snowdate, aes(x = date, y = Melt_2014 * 1250), color = '#F8766D')
p <- p + geom_point(data = snowdate, aes(x = date, y = Melt_2012 * 1250), color = '#00BFC4')
p <- p + geom_vline(xintercept = as.Date('2012-04-08'), linetype = 'dashed', color = '#F8766D')
p <- p + geom_vline(xintercept = as.Date('2012-04-19'), linetype = 'dashed', color = '#00BFC4')
p <- p + geom_hline(yintercept = 0, linetype = 'dotted')
p <- p + scale_color_discrete(name = "", labels = c("Post-Fire", "Pre-Fire"))
p <- p + theme_bw(base_size = 16)
p <- p + scale_x_date(date_breaks = "1 month", date_labels =  "%b") 
p <- p + scale_y_continuous(limits = c(-300, 1300), 
                            breaks = seq(-250, 1250, by = 250), 
                            expand = c(0,0),
                            sec.axis = sec_axis(~./12.5, 
                                                name = 'Snow Cover (%)',
                                                breaks = seq(0, 100, by = 25)))
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18))
p <- p + labs(colour = "Year", title = 'Neal Bridge - Anderson Dam', 
              x = '', y = expression('Discharge (ft'^{3}*'/sec)'))
plot(p)



####cumulative discharge
overlay$cumul_pre <- cumsum(overlay$diff_pre)
overlay$cumul_post <- cumsum(overlay$diff_post)
cumulative <- overlay[,-c(3:11)]

#make cumulative long form
cumul_long <- gather(cumulative, key = time, value = discharge, -c(date, ID))
###########################ADD SECOND AXIS labels; is scaling correct?

#plot cumulative discharge
p <- ggplot()
p <- p + geom_line(data = cumul_long, aes(x = date, y = discharge, color = time))
#p <- p + geom_line(aes(y = cumul_pre), color = '#00BFC4')
p <- p + geom_point(data = snowdate, aes(x = date, y = Melt_2014 * 40000), color = '#F8766D')
p <- p + geom_point(data = snowdate, aes(x = date, y = Melt_2012 * 40000), color = '#00BFC4')
p <- p + geom_vline(xintercept = as.Date('2012-04-08'), linetype = 'dashed', color = '#F8766D')
p <- p + geom_vline(xintercept = as.Date('2012-04-19'), linetype = 'dashed', color = '#00BFC4')
p <- p + scale_color_discrete(name = "", labels = c("Post-Fire", "Pre-Fire"))
p <- p + theme_bw(base_size = 16)
p <- p + scale_x_date(date_breaks = "1 month", date_labels =  "%b") 
p <- p + scale_y_continuous(limits = c(0, 40000), 
                            sec.axis = sec_axis(~./400, name = 'Snow Cover (%)'))
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18))
p <- p + labs(colour = "Year", 
              title = 'Cumulative Discharge',
              x = '',
              y = expression('Discharge (ft'^{3}*'/sec)'))
plot(p)


