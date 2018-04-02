library(ggplot2)
#library(tidyverse)

#read in data, look at structure of data
#need to look at column headings on website to connect to column headings in R
url <- 'https://waterdata.usgs.gov/nwis/dv?cb_00010=on&cb_00060=on&format=rdb&site_no=13192200&referred_module=sw&period=&begin_date=2012-10-01&end_date=2013-08-01'
df <- read.table(url, sep = '\t',header = TRUE)
str(df)
head(df)

#delete unneeded column headings
d <- df[-1, c(3, 10)]

#rename column headings
names(d)
names(d) <- c('date', 'discharge')

#coerce date and discharge into useful object classes
d$date <- as.Date(d$date)
d$discharge <- as.numeric(levels(d$discharge))[d$discharge] 

#plot data
p <- ggplot(data = d, aes(x = date, y = discharge))
p <- p + geom_line() + scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")
p <- p + theme_bw()
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p <- p + labs(title = 'Discharge of South Fork Boise River at Neal Bridge', subtitle = 'Oct 2012 to Aug 2013', 
              x = '', y = expression('Discharge (ft'^{3}*'/sec)'))
plot(p)

