library(tidyr)
library(dplyr)
library(ggplot2)

herb <- read.csv('C:\\Users\\User\\Documents\\GitHub\\thesis\\herb\\data_20180425112112.csv', header = TRUE)

#clean data
herb <- filter(herb, year != 'NA' & sp_phase != 'NA'& scode !='NA')   #remove rows where year = NA or sp_phase = NA
herb <- distinct(herb)    
herb <- filter(herb, herb_fuel_live_wd != 'NA')


#create table with site codes and year of implementation
scode <- as.character(c('BM', 'BC', 'DR', 'WB', 'MC', 'SV', 'GR', 'ON', 'SC'))
y_imp <- as.numeric(c(7, 6, 7, 6, 6, 7, 7, 6, 7))
relation <- data.frame(scode, y_imp, stringsAsFactors = FALSE)

#combine table above with larger herb dataframe
data <- left_join(herb, relation, by = 'scode')


#create year since treatment column
data$yst <- data$year - data$ y_imp

#filter select full sampling years
data <- filter(data, yst %in% c(-1, 0, 1, 2, 3, 6, 10))

#make year of pre-treatment 0#######################

#plot data by year since treatment
p <- ggplot(data = data, aes(x = yst, y = herb_fuel_live_wd, color = factor(sp_phase)))
p <- p + geom_point(alpha = .1)
p <- p + facet_wrap(~rcode) + theme_bw()
p <- p + scale_x_discrete(limits = seq(-1, 10, by = 1), name = 'Years Since Treatment')
plot(p)

sum_herb <- data %>% 
  group_by(yst, sp_phase) %>% 
  summarise('n' = n(), 'mean' = mean(herb_fuel_live_wd))
View(sum_herb)
