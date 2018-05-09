library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

herb <- read.csv('C:\\Users\\User\\Documents\\GitHub\\thesis\\herb\\data_20180425112112.csv', header = TRUE)

#clean data
herb <- herb %>%
  filter(!is.na(year)) %>%
  filter(!is.na(scode)) %>%
  filter(!is.na(sp_phase)) %>%
  filter(!is.na(herb_fuel_live_wd))

#remove South Ruby
herb <- filter(herb, scode != 'SR')

#create table with site codes and year of implementation
scode <- as.character(c('BM', 'BC', 'DR', 'WB', 'MC', 'SV', 'GR', 'ON', 'SC'))
y_imp <- as.numeric(c(7, 6, 7, 6, 6, 7, 7, 6, 7))
relation <- data.frame(scode, y_imp, stringsAsFactors = FALSE)

#combine table above with larger herb dataframe
data <- left_join(herb, relation, by = 'scode')

#create year since treatment column
data$yst <- data$year - data$ y_imp

#filter select full sampling years
data <- filter(data, yst %in% c( 0, 1, 2, 3, 6, 10))


###plot only jp
d <- filter(data, rcode =='JP')


p <- ggplot(data = d, aes(x = yst, y = herb_fuel_live_wd, color = factor(sp_phase)))
p <- p + geom_point(alpha = .5, position=position_dodge(.6))
#p <- p + geom_point(data = sum_herb, aes(x = yst, y = mean, fill = factor(sp_phase)), 
#alpha = 0.7, position=position_jitterdodge())
p <- p + labs(y = 'Live Herbaceous Fuel Loading (kg/ha)',
              color = 'Pre-Treatment \nWoodland Phase')
p <- p + facet_grid(~treatment) + theme_bw()
p <- p + scale_x_discrete(limits = seq(0, 10, by = 1), name = 'Years Since Treatment')
plot(p)


#########no colors
d$treatment <- factor(d$treatment, levels = c('CO', 'FI', 'BM', 'ME'))
p <- ggplot(data = d, aes(x = yst, y = herb_fuel_live_wd))
p <- p + geom_point(color = '#1c9099', size = 2, shape = 21)
p <- p + labs(y = 'Fuel Loading (kg/ha)')
p <- p + facet_grid(~treatment) + theme_bw()
p <- p + scale_x_discrete(limits = c(0,1,2,3,6,10), name = 'Years Since Treatment')
p <- p + theme_bw(base_size = 18)
p <- p + theme(axis.text.x = element_text(size = 14))
plot(p)

ggsave(p, file="C:\\Users\\User\\Documents\\GitHub\\thesis\\sage_pres\\herb.png", 
       width=10, height=4, 
       units="in", dpi=800)

sum_herb <- data %>% 
  group_by(yst, sp_phase, rcode, treatment) %>% 
  summarise('n' = n(), 
            'mean' = mean(herb_fuel_live_wd + herb_fuel_dead_wd, na.rm = TRUE),
            'se' = std.e(herb_fuel_live_wd + herb_fuel_dead_wd))

#plot data by year since treatment
#p <- p + ggplot()
p <- ggplot(data = data, aes(x = yst, y = herb_fuel_live_wd, color = factor(sp_phase)))
p <- p + geom_point(alpha = .5, position=position_dodge(.6))
#p <- p + geom_point(data = sum_herb, aes(x = yst, y = mean, fill = factor(sp_phase)), 
                    #alpha = 0.7, position=position_jitterdodge())
p <- p + facet_grid(rcode~treatment) + theme_bw()
p <- p + scale_x_discrete(limits = seq(0, 10, by = 1), name = 'Years Since Treatment')
plot(p)

a <- ggplot(data = data, aes(herb_fuel_live_wd))
a <- a + geom_histogram() + facet_wrap(rcode ~ yst)
plot(a)



####
p <- ggplot(data = sum_herb, aes(x = yst, y = mean, color = factor(sp_phase)))
p <- p + geom_point(alpha = 0.7)
p <- p + facet_grid(rcode~treatment) + theme_bw()
p <- p + scale_x_discrete(limits = seq(0, 10, by = 1), name = 'Years Since Treatment')
plot(p)

sh <- filter(sum_herb, rcode == 'JP')

sh$sp_phase <- factor(sh$sp_phase, levels = c('1','2','3'))
sh$treatment <- factor(sh$treatment, levels = c('CO', 'FI', 'ME', 'BM'))

p <- ggplot(data = sh, aes(x = yst, y = mean, color = sp_phase))
p <- p + geom_point(size = 2)
#p <- p + geom_errorbar(data = sh, aes(ymin = mean - se, ymax = mean + se), 
  #                     width = 0.3)
p <- p + facet_grid(~treatment) + theme_bw()
p <- p + scale_x_discrete(limits = seq(0, 10, by = 1), name = 'Years Since Treatment')
plot(p)


####

data$pre_tc <- NA
for (i in unique(data$subplot_id)) {                                                            #for each unique subplot_id
  pre_tree_c <- data$tree_cover_ttl[data$subplot_id == i][which.min(data$year)]                      #take the tree cover value from the first year of data collection
  data$pre_tc[data$subplot_id == i] <- pre_tree_c                                                #put those tree cover values in a column
}


#sub_data <- filter(data, yst %in% c(0,0,1,3,10))
#sub_data$yst <- factor(sub_data$yst, levels = c(0, 0, 1, 3, 10))
#levels(sub_data$yst) <- c('pre', 'pre', '1', '3', '10')

p <- ggplot(data = sub_data, aes(x = pre_tc, y = herb_fuel_live_wd, color = factor(yst)))
p <- p + geom_point(alpha = 0.5)
p <- p + facet_grid(rcode~treatment) + theme_bw()
#p <- p + scale_x_discrete(limits = seq(0, 10, by = 1), name = 'Years Since Treatment')
plot(p)








mast_tdi <- read.csv('C:\\Users\\User\\Documents\\GitHub\\thesis\\Bruce\\edited\\tdi_mast.csv', skip = 5)
m_tdi <- select(mast_tdi, 1:9)
m_tdi$TRT <- 'GC'
m_tdi <- filter(m_tdi, LOC != 'ST')

m_tdi$pre_sub <- NA
m_tdi$pre_sub <- ifelse(m_tdi$SUB < 10, paste0('00', m_tdi$SUB), paste0('0', m_tdi$SUB))
m_tdi$pre_sub <- paste0(m_tdi$TRT, m_tdi$pre_sub)

a <- unite(m_tdi, subplot_id, REG, LOC, pre_sub, sep = '-') 
d <- merge(m_tdi, a)
tdi <- select(d, c(TDI, subplot_id))
tdi$subplot_id <- factor(tdi$subplot_id)
dtdi <- left_join(tdi, data, by = 'subplot_id')

m <- lm(herb_fuel_live_wd ~ yst*pre_tc, data = dtdi)
summary(m)

p <- ggplot(dtdi, aes(x = yst, y = herb_fuel_live_wd, color = TDI))
p <- p + geom_jitter(width = .1)
p <- p + facet_grid(~treatment) + theme_bw()
p <- p + scale_x_discrete(limits = seq(0, 10, by = 1), name = 'Years Since Treatment')
p <- p + scale_color_gradient(low = '#ccece6', high = '#006d2c')
plot(p)


p <- ggplot(dtdi, aes(x = yst, y = herb_fuel_live_wd, colour = TDI))
p <- p + geom_jitter(width = .2, size = 3)
p <- p + facet_grid(~treatment) + theme_bw()
p <- p + scale_x_discrete(limits = seq(0, 10, by = 1), name = 'Years Since Treatment')
p <- p + scale_colour_gradient2(low = '#313695', mid = '#ffffbf', high = '#d73027', midpoint = mean(dtdi$TDI))
plot(p)

str(dtdi)
?scale_color_brewer
unique(dtdi$TDI)
?geom_jitter

dtdi$TDI_cat <- NA
dtdi$TDI_cat <- as.character(dtdi$TDI_cat)
dtdi$TDI_cat <- for (i in dtdi$TDI) {
  if(dtdi$TDI[i] < .33){
    dtdi$TDI_cat[i] <- '< 0.33'
    }
  else if(dtdi$TDI[i] > .33 & dtdi$TDI[i] < .66) {
    dtdi$TDI_cat[i] <- '0.33 - 0.66'
  }
  else {
    dtdi$TDI_cat[i] <- '> 0.66'
  }
}


