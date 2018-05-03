library(cowplot)
library(dplyr)
library(tidyr)
library(ggplot2)

data <- read.csv('C:\\Users\\User\\Documents\\GitHub\\thesis\\saddlemtn\\saddle.csv', header = TRUE)

data <- data %>%
  filter(!is.na(year)) %>%   #remove rows where year = NA or sp_phase = NA
  filter(!is.na(treatment)) %>%
  filter(year != 16) %>%
  distinct()

data$shrub_bio_ttl <- abs(data$shrub_bio_ttl)
data$herb_total <- data$herb_fuel_dead_ss + data$herb_fuel_live_ss
data$dwd_fuel_1000h <- data$dwd_fuel_1000h_s + data$dwd_fuel_1000h_r


std.e <- function(x) sd(x, na.rm=TRUE)/sqrt(length(na.omit(x)))

#summarise data for graphing
fuel <- data %>%
  group_by(year, treatment) %>%
  summarise('n' = n(),
            'mean_herb' = round(mean(herb_total, na.rm = TRUE), 0),
            'se_herb' = round(std.e(herb_total), 0),
            'mean_dwd10' = round(mean(dwd_fuel_10h, na.rm = TRUE), 0),
            'se_dwd10' = round(std.e(dwd_fuel_10h), 0),
            'mean_dwd100' = round(mean(dwd_fuel_100h, na.rm = TRUE), 0),
            'se_dwd100' = round(std.e(dwd_fuel_100h), 0),
            'mean_dwd1000' = round(mean(dwd_fuel_1000h, na.rm = TRUE), 0),
            'se_dwd1000' = round(std.e(dwd_fuel_1000h), 0),
            'mean_shrub' = round(mean(abs(shrub_bio_ttl), na.rm = TRUE), 0),
            'se_shrub' = round(std.e(abs(shrub_bio_ttl)), 0))

fuel$year <- factor(fuel$year)
levels(fuel$year) <- list('Pre' = '7', '1' = '9', '3' = '11', '6' = '14')

#shrub fuel loading
p <- ggplot(data = fuel, aes(x = treatment, y = mean_shrub, fill = year))
p <- p + geom_col(position = 'dodge') + theme_bw()
p <- p + geom_errorbar(aes(ymin = mean_shrub - se_shrub, ymax = mean_shrub + se_shrub), 
                       position = position_dodge(0.9), 
                       width = 0.3)
p <- p + labs(title = 'Shrub Fuel Loading',
          x = 'Treatment',
          y = 'Mean Shrub Fuel Loading ± SE (kg/ha)',
          fill = 'Years Since \nTreatment')
p <- p + scale_fill_manual(values = c('#a8ddb5', '#7bccc4', '#43a2ca', '#0868ac'))
plot(p)

#herb fuel loading
p <- ggplot(data = fuel, aes(x = treatment, y = mean_herb, fill = year))
p <- p + geom_col(position = 'dodge') + theme_bw()
p <- p + geom_errorbar(aes(ymin = mean_herb - se_herb, ymax = mean_herb + se_herb), 
                       position = position_dodge(width = 0.9), 
                       width = 0.3)
p <- p + labs(title = 'Herbaceous (live + dead) Fuel Loading',
              x = 'Treatment',
              y = 'Mean Herbaceous Fuel Loading ± SE (kg/ha)',
              fill = 'Years Since \nTreatment')
p1 <- p + scale_fill_manual(values = c('#a8ddb5', '#7bccc4', '#43a2ca', '#0868ac'))
plot(p1)

#dwd10
p <- ggplot(data = fuel, aes(x = treatment, y = mean_dwd10, fill = year))
p <- p + geom_col(position = 'dodge') + theme_bw()
p <- p + geom_errorbar(aes(ymin = mean_dwd10 - se_dwd10, ymax = mean_dwd10 + se_dwd10), 
                       position = position_dodge(width = 0.9), 
                       width = 0.3)
p <- p + labs(title = '10h DWD Fuel Loading',
              x = 'Treatment',
              y = 'Mean Shrub Fuel Loading ± SE (kg/ha)',
              fill = 'Years Since \nTreatment')
p2 <- p + scale_fill_manual(values = c('#a8ddb5', '#7bccc4', '#43a2ca', '#0868ac'))
plot(p2)

#dwd100
p <- ggplot(data = fuel, aes(x = treatment, y = mean_dwd100, fill = year))
p <- p + geom_col(position = 'dodge') + theme_bw()
p <- p + geom_errorbar(aes(ymin = mean_dwd100 - se_dwd100, ymax = mean_dwd100 + se_dwd100),
                       position = position_dodge(width = 0.9), 
                       width = 0.3)
p <- p + labs(title = '100h DWD Fuel Loading',
              x = 'Treatment',
              y = 'Mean 100h DWD Fuel Loading ± SE (kg/ha)',
              fill = 'Years Since \nTreatment')
p3 <- p + scale_fill_manual(values = c('#a8ddb5', '#7bccc4', '#43a2ca', '#0868ac'))
plot(p3)

#dwd1000
p <- ggplot(data = fuel, aes(x = treatment, y = mean_dwd1000, fill = year))
p <- p + geom_col(position = 'dodge') + theme_bw()
p <- p + geom_errorbar(aes(ymin = mean_dwd1000 - se_dwd1000, ymax = mean_dwd1000 + se_dwd1000), 
                       position = position_dodge(width = 0.9), 
                       width = 0.3)
p <- p + labs(title = '1000h DWD Fuel Loading',
              x = 'Treatment',
              y = 'Mean 1000h DWD Fuel Loading ± SE (kg/ha)',
              fill = 'Years Since \nTreatment')
p4 <- p + scale_fill_manual(values = c('#a8ddb5', '#7bccc4', '#43a2ca', '#0868ac'))
plot(p4)

ggsave(p, file = 'C:\\Users\\User\\Documents\\GitHub\\thesis\\saddlemtn\\p.png', device = 'png', 
       width = 20*1.25, height = 15*1.25, units = "cm", dpi = 1200) 
ggsave(p1, file = 'C:\\Users\\User\\Documents\\GitHub\\thesis\\saddlemtn\\p1.png', device = 'png', 
       width = 20*1.25, height = 15*1.25, units = "cm", dpi = 1200) 
ggsave(p2, file = 'C:\\Users\\User\\Documents\\GitHub\\thesis\\saddlemtn\\p2.png', device = 'png', 
       width = 20*1.25, height = 15*1.25, units = "cm", dpi = 1200) 
ggsave(p3, file = 'C:\\Users\\User\\Documents\\GitHub\\thesis\\saddlemtn\\p3.png', device = 'png', 
       width = 20*1.25, height = 15*1.25, units = "cm", dpi = 1200) 
ggsave(p4, file = 'C:\\Users\\User\\Documents\\GitHub\\thesis\\saddlemtn\\p4.png', device = 'png', 
       width = 20*1.25, height = 15*1.25, units = "cm", dpi = 1200) 
ggsave(p5, file = 'C:\\Users\\User\\Documents\\GitHub\\thesis\\saddlemtn\\p5.png', device = 'png', 
       width = 20*1.25, height = 15*1.25, units = "cm", dpi = 1200) 




#######################COMBINE TREATMENTS
data <- read.csv('C:\\Users\\User\\Documents\\GitHub\\thesis\\saddlemtn\\saddle.csv', header = TRUE)

data <- data %>%
  filter(!is.na(year)) %>%   #remove rows where year = NA or sp_phase = NA
  filter(!is.na(treatment)) %>%
  filter(year != 16) %>%
  distinct()

data$shrub_bio_ttl <- abs(data$shrub_bio_ttl)
data$herb_total <- data$herb_fuel_dead_ss + data$herb_fuel_live_ss
data$dwd_fuel_1000h <- data$dwd_fuel_1000h_s + data$dwd_fuel_1000h_r

data$trt <- factor(data$treatment)
levels(data$trt) <- list('Control' = c('CO', 'CP'), 
                         'Fire' = c('FI', 'FP'), 
                         'Mow' = c('MO', 'MP'), 
                         'Tebuthiuron' = c('TE', 'TP'))


std.e <- function(x) sd(x, na.rm=TRUE)/sqrt(length(na.omit(x)))

#summarise data for graphing
fuel <- data %>%
  group_by(year, trt) %>%
  summarise('n' = n(),
            'mean_herb' = round(mean(herb_total, na.rm = TRUE), 0),
            'se_herb' = round(std.e(herb_total), 0),
            'mean_dwd10' = round(mean(dwd_fuel_10h, na.rm = TRUE), 0),
            'se_dwd10' = round(std.e(dwd_fuel_10h), 0),
            'mean_dwd100' = round(mean(dwd_fuel_100h, na.rm = TRUE), 0),
            'se_dwd100' = round(std.e(dwd_fuel_100h), 0),
            'mean_dwd1000' = round(mean(dwd_fuel_1000h, na.rm = TRUE), 0),
            'se_dwd1000' = round(std.e(dwd_fuel_1000h), 0),
            'mean_shrub' = round(mean(abs(shrub_bio_ttl), na.rm = TRUE), 0),
            'se_shrub' = round(std.e(abs(shrub_bio_ttl)), 0))

fuel$year <- factor(fuel$year)
levels(fuel$year) <- list('Pre' = '7', '1' = '9', '3' = '11', '6' = '14')

#shrub fuel loading
p <- ggplot(data = fuel, aes(x = trt, y = mean_shrub, fill = year))
p <- p + geom_col(position = 'dodge') 
p <- p + geom_errorbar(aes(ymin = mean_shrub - se_shrub, ymax = mean_shrub + se_shrub), 
                       position = position_dodge(0.9), 
                       width = 0.3)
p <- p + labs(title = 'Shrub Fuel Loading',
              x = 'Treatment',
              y = 'Mean Fuel Loading ± SE (kg/ha)',
              fill = 'Years Since \nTreatment')
p <- p + theme_classic(base_size = 15)
p <- p + theme(axis.text.x = element_text(size = 14))
ps <- p + scale_fill_manual(values = c('#a8ddb5', '#7bccc4', '#43a2ca', '#0868ac'))
plot(ps)

#herb fuel loading
p <- ggplot(data = fuel, aes(x = trt, y = mean_herb, fill = year))
p <- p + geom_col(position = 'dodge') 
p <- p + geom_errorbar(aes(ymin = mean_herb - se_herb, ymax = mean_herb + se_herb), 
                       position = position_dodge(width = 0.9), 
                       width = 0.3)
p <- p + labs(title = 'Herbaceous (live + dead) Fuel Loading',
              x = 'Treatment',
              y = 'Mean Fuel Loading ± SE (kg/ha)',
              fill = 'Years Since \nTreatment')
p <- p + theme_classic(base_size = 15)
p <- p + theme(axis.text.x = element_text(size = 14))
p1 <- p + scale_fill_manual(values = c('#a8ddb5', '#7bccc4', '#43a2ca', '#0868ac'))
plot(p1)

#dwd10
p <- ggplot(data = fuel, aes(x = trt, y = mean_dwd10, fill = year))
p <- p + geom_col(position = 'dodge') 
p <- p + geom_errorbar(aes(ymin = mean_dwd10 - se_dwd10, ymax = mean_dwd10 + se_dwd10), 
                       position = position_dodge(width = 0.9), 
                       width = 0.3)
p <- p + labs(title = '10h DWD Fuel Loading',
              x = 'Treatment',
              y = 'Mean Loading ± SE (kg/ha)',
              fill = 'Years Since \nTreatment')
p <- p + theme_classic(base_size = 15)
p <- p + theme(axis.text.x = element_text(size = 14))
p2 <- p + scale_fill_manual(values = c('#a8ddb5', '#7bccc4', '#43a2ca', '#0868ac'))
plot(p2)

#dwd100
p <- ggplot(data = fuel, aes(x = trt, y = mean_dwd100, fill = year))
p <- p + geom_col(position = 'dodge') 
p <- p + geom_errorbar(aes(ymin = mean_dwd100 - se_dwd100, ymax = mean_dwd100 + se_dwd100),
                       position = position_dodge(width = 0.9), 
                       width = 0.3)
p <- p + labs(title = '100h DWD Fuel Loading',
              x = 'Treatment',
              y = 'Mean Fuel Loading ± SE (kg/ha)',
              fill = 'Years Since \nTreatment')
p <- p + theme_classic(base_size = 15)
p <- p + theme(axis.text.x = element_text(size = 14))
p3 <- p + scale_fill_manual(values = c('#a8ddb5', '#7bccc4', '#43a2ca', '#0868ac'))
plot(p3)

#dwd1000
p <- ggplot(data = fuel, aes(x = trt, y = mean_dwd1000, fill = year))
p <- p + geom_col(position = 'dodge') 
p <- p + geom_errorbar(aes(ymin = mean_dwd1000 - se_dwd1000, ymax = mean_dwd1000 + se_dwd1000), 
                       position = position_dodge(width = 0.9), 
                       width = 0.3)
p <- p + labs(title = '1000h DWD Fuel Loading',
              x = 'Treatment',
              y = 'Mean Fuel Loading ± SE (kg/ha)',
              fill = 'Years Since \nTreatment')
p <- p + theme_classic(base_size = 15)
p <- p + theme(axis.text.x = element_text(size = 14))
p4 <- p + scale_fill_manual(values = c('#a8ddb5', '#7bccc4', '#43a2ca', '#0868ac'))
plot(p4)

a <- plot_grid(p1, ps, nrow = 2)
b <- plot_grid(p2,p3,p4, nrow = 3)

ggsave(a, file = 'C:\\Users\\User\\Documents\\GitHub\\thesis\\saddlemtn\\a.png', device = 'png', 
       width = 20*1.25, height = 2*15*1.25, units = "cm", dpi = 1200) 
ggsave(b, file = 'C:\\Users\\User\\Documents\\GitHub\\thesis\\saddlemtn\\b.png', device = 'png', 
       width = 20*1.25, height = 3*15*1.25, units = "cm", dpi = 1200) 


#############summarize data in table
d <- read.csv('C:\\Users\\User\\Documents\\GitHub\\thesis\\saddlemtn\\sm_photo_des.csv', header = TRUE)

subplots <- list('SW-SD-CC005', 'SW-SD-BC130', 'SW-SD-MC121', 'SW-SD-HC179')
d <- filter(d, subplot_id %in% subplots)

yst <- c('Pre', '3','6','9')
year <- c(7,11,14,17)
relate <- data.frame(year, yst)

d <- left_join(d, relate, by = 'year')
output <- select(d, c(subplot_id, 
                      yst, 
                      shrub = can_cover_pt_shrub, 
                      perennial_grass = can_cover_pt_pgrass,
                      annual_grass = can_cover_pt_agrass,
                      bare_ground = fc_bare_gnd_fol_cvr))


