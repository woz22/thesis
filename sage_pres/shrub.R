data <- read.csv('C:\\Users\\User\\Documents\\GitHub\\thesis\\shrub\\data_20180510142754.csv', header = TRUE)
tc <- read.csv('C:\\Users\\User\\Documents\\GitHub\\thesis\\herb\\pre_tc.csv', header = TRUE)

#tc$pre_tc <- tc$tree_cover_ttl
#pre <- select(tc, subplot_id, pre_tc)

#trees <- left_join(d, pre, by = 'subplot_id')

scode <- as.character(c('BM', 'BC', 'DR', 'WB', 'MC', 'SV', 'GR', 'ON', 'SC'))
y_imp <- as.numeric(c(7, 6, 7, 6, 6, 7, 7, 6, 7))
relation <- data.frame(scode, y_imp, stringsAsFactors = FALSE)

#combine table above with larger herb dataframe
data <- left_join(data, relation, by = 'scode')

#create year since treatment column
data$yst <- data$year - data$ y_imp

data <- filter(data, yst %in% c(0, 1, 2, 3, 6, 10))

shrub <- data %>%
  group_by(yst, treatment, sp_phase) %>%
  summarise('mean' = mean(abs(shrub_bio_ttl), na.rm = TRUE))

p <- ggplot(data = shrub, aes(x = yst, y = mean, color = factor(sp_phase)))
p <- p + geom_point()
p <- p + facet_wrap(~treatment, nrow = 1)
p <- p + scale_x_continuous(breaks = seq(0,10, by = 2))
plot(p)

unique(data$yst)

#standard error function for summary stats
std.e <- function(x) sd(x, na.rm=TRUE)/sqrt(length(na.omit(x)))

sum_shrub <- data %>%
  group_by(yst, treatment) %>%
  summarise('mean' = mean(abs(shrub_bio_ttl), na.rm = TRUE),
            'se' = std.e(abs(shrub_bio_ttl)))

p <- ggplot(data = sum_shrub, aes(x = yst, y = mean))
p <- p + geom_bar(stat = 'identity', position = 'dodge')
p <- p + geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.3, position = position_dodge())
p <- p + facet_wrap(~treatment)
plot(p)