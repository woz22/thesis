library(dplyr)
library(tidyr)
library(ggplot2)

#read in data
data <- read.csv("C:\\Users\\User\\Documents\\GitHub\\thesis\\duff_regions\\duff.csv", header = TRUE) 

#remove observations where year = NA
data[which(is.na(data$year)),]
data <- data[-which(is.na(data$year)),]
which(is.na(data$year))

#remove observations where sp_phase = NA
data[which(is.na(data$sp_phase)),]
data <- data[-which(is.na(data$sp_phase)),]

#remove observation where duff load = Infinity
data[which(data$tree_ltr_duff_ld > 1000000),]
data <- data[-which(data$tree_ltr_duff_ld > 1000000),]

#keep only the columns/variables of interest
data <- data[, c("subplot_id", "year", "tree_ltr_ld", "tree_duff_ld", "tree_cover_ttl", "tree_ltr_duff_ld", "scode", "rcode", "treatment", "sp_phase")]

#create relational table with site code, implementation year, 
#and column for time (years) before treatment implementation when pre-treatment monitoring data was collected
scode <- c('ON', 'BC', 'WB', 'MC','BM', 'DR', 'SV', 'GR', 'SC', 'SR')
y_imp <- c(6, 6, 6, 6, 7, 7, 7, 7, 7, 8)
pre_m_year <- c(0, 0, 0, 0, 0, -1, -1, -1, -1, -2)              #YES, BM = 0
relation <- data.frame(scode, y_imp, pre_m_year)
head(relation, n = 10)

d <- left_join(data, relation, by = "scode")

#REMOVE SOUTH RUBY
d <- d[!(a$scode == 'SR'),]

#error checks?
print(which(is.na(d$y_imp)))
sample_n(d, size = 150)

#create year since treatment column
d$yst <- d$year - d$y_imp


#subset d such that you keep only observations where yst = 10, 1 or = pre_m_year
d <- subset(d, subset = yst == 10| yst == 1| yst == pre_m_year)

#make all pre-treatment values in d$yst = 0 
a <- replace(d$yst, d$yst <= 0, 0)
d$yst <- a


#create column with pre-treatment tree cover titled 'pre_tc'
d$pre_tc <- NA
for (i in unique(d$subplot_id)) {                                                            #for each unique subplot_id
  pre_tree_c <- d$tree_cover_ttl[d$subplot_id == i][which.min(d$year)]                      #take the tree cover value from the first year of data collection
  d$pre_tc[d$subplot_id == i] <- pre_tree_c                                                #put those tree cover values in a column
}

#subset rows where yst = 1 and tree_ltr_ld = NA and tree_duff_ld = NA        ###########################BE CAREFUL--CHECK THIS!!!!!!!!!!!!!!!
b <- d[-which(d$yst == 1 & is.na(d$tree_ltr_ld) & is.na(d$tree_duff_ld)),]

#create column with tree litter + duff load for every year
b$DUFF1 <- ifelse(b$yst == 10, b$tree_ltr_duff_ld, rowSums(b[,c("tree_ltr_ld", "tree_duff_ld")], na.rm=TRUE))
b[which(is.na(b$DUFF1)),]
b <- b[-which(is.na(b$DUFF1)),]

#summary stats
std.e <- function(x) sd(x, na.rm=TRUE)/sqrt(length(na.omit(x)))
d1 <- b %>%
  group_by(treatment, sp_phase, yst, rcode) %>%
  summarise("n"=n(), 
            "mean_duff1"=round(mean(DUFF1, na.rm=TRUE), 2),
            "se_duff1"=round(std.e(DUFF1),2))
d1
d1$yst <- as.factor(d1$yst)

#plot
p <- ggplot(d1, aes(x = sp_phase, y = mean_duff1, fill = yst))
p <- p + geom_col(position = "dodge")
p <- p + geom_errorbar(data = d1, aes(ymin = mean_duff1 - se_duff1, ymax = mean_duff1 + se_duff1), 
                         position = position_dodge(width = 0.9), 
                         width = 0.3)
p <- p + labs(title = "",
                x = "Pre-treatment Woodland Phase", 
                y = "Mean Fuel Loading (kg/ha) ± se", 
                fill = "Years \nsince \ntreatment")
p <- p + ylim(0,50000)
p <- p + theme_classic(base_size = 18)
p <- p + theme(axis.text.x = element_text(size = 18), legend.justification = c(.5, .5))
p <- p + facet_grid(rcode~treatment)
plot(p)

b[which(b$DUFF1 > 20000 & b$rcode == 'WJ' & b$sp_phase == 2 & b$treatment == "ME"),]

#WJ-BM-MC013 year 10 duff load = Inf


pj_control <- subset(b, subset = (b$rcode == 'PJ' & b$treatment == 'CO'))
pj_control
##############look at pj_control observation 841 --> year 10 duff = 56000