library(dplyr)
library(tidyr)
library(ggplot2)

data <- read.csv("C:\\Users\\User\\Documents\\GitHub\\thesis\\duff_regions\\duff.csv", header = TRUE) 

#data structure
str(data)

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
str(data)

#create relational table with site code and implementation year
scode <- c('ON', 'BC', 'WB', 'MC','BM', 'DR', 'SV', 'GR', 'SC', 'SR')
y_imp <- c(6, 6, 6, 6, 7, 7, 7, 7, 7, 8)
relation <- data.frame(scode, y_imp)
head(relation, n = 10)

d <- left_join(data, relation, by = "scode")

#error checks?
print(which(is.na(d$y_imp)))
sample_n(d, size = 150)

#create year since treatment column
d$yst <- d$year - d$y_imp

#create column with pre-treatment tree cover titled 'pre_tc'
d$pre_tc <- NA
for (i in unique(d$subplot_id)) {                                                            #for each unique subplot_id
  pre_tree_c <- d$tree_cover_ttl[d$subplot_id == i][which.min(d$year)]                      #take the tree cover value from the first year of data collection
  d$pre_tc[d$subplot_id == i] <- pre_tree_c                                                #put those tree cover values in a column
}

#subset columns for yst <=1 or yst = 10
a <- d[which(d$yst == 1 | d$yst == 10),]
b <- a[!(a$scode == 'SR'),]

#subset rows where yst = 1 and tree_ltr_ld = NA and tree_duff_ld = NA
na0 <- subset(b, !(yst == 1 & is.na(tree_ltr_ld) & is.na(tree_duff_ld)))
b <- b[-which(b$yst == 1 & is.na(b$tree_ltr_ld) & is.na(b$tree_duff_ld)),]

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


##############
p <- ggplot(b, aes(x = pre_tc, y = DUFF1, color = factor(yst)))
p <- p + geom_point(position = position_jitterdodge())
#p <- p + geom_errorbar(data = d1, aes(ymin = mean_duff1 - se_duff1, ymax = mean_duff1 + se_duff1), 
                       #position = position_dodge(width = 0.9), 
                       #width = 0.3)
p <- p + labs(title = "",
              x = "Pre-treatment Woodland Phase", 
              y = "Mean Fuel Loading (kg/ha) ± se", 
              fill = "Years \nsince \ntreatment")
p <- p + ylim(0,50000)
p <- p + theme_bw(base_size = 18)
p <- p + theme(axis.text.x = element_text(size = 18), legend.justification = c(.5, .5))
p <- p + facet_grid(rcode~treatment)
plot(p)

m <- glm(DUFF1 ~ rcode + scode + treatment + factor(yst) + pre_tc + factor(yst):pre_tc, 
         family = quasi, 
         data = b)

summary(m)$coefficients
confint(m)
cbind(summary(m)$coefficients, confint(m))

#NO SITE CODE, NEED TO RELEVEL SO CO IS REF LEVEL
ml <- lm(DUFF1 ~ rcode * treatment + treatment * factor(yst) * pre_tc, data = b)### NA's for PJ:BM WJ:BM --no starting values
b$treatment <- factor(b$treatment, levels = c('CO', 'FI', 'ME', 'BM'))
m <- glm(DUFF1 ~ rcode * treatment + treatment * factor(yst) * pre_tc, 
         family = quasi,
         data = b)
summary(m)$coefficients
cbind(summary(m)$coefficients, confint(m))

#plot model
d <- expand.grid(yst = c('1', '10'), 
                 pre_tc= seq(0,70, by = 0.1), 
                 rcode = c('WJ', 'JP', 'PJ'), 
                 treatment = c('CO', 'FI', 'ME', 'BM'))
d$yhat <- predict(m, newdata = d, type = 'response')

p <- ggplot(b, aes(x = pre_tc, y = DUFF1, color = factor(yst)))
p <- p + geom_line(data = d, aes(y = yhat))
p <- p + geom_point(position = position_jitterdodge(), alpha = 0.5)
p <- p + labs(title = "",
              x = "Pre-treatment Tree Cover", 
              y = "Mean Fuel Loading (kg/ha) ± se", 
              fill = "Years \nsince \ntreatment")
p <- p + ylim(0,45000)
p <- p + theme_bw(base_size = 18)
p <- p + theme(axis.text.x = element_text(size = 18), legend.justification = c(.5, .5))
p <- p + facet_grid(rcode~treatment)
plot(p)

#############
p <- ggplot(b, aes(x = pre_tc, y = DUFF1, color = factor(yst)))
p <- p + geom_point(alpha = 0.5, position = position_jitterdodge())
#p <- p + geom_errorbar(data = d1, aes(ymin = mean_duff1 - se_duff1, ymax = mean_duff1 + se_duff1), 
#position = position_dodge(width = 0.9), 
#width = 0.3)
p <- p + labs(title = "",
              x = "Pre-treatment Woodland Phase", 
              y = "Mean Fuel Loading (kg/ha) ± se", 
              fill = "Years \nsince \ntreatment")
p <- p + ylim(0,50000)
p <- p + theme_bw(base_size = 18)
p <- p + theme(axis.text.x = element_text(size = 18), legend.justification = c(.5, .5))
p <- p + facet_grid(rcode~treatment)
plot(p)


d2 <- b %>%
  group_by(treatment, sp_phase, yst, rcode) %>%
  summarise("n"=n(), 
            "mean_duff1"=round(mean(DUFF1, na.rm=TRUE), 2),
            "se_duff1"=round(std.e(DUFF1),2))
d2

#subset to JP region and plot
jp <- filter(b, rcode == 'JP')

p <- ggplot(jp, aes(x = pre_tc, y = DUFF1, color = factor(yst)))
p <- p + geom_jitter()
p <- p + labs(title = "",
              x = "Pre-treatment Tree Cover", 
              y = "Fuel Loading (kg/ha)", 
              color = "Years \nsince \ntreatment")
p <- p + ylim(0,45000)
p <- p + theme_classic(base_size = 18)
p <- p + theme(axis.text.x = element_text(size = 18), legend.justification = c(.5, .5))
p <- p + facet_grid(~treatment)
plot(p)

ggsave(p, file="C:\\Users\\User\\Documents\\GitHub\\thesis\\sage_pres\\duff.png", 
       width=10, height=6, 
       units="in", dpi=800)


#WJ-BM-MC013 year 10 duff load = Inf


pj_control <- subset(b, subset = (b$rcode == 'PJ' & b$treatment == 'CO'))
pj_control
##############look at pj_control observation 841 --> year 10 duff = 56000