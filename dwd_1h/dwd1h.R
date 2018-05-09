#mast=read.csv("C:/Users/User/Desktop/nifc3/dwd_1h/mastication.ed2.csv", header=T)

library(dplyr)
library(ggplot2)
library(trtools)
library(cowplot)
library(reshape2)
library(dunn.test)
library(FSA)
library(car)

mast <- read.csv("G:/backup/nifc3/dwd_1h/mastication.ed2.csv", header = TRUE)

options(digits = 4)

mast <- mast[ - c(106, 107, 108, 138), ] #observations time = 10, sp_phase = NA and tree cover = NA

mast$time <- factor(mast$time)
mast$sp_phase <- factor(mast$sp_phase)

#creating data frame with means and se's
std.e <- function(x) sd(x, na.rm=TRUE)/sqrt(length(na.omit(x)))

mast1 <- subset(mast, subset = mast$sp_phase == "1" |mast$sp_phase == "2" | mast$sp_phase == "3" )

mast_sum <- mast1 %>%
  group_by(time, sp_phase) %>%
  summarise("n"=n(), 
            "mean_1h"=round(mean(kgha_1h, na.rm=TRUE), 2), 
            "se_1h"=round(std.e(kgha_1h),2))
mast_sum


#significance over time within each phase

m_s <- as.data.frame(mast_sum)
m_s$time <- factor(m_s$time)
m_s$sig <- c('a', 'a', 'a', 'ab', 'ab', 'b', 'b', 'b', 'b')

#want graph like this:

m_s <- as.data.frame(mast_sum)
m_s$time <- factor(m_s$time)
m_s$sig <- c('a', 'a', 'a', 'ab', 'ab', 'b', 'b', 'b', 'b')


p2 <- ggplot(m_s, aes(x = sp_phase, y = mean_1h, 
                      fill = time, na.rm=TRUE))
p2 <- p2 + geom_col(position = "dodge", na.rm=TRUE)
p2 <- p2 + geom_errorbar(aes(ymin = mean_1h - se_1h, ymax = mean_1h + se_1h), 
                         position = position_dodge(width = 0.9), 
                         width = 0.3)
p2 <- p2 + labs(title = "Change in 1h DWD Fuel Loading in Masticated Woodlands", 
                x = "Woodland Phase", y = "1h Fuel Loading (kg/ha) mean ± se", 
                fill = "Years since \ntreatment", na.rm=TRUE)
p2 <- p2 + scale_fill_manual(values = c("#bae4bc", "#7bccc4", "#43a2ca"))
p2 <- p2 + scale_y_continuous(breaks = c(0, 2500, 5000, 7500, 10000, 12500)) 
p2 <- p2 + theme_classic(base_size = 15)
p2 <- p2 + theme(axis.text.x = element_text(size = 14))
p2 <- p2 + geom_text(aes(label = sig, y = mean_1h + se_1h + 300), 
                     position = position_dodge(width = 0.9),
                     size = 4)

plot(p2)

?labs()
ggsave(p2, file="F:/nifc2/fig1_dwd1h.png", width=20*1.25, height=15*1.25, units="cm", dpi=1200) 

#in order to test significance between years within each phase, need linear model
#linear model with phase as categorical variable
m1 <- lm(kgha_1h~sp_phase + time + sp_phase:time, data=mast)

mquasi <- glm(kgha_1h~sp_phase + time + sp_phase:time, family = quasi(link = "identity", variance = "mu"), data = mast)
plot(predict(mquasi), rstudent(mquasi))

m1
summary(m1)

d1 <- expand.grid(sp_phase=levels(mast$sp_phase), time = levels(mast$time))
d1$yhat <- predict(m1, newdata = d1)

p1 <- ggplot(mast, aes(x = sp_phase, y = kgha_1h, color = time))
p1 <- p1 + geom_point(na.rm=TRUE)
p1 <- p1 + geom_point(aes(y = yhat), size = 3, data = d1)
p1 <- p1 + geom_line(aes(y = yhat, group = time), data = d1)
p1 <- p1 + labs(title = "Decrease in 1h DWD in Masticated Woodlands", 
                x = "Pre-treatment Phase", 
                y = "1h Fuel Loading (kg/ha)", 
                color = "Years since treatment")
p1 <- p1 + theme_classic()

plot(p1)

#looking at ANOVA assumtions 
#increasing variance
#nonnormal
res = rstudent(m1)
pred = fitted(m1)
hist(res) #hist of residuals centered at 0
plot(pred,res,main="Predicted vs. Residuals") #increasing variance
dwt(m1)
qqnorm(res); qqline(res)

hist(mast$kgha_1h, breaks = 20)

m_ph1 <- subset(mast, subset = mast$sp_phase == '1')
m_ph2 <- subset(mast, subset = mast$sp_phase == '2')
m_ph3 <- subset(mast, subset = mast$sp_phase == '3')

hist(m_ph1$kgha_1h)
hist(m_ph2$kgha_1h)
hist(m_ph3$kgha_1h)

kruskal.test(kgha_1h ~ time, data = m_ph1)
dunnTest(kgha_1h ~ time, data = m_ph1, method = "bonferroni")

kruskal.test(kgha_1h ~ time, data = m_ph2)
dunnTest(kgha_1h ~ time, data = m_ph2, method = "bonferroni")

kruskal.test(kgha_1h ~ time, data = m_ph3)
dunnTest(kgha_1h ~ time, data = m_ph3, method = "bonferroni")


###########no pvalue adjustment
kruskal.test(kgha_1h ~ time, data = m_ph1)
dunnTest(kgha_1h ~ time, data = m_ph1, method = "bonferroni")

kruskal.test(kgha_1h ~ time, data = m_ph2)
dunnTest(kgha_1h ~ time, data = m_ph2, method = "bonferroni")

kruskal.test(kgha_1h ~ time, data = m_ph3)
dunnTest(kgha_1h ~ time, data = m_ph3, method = "none")
?dunnTest()

hist(m_ph3$kgha_1h)
mt10_ph3 <- m_ph3[m_ph3$time == '10',]
hist(mt10_ph3$kgha_1h)
