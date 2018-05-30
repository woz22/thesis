library(ggplot2)
library(dplyr)
library(lme4)
library(trtools)

d <- read.csv('C:\\Users\\User\\Documents\\GitHub\\thesis\\Bruce\\edited\\mast_all.csv', header = TRUE)

#create subplot id column
#d$subplot_id <- NA
#d$TRT <- 'GC'
#d$REG <- 'JP'
#d$subplot_id <- ifelse(d$subplot < 10, paste0('00', d$subplot), paste0('0', d$subplot))
#d$subplot_id <- paste0(d$TRT, d$pre_sub)

#head(d$subplot_id)
#     a <- unite(d, REG, site, subplot_id, sep = '-') 
#d <- merge(m_tdi, a)


#make year since treatment a factor and relabel 5 yst to 5-6 (because Onaqui sampled 6 yst)
d$yst <- factor(d$yst)
levels(d$yst) <- c('1', '5-6', '10')


#plot by yst on x-axis and fuel loading on y-axis
f1 <- ggplot(data = d, aes(x = yst, y = kgha_1h))
f1 <- f1 + geom_jitter(aes(color = site), width = 0.1)
plot(f1)


f1 <- ggplot(data = d, aes(x = yst, y = kgha_10h, color = site))
f1 <- f1 + geom_jitter(aes(color = site), width = 0.1)
plot(f1)

f1 <- ggplot(data = d, aes(x = yst, y = kgha_100_1000h, color = site))
f1 <- f1 + geom_jitter(aes(color = site), width = 0.1)
plot(f1)



#Masticated 1h fuels
m1 <- lmer(kgha_1h ~ TC + yst + TC:yst + (1|site), data = d)
summary(m1)
lincon(m1)
d <- filter(d, !is.na(sp_phase))
d$yhat1 <- predict(m1, re.form = NA)

f1 <- ggplot(data = d, aes(x = TC, y = kgha_1h, color = yst))
f1 <- f1 + geom_jitter()
f1 <- f1 + geom_line(aes(y = yhat1))
f1 <- f1 + theme_classic()
f1 <- f1 + labs(title = 'Masticated 1h Fuels',
                x = 'Pre-treatment Tree Cover (%)',
                y = 'Fuel Loading (kg/ha)')
plot(f1)

qqnorm(resid(m1)); qqline(resid(m1))



#Masticated 10h fuels
m10 <- lmer(kgha_10h ~ TC + yst + TC:yst + (1|site), data = d)
summary(m10)
lincon(m10)

d$yhat10 <- predict(m10, re.form = NA)
f1 <- ggplot(data = d, aes(x = TC, y = kgha_10h, color = yst))
f1 <- f1 + geom_jitter()
f1 <- f1 + geom_line(aes(y = yhat10))
f1 <- f1 + theme_classic()
f1 <- f1 + labs(title = 'Masticated 1h Fuels',
                x = 'Pre-treatment Tree Cover (%)',
                y = 'Fuel Loading (kg/ha)')
plot(f1)

qqnorm(resid(m10)); qqline(resid(m10))



#Masticated 100 + 1000h fuels
m100_1000 <- lmer(kgha_100_1000h ~ TC + yst + TC:yst + (1|site), data = d)
summary(m100_1000)
lincon(m100_1000)

d$yhat100_1000 <- predict(m100_1000, re.form = NA)
f1 <- ggplot(data = d, aes(x = TC, y = kgha_100_1000h, color = yst))
f1 <- f1 + geom_jitter()
f1 <- f1 + geom_line(aes(y = yhat100_1000))
f1 <- f1 + theme_classic()
f1 <- f1 + labs(title = 'Masticated 1h Fuels',
                x = 'Pre-treatment Tree Cover (%)',
                y = 'Fuel Loading (kg/ha)',
                color = 'Year Since \nTreatment')
plot(f1)

qqnorm(resid(m100_1000)); qqline(resid(m100_1000))







m <- lmer(log(kgha_1h) ~ TC + yst + TC: yst + (1|site), data = d)
summary(m)
lincon(m)




#plot by pre_tc on x-axis and fuel loading on y-axis
d <- filter(d, !is.na(sp_phase))
d$yhat1 <- predict(m1, re.form = NA)

f1 <- ggplot(data = d, aes(x = TC, y = kgha_1h, color = yst))
f1 <- f1 + geom_jitter()
f1 <- f1 + geom_line(aes(y = yhat1))
f1 <- f1 + theme_classic()
f1 <- f1 + labs(title = 'Masticated 1h Fuels',
                x = 'Pre-treatment Tree Cover (%)',
                y = 'Fuel Loading (kg/ha)')
plot(f1)

d$yhat10 <- predict(m10, re.form = NA)
f1 <- ggplot(data = d, aes(x = TC, y = kgha_10h, color = yst))
f1 <- f1 + geom_jitter()
f1 <- f1 + geom_line(aes(y = yhat10))
f1 <- f1 + theme_classic()
f1 <- f1 + labs(title = 'Masticated 1h Fuels',
                x = 'Pre-treatment Tree Cover (%)',
                y = 'Fuel Loading (kg/ha)')
plot(f1)


d$yhat100_1000 <- predict(m100_1000, re.form = NA)
f1 <- ggplot(data = d, aes(x = TC, y = kgha_100_1000h, color = yst))
f1 <- f1 + geom_jitter()
f1 <- f1 + geom_line(aes(y = yhat100_1000))
f1 <- f1 + theme_classic()
f1 <- f1 + labs(title = 'Masticated 1h Fuels',
                x = 'Pre-treatment Tree Cover (%)',
                y = 'Fuel Loading (kg/ha)',
                color = 'Year Since \nTreatment')
plot(f1)



