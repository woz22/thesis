library(ggplot2)

mast <- read.csv("E:/backup/nifc3/dwd_1h/mastication.ed2.csv", header=T)
str(mast)
View(mast)

#remove unneccessary columns of data
mast <- mast[, -c(5, 7:16)]

#remove rows where tree cover = NA
which(is.na(mast$TC))
mast <- mast[ - c(which(is.na(mast$TC))), ]



#make time a factor
mast$time <- as.factor(mast$time)
mast$sp_phase <- as.factor(mast$sp_phase)

#glm for 1 hr dwd; phase on x-axis, time as color
mquasi <- glm(kgha_1h ~ sp_phase + time + sp_phase:time, family = quasi(link = "identity", variance = "mu"), data = mast)
plot(predict(mquasi), rstudent(mquasi))

summary(mquasi)

d <- expand.grid(sp_phase = levels(mast$sp_phase), time = levels(mast$time))
d$yhat <- predict(mquasi, newdata = d)

p1 <- ggplot(d, aes(x = sp_phase, y = kgha_1h, color = time))
p1 <- p1 + geom_point(aes(y = yhat), size = 3, data = d)
p1 <- p1 + geom_line(aes(y = yhat, group = time), data = d)
p1 <- p1 + labs(title = "Decrease in 1h DWD in Masticated Woodlands", 
                x = "Pre-treatment Phase", 
                y = "1h Fuel Loading (kg/ha)", 
                color = "Years since treatment")
p1 <- p1 + theme_classic()

plot(p1)

#glm for 1 hr dwd; time on x-axis, phase as color
p1 <- ggplot(d, aes(x = time, y = kgha_1h, color = sp_phase))
p1 <- p1 + geom_point(aes(y = yhat), size = 3, data = d)
p1 <- p1 + geom_line(aes(y = yhat, group = sp_phase), data = d)
p1 <- p1 + geom_point(data = mast, aes(x = time, y = kgha_1h, color = sp_phase), alpha = 0.5)
p1 <- p1 + labs(title = "Decrease in 1h DWD in Masticated Woodlands", 
                x = "Time since treatment (years)", 
                y = "1h Fuel Loading (kg/ha)", 
                color = "Pre-treatment Woodland Phase")
p1 <- p1 + theme_classic()

plot(p1)

#glm for 1 hr dwd; time on x-axis, TC as color



#plot with time on x-axis
mast$time <- as.factor(mast$time)
mast$sp_phase <- as.factor(sp_phase)
mquasi <- glm(kgha_1h ~ time + sp_phase + time:sp_phase, family = quasi(link = "identity", variance = "mu"), data = mast)
plot(predict(mquasi), rstudent(mquasi))

summary(mquasi)
d <- expand.grid(time = levels(mast$time), sp_phase = levels(mast$sp_phase))
d$yhat <- predict(mquasi, newdata = d)
levels(mast$sp_phase)
p1 <- ggplot(d, aes(x = time, y = kgha_1h, color = sp_phase))
p1 <- p1 + geom_point(data = mast, aes(x = time, y = kgha_1h), alpha = .5)
p1 <- p1 + geom_line(aes(y = yhat, group = sp_phase), data = d)
p1 <- p1 + labs(title = "Decrease in 1h DWD in Masticated Woodlands", 
                x = "Time Since Treatment", 
                y = "1h Fuel Loading (kg/ha)", 
                color = "Pre-Treatment Woodland Phase")
p1 <- p1 + theme_classic()

plot(p1)


###quasi with tree cover gradient
mlm <- lm(kgha_1h ~ TC + time + TC:time, data = mast)
mquasi <- glm(kgha_1h ~ TC + time + TC:time, family = quasi(link = "identity", variance = "mu"), data = mast, 
              start = coef(mlm))
summary(mquasi)
plot(predict(mquasi), rstudent(mquasi))


d <- expand.grid(TC = c(0,5,10,15,20,25,30,35,40), time = levels(mast$time))
d$yhat <- predict(mquasi, newdata = d)

p1 <- ggplot(d, aes(x = TC, y = kgha_1h, color = time))
p1 <- p1 + geom_point(data = mast, aes(x = TC, y = kgha_1h))
p1 <- p1 + geom_line(aes(y = yhat, group = time), data = d)
p1 <- p1 + labs(title = "Decrease in 1h DWD in Masticated Woodlands", 
                x = "Pre-treatment Tree Cover (%)", 
                y = "1h Fuel Loading (kg/ha)", 
                color = "Time since treatment (years)")
p1 <- p1 + theme_classic()

plot(p1)

###quasi with tree cover gradient
mlm <- lm(kgha_1h ~ TC + time + TC:time, data = mast)
mquasi <- glm(kgha_1h ~ TC + time + TC:time, family = quasi(link = "identity", variance = "mu"), data = mast, 
              start = coef(mlm))
summary(mquasi)
plot(predict(mquasi), rstudent(mquasi))


d <- expand.grid(TC = c(0,5,10,15,20,25,30,35,40), time = levels(mast$time))
d$yhat <- predict(mquasi, newdata = d)

p1 <- ggplot(d, aes(x = time, y = kgha_1h, color = TC))
p1 <- p1 + geom_point(data = mast, aes(x = time, y = kgha_1h))
p1 <- p1 + geom_line(aes(y = yhat, group = TC))
p1 <- p1 + labs(title = "Decrease in 1h DWD in Masticated Woodlands", 
                x = "Pre-treatment Tree Cover (%)", 
                y = "1h Fuel Loading (kg/ha)", 
                color = "Time since treatment (years)")
p1 <- p1 + theme_classic()

plot(p1)


#10h
mast$time <- as.factor(mast$time)
mlm <- lm(kgha_10h ~ TC + time + TC:time, data = mast)
mquasi <- glm(kgha_10h ~ TC + time + TC:time, family = quasi(link = "identity", variance = "mu"), data = mast, 
              start = coef(mlm))
summary(mquasi)
plot(predict(mquasi), rstudent(mquasi))


d <- expand.grid(TC = seq(0, 50, length = 1000), time = levels(mast$time))
d$yhat <- predict(mquasi, newdata = d)

p1 <- ggplot(d, aes(x = TC, y = kgha_10h, color = time))
p1 <- p1 + geom_point(data = mast, aes(x = TC, y = kgha_1h))
p1 <- p1 + geom_line(aes(y = yhat, group = time), data = d)
p1 <- p1 + labs(title = "Decrease in 10h DWD in Masticated Woodlands", 
                x = "Pre-treatment Phase", 
                y = "1h Fuel Loading (kg/ha)", 
                color = "Years since treatment")
p1 <- p1 + theme_classic()

plot(p1)




##10h by phase
mast$time <- as.factor(mast$time)
mast$sp_phase <- as.factor(mast$sp_phase)
mlm <- lm(kgha_10h ~ sp_phase + time + sp_phase:time, data = mast)
mquasi <- glm(kgha_10h ~ sp_phase + time + sp_phase:time, family = quasi(link = "identity", variance = "mu"), data = mast, 
              start = coef(mlm))
summary(mquasi)
plot(predict(mquasi), rstudent(mquasi))


d <- expand.grid(sp_phase = levels(mast$sp_phase), time = levels(mast$time))
d$yhat <- predict(mquasi, newdata = d)

p1 <- ggplot(d, aes(x = time, y = kgha_10h, color = sp_phase))
p1 <- p1 + geom_point(data = mast, aes(x = time, y = kgha_10h), alpha = 0.5)
p1 <- p1 + geom_line(aes(y = yhat, group = sp_phase), data = d)
p1 <- p1 + labs(title = "Decrease in 10h DWD in Masticated Woodlands", 
                x = "Time since treatment (years)", 
                y = "1h Fuel Loading (kg/ha)", 
                color = "Pre-treatment Phase")
p1 <- p1 + theme_classic()

plot(p1)


#100h
mast$time <- as.factor(mast$time)
mlm <- lm(kgha_100h_1000h ~ TC + time + TC:time, data = mast)
coef(mlm)
mquasi <- glm(kgha_100h_1000h ~ TC + time + TC:time, family = quasi(link = "identity", variance = "mu"), data = mast, 
              start = c(summary(mlm)$coefficients))
summary(mquasi)
plot(predict(mquasi), rstudent(mquasi))


d <- expand.grid(TC = seq(0, 50, length = 1000), time = levels(mast$time))
d$yhat <- predict(mquasi, newdata = d)

p1 <- ggplot(d, aes(x = TC, y = kgha_100h_1000h, color = time))
p1 <- p1 + geom_point(data = mast, aes(x = TC, y = kgha_100h_1000h), alpha = 0.5)
p1 <- p1 + geom_line(aes(y = yhat, group = time), data = d)
p1 <- p1 + labs(title = "Decrease in 10h DWD in Masticated Woodlands", 
                x = "Pre-treatment Phase", 
                y = "1h Fuel Loading (kg/ha)", 
                color = "Years since treatment")
p1 <- p1 + theme_classic()

plot(p1)

