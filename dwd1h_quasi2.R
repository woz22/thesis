library(ggplot2)
library(lmtest)

#read in data
mast <- read.csv("E:/backup/nifc3/dwd_1h/mastication.ed2.csv", header=T)

#remove unneccessary columns of data
mast <- mast[, -c(5, 7:16)]

#remove rows where tree cover = NA
which(is.na(mast$TC))
mast <- mast[ - c(which(is.na(mast$TC))), ]

#subset time periods for plotting residuals
yst1 <- subset(mast, mast$time == 1)
yst5 <- subset(mast, mast$time == 5)
yst6 <- subset(mast, mast$time == 10)

#how to plot predict vs res with subsets of time?

###quasi with tree cover gradient
mlm <- lm(kgha_1h ~ TC + time + TC:time, data = mast)
mquasi <- glm(kgha_1h ~ TC + time + TC:time, family = quasi(link = identity, variance = "mu"), data = mast, 
              start = coef(mlm))
cbind(summary(mquasi)$coefficients, coefci(mquasi))

plot(predict(mquasi), rstudent(mquasi))


d <- expand.grid(time = c(1,5,10), TC = c(10, 20, 30))
d$yhat <- predict(mquasi, newdata = d)

p1 <- ggplot(d, aes(x = time, y = kgha_1h, color = TC))
p1 <- p1 + geom_point(data = mast, aes(x = time, y = kgha_1h))
p1 <- p1 + geom_line(aes(y = yhat, x = time, group = TC), data = d)
p1 <- p1 + labs(title = "Decrease in 1h DWD in Masticated Woodlands", 
                x = "Time since treatment (years)", 
                y = "1h Fuel Loading (kg/ha)", 
                color = "Pre-Treatment Tree Cover (%)")
p1 <- p1 + theme_classic()

plot(p1)

#plot same model with tree cover on x-axis
d <- expand.grid(time = c(1,5,10), TC = seq(0,40, length = 100))
d$yhat <- predict(mquasi, newdata = d)

p1 <- ggplot(d, aes(x = TC, y = kgha_1h, color = time))
p1 <- p1 + geom_point(data = mast, aes(x = TC, y = kgha_1h, color = time))
p1 <- p1 + geom_line(aes(y = yhat, x = TC, group = time), data = d)
p1 <- p1 + labs(title = "Decrease in 1h DWD in Masticated Woodlands", 
                x = "Pre-Treatment Tree Cover (%)", 
                y = "1h Fuel Loading (kg/ha)", 
                color = "Time since treatment (years)")
p1 <- p1 + theme_classic()

plot(p1)


###quasi with log2(time)

mquasi <- glm(kgha_1h ~ TC + log2(time) + TC:log2(time), family = quasi(link = identity, variance = "mu"), data = mast, start = coef(mlm))
summary(mquasi)$coefficients
cbind(summary(mquasi)$coefficients, coefci(mquasi))
plot(predict(mquasi), rstudent(mquasi))

zero <- subset(mast, mast$TC == 0)
zero
d <- expand.grid(time = seq(1,10, length = 1000), TC = c(10, 20, 30, 40))
d$yhat <- predict(mquasi, newdata = d, type = "response")

p1 <- ggplot(d, aes(x = time, y = kgha_1h, color = TC))
p1 <- p1 + geom_point(data = mast, aes(x = time, y = kgha_1h))
p1 <- p1 + geom_line(aes(y = yhat, x = time, group = TC), data = d)
p1 <- p1 + labs(title = "Decrease in 1h DWD in Masticated Woodlands", 
                x = "Time since treatment (years)", 
                y = "1h Fuel Loading (kg/ha)", 
                color = "Pre-Treatment Tree Cover (%)")
p1 <- p1 + theme_classic()

plot(p1)


###gamma log link with log2(time)
mlm <- lm(kgha_1h ~ TC + time + TC:time, data = mast)
mgamma <- glm(kgha_1h ~ TC + log2(time) + TC:log2(time), family = Gamma(link = log), data = mast)
summary(mgamma)$coefficients
plot(predict(mgamma), rstudent(mgamma))


d <- expand.grid(time = seq(1,10, length = 1000), TC = c(10, 20, 30))
d$yhat <- predict(mgamma, newdata = d, type = "response")

p1 <- ggplot(d, aes(x = time, y = kgha_1h, color = TC))
p1 <- p1 + geom_point(data = mast, aes(x = time, y = kgha_1h))
p1 <- p1 + geom_line(aes(y = yhat, x = time, group = TC), data = d)
p1 <- p1 + labs(title = "Decrease in 1h DWD in Masticated Woodlands", 
                x = "Time since treatment (years)", 
                y = "1h Fuel Loading (kg/ha)", 
                color = "Pre-Treatment Tree Cover (%)")
p1 <- p1 + theme_classic()

plot(p1)


##gamma log link

mgamma <- glm(kgha_1h ~ TC + time + TC:time, family = Gamma(link = log), data = mast)
summary(mgamma)$coefficients
plot(predict(mgamma), rstudent(mgamma))


d <- expand.grid(time = seq(1,10, length = 1000), TC = c(10, 20, 30))
d$yhat <- predict(mgamma, newdata = d, type = "response")

p1 <- ggplot(d, aes(x = time, y = kgha_1h, color = TC))
p1 <- p1 + geom_point(data = mast, aes(x = time, y = kgha_1h))
p1 <- p1 + geom_line(aes(y = yhat, x = time, group = TC), data = d)
p1 <- p1 + labs(title = "Decrease in 1h DWD in Masticated Woodlands", 
                x = "Time since treatment (years)", 
                y = "1h Fuel Loading (kg/ha)", 
                color = "Pre-Treatment Tree Cover (%)")
p1 <- p1 + theme_classic()

plot(p1)

#gamma inverse link with log transformation--doesn't look right
mgamma <- glm(kgha_1h ~ TC + log2(time) + TC:log2(time), family = Gamma(link = inverse), data = mast)
summary(mgamma)
plot(predict(mgamma), rstudent(mgamma))


d <- expand.grid(time = seq(1,10, length = 1000), TC = c(10, 20, 30))
d$yhat <- predict(mgamma, newdata = d, type = "response")

p1 <- ggplot(d, aes(x = time, y = kgha_1h, color = TC))
p1 <- p1 + geom_point(data = mast, aes(x = time, y = kgha_1h))
p1 <- p1 + geom_line(aes(y = yhat, x = time, group = TC), data = d)
p1 <- p1 + labs(title = "Decrease in 1h DWD in Masticated Woodlands", 
                x = "Time since treatment (years)", 
                y = "1h Fuel Loading (kg/ha)", 
                color = "Pre-Treatment Tree Cover (%)")
p1 <- p1 + theme_classic()

plot(p1)



