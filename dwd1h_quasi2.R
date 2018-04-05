library(ggplot2)

#read in data
mast <- read.csv("E:/backup/nifc3/dwd_1h/mastication.ed2.csv", header=T)

#remove unneccessary columns of data
mast <- mast[, -c(5, 7:16)]

#remove rows where tree cover = NA
which(is.na(mast$TC))
mast <- mast[ - c(which(is.na(mast$TC))), ]

###quasi with tree cover gradient
mlm <- lm(kgha_1h ~ TC + time + TC:time, data = mast)
mquasi <- glm(kgha_1h ~ TC + time + TC:time, family = quasi(link = "identity", variance = "mu"), data = mast, 
              start = coef(mlm))
summary(mquasi)
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