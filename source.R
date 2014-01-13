# Libraries ====

library(ggplot2)
library(gridExtra)

# Prepare Data ====

overcrowd.frame <- Data_personsperroom_englandwales_LADs
tenure.frame    <- Data_tenure_lad
unemp.frame     <- Data_unemployed_economicallyactive_engwal_LADs
car.frame       <- Data_CARVAN_UNIT

ggplot(overcrowd.frame, aes(overcrowd.frame$pcGt1PPerRoom)) + geom_density() + ggtitle("Density plot of overcrowding") + xlab("Percent of households overcrowded") + stat_function(fun = dnorm, args = list(mean = mean(overcrowd.frame$pcGt1PPerRoom, na.rm = T), sd = sd(overcrowd.frame$pcGt1PPerRoom, na.rm = T)))

ggplot(tenure.frame, aes(tenure.frame$pcNotOO)) + geom_density() + ggtitle("Density plot of tenure") + xlab("Percent of households not owner occupied") + stat_function(fun = dnorm, args = list(mean = mean(tenure.frame$pcNotOO, na.rm = T), sd = sd(tenure.frame$pcNotOO, na.rm = T)))


ggplot(unemp.frame, aes(unemp.frame$pcEconActUnem)) + geom_density() + ggtitle("Density plot of unemployment") + xlab("Percent of individuals economically active who are unemployed") + stat_function(fun = dnorm, args = list(mean = mean(unemp.frame$pcEconActUnem, na.rm = T), sd = sd(unemp.frame$pcEconActUnem, na.rm = T)))

ggplot(car.frame, aes(car.frame$pcNoCar)) + geom_density() + ggtitle("Density plot of car ownership") + xlab("Percent of households who do not own a car") + stat_function(fun = dnorm, args = list(mean = mean(car.frame$pcNoCar, na.rm = T), sd = sd(car.frame$pcNoCar, na.rm = T)))

# Transformations ====
# Townsend et al used y = log(x + 1)

# Overcrowding
logOvercrowding <- log(overcrowd.frame$pcGt1PPerRoom + 1)
ggplot(overcrowd.frame, aes(logOvercrowding)) + geom_density() + stat_function(fun = dnorm, args = list(mean = mean(logOvercrowding, na.rm = T), sd = sd(logOvercrowding, na.rm = T)))

sqrtOvercrowding <- sqrt(overcrowd.frame$pcGt1PPerRoom + 1)
ggplot(overcrowd.frame, aes(sqrtOvercrowding)) + geom_density() + stat_function(fun = dnorm, args = list(mean = mean(sqrtOvercrowding, na.rm = T), sd = sd(sqrtOvercrowding, na.rm = T)))

# Natural log transformation of overcrowding is better, go with that.

# Car Ownership

logCarOwn <- log(car.frame$pcNoCar + 1)
ggplot(car.frame, aes(logCarOwn)) + geom_density() + stat_function(fun = dnorm, args = list(mean = mean(logCarOwn, na.rm = T), sd = sd(logCarOwn, na.rm = T)))

# Natural log transformation of car ownership looks good.

# QQ Plots ====

# QQ plots to check the normality of the distributions. Linear = good

require(gridExtra)
plot1 <- ggplot(overcrowd.frame, aes(overcrowd.frame$pcGt1PPerRoom)) + geom_density() + ggtitle("Density plot of overcrowding") + xlab("Percent of households overcrowded") + stat_function(fun = dnorm, args = list(mean = mean(overcrowd.frame$pcGt1PPerRoom, na.rm = T), sd = sd(overcrowd.frame$pcGt1PPerRoom, na.rm = T)))
plot2 <- ggplot(overcrowd.frame, aes(sample = overcrowd.frame$pcGt1PPerRoom)) + stat_qq()
plot3 <- ggplot(overcrowd.frame, aes(sample = logOvercrowding)) + stat_qq()
plot4 <- ggplot(overcrowd.frame, aes(sample = sqrtOvercrowding)) + stat_qq()
grid.arrange(plot1, plot2, plot3, plot4,
             ncol = 2)
rm(plot1, plot2, plot3, plot4)


ggplot(overcrowd.frame, aes(sample = logOvercrowding)) + stat_qq()
ggplot(overcrowd.frame, aes(sample = sqrtOvercrowding)) + stat_qq()

ggplot(tenure.frame, aes(sample = tenure.frame$pcNotOO)) + stat_qq()

ggplot(unemp.frame, aes(sample = unemp.frame$pcEconActUnem)) + stat_qq()

ggplot(car.frame, aes(sample = car.frame$pcNoCar)) + stat_qq()
