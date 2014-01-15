# Libraries ====

library(ggplot2)
library(gridExtra) # For panelling ggplots

# Prepare Data ====

overcrowd.frame <- Data_personsperroom_englandwales_LADs
tenure.frame    <- Data_tenure_lad
unemp.frame     <- Data_unemployed_economicallyactive_engwal_LADs
car.frame       <- Data_CARVAN_UNIT

ggplot(overcrowd.frame, aes(overcrowd.frame$pcGt1PPerRoom)) + geom_density() + ggtitle("Density plot of overcrowding") + xlab("Percent of households overcrowded") + stat_function(fun = dnorm, args = list(mean = mean(overcrowd.frame$pcGt1PPerRoom, na.rm = T), sd = sd(overcrowd.frame$pcGt1PPerRoom, na.rm = T)))
# Needs transforming. See later section.

ggplot(tenure.frame, aes(tenure.frame$pcNotOO)) + geom_density() + ggtitle("Density plot of tenure") + xlab("Percent of households not owner occupied") + stat_function(fun = dnorm, args = list(mean = mean(tenure.frame$pcNotOO, na.rm = T), sd = sd(tenure.frame$pcNotOO, na.rm = T)))
# Might need transforming. See later section.

ggplot(unemp.frame, aes(unemp.frame$pcEconActUnem)) + geom_density() + ggtitle("Density plot of unemployment") + xlab("Percent of individuals economically active who are unemployed") + stat_function(fun = dnorm, args = list(mean = mean(unemp.frame$pcEconActUnem, na.rm = T), sd = sd(unemp.frame$pcEconActUnem, na.rm = T)))
# Probably ok. Townsend et al. transformed this in 1988, but probably had a different distribution!

ggplot(car.frame, aes(car.frame$pcNoCar)) + geom_density() + ggtitle("Density plot of car ownership") + xlab("Percent of households who do not own a car") + stat_function(fun = dnorm, args = list(mean = mean(car.frame$pcNoCar, na.rm = T), sd = sd(car.frame$pcNoCar, na.rm = T)))
# Probably need transforming.

# Transformations ====
# Townsend et al used y = log(x + 1). Try that first.

# Overcrowding
logOvercrowding <- log(overcrowd.frame$pcGt1PPerRoom + 1)
ggplot(overcrowd.frame, aes(logOvercrowding)) + geom_density() + stat_function(fun = dnorm, args = list(mean = mean(logOvercrowding, na.rm = T), sd = sd(logOvercrowding, na.rm = T)))

sqrtOvercrowding <- sqrt(overcrowd.frame$pcGt1PPerRoom + 1)
ggplot(overcrowd.frame, aes(sqrtOvercrowding)) + geom_density() + stat_function(fun = dnorm, args = list(mean = mean(sqrtOvercrowding, na.rm = T), sd = sd(sqrtOvercrowding, na.rm = T)))

# Natural log transformation of overcrowding is better, go with that.

# Tenure
logTenure <- log(tenure.frame$pcNotOO)
ggplot(tenure.frame, aes(logTenure)) + geom_density() + stat_function(fun = dnorm, args = list(mean = mean(logTenure, na.rm = T), sd = sd(logTenure, na.rm = T)))
# Not a bad little distribution.

# Unemployment
# Doesn't need transforming.

# Car Ownership

logCarOwn <- log(car.frame$pcNoCar + 1)
ggplot(car.frame, aes(logCarOwn)) + geom_density() + stat_function(fun = dnorm, args = list(mean = mean(logCarOwn, na.rm = T), sd = sd(logCarOwn, na.rm = T)))
# Looking pretty normal.

# QQ Plots ====

# QQ plots to check the normality of the distributions. Linear = good

require(gridExtra)
plot1 <- ggplot(overcrowd.frame, aes(overcrowd.frame$pcGt1PPerRoom)) + 
  geom_density() + ggtitle("Density plot of overcrowding (x)") + 
  xlab("Percent of households overcrowded") + 
  stat_function(fun = dnorm, 
                args = list(mean = mean(overcrowd.frame$pcGt1PPerRoom, na.rm = T), 
                            sd = sd(overcrowd.frame$pcGt1PPerRoom, na.rm = T)))
plot2 <- ggplot(overcrowd.frame, aes(sample = overcrowd.frame$pcGt1PPerRoom)) + 
  stat_qq() + 
  ylim(c(0, 15)) + 
  ggtitle("Overcrowding: original")
plot3 <- ggplot(overcrowd.frame, aes(logOvercrowding)) +
  geom_density() +
  ggtitle("Density plot of y = ln(x + 1)") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(logOvercrowding, na.rm = T), 
                            sd = sd(logOvercrowding, na.rm = T)))  
plot4 <- ggplot(overcrowd.frame, aes(sample = logOvercrowding)) + 
  stat_qq() + 
  ylim(c(0, 15)) + 
  ggtitle("Overcrowding: y = ln(x + 1)")
plot5 <- ggplot(overcrowd.frame, aes(sqrtOvercrowding)) +
  geom_density() +
  ggtitle("Density plot of y = sqrt(x + 1)") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(sqrtOvercrowding, na.rm = T), 
                            sd = sd(sqrtOvercrowding, na.rm = T)))  
plot6 <- ggplot(overcrowd.frame, aes(sample = sqrtOvercrowding)) + 
  stat_qq() + 
  ylim(c(0, 15)) + 
  ggtitle("Overcrowding: y = sqrt(x + 1)")
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6,
             ncol = 2,
             main = "Overcrowding")
rm(plot1, plot2, plot3, plot4, plot5, plot6)
# Definitely natural log transformation

require(gridExtra)
plot1 <- ggplot(tenure.frame, aes(tenure.frame$pcNotOO)) +
  geom_density() + 
  stat_function(fun = dnorm, args = list(mean = mean(tenure.frame$pcNotOO, na.rm = T), 
                                           sd = sd(tenure.frame$pcNotOO, na.rm = T))) +
  ggtitle("Density plot of tenure")
plot2 <- ggplot(tenure.frame, aes(sample = tenure.frame$pcNotOO)) +
  stat_qq() +
  ggtitle("Tenure: original")
plot3 <- ggplot(tenure.frame, aes(logTenure)) + 
  geom_density() +
  stat_function(fun = dnorm, args = list(mean = mean(logTenure, na.rm = T),
                                         sd = sd(logTenure, na.rm = T))) +
  ggtitle("Density plot of density transformed by y = ln(x + 1)")
plot4 <- ggplot(tenure.frame, aes(sample = logTenure)) +
  stat_qq() +
  ggtitle("Tenure: y = ln(x)")

grid.arrange(plot1, plot2, plot3, plot4,
             ncol = 2,
             main = "Tenure")
rm(plot1, plot2, plot3, plot4)

require(gridExtra)
plot1 <- ggplot(unemp.frame, aes(unemp.frame$pcEconActUnem)) +
  geom_density() + 
  stat_function(fun = dnorm, args = list(mean = mean(unemp.frame$pcEconActUnem, na.rm = T), sd = sd(unemp.frame$pcEconActUnem, na.rm = T))) +
  ggtitle("Density plot of Unemployment")
plot2 <- ggplot(unemp.frame, aes(sample = unemp.frame$pcEconActUnem)) +
  stat_qq() +
  ggtitle("Unemployment: original")
grid.arrange(plot1, plot2,
             ncol = 2,
             main = "Unemployment")
rm(plot1, plot2)

require(gridExtra)
plot1 <- ggplot(car.frame, aes(car.frame$pcNoCar)) +
  geom_density() +
  stat_function(fun = dnorm, args = list(mean = mean(car.frame$pcNoCar, na.rm = T), 
                                         sd = sd(car.frame$pcNoCar, na.rm = T))) +
  ggtitle("Car Ownership Density")
plot2 <- ggplot(car.frame, aes(sample = car.frame$pcNoCar)) +
  stat_qq() +
  ggtitle("Car Ownership (x): original")
plot3 <- ggplot(car.frame, aes(logCarOwn)) +
  geom_density() +
  stat_function(fun = dnorm, args = list(mean = mean(logCarOwn, na.rm = T),
                                         sd = sd(logCarOwn, na.rm = T))) +
  ggtitle("y = ln(x + 1)")
plot4 <- ggplot(car.frame, aes(sample = logCarOwn)) +
  stat_qq() +
  ggtitle("Car Ownership: y = ln(x + 1) qq plot")
grid.arrange(plot1, plot2, plot3, plot4,
             ncol = 2,
             main = "Car Ownership")
rm(plot1, plot2, plot3, plot4)
# Use the log transformation

# Z-scores ====

zOvercrowd <- scale(logOvercrowding, center = T, scale = T)
zTenure    <- scale(logTenure, center = T, scale = T)
zUnemp     <- scale(unemp.frame$pcEconActUnem, center = T, scale = T)
zCar       <- scale(logCarOwn, center = T, scale = T)

merged <- merge(overcrowd.frame, tenure.frame, by.x = "GEOCODE", by.y = "GEOCODE")
merged <- merge(merged, unemp.frame, by.x = "GEOCODE", by.y = "GEOCODE")
merged <- merge(merged, car.frame, by.x = "GEOCODE", by.y = "GEOCODE")
write.csv(merged, file="master.csv")
