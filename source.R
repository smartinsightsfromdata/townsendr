# Libraries ====

library(ggplot2)
library(gridExtra) # For panelling ggplots

# Prepare Data ====

car       <- read.csv("Data_CARVAN_UNIT.csv")
overcrowd <- read.csv("Data_personsperroom_englandwales_LADs.csv")
tenure    <- read.csv("Data_tenure_lad.csv")
unemp     <- read.csv("Data_unemployed_economicallyactive_engwal_LADs.csv")

# Check plots are (para)normal ====

# Make log and sqrt versions of each variable
# Z-scores don't require normal dist of variable, but Townsend et al. did it, so...

logOvercrowding  <- log(overcrowd$pcGt1PPerRoom + 1)
sqrtOvercrowding <- sqrt(overcrowd$pcGt1PPerRoom + 1)

logTenure        <- log(tenure$pcNotOO)       # tenure doesn't need +1 because min > 1
sqrtTenure       <- sqrt(tenure$pcNotOO)

logUnemp         <- log(unemp$pcEconActUnem)  # unemp doesn't need +1 because min > 1
sqrtUnemp        <- sqrt(unemp$pcEconActUnem)

logCar           <- log(car$pcNoCar)          # car doesn't need +1 because min > 1
sqrtCar          <- sqrt(car$pcNoCar)

# Plot each varaible as density and qq plot in the following order: untransformed, log, sqrt
# QQ plots to check the normality of the distributions. Linear = good

# overcrowd

require(gridExtra)
plot1 <- ggplot(overcrowd, aes(overcrowd$pcGt1PPerRoom)) + 
  geom_density() + 
  ggtitle("Density: original (x)") + 
  xlab("Percent of households overcrowded") + 
  stat_function(fun = dnorm, 
                args = list(mean = mean(overcrowd$pcGt1PPerRoom, na.rm = T), 
                            sd = sd(overcrowd$pcGt1PPerRoom, na.rm = T)))
plot2 <- ggplot(overcrowd, aes(sample = overcrowd$pcGt1PPerRoom)) + 
  stat_qq() + 
  ylim(c(0, 15)) + 
  ggtitle("QQ: original")
plot3 <- ggplot(overcrowd, aes(logOvercrowding)) +
  geom_density() +
  ggtitle("Density: y = ln(x + 1)") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(logOvercrowding, na.rm = T), 
                            sd = sd(logOvercrowding, na.rm = T)))  
plot4 <- ggplot(overcrowd, aes(sample = logOvercrowding)) + 
  stat_qq() + 
  ylim(c(0, 15)) + 
  ggtitle("QQ: y = ln(x + 1)")
plot5 <- ggplot(overcrowd, aes(sqrtOvercrowding)) +
  geom_density() +
  ggtitle("Density: y = sqrt(x + 1)") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(sqrtOvercrowding, na.rm = T), 
                            sd = sd(sqrtOvercrowding, na.rm = T)))  
plot6 <- ggplot(overcrowd, aes(sample = sqrtOvercrowding)) + 
  stat_qq() + 
  ylim(c(0, 15)) + 
  ggtitle("QQ: y = sqrt(x + 1)")
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6,
             ncol = 2,
             main = "Overcrowding")
dev.copy2pdf(file = "overcrowd-normal.pdf", width = 8.27, height = 11.69)
dev.off()
rm(plot1, plot2, plot3, plot4, plot5, plot6)

# tenure
require(gridExtra)
plot1 <- ggplot(tenure, aes(tenure$pcNotOO)) +
  geom_density() + 
  stat_function(fun = dnorm, args = list(mean = mean(tenure$pcNotOO, na.rm = T), 
                                           sd = sd(tenure$pcNotOO, na.rm = T))) +
  ggtitle("Density: original (x)") +
  xlab("Percentage of households not owner occupied")
plot2 <- ggplot(tenure, aes(sample = tenure$pcNotOO)) +
  stat_qq() +
  ggtitle("QQ:original (x)")
plot3 <- ggplot(tenure, aes(logTenure)) + 
  geom_density() +
  stat_function(fun = dnorm, args = list(mean = mean(logTenure, na.rm = T),
                                         sd = sd(logTenure, na.rm = T))) +
  ggtitle("Density: y = ln(x)")
plot4 <- ggplot(tenure, aes(sample = logTenure)) +
  stat_qq() +
  ggtitle("QQ: y = ln(x)")
plot5 <- ggplot(tenure, aes(sqrtTenure)) + 
  geom_density() +
  stat_function(fun = dnorm, args = list(mean = mean(sqrtTenure, na.rm = T),
                                         sd = sd(sqrtTenure, na.rm = T))) +
  ggtitle("Density: y = ln(x)")
plot6 <- ggplot(tenure, aes(sample = logTenure)) +
  stat_qq() +
  ggtitle("QQ: y = ln(x)")
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6,
             ncol = 2,
             main = "Tenure")
dev.copy2pdf(file = "tenure-normal.pdf", width = 8.27, height = 11.69)
dev.off()
rm(plot1, plot2, plot3, plot4, plot5, plot6)

require(gridExtra)
plot1 <- ggplot(unemp, aes(unemp$pcEconActUnem)) +
  geom_density() + 
  stat_function(fun = dnorm, args = list(mean = mean(unemp$pcEconActUnem, na.rm = T), sd = sd(unemp$pcEconActUnem, na.rm = T))) +
  ggtitle("Density: original") +
  xlab("Percent of individuals unemployed who are eligible")
plot2 <- ggplot(unemp, aes(sample = unemp$pcEconActUnem)) +
  stat_qq() +
  ggtitle("QQ: original")
plot3 <- ggplot(unemp, aes(logUnemp)) +
  geom_density() + 
  stat_function(fun = dnorm, args = list(mean = mean(logUnemp, na.rm = T), 
                                         sd = sd(logUnemp, na.rm = T))) +
  ggtitle("Density: y = ln(x)")
plot4 <- ggplot(unemp, aes(sample = logUnemp)) +
  stat_qq() +
  ggtitle("QQ: y = ln(x)")
plot5 <- ggplot(unemp, aes(sqrtUnemp)) +
  geom_density() + 
  stat_function(fun = dnorm, args = list(mean = mean(sqrtUnemp, na.rm = T), 
                                         sd = sd(sqrtUnemp, na.rm = T))) +
  ggtitle("Density: y = sqrt(x)")
plot6 <- ggplot(unemp, aes(sample = sqrtUnemp)) +
  stat_qq() +
  ggtitle("QQ: y = sqrt(x)")
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6,
             ncol = 2,
             main = "Unemployment")
dev.copy2pdf(file = "unemp-normal.pdf", width = 8.27, height = 11.69)
dev.off()
rm(plot1, plot2, plot3, plot4, plot5, plot6)

require(gridExtra)
plot1 <- ggplot(car, aes(car$pcNoCar)) +
  geom_density() + 
  stat_function(fun = dnorm, args = list(mean = mean(car$pcNoCar, na.rm = T), 
                                         sd = sd(car$pcNoCar, na.rm = T))) +
  ggtitle("Density: original") +
  xlab("Percent of households without a car")
plot2 <- ggplot(car, aes(sample = car$pcNoCar)) +
  stat_qq() +
  ggtitle("QQ: original")
plot3 <- ggplot(car, aes(logCar)) +
  geom_density() + 
  stat_function(fun = dnorm, args = list(mean = mean(logCar, na.rm = T), 
                                         sd = sd(logCar, na.rm = T))) +
  ggtitle("Density: y = ln(x)")
plot4 <- ggplot(car, aes(sample = logCar)) +
  stat_qq() +
  ggtitle("QQ: y = ln(x)")
plot5 <- ggplot(car, aes(sqrtCar)) +
  geom_density() + 
  stat_function(fun = dnorm, args = list(mean = mean(sqrtCar, na.rm = T), 
                                         sd = sd(sqrtCar, na.rm = T))) +
  ggtitle("Density: y = sqrt(x)")
plot6 <- ggplot(car, aes(sample = sqrtCar)) +
  stat_qq() +
  ggtitle("QQ: y = sqrt(x)")
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6,
             ncol = 2,
             main = "Car Ownership")
dev.copy2pdf(file = "car-normal.pdf", width = 8.27, height = 11.69)
dev.off()
rm(plot1, plot2, plot3, plot4, plot5, plot6)

# Z-scores ====

car$zCar             <- scale(logCar,          center = T, scale = T)
overcrowd$zOvercrowd <- scale(logOvercrowding, center = T, scale = T)
tenure$zTenure       <- scale(logTenure,       center = T, scale = T)
unemp$zUnemp         <- scale(logUnemp,        center = T, scale = T)

master  <- merge.data.frame(car, overcrowd, by.x = "GEOCODE", by.y = "GEOCODE")
master  <- merge.data.frame(master, tenure, by.x = "GEOCODE", by.y = "GEOCODE")
master  <- merge.data.frame(master, unemp,  by.x = "GEOCODE", by.y = "GEOCODE")
master  <- subset(master, select = c(GEOCODE,
                                     zCar,
                                     zOvercrowd,
                                     zTenure,
                                     zUnemp))
# Combine z-scores in to one score


write.csv(master, file = "master.csv")