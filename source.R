# License ====

# Calculate Townsend Material Deprivation Score from 2011 Census Data in the UK
# Copyright (C) 2014 Phil Mike Jones - orcid.org/0000-0001-5173-3245
  
# This programme is free software: you can redistribute it and/or modify it 
# under the terms of the GNU General Public License as published by the 
# Free Software Foundation, either version 3 of the License, or 
# (at your option) any later version.

# This programme is distributed in the hope that it will be useful, but 
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License 
# for more details.

# You should have received a copy of the GNU General Public License along with
# this programme. If not, see <http://www.gnu.org/licenses/>.



# Libraries ====

library(ggplot2)
library(gridExtra) # For panelling ggplots



# Prepare Data ====

# Read in files for car access, persons per room, tenure, and unemp
# These can be at any geography desired (e.g. LSOA, MSOA, ward, LAD, etc)
# These can be obtained from Nomis Web's Census table finder:
# http://www.nomisweb.co.uk/census/2011/data_finder

# Car Access
car <- read.csv("car.csv", header = T)
car <- subset(car, Rural.Urban == "Total")
car <- as.data.frame(car)
car$yesCar <- rowSums(car[, 7:10])
colnames(car)[5] <- "allHh"
colnames(car)[6] <- "noCar"
car$pcNoCar <- (car$noCar / car$allHh) * 100
keep <- c("geography.code", "allHh", "noCar", "yesCar", "pcNoCar")
car <- car[keep]
rm(keep)
View(car)

# Overcrowding (more than one person per room (NOT bedroom!))
oc <- read.csv("oc.csv", header = T)
oc <- subset(oc, Rural.Urban == "Total")
oc <- as.data.frame(oc)
oc$yesOc <- rowSums(oc[, 8:9])
oc$noOc  <- rowSums(oc[, 6:7])
colnames(oc)[5] <- "allHh"
oc$pcYesOc <- (oc$yesOc / oc$allHh) * 100
keep <- c("geography.code", "allHh", "noOc", "yesOc", "pcYesOc")
oc <- oc[keep]
rm(keep)

# Tenure (not owner-occupied. Shared ownership not included in O-O)
tenure <- read.csv("tenure.csv", header = T)
tenure <- subset(tenure, Rural.Urban == "Total")
tenure <- as.data.frame(tenure)
colnames(tenure)[5] <- "allHh"
colnames(tenure)[6] <- "yesOo" # Does not include shared ownership
keep <- c("geography.code", "allHh", "yesOo", "Tenure..Shared.ownership..part.owned.and.part.rented...measures..Value", "Tenure..Social.rented..measures..Value", "Tenure..Private.rented..measures..Value", "Tenure..Living.rent.free..measures..Value")
tenure <- tenure[keep]
rm(keep)
tenure$noOo <- rowSums(tenure[, 4:7])
keep <- c("geography.code", "allHh", "yesOo", "noOo")
tenure <- tenure[keep]
rm(keep)
tenure$pcNoOo <- (tenure$noOo / tenure$allHh) * 100

# Economically active unemployed (Census table QS601EW)
unemp <- read.csv("unemp.csv", header = T)
unemp <- subset(unemp, Rural.Urban == "Total")
unemp <- as.data.frame(unemp)
# Use all econ active residents, NOT all persons (see Townsend 1988: 36)
colnames(unemp)[6] <- "allEconAct"
colnames(unemp)[13] <- "yesUnemp"
unemp$pcUnemp <- (unemp$yesUnemp / unemp$allEconAct) * 100
keep <- c("geography.code", "allEconAct", "yesUnemp", "pcUnemp")
unemp <- unemp[keep]
rm(keep)



# Create Master File ====

master <- merge(car, oc, by = "geography.code")
master <- merge(master, tenure, by = "geography.code")
master <- merge(master, unemp,  by = "geography.code")



# Check distributions ====

# By default the code to check the distributions is commented out.
# Z-scores don't assume a normal distribution.
# Townsend et al (1988) transformed unemployment and overcrowding (p. 165).
# This block is therefore provided if you want to check and transform.
# It will output a pdf - "check-distributions.pdf" - for inspection.

# require(gridExtra)
# # Plot each varaible as density and qq plot. Linear = good
# plot1 <- ggplot(master, aes(master$pcNoCar)) +
#   geom_density() +
#   ggtitle("Percent of households with no car") +
#   xlab("Density: car") +
#   stat_function(fun = dnorm, 
#                 args = list(mean = mean(master$pcNoCar, na.rm = T), 
#                             sd = sd(master$pcNoCar, na.rm = T)))
# plot2 <- ggplot(master, aes(sample = master$pcNoCar)) + 
#   stat_qq() + 
#   ggtitle("QQ: car")
# 
# plot3 <- ggplot(master, aes(master$pcYesOc)) +
#   geom_density() +
#   ggtitle("Percent of households with more than one person per room") +
#   xlab("Density: overcrowd") +
#   stat_function(fun = dnorm, 
#                 args = list(mean = mean(master$pcYesOc, na.rm = T), 
#                             sd = sd(master$pcYesOc, na.rm = T)))
# plot4 <- ggplot(master, aes(sample = master$pcYesOc)) + 
#   stat_qq() + 
#   ggtitle("QQ: overcrowd")
# 
# plot5 <- ggplot(master, aes(master$pcNoOo)) +
#   geom_density() +
#   ggtitle("Percent of households not owner-occupied") +
#   xlab("Density: tenure") +
#   stat_function(fun = dnorm, 
#                 args = list(mean = mean(master$pcNoOo, na.rm = T), 
#                             sd = sd(master$pcNoOo, na.rm = T)))
# plot6 <- ggplot(master, aes(sample = master$pcNoOo)) + 
#   stat_qq() + 
#   ggtitle("QQ: tenure")
# 
# plot7 <- ggplot(master, aes(master$pcUnemp)) +
#   geom_density() +
#   ggtitle("Percent of individuals economically active unemployed") +
#   xlab("Density: unemp") +
#   stat_function(fun = dnorm, 
#                 args = list(mean = mean(master$pcUnemp, na.rm = T), 
#                             sd = sd(master$pcUnemp, na.rm = T)))
# plot8 <- ggplot(master, aes(sample = master$pcUnemp)) + 
#   stat_qq() + 
#   ggtitle("QQ: unemp")
# 
# grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8,
#              ncol = 2,
#              main = "Distributions of Townsend Domains")
# dev.copy2pdf(file = "check-distributions.pdf", width = 8.27, height = 11.69)
# dev.off()
# rm(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8)

# If you want to transform any of the variables, you could do something like:
# master$logCar <- log(master$pcNoCar)



# Z-scores ====

# Calculate z-score

master$zCar       <- scale(master$pcNoCar, center = T, scale = T)
master$zOvercrowd <- scale(master$pcYesOc, center = T, scale = T)
master$zTenure    <- scale(master$pcNoOo, center = T, scale = T)
master$zUnemp     <- scale(master$pcUnemp, center = T, scale = T)

# Combine z-scores in to one score
master$z <- rowSums(master[c("zCar", "zOvercrowd", "zTenure", "zUnemp")])

# Drop unnecessary colums
keep   <- c("geography.code", "z")
master <- master[keep]
rm(keep)



# Output final file ====

write.csv(master, file = "master.csv")
