

# License ====

# Calculate Townsend Material Deprivation Score from 2011 Census Data in the UK
# Copyright (C) 2014 Phil Mike Jones - orcid.org/0000-0001-5173-3245
  
# This programme is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This programme is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this programme. If not, see <http://www.gnu.org/licenses/>.



# Thanks and acknowledgements ====

# Thanks to Robin Lovelace - https://github.com/Robinlovelace - for having
# a once-over of the script and his suggestions.
# (Dimitris: he did this after I handed it in for assessment!)



# Libraries ====

library(ggplot2)
library(gridExtra) # For panelling ggplots



# Prepare Data ====

# Read in files for car access, persons per room, tenure, and unemp
# These can be at any geography desired (e.g. LSOA, MSOA, ward, LAD, etc)

# Car Access
car         <- read.csv("Data_car_engwal_lsoa.csv",
                 header = F,
                 skip = 2)
keep        <- c("V2", "V6", "V7")
car         <- car[keep]
rm(keep)
names(car)  <- c("lsoacode", "allHh", "noCar")
car$pcNoCar <- 100 * (car$noCar / car$allHh)
keep        <- c("lsoacode", "pcNoCar")
car         <- car[keep]
rm(keep)

# Overcrowding (more than one person per room)
overcrowd         <- read.csv("Data_personperroom_engwal_lsoa.csv",
                      header = F,
                      skip = 2)
keep              <- c("V2", "V6", "V9", "V10")
overcrowd         <- overcrowd[keep]
rm(keep)
names(overcrowd)  <- c("lsoacode", "allHh", "ppr1", "ppr15")
overcrowd$yesOc   <- rowSums(overcrowd[, 3:4])
overcrowd$pcYesOc <- 100 * (overcrowd$yesOc / overcrowd$allHh)
keep              <- c("lsoacode", "pcYesOc")
overcrowd         <- overcrowd[keep]
rm(keep)

# Tenure (not owner-occupied)
tenure         <- read.csv("Data_tenure_engwal_lsoa.csv",
                              header = F,
                              skip = 2)
keep           <- c("V2", "V6", "V11", "V12", "V13")
tenure         <- tenure[keep]
rm(keep)
names(tenure)  <- c("lsoacode", "allHh", "socRent", "prvRent", "rentFree")
tenure$notOo   <- rowSums(tenure[, 3:5])
tenure$pcNotOo <- 100 * (tenure$notOo / tenure$allHh)
keep           <- c("lsoacode", "pcNotOo")
tenure         <- tenure[keep]
rm(keep)

# Economically active unemployed
unemp     <- read.csv("Data_unemp_engwal_lsoa.csv",
                      header = F,
                      skip = 2)
keep           <- c("V2", "V6", "V7")
unemp         <- unemp[keep]
rm(keep)
names(unemp)  <- c("lsoacode", "unempEa", "allPers")
unemp$pcUnemp <- 100 * (unemp$unempEa / unemp$allPers)
keep           <- c("lsoacode", "pcUnemp")
unemp         <- unemp[keep]
rm(keep)

# Merge file
master <- merge(car, overcrowd, by = "lsoacode")
master <- merge(master, tenure, by = "lsoacode")
master <- merge(master, unemp,  by = "lsoacode")
View(master)

# Check distributions ====

# By default the code to check the distributions is commented out.
# Z-scores can be created from the original distribution, even if it's not ideal
# Townsend et al (1988) did transform some of the variables.
# The code is provided to check the distributions if you want to transform.

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
# plot5 <- ggplot(master, aes(master$pcNotOo)) +
#   geom_density() +
#   ggtitle("Percent of households not owner-occupied") +
#   xlab("Density: tenure") +
#   stat_function(fun = dnorm, 
#                 args = list(mean = mean(master$pcNotOo, na.rm = T), 
#                             sd = sd(master$pcNotOo, na.rm = T)))
# plot6 <- ggplot(master, aes(sample = master$pcNotOo)) + 
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



# Z-scores ====

# Calculate z-score

master$zCar       <- scale(master$pcNoCar, center = T, scale = T)
master$zOvercrowd <- scale(master$pcYesOc, center = T, scale = T)
master$zTenure    <- scale(master$pcNotOo, center = T, scale = T)
master$zUnemp     <- scale(master$pcUnemp, center = T, scale = T)


# Combine z-scores in to one score
master$z <- rowSums(master[c("zCar", "zOvercrowd", "zTenure", "zUnemp")])

# Drop unnecessary colums
keep   <- c("lsoacode", "z")
master <- master[keep]
rm(keep)



# Output final file ====

write.csv(master, file = "master.csv")
