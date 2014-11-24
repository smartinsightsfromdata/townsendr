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

require("ggplot2")
require("gridExtra")  # For panelling ggplots
require("rgdal")  # for readOGR()
require("rgeos")



# Prepare Data ====

# Read in files for car access, persons per room, tenure, and unemp
# These can be at any geography desired (e.g. LSOA, MSOA, ward, LAD, etc)
# These can be obtained from Nomis Web's Census table finder:
# http://www.nomisweb.co.uk/census/2011/data_finder

# Car Access
car <- read.csv("data/car.csv", header = T)
car <- subset(car, Rural.Urban == "Total")
car$yesCar <- rowSums(car[, 7:10])
colnames(car)[5] <- "allHh"
colnames(car)[6] <- "noCar"
car$pcNoCar <- (car$noCar / car$allHh) * 100
car <- subset(car, select = c("geography.code", "pcNoCar"))

# Overcrowding (more than one person per room (NOT bedroom!))
oc <- read.csv("data/oc.csv", header = T)
oc <- subset(oc, Rural.Urban == "Total")
oc$yesOc <- rowSums(oc[, 8:9])
oc$noOc  <- rowSums(oc[, 6:7])
colnames(oc)[5] <- "allHh"
oc$pcYesOc <- (oc$yesOc / oc$allHh) * 100
oc <- subset(oc, select = c("geography.code", "pcYesOc"))

# Tenure (not owner-occupied. Shared ownership not included in O-O)
ten <- read.csv("data/tenure.csv", header = T)
ten <- subset(ten, Rural.Urban == "Total")
colnames(ten)[5] <- "allHh"
colnames(ten)[6] <- "yesOo" # Does not include shared ownership
colnames(ten)[9] <- "shared"
colnames(ten)[10] <- "social"
colnames(ten)[13] <- "private"
colnames(ten)[ncol(ten)] <- "free"
ten <- subset(ten, select = c("geography.code", "allHh", "yesOo", "shared", 
                               "social", "private", "free"))
ten$noOo <- rowSums(ten[, 4:7])
ten$pcNoOo <- (ten$noOo / ten$allHh) * 100
ten <- subset(ten, select = c("geography.code", "pcNoOo"))

# Economically active unemployed (Census table QS601EW)
eau <- read.csv("data/unemp.csv", header = T)
eau <- subset(eau, Rural.Urban == "Total")
# Use all econ active residents, NOT all persons (see Townsend 1988: 36)
colnames(eau)[6] <- "allEconAct"
colnames(eau)[13] <- "yesUnemp"
eau$pceau <- (eau$yesUnemp / eau$allEconAct) * 100
eau <- subset(eau, select = c("geography.code", "pceau"))

              
              
# Create Master File ====

master <- merge(car, oc, by = "geography.code")
master <- merge(master, ten, by = "geography.code")
master <- merge(master, eau,  by = "geography.code")



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

master$zCar <- scale(master$pcNoCar, center = T, scale = T)
master$zOc  <- scale(master$pcYesOc, center = T, scale = T)
master$zTen <- scale(master$pcNoOo, center = T, scale = T)
master$zEau <- scale(master$pceau, center = T, scale = T)

# Combine z-scores in to one score
master$z <- rowSums(master[c("zCar", "zOc", "zTen", "zEau")])

# Drop unnecessary items
master <- subset(master, select = c("geography.code", "z"))
rm(car, eau, oc, ten)

# Bin into quintiles
master$quintile <- cut(master$z, breaks = 5)



# Plot results ====
map <- theme(line = element_blank(), 
             text = element_blank(), 
             title = element_blank(),
             panel.background = element_rect(fill = "transparent"),
             legend.position = "none")
mapl <- theme(line = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              panel.background = element_rect(fill = "transparent"))
port  <- c(5.39, 7.19)  # full-page LaTeX A4 body
land <- c(5.39, 3.595)  # half-page LaTeX A4 body

elad <- readOGR("shapes/ewlad", "englandWalesLADs")
proj4string(elad) <- CRS("+init=epsg:27700")
eladf <- fortify(elad, region = "code")
eladf <- merge(eladf, elad, by.x = "id", by.y = "code")
eladf <- merge(eladf, master, by.x = "id", by.y = "geography.code")
ggplot() + 
  geom_polygon(data = eladf, aes(long, lat, group = group, fill = quintile), 
                        colour = "dark grey") +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363", 
                               "#252525"),
                    name = "Townsend Deprivation Quintile",
                    labels = c("Least deprived quintile", "20-40%", "40-60%", 
                               "60-80%", "Most deprived quintile")) +
  coord_equal() + mapl
ggsave("maps/td-e-lad.pdf", width = land[1], height = land[2])
