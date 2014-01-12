# Purpose ====

# This R script is to prepare the Census data:
# 1. Check and if necessary transform the data so that it can be used
# 2. Convert the four domains in to z-scores so they can be combined
# 3. Combine the z-scores to form one overall index

# License ====

# GPL v2. See LICENSE.md

# Libraries ====

library(ggplot2)

# Prepare Data ====

overcrowd.frame <- Data_personsperroom_englandwales_LADs
tenure.frame    <- Data_tenure_lad
unemp.frame     <- Data_unemployed_economicallyactive_engwal_LADs
car.frame       <- Data_CARVAN_UNIT

ggplot(overcrowd.frame, aes(overcrowd.frame$pcGt1PPerRoom)) + geom_density() + ggtitle("Density plot of overcrowding") + xlab("Percent of households overcrowded")

ggplot(tenure.frame, aes(tenure.frame$pcNotOO)) + geom_density() + ggtitle("Density plot of tenure") + xlab("Percent of households not owner occupied")

ggplot(unemp.frame, aes(unemp.frame$pcEconActUnem)) + geom_density() + ggtitle("Density plot of unemployment") + xlab("Percent of individuals economically active who are unemployed")

ggplot(car.frame, aes(car.frame$pcNoCar)) + geom_density() + ggtitle("Density plot of car ownership") + xlab("Percent of households who do not own a car")

