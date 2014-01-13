# Purpose ====

# This R script is to prepare the Census data:
# 1. Check and if necessary transform the data so that it can be used
# 2. Convert the four domains in to z-scores so they can be combined
# 3. Combine the z-scores to form one overall index

# License ====

# The code is GPL v3 license. See LICENSE.txt.
# Because the data is from the UK Census it is protected by Crown Copyright.
# See census-copyright.txt

# References ====

# Office for National Statistics, 2011 Census: Aggregate data (England and Wales) [computer file]. UK Data Service Census Support. Downloaded from: http://infuse.mimas.ac.uk. This information is licensed under the terms of the Open Government Licence [http://www.nationalarchives.gov.uk/doc/open-government-licence/version/2].

# Office for National Statistics, 2011 Census: Digitised Boundary Data (England and Wales) [computer file]. UK Data Service Census Support. Downloaded from: http://edina.ac.uk/census

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

# Transformations ====
# Townsend et al used y = log(x + 1)

# Overcrowding
logOvercrowding <- log(overcrowd.frame$pcGt1PPerRoom + 1)
ggplot(overcrowd.frame, aes(logOvercrowding)) + geom_density()

sqrtOvercrowding <- sqrt(overcrowd.frame$pcGt1PPerRoom + 1)
ggplot(overcrowd.frame, aes(sqrtOvercrowding)) + geom_density()
