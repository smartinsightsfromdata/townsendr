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



# Packages ====
require("dplyr")



# Prepare Data ====
# Read in files for: car or van access
#                    persons per room
#                    tenure, and
#                    unemployment
# These can be at any geography desired (e.g. LSOA, MSOA, ward, LAD, etc)
# These can be obtained from Nomis Web's API
# API documentation: https://www.nomisweb.co.uk/api/v01/help

# Car or van access
car <- read.csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_548_1.data.csv?geography=TYPE464&RURAL_URBAN=0&CELL=2,3,4,5&MEASURES=20301&select=GEOGRAPHY_NAME,GEOGRAPHY_CODE,CELL_NAME,OBS_VALUE",
                header = TRUE, stringsAsFactors = FALSE)
GEOGRAPHY_NAME <- unique(car$GEOGRAPHY_NAME)
GEOGRAPHY_CODE <- unique(car$GEOGRAPHY_CODE)
OBS_VALUE <- lapply(as.list(GEOGRAPHY_CODE), function(x){
  sum(car$OBS_VALUE[car$GEOGRAPHY_CODE == x])
})
OBS_VALUE <- as.double(OBS_VALUE)
car <- data.frame(GEOGRAPHY_CODE, GEOGRAPHY_NAME, OBS_VALUE)
car <- arrange(car, GEOGRAPHY_CODE)
rm(GEOGRAPHY_CODE, GEOGRAPHY_NAME, OBS_VALUE)


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



# Export results ====
write.csv(master, file = "townsend-dep-score.csv")
