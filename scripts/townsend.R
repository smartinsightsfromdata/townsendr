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
require("reshape2")
require("ggplot2")



# Prepare Data ====
# Read in files for: car or van access
#                    persons per room
#                    tenure, and
#                    unemployment
# These can be obtained from Nomis Web's API
# API documentation: https://www.nomisweb.co.uk/api/v01/help

# Percentage of households with access to a light vehicle
car <- read.csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_548_1.data.csv?geography=TYPE464&RURAL_URBAN=0&CELL=2,3,4,5&MEASURES=20301&select=GEOGRAPHY_NAME,GEOGRAPHY_CODE,CELL_NAME,OBS_VALUE",
                header = TRUE, stringsAsFactors = FALSE)
car <- tbl_df(car)
car <- dcast(car, GEOGRAPHY_CODE ~ CELL_NAME)
car$car <- rowSums(car[, 2:length(car)])
car <- select(car, GEOGRAPHY_CODE, car)

# Percentage of households overcrowded (more than 1 person per room)
ovc <- read.csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_541_1.data.csv?GEOGRAPHY=TYPE464&RURAL_URBAN=0&C_PPROOMHUK11=3,4&MEASURES=20301&select=GEOGRAPHY_NAME,GEOGRAPHY_CODE,C_PPROOMHUK11_NAME,OBS_VALUE",
                header = TRUE, stringsAsFactors = FALSE)
ovc <- tbl_df(ovc)
ovc <- dcast(ovc, GEOGRAPHY_CODE ~ C_PPROOMHUK11_NAME)
ovc$ovc <- rowSums(ovc[, 2:length(ovc)])
ovc <- select(ovc, GEOGRAPHY_CODE, ovc)

# Percentage of households not owner-occupied. Shared ownership not included)
ten <- read.csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_537_1/GEOGRAPHY/2092957703TYPE464/RURAL_URBAN/0/C_TENHUK11/4,5,8,13/MEASURES/20301/data.csv?select=GEOGRAPHY_NAME,GEOGRAPHY_CODE,C_TENHUK11_NAME,OBS_VALUE",
                header = TRUE, stringsAsFactors = FALSE)
ten <- tbl_df(ten)
ten <- dcast(ten, GEOGRAPHY_CODE ~ C_TENHUK11_NAME)
ten$ten <- rowSums(ten[, 2:length(ten)])
ten <- select(ten, GEOGRAPHY_CODE, ten)

# Percent of individuals economically active unemployed (Census table QS601EW)
eau <- read.csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_556_1/GEOGRAPHY/2092957703TYPE464/RURAL_URBAN/0/CELL/1,8/MEASURES/20100/data.csv?select=GEOGRAPHY_NAME,GEOGRAPHY_CODE,CELL_NAME,OBS_VALUE",
                header = TRUE, stringsAsFactors = FALSE)
eau <- tbl_df(eau)
eau <- dcast(eau, GEOGRAPHY_CODE ~ CELL_NAME)
eau$eau <- (eau[[3]] / eau[[2]]) * 100
  # Use all econ active residents, NOT all persons, as denominator (p36)
eau <- select(eau, GEOGRAPHY_CODE, eau)

# Merge in to one file
td <- car
td <- left_join(td, eau, by = "GEOGRAPHY_CODE")
td <- left_join(td, ovc, by = "GEOGRAPHY_CODE")
td <- left_join(td, ten, by = "GEOGRAPHY_CODE")
rm(car, eau, ovc, ten)



# Z-scores ====
# Calculate z-score
td$zCar <- scale(td$car, center = T, scale = T)
td$zOvc  <- scale(td$ovc, center = T, scale = T)
td$zTen <- scale(td$ten, center = T, scale = T)
td$zEau <- scale(td$eau, center = T, scale = T)

# Combine z-scores in to one score
td$z <- rowSums(td[c("zCar", "zOvc", "zTen", "zEau")])

# Drop unnecessary items
td <- select(td, GEOGRAPHY_CODE, z)

# Bin into quintiles
td$quintile <- cut(td$z, breaks = 5)



# Plot results ====
download.file("http://census.edina.ac.uk/ukborders/easy_download/prebuilt/shape/England_lad_2011.zip",
              destfile = "shapes/elad")  # 25MB
download.file("http://census.edina.ac.uk/ukborders/easy_download/prebuilt/shape/Wales_lad_2011.zip",
              destfile = "shapes/wlad")  # 3MB


# Export results ====
write.csv(td, file = "townsend-dep-score.csv")
