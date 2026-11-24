#This script loads in conveyal results for the WSDOT Community Support Services (CSS) destination category, merges results from eastern and western Washington into a single layer for each walking profile, and including each walking profile as a layer within one SpatRaster file. 
library(sf)
library(terra)
library(tidyverse)
library(mapview)

#load in food data

ewa_food_WP1 <- rast("./data/Food/E_WA_food_WP1.tif")
ewa_food_WP2 <- rast("./data/Food/E_WA_food_WP2.tif")
ewa_food_WP3 <- rast("./data/Food/E_WA_food_WP3.tif")
ewa_food_WP4 <- rast("./data/Food/E_WA_food_WP4.tif")
wwa_food_WP1 <- rast("./data/Food/W_WA_food_WP1.tif")
wwa_food_WP2 <- rast("./data/Food/W_WA_food_WP2.tif")
wwa_food_WP3 <- rast("./data/Food/W_WA_food_WP3.tif")
wwa_food_WP4 <- rast("./data/Food/W_WA_food_WP4.tif")

wwa_food_car <- rast("./data/Food/W_WA_food_car.tif")
ewa_food_car <- rast("./data/Food/E_WA_food_car.tif")
#merge e-w - -----
#mosaic averages the merged rasters, where merge uses either only values from either rast
food_WP1 <- mosaic(ewa_food_WP1, wwa_food_WP1, fun = mean)
food_WP2 <- mosaic(ewa_food_WP2, wwa_food_WP2, fun = mean)
food_WP3 <- mosaic(ewa_food_WP3, wwa_food_WP3, fun = mean)
food_WP4 <- mosaic(ewa_food_WP4, wwa_food_WP4, fun = mean)
food_car <- mosaic(ewa_food_car, wwa_food_car, fun = mean)


freq(food_WP1)
freq(food_WP2)
freq(food_WP3)
freq(food_WP4)
freq(food_car)

#Conveyal jump correction -----
food_WP2[food_WP2 < food_WP4] <- 0
food_WP3[food_WP3 < food_WP1] <- 0


#consolidate into layered raster
wa_food <- c(food_WP1, food_WP2, food_WP3, food_WP4, food_car)


remove(wwa_food_car)
remove(wwa_food_WP1)
remove(wwa_food_WP2)
remove(wwa_food_WP3)
remove(wwa_food_WP4)

remove(ewa_food_car)
remove(ewa_food_WP1)
remove(ewa_food_WP2)
remove(ewa_food_WP3)
remove(ewa_food_WP4)

remove(food_car)
remove(food_WP1)
remove(food_WP2)
remove(food_WP3)
remove(food_WP4)

#ratio difference dataset ----
food_wp2vwp1 <- food_WP2 / food_WP1
food_wp3vwp1 <- food_WP3 / food_WP1
food_wp4vwp1 <- food_WP4 / food_WP1

food_wp2vwp1

freq(food_wp2vwp1)
freq(food_wp3vwp1)
freq(food_wp4vwp1)

food_wp2vwp1
food_wp3vwp1

#time difference dataset ----
food_wp2wp1_diff <- food_WP2 - food_WP1
food_wp3wp1_diff <- food_WP3 - food_WP1
food_wp4wp1_diff <- food_WP4 - food_WP1


plot(food_wp2vwp1)
plot(food_WP2)

mapview(food_wp2vwp1) +
  mapview(food_wp3vwp1) +
  mapview(food_wp4vwp1)

mapview(food_wp2wp1_diff) +
  mapview(food_wp3wp1_diff) +
  mapview(food_wp4wp1_diff)

summary(wa_food)

# #update values over 120 to 121 minutes----
food_WP1_121 <- food_WP1
  food_WP1_121[is.nan(food_WP1_121)| food_WP1_121 == 0] <- 121

  food_WP2_121 <- food_WP2
  food_WP2_121[is.nan(food_WP2_121)| food_WP2_121 == 0] <- 121

food_WP3_121 <- food_WP3 
  food_WP3_121[is.nan(food_WP3_121)| food_WP3_121 == 0] <- 121

food_WP4_121 <- food_WP4
food_WP4_121[is.nan(food_WP4_121)| food_WP4_121 == 0] <- 121

food_car_121 <- food_car
food_car_121[is.nan(food_car_121)| food_car_121 == 0] <- 121

#consolidate into layered raster
wa_food_121 <- c(food_WP1_121, food_WP2_121, food_WP3_121, food_WP4_121, food_car_121)



#write rasters ----
writeRaster(
  wa_food, 
  "./data/Food/wa_food.tif",
  overwrite = TRUE)

wa_food <- rast("./data/Food/wa_food.tif")

wa_food[wa_food == 0] <- NA

summary(wa_food)

wa_food

freq(wa_food)

writeRaster(
  wa_food_121, 
  "./data/Food/wa_food_121.tif",
  overwrite = TRUE)
wa_food_121 <- rast( "./data/Food/wa_food_121.tif")
