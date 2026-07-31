#This script loads in conveyal results for the WSDOT Community Support Services (CSS) destination category, merges results from eastern and western Washington into a single layer for each walking profile, and including each walking profile as a layer within one SpatRaster file. 

library(sf)
library(terra)
library(tidyverse)
library(mapview)

#parks will be in quarter acre values - 
# will need to divide by 4 in order to get acres
# 

#load in Leisure data

ewa_Parks_WP1 <- rast("./data/Parks/E_WA_parks_WP1.tif")
ewa_Parks_WP2 <- rast("./data/Parks/E_WA_parks_WP2.tif")
ewa_Parks_WP3 <- rast("./data/Parks/E_WA_parks_WP3.tif")
ewa_Parks_WP4 <- rast("./data/Parks/E_WA_parks_WP4.tif")
wwa_Parks_WP1 <- rast("./data/Parks/W_WA_parks_WP1.tif")
wwa_Parks_WP2 <- rast("./data/Parks/W_WA_parks_WP2.tif")
wwa_Parks_WP3 <- rast("./data/Parks/W_WA_parks_WP3.tif")
wwa_Parks_WP4 <- rast("./data/Parks/W_WA_parks_WP4.tif")

wwa_Parks_car <- rast("./data/Parks/W_WA_parks_Car.tif")
ewa_Parks_car <- rast("./data/Parks/E_WA_parks_Car.tif")

wwa_Parks_walk1 <- rast("./data/Parks/W_WA_parks_Walk1.tif")
ewa_Parks_walk1 <- rast("./data/Parks/E_WA_parks_Walk1.tif")
wwa_Parks_walk2 <- rast("./data/Parks/W_WA_parks_Walk2.tif")
ewa_Parks_walk2 <- rast("./data/Parks/E_WA_parks_Walk2.tif")


#merge e-w - -----
#mosaic averages the merged rasters, where merge uses either only values from either rast
Parks_WP1 <- mosaic(ewa_Parks_WP1, wwa_Parks_WP1, fun = mean)
Parks_WP2 <- mosaic(ewa_Parks_WP2, wwa_Parks_WP2, fun = mean)
Parks_WP3 <- mosaic(ewa_Parks_WP3, wwa_Parks_WP3, fun = mean)
Parks_WP4 <- mosaic(ewa_Parks_WP4, wwa_Parks_WP4, fun = mean)
Parks_car <- mosaic(ewa_Parks_car, wwa_Parks_car, fun = mean)
Parks_walk1 <- mosaic(wwa_Parks_walk1, ewa_Parks_walk1, fun = mean)
Parks_walk2 <- mosaic(wwa_Parks_walk2, ewa_Parks_walk2, fun = mean)

# #crop to align raster sizes
# wa_POW_WP2 <- crop(wa_POW_WP2, wa_POW_WP1, mask = TRUE)
# wa_POW_WP3 <- crop(wa_POW_WP3, wa_POW_WP1, mask = TRUE)
# wa_POW_WP4 <- crop(wa_POW_WP4, wa_POW_WP1, mask = TRUE)


freq(Parks_WP1)


#Conveyal jump correction -----
Parks_WP2[Parks_WP2 > Parks_WP4] <- 0
Parks_WP3[Parks_WP3 > Parks_WP1] <- 0
Parks_walk2[Parks_walk2 > Parks_walk1] <- 0



#consolidate into layered raster
wa_Parks <- c(Parks_WP1, Parks_WP2, Parks_WP3, Parks_WP4, Parks_car, Parks_walk1, Parks_walk2)

freq(wa_Parks[[1]])

freq(wa_Parks[[6]])


# #update values from 1/4 acre increments to acres ----
wa_Parks <- wa_Parks / 4

freq(wa_Parks[[7]])
freq(wa_Parks[[2]])
wa_Parks[[7]][wa_Parks[[7]] > wa_Parks[[4]]] <- 0

#remove leftover files after consolidation
remove(Parks_car)
remove(Parks_walk1)
remove(Parks_walk2)
remove(Parks_WP1)
remove(Parks_WP2)
remove(Parks_WP3)
remove(Parks_WP4)

remove(ewa_Parks_car)
remove(ewa_Parks_walk1)
remove(ewa_Parks_walk2)
remove(ewa_Parks_WP1)
remove(ewa_Parks_WP2)
remove(ewa_Parks_WP3)
remove(ewa_Parks_WP4)

remove(wwa_Parks_car)
remove(wwa_Parks_walk1)
remove(wwa_Parks_walk2)
remove(wwa_Parks_WP1)
remove(wwa_Parks_WP2)
remove(wwa_Parks_WP3)
remove(wwa_Parks_WP4)

#write rasters ----
writeRaster(
  wa_Parks, 
  "./data/Parks/wa_Parks.tif",
  overwrite = TRUE)

wa_Parks <- rast("./data/Parks/wa_Parks.tif")
