#This script loads in conveyal results for the WSDOT leisure/culture/entertainment destination category, merges results from eastern and western Washington into a single layer for each walking profile, and including each walking profile as a layer within one SpatRaster file. 
#The script also corrects for scenarios where walking profile 2 and walking profile 3 outperform walking profile one and normalizes the number of destinations into clusters. 
#The output files are used in later data consolidation scripts and the file is saved for later use. 

library(sf)
library(terra)
library(tidyverse)
library(mapview)

#load in Leisure Conveyal output data
ewa_Leisure_WP1 <- rast("./data/Leisure/E_WA_Leisure_WP1.tif")
ewa_Leisure_WP2 <- rast("./data/Leisure/E_WA_Leisure_WP2.tif")
ewa_Leisure_WP3 <- rast("./data/Leisure/E_WA_Leisure_WP3.tif")
ewa_Leisure_WP4 <- rast("./data/Leisure/E_WA_Leisure_WP4.tif")
wwa_Leisure_WP1 <- rast("./data/Leisure/W_WA_Leisure_WP1.tif")
wwa_Leisure_WP2 <- rast("./data/Leisure/W_WA_Leisure_WP2.tif")
wwa_Leisure_WP3 <- rast("./data/Leisure/W_WA_Leisure_WP3.tif")
wwa_Leisure_WP4 <- rast("./data/Leisure/W_WA_Leisure_WP4.tif")

wwa_Leisure_car <- rast("./data/Leisure/W_WA_Leisure_Car.tif")
ewa_Leisure_car <- rast("./data/Leisure/E_WA_Leisure_Car.tif")

wwa_Leisure_walk1 <- rast("./data/Leisure/W_WA_Leisure_Walk1.tif")
ewa_Leisure_walk1 <- rast("./data/Leisure/E_WA_Leisure_Walk1.tif")
wwa_Leisure_walk2 <- rast("./data/Leisure/W_WA_Leisure_Walk2.tif")
ewa_Leisure_walk2 <- rast("./data/Leisure/E_WA_Leisure_Walk2.tif")

#merge e-w - -----
#mosaic averages the merged rasters, where merge uses either only values from either rast
Leisure_WP1 <- mosaic(ewa_Leisure_WP1, wwa_Leisure_WP1, fun = mean)
Leisure_WP2 <- mosaic(ewa_Leisure_WP2, wwa_Leisure_WP2, fun = mean)
Leisure_WP3 <- mosaic(ewa_Leisure_WP3, wwa_Leisure_WP3, fun = mean)
Leisure_WP4 <- mosaic(ewa_Leisure_WP4, wwa_Leisure_WP4, fun = mean)
Leisure_car <- mosaic(ewa_Leisure_car, wwa_Leisure_car, fun = mean)
Leisure_walk1 <- mosaic(ewa_Leisure_walk1, wwa_Leisure_walk1, fun = mean)
Leisure_walk2 <- mosaic(ewa_Leisure_walk2, wwa_Leisure_walk2, fun = mean)

#data QA/QC check
freq(Leisure_WP1)
freq(Leisure_WP2)
freq(Leisure_WP3)
freq(Leisure_WP4)
freq(Leisure_car)

#Conveyal jump correction -----
Leisure_WP2[Leisure_WP2 > Leisure_WP4] <- 0
Leisure_WP3[Leisure_WP3 > Leisure_WP1] <- 0
Leisure_walk2[Leisure_walk2 > Leisure_walk1] <- 0
wa_Leisure_normalized[[7]][wa_Leisure_normalized[[7]] > wa_Leisure_normalized[[2]]] <- 0

#consolidate into layered raster
wa_Leisure <- c(Leisure_WP1, Leisure_WP2, Leisure_WP3, Leisure_WP4, Leisure_car, Leisure_walk1, Leisure_walk2)

#Leisure clusters math ----
# clusters were pre-filtered to only include destinations that had other 2 other leisure destinations within 200 yards. So, updating the value to be clusters of 3, instead of raw counts of how many destinations are reachable. 
wa_Leisure[wa_Leisure > 0 & wa_Leisure < 3] <- 3
wa_Leisure_normalized <-  round(wa_Leisure / 3, digits = 0)

#cleaning up individual DFs that are no longer needed ----
remove(Leisure_car)
remove(Leisure_walk1)
remove(Leisure_walk2)
remove(Leisure_WP1)
remove(Leisure_WP2)
remove(Leisure_WP3)
remove(Leisure_WP4)

remove(wwa_Leisure_car)
remove(wwa_Leisure_walk1)
remove(wwa_Leisure_walk2)
remove(wwa_Leisure_WP1)
remove(wwa_Leisure_WP2)
remove(wwa_Leisure_WP3)
remove(wwa_Leisure_WP4)

remove(ewa_Leisure_car)
remove(ewa_Leisure_walk1)
remove(ewa_Leisure_walk2)
remove(ewa_Leisure_WP1)
remove(ewa_Leisure_WP2)
remove(ewa_Leisure_WP3)
remove(ewa_Leisure_WP4)

freq(wa_Leisure_normalized[[3]])

freq(wa_Leisure_normalized[[1]])

#ratio difference dataset for comparison visualizations ----
Leisure_wp2vwp1 <- Leisure_WP2 / Leisure_WP1
Leisure_wp3vwp1 <- Leisure_WP3 / Leisure_WP1
Leisure_wp4vwp1 <- Leisure_WP4 / Leisure_WP1

#mapping option to get quick view of comparisons 
# mapview(Leisure_wp2vwp1) +
#   mapview(Leisure_wp3vwp1) +
#   mapview(Leisure_wp4vwp1)
# 
# mapview(Leisure_wp2wp1_diff) +
#   mapview(Leisure_wp3wp1_diff) +
#   mapview(Leisure_wp4wp1_diff)


#write rasters ----
writeRaster(
  wa_Leisure_normalized, 
  "./data/Leisure/06wa_Leisure_normalized.tif",
  overwrite = TRUE)

wa_Leisure_normalized <- rast("./data/Leisure/06wa_Leisure_normalized.tif")
summary(wa_Leisure_normalized)


