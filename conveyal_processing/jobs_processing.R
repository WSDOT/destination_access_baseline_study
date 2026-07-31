#This script works with a Conveyal Analysis tiff export file, combining East and West regional data, cleans raster data to ensure path "hopping" in incomplete network does not lead to higher values than faster/more complete networks, and prepares the files to be integrated with Census data
#Walking profiles 2 & 3 are set up to change raster value to 0 if they exceed WP1. This is because those areas indicate "warping" to the closest pathway and getting a headstart in travel on a less complete traversal network. There are 50K occurances, so may want to review impact

library(sf)
library(terra)
library(tidyverse)
library(mapview)
library(tmap)
#rasterVis package provides opportunity to improve viz
#
getwd()


#load in raster datasets
#walking profile 1 ----------------------------------------
ewa_AM_jobs_WP1 <- rast("./data/Jobs/ewa_jobs_AM_WP1_P50_C45.tif")
ewa_PM_jobs_WP1 <- rast("./data/Jobs/ewa_jobs_PM_WP1_P50_C45.tif")
wwa_AM_jobs_WP1 <- rast("./data/Jobs/wwa_jobs_AM_WP1_P50_C45.tif")
wwa_PM_jobs_WP1 <- rast("./data/Jobs/wwa_jobs_PM_WP1_P50_C45.tif")

#merge datasets 
wa_AM_jobs_WP1 <- mosaic(ewa_AM_jobs_WP1, wwa_AM_jobs_WP1)
wa_PM_jobs_WP1 <- mosaic(ewa_PM_jobs_WP1, wwa_PM_jobs_WP1)

wa_AM_jobs_WP1
wa_PM_jobs_WP1


#walking profile 3 ---------------------------------------
#implement logic, where if any square is >WP1, it is set to 0
ewa_AM_jobs_WP3 <- rast("./data/Jobs/ewa_jobs_AM_WP3_P50_C45.tif")
ewa_PM_jobs_WP3 <- rast("./data/Jobs/ewa_jobs_PM_WP3_P50_C45.tif")
wwa_AM_jobs_WP3 <- rast("./data/Jobs/wwa_jobs_AM_WP3_P50_C45.tif")
wwa_PM_jobs_WP3 <- rast("./data/Jobs/wwa_jobs_PM_WP3_P50_C45.tif")



#merge datasets 
wa_AM_jobs_WP3 <- mosaic(ewa_AM_jobs_WP3, wwa_AM_jobs_WP3)
wa_PM_jobs_WP3 <- mosaic(ewa_PM_jobs_WP3, wwa_PM_jobs_WP3)

#currently, sets values where
wa_AM_jobs_WP3[wa_AM_jobs_WP3 > wa_AM_jobs_WP1] <- 0
wa_PM_jobs_WP3[wa_PM_jobs_WP3 > wa_PM_jobs_WP1] <- 0

#walking profile 4 -----------------------------------------------
#has identical network to WP1, inherently has lower access
ewa_AM_jobs_WP4 <- rast("./data/Jobs/ewa_jobs_AM_WP4_P50_C45.tif")
ewa_PM_jobs_WP4 <- rast("./data/Jobs/ewa_jobs_PM_WP4_P50_C45.tif")

wwa_AM_jobs_WP4 <- rast("./data/Jobs/wwa_jobs_AM_WP4_P50_C45.tif")
wwa_PM_jobs_WP4 <- rast("./data/Jobs/wwa_jobs_PM_WP4_P50_C45.tif")

#merge datasets 
wa_AM_jobs_WP4 <- mosaic(ewa_AM_jobs_WP4, wwa_AM_jobs_WP4)
wa_PM_jobs_WP4 <- mosaic(ewa_PM_jobs_WP4, wwa_PM_jobs_WP4)

#walking profile 2 ----------------------------------------
#implement logic, where if any square is >WP1, it is set to 0
ewa_AM_jobs_WP2 <- rast("./data/Jobs/ewa_jobs_AM_WP2_P50_C45.tif")
ewa_PM_jobs_WP2 <- rast("./data/Jobs/ewa_jobs_PM_WP2_P50_C45.tif")
wwa_AM_jobs_WP2 <- rast("./data/Jobs/wwa_jobs_AM_WP2_P50_C45.tif")
wwa_PM_jobs_WP2 <- rast("./data/Jobs/wwa_jobs_PM_WP2_P50_C45.tif")

#merge datasets 
wa_AM_jobs_WP2 <- mosaic(ewa_AM_jobs_WP2, wwa_AM_jobs_WP2)
wa_PM_jobs_WP2 <- mosaic(ewa_PM_jobs_WP2, wwa_PM_jobs_WP2)

wa_AM_jobs_WP2[wa_AM_jobs_WP2 > wa_AM_jobs_WP4] <- 0
wa_PM_jobs_WP2[wa_PM_jobs_WP2 > wa_PM_jobs_WP4] <- 0



#combined jobs raster with many layers-----
wa_jobs <- c(wa_AM_jobs_WP1, wa_PM_jobs_WP1, wa_AM_jobs_WP2, wa_PM_jobs_WP2, wa_AM_jobs_WP3, wa_PM_jobs_WP3, wa_AM_jobs_WP4, wa_PM_jobs_WP4)
remove(ewa_AM_jobs_WP1)
remove(wwa_AM_jobs_WP1)
remove(ewa_AM_jobs_WP2)
remove(wwa_AM_jobs_WP2)
remove(ewa_AM_jobs_WP3)
remove(wwa_AM_jobs_WP3)
remove(ewa_AM_jobs_WP4)
remove(wwa_AM_jobs_WP4)

remove(ewa_PM_jobs_WP1)
remove(wwa_PM_jobs_WP1)
remove(ewa_PM_jobs_WP2)
remove(wwa_PM_jobs_WP2)
remove(ewa_PM_jobs_WP3)
remove(wwa_PM_jobs_WP3)
remove(ewa_PM_jobs_WP4)
remove(wwa_PM_jobs_WP4)



writeRaster(wa_jobs, "./data/Jobs/all_WP_layered.tif", overwrite = TRUE)
#reloading combined jobs data ----
wa_jobs <- rast("./data/Jobs/all_WP_layered.tif")

wa_jobs

#driving data ----
wwa_jobs_driving <- rast("./data/Jobs/wwa_jobs_car_P50_C45.tif")
ewa_jobs_driving <- rast("./data/Jobs/ewa_jobs_car_P50_C45.tif")

#combine e + w regions
wa_jobs_driving <- merge(ewa_jobs_driving, wwa_jobs_driving)

writeRaster(wa_jobs_driving, "./data/Jobs/statewide_driving_jobs.tif", overwrite = TRUE)
wa_jobs_driving <- rast("./data/Jobs/statewide_driving_jobs.tif")

#walk only - WP1 ----
ewa_jobs_walk1 <- rast("./data/Jobs/ewa_jobs_walk_WP1.tif")
wwa_jobs_walk1 <- rast("./data/Jobs/wwa_jobs_walk_WP1.tif")

wa_jobs_walk1 <- mosaic(ewa_jobs_walk1, wwa_jobs_walk1) 


ewa_jobs_walk2 <- rast("./data/Jobs/ewa_jobs_walk_WP2.tif")
wwa_jobs_walk2 <- rast("./data/Jobs/wwa_jobs_walk_WP2.tif")

wa_jobs_walk2 <- mosaic(ewa_jobs_walk2, wwa_jobs_walk2)

wa_jobs_walk2[wa_jobs_walk2 > wa_jobs_walk1] <- 0
wa_jobs_walk1[[2]][wa_jobs_walk1[[2]] > wa_jobs[[4]]] <- 0

varnames(wa_jobs_walk1) <- "Jobs_Walk1"

wa_jobs_walk1 <- c(wa_jobs_walk1, wa_jobs_walk2)



#will want to add other WPs
writeRaster(wa_jobs_walk1, "./data/Jobs/wa_jobs_walk.tif", overwrite = TRUE)
wa_jobs_walk1 <- rast("./data/Jobs/wa_jobs_walk.tif")

summary(wa_jobs)


wa_jobs_walk1

#comparisons -----
# wa_WP1_AM_vs_PM_jobs <- terra::ifel(wa_AM_jobs_WP1 == 0, 0, (wa_PM_jobs_WP1/ wa_AM_jobs_WP1))
# wa_WP1_AM_vs_PM_jobs
# wa_WP2_AM_vs_WP1_AM_jobs <- terra::ifel(wa_AM_jobs_WP1 == 0, 0, (wa_AM_jobs_WP2/ wa_AM_jobs_WP1))
# wa_WP3_AM_vs_WP1_AM_jobs <- terra::ifel(wa_AM_jobs_WP1 == 0, 0, (wa_AM_jobs_WP3/ wa_AM_jobs_WP1))
# wa_WP4_AM_vs_WP1_AM_jobs <- terra::ifel(wa_AM_jobs_WP1 == 0, 0, (wa_AM_jobs_WP4/ wa_AM_jobs_WP1))
# wa_WP1_AM_vs_Car_jobs <- terra::ifel(wa_jobs_driving == 0, 0, (wa_AM_jobs_WP1/ wa_jobs_driving))
# wa_WP1_AM_vs_Car_jobs

wa_jobs <- rast("./data/Jobs/all_WP_layered.tif")
wa_jobs_driving <- rast("./data/Jobs/statewide_driving_jobs.tif")
wa_jobs_walk1 <- rast("./data/Jobs/wa_jobs_walk.tif")

wa_jobs_combined <- c(wa_jobs, wa_jobs_driving, wa_jobs_walk1)

names(wa_jobs_combined)
# 2 - WP1 PM
# 4 - WP2 PM
# 6 - WP3 PM
# 8 - WP4 PM
# 9 - car
# 10 - WP1 walk

wa_jobs_combined[[2]]
wa_jobs_combined[[4]]
wa_jobs_combined[[6]]
wa_jobs_combined[[8]]
wa_jobs_combined[[9]]
wa_jobs_combined[[10]]

wa_PM2_PM1 <- terra::ifel(wa_jobs_combined[[2]] == 0, NaN, (wa_jobs_combined[[4]]/ wa_jobs_combined[[2]]))
wa_PM3_PM1 <- terra::ifel(wa_jobs_combined[[2]] == 0, NaN, (wa_jobs_combined[[6]]/ wa_jobs_combined[[2]]))
wa_PM4_PM1 <- terra::ifel(wa_jobs_combined[[2]] == 0, NaN, (wa_jobs_combined[[8]]/ wa_jobs_combined[[2]]))
wa_WK1_PM1 <- terra::ifel(wa_jobs_combined[[2]] == 0, NaN, (wa_jobs_combined[[10]]/ wa_jobs_combined[[2]]))
wa_PM1_car <- terra::ifel(wa_jobs_combined[[9]] == 0, NaN, (wa_jobs_combined[[2]]/ wa_jobs_combined[[9]]))

wa_PM1_car

wa_jobs_comps <- c(wa_PM2_PM1, wa_PM3_PM1,wa_PM4_PM1, wa_WK1_PM1, wa_PM1_car)

tmap_mode("view")

mapview(wa_jobs_combined[[10]]) +
  mapview(wa_jobs_combined[[2]]) +
  mapview(wa_WK1_PM1) 

