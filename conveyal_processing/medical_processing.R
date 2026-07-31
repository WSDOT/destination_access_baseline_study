#This script loads in and transforms Conveyal geotiff regional analysis export data for WSDOT analyses for Hospitals and other medical category destinations. The files are loaded in as spatrasters using terra. Each medical destination category is then weighted to a binary value of 1 if the destination is reachable or a 0 is retained (additional counts of reachable destinations are not retained). Next, each category is assigned a weighted value to align with the aggregation value, 5 for hospitals and 1 for each of the other destination types. East and west regional analyses are merged together using terra mosaic. 
#This could have been done more efficiently with functions and aggregating layers that would undergo the same transformation earlier on. Retaining original code for visibility. 

library(tidyverse)
library(terra) #using rast keeps resulting raster files compressed, raster does not. Removes need for re-compressed file after compute

setwd("~/Projects/R Explore/Conveyal Exports")
getwd()

#Hospitals ----
#WP1
wwa_hospitals_WP1 <- rast("./data/Medical/W_WA_hospitals_WP1.tif")
ewa_hospitals_WP1 <- rast("./data/Medical/E_WA_hospitals_WP1.tif")


#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
#need to evaluate decay functions ability to give outputs at low destination counts. Do rasters from Conveyal get to decimals? 
wwa_hospitals_WP1[wwa_hospitals_WP1 > 1] <- 1
ewa_hospitals_WP1[ewa_hospitals_WP1 > 1] <- 1

wwa_hospitals_WP1[wwa_hospitals_WP1] <- wwa_hospitals_WP1 * 5
ewa_hospitals_WP1[ewa_hospitals_WP1] <- ewa_hospitals_WP1 * 5

freq(wwa_hospitals_WP1)

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_hospitals_WP1 <- mosaic(wwa_hospitals_WP1, ewa_hospitals_WP1)

#WP2
wwa_hospitals_WP2 <- rast("./data/Medical/W_WA_hospitals_WP2.tif")
ewa_hospitals_WP2 <- rast("./data/Medical/E_WA_hospitals_WP2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_hospitals_WP2[wwa_hospitals_WP2 > 1] <- 1
ewa_hospitals_WP2[ewa_hospitals_WP2 > 1] <- 1

wwa_hospitals_WP2[wwa_hospitals_WP2] <- wwa_hospitals_WP2 * 5
ewa_hospitals_WP2[ewa_hospitals_WP2] <- ewa_hospitals_WP2 * 5

#mosaic datasets - combines two spatially overlapping rasters.  
wa_hospitals_WP2 <- mosaic(wwa_hospitals_WP2, ewa_hospitals_WP2)

#WP3
wwa_hospitals_WP3 <- rast("./data/Medical/W_WA_hospitals_WP3.tif")
ewa_hospitals_WP3 <- rast("./data/Medical/E_WA_hospitals_WP3.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_hospitals_WP3[wwa_hospitals_WP3 > 1] <- 1
ewa_hospitals_WP3[ewa_hospitals_WP3 > 1] <- 1

wwa_hospitals_WP3[wwa_hospitals_WP3] <- wwa_hospitals_WP3 * 5
ewa_hospitals_WP3[ewa_hospitals_WP3] <- ewa_hospitals_WP3 * 5

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_hospitals_WP3 <- mosaic(wwa_hospitals_WP3, ewa_hospitals_WP3)

#WP4
wwa_hospitals_WP4 <- rast("./data/Medical/W_WA_hospitals_WP4.tif")
ewa_hospitals_WP4 <- rast("./data/Medical/E_WA_hospitals_WP4.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_hospitals_WP4[wwa_hospitals_WP4 > 1] <- 1
ewa_hospitals_WP4[ewa_hospitals_WP4 > 1] <- 1

wwa_hospitals_WP4[wwa_hospitals_WP4] <- wwa_hospitals_WP4 * 5
ewa_hospitals_WP4[ewa_hospitals_WP4] <- ewa_hospitals_WP4 * 5

wwa_hospitals_WP4
ewa_hospitals_WP4

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_hospitals_WP4 <- mosaic(wwa_hospitals_WP4, ewa_hospitals_WP4)

#Car
wwa_hospitals_car <- rast("./data/Medical/W_WA_hospitals_car.tif")
ewa_hospitals_car <- rast("./data/Medical/E_WA_hospitals_car.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_hospitals_car[wwa_hospitals_car > 1] <- 1
ewa_hospitals_car[ewa_hospitals_car > 1] <- 1

wwa_hospitals_car[wwa_hospitals_car] <- wwa_hospitals_car * 5
ewa_hospitals_car[ewa_hospitals_car] <- ewa_hospitals_car * 5

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_hospitals_car <- mosaic(wwa_hospitals_car, ewa_hospitals_car)
wa_hospitals_car

#walk1
wwa_hospitals_walk1 <- rast("./data/Medical/W_WA_hospitals_Walk1.tif")
ewa_hospitals_walk1 <- rast("./data/Medical/E_WA_hospitals_Walk1.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_hospitals_walk1[wwa_hospitals_walk1 > 1] <- 1
ewa_hospitals_walk1[ewa_hospitals_walk1 > 1] <- 1

wwa_hospitals_walk1[wwa_hospitals_walk1] <- wwa_hospitals_walk1 * 5
ewa_hospitals_walk1[ewa_hospitals_walk1] <- ewa_hospitals_walk1 * 5

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_hospitals_walk1 <- mosaic(wwa_hospitals_walk1, ewa_hospitals_walk1)
wa_hospitals_walk1

#walk2
wwa_hospitals_walk2 <- rast("./data/Medical/W_WA_hospitals_Walk2.tif")
ewa_hospitals_walk2 <- rast("./data/Medical/E_WA_hospitals_Walk2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_hospitals_walk2[wwa_hospitals_walk2 > 1] <- 1
ewa_hospitals_walk2[ewa_hospitals_walk2 > 1] <- 1

wwa_hospitals_walk2[wwa_hospitals_walk2] <- wwa_hospitals_walk2 * 5
ewa_hospitals_walk2[ewa_hospitals_walk2] <- ewa_hospitals_walk2 * 5

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_hospitals_walk2 <- mosaic(wwa_hospitals_walk2, ewa_hospitals_walk2)
wa_hospitals_walk2

#create layered datasets 
wa_hospitals <- c(wa_hospitals_WP1, wa_hospitals_WP2, wa_hospitals_WP3, wa_hospitals_WP4, wa_hospitals_car, wa_hospitals_walk1, wa_hospitals_walk2)



#metadata QA/QC
# freq(wa_hospitals_WP2)
# #plots the raster data
# plot(wa_hospitals_WP2)

# other medical destinations ----
# The same normalization and merging is completed for each of the non-hospital destinations.
#dental ----
#
#WP1
wwa_dental_WP1 <- rast("./data/Medical/W_WA_dental_WP1.tif")
ewa_dental_WP1 <- rast("./data/Medical/E_WA_dental_WP1.tif")


wwa_dental_WP1[wwa_dental_WP1 > 1] <- 1
ewa_dental_WP1[ewa_dental_WP1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_dental_WP1 <- mosaic(wwa_dental_WP1, ewa_dental_WP1)

#WP2
wwa_dental_WP2 <- rast("./data/Medical/W_WA_dental_WP2.tif")
ewa_dental_WP2 <- rast("./data/Medical/E_WA_dental_WP2.tif")


wwa_dental_WP2[wwa_dental_WP2 > 1] <- 1
ewa_dental_WP2[ewa_dental_WP2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_dental_WP2 <- mosaic(wwa_dental_WP2, ewa_dental_WP2)

#WP3
wwa_dental_WP3 <- rast("./data/Medical/W_WA_dental_WP3.tif")
ewa_dental_WP3 <- rast("./data/Medical/E_WA_dental_WP3.tif")


wwa_dental_WP3[wwa_dental_WP3 > 1] <- 1
ewa_dental_WP3[ewa_dental_WP3 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_dental_WP3 <- mosaic(wwa_dental_WP3, ewa_dental_WP3)

#WP4
wwa_dental_WP4 <- rast("./data/Medical/W_WA_dental_WP4.tif")
ewa_dental_WP4 <- rast("./data/Medical/E_WA_dental_WP4.tif")


wwa_dental_WP4[wwa_dental_WP4 > 1] <- 1
ewa_dental_WP4[ewa_dental_WP4 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_dental_WP4 <- mosaic(wwa_dental_WP4, ewa_dental_WP4)

wa_dental_WP4

#Car
wwa_dental_car <- rast("./data/Medical/W_WA_dental_car.tif")
ewa_dental_car <- rast("./data/Medical/E_WA_dental_car.tif")


wwa_dental_car[wwa_dental_car > 1] <- 1
ewa_dental_car[ewa_dental_car > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_dental_car <- mosaic(wwa_dental_car, ewa_dental_car)

#walk1
wwa_dental_walk1 <- rast("./data/Medical/W_WA_dental_Walk1.tif")
ewa_dental_walk1 <- rast("./data/Medical/E_WA_dental_Walk1.tif")


wwa_dental_walk1[wwa_dental_walk1 > 1] <- 1
ewa_dental_walk1[ewa_dental_walk1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_dental_walk1 <- mosaic(wwa_dental_walk1, ewa_dental_walk1)
wa_dental_walk1

#walk2
wwa_dental_walk2 <- rast("./data/Medical/W_WA_dental_Walk2.tif")
ewa_dental_walk2 <- rast("./data/Medical/E_WA_dental_Walk2.tif")


wwa_dental_walk2[wwa_dental_walk2 > 1] <- 1
ewa_dental_walk2[ewa_dental_walk2 > 1] <- 1


#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_dental_walk2 <- mosaic(wwa_dental_walk2, ewa_dental_walk2)
wa_dental_walk2

#create layered datasets 
wa_dental <- c(wa_dental_WP1, wa_dental_WP2, wa_dental_WP3, wa_dental_WP4, wa_dental_car, wa_dental_walk1, wa_dental_walk2)



#pharm ----
#WP1
wwa_pharm_WP1 <- rast("./data/Medical/W_WA_pharm_WP1.tif")
ewa_pharm_WP1 <- rast("./data/Medical/E_WA_pharm_WP1.tif")


wwa_pharm_WP1[wwa_pharm_WP1 > 1] <- 1
ewa_pharm_WP1[ewa_pharm_WP1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_pharm_WP1 <- mosaic(wwa_pharm_WP1, ewa_pharm_WP1)

#WP2
wwa_pharm_WP2 <- rast("./data/Medical/W_WA_pharm_WP2.tif")
ewa_pharm_WP2 <- rast("./data/Medical/E_WA_pharm_WP2.tif")


wwa_pharm_WP2[wwa_pharm_WP2 > 1] <- 1
ewa_pharm_WP2[ewa_pharm_WP2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_pharm_WP2 <- mosaic(wwa_pharm_WP2, ewa_pharm_WP2)

#WP3
wwa_pharm_WP3 <- rast("./data/Medical/W_WA_pharm_WP3.tif")
ewa_pharm_WP3 <- rast("./data/Medical/E_WA_pharm_WP3.tif")


wwa_pharm_WP3[wwa_pharm_WP3 > 1] <- 1
ewa_pharm_WP3[ewa_pharm_WP3 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_pharm_WP3 <- mosaic(wwa_pharm_WP3, ewa_pharm_WP3)

#WP4
wwa_pharm_WP4 <- rast("./data/Medical/W_WA_pharm_WP4.tif")
ewa_pharm_WP4 <- rast("./data/Medical/E_WA_pharm_WP4.tif")


wwa_pharm_WP4[wwa_pharm_WP4 > 1] <- 1
ewa_pharm_WP4[ewa_pharm_WP4 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_pharm_WP4 <- mosaic(wwa_pharm_WP4, ewa_pharm_WP4)


#Car
wwa_pharm_car <- rast("./data/Medical/W_WA_pharm_car.tif")
ewa_pharm_car <- rast("./data/Medical/E_WA_pharm_car.tif")


wwa_pharm_car[wwa_pharm_car > 1] <- 1
ewa_pharm_car[ewa_pharm_car > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_pharm_car <- mosaic(wwa_pharm_car, ewa_pharm_car)

#walk1
wwa_pharm_walk1 <- rast("./data/Medical/W_WA_pharm_Walk1.tif")
ewa_pharm_walk1 <- rast("./data/Medical/E_WA_pharm_Walk1.tif")


wwa_pharm_walk1[wwa_pharm_walk1 > 1] <- 1
ewa_pharm_walk1[ewa_pharm_walk1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_pharm_walk1 <- mosaic(wwa_pharm_walk1, ewa_pharm_walk1)
wa_pharm_walk1

#walk2
wwa_pharm_walk2 <- rast("./data/Medical/W_WA_pharm_Walk2.tif")
ewa_pharm_walk2 <- rast("./data/Medical/E_WA_pharm_Walk2.tif")


wwa_pharm_walk2[wwa_pharm_walk2 > 1] <- 1
ewa_pharm_walk2[ewa_pharm_walk2 > 1] <- 1


#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_pharm_walk2 <- mosaic(wwa_pharm_walk2, ewa_pharm_walk2)
wa_pharm_walk2

#create layered datasets 
wa_pharm <- c(wa_pharm_WP1, wa_pharm_WP2, wa_pharm_WP3, wa_pharm_WP4, wa_pharm_car, wa_pharm_walk1, wa_pharm_walk2)


#primary/urgent/family medical ----
#WP1
wwa_primary_WP1 <- rast("./data/Medical/W_WA_primary_WP1.tif")
ewa_primary_WP1 <- rast("./data/Medical/E_WA_primary_WP1.tif")


wwa_primary_WP1[wwa_primary_WP1 > 1] <- 1
ewa_primary_WP1[ewa_primary_WP1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_primary_WP1 <- mosaic(wwa_primary_WP1, ewa_primary_WP1)

#WP2
wwa_primary_WP2 <- rast("./data/Medical/W_WA_primary_WP2.tif")
ewa_primary_WP2 <- rast("./data/Medical/E_WA_primary_WP2.tif")


wwa_primary_WP2[wwa_primary_WP2 > 1] <- 1
ewa_primary_WP2[ewa_primary_WP2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_primary_WP2 <- mosaic(wwa_primary_WP2, ewa_primary_WP2)

#WP3
wwa_primary_WP3 <- rast("./data/Medical/W_WA_primary_WP3.tif")
ewa_primary_WP3 <- rast("./data/Medical/E_WA_primary_WP3.tif")


wwa_primary_WP3[wwa_primary_WP3 > 1] <- 1
ewa_primary_WP3[ewa_primary_WP3 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_primary_WP3 <- mosaic(wwa_primary_WP3, ewa_primary_WP3)

#WP4
wwa_primary_WP4 <- rast("./data/Medical/W_WA_primary_WP4.tif")
ewa_primary_WP4 <- rast("./data/Medical/E_WA_primary_WP4.tif")


wwa_primary_WP4[wwa_primary_WP4 > 1] <- 1
ewa_primary_WP4[ewa_primary_WP4 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_primary_WP4 <- mosaic(wwa_primary_WP4, ewa_primary_WP4)

#Car
wwa_primary_car <- rast("./data/Medical/W_WA_primary_car.tif")
ewa_primary_car <- rast("./data/Medical/E_WA_primary_car.tif")


wwa_primary_car[wwa_primary_car > 1] <- 1
ewa_primary_car[ewa_primary_car > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_primary_car <- mosaic(wwa_primary_car, ewa_primary_car)

#walk1
wwa_primary_walk1 <- rast("./data/Medical/W_WA_primary_Walk1.tif")
ewa_primary_walk1 <- rast("./data/Medical/E_WA_primary_Walk1.tif")


wwa_primary_walk1[wwa_primary_walk1 > 1] <- 1
ewa_primary_walk1[ewa_primary_walk1 > 1] <- 1


#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_primary_walk1 <- mosaic(wwa_primary_walk1, ewa_primary_walk1)
wa_primary_walk1

#walk2
wwa_primary_walk2 <- rast("./data/Medical/W_WA_primary_Walk2.tif")
ewa_primary_walk2 <- rast("./data/Medical/E_WA_primary_Walk2.tif")


wwa_primary_walk2[wwa_primary_walk2 > 1] <- 1
ewa_primary_walk2[ewa_primary_walk2 > 1] <- 1


#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_primary_walk2 <- mosaic(wwa_primary_walk2, ewa_primary_walk2)
wa_primary_walk2

#create layered datasets 
wa_primary <- c(wa_primary_WP1, wa_primary_WP2, wa_primary_WP3, wa_primary_WP4, wa_primary_car, wa_primary_walk1, wa_primary_walk2)



#specialty ----
#WP1
wwa_specialty_WP1 <- rast("./data/Medical/W_WA_specialty_WP1.tif")
ewa_specialty_WP1 <- rast("./data/Medical/E_WA_specialty_WP1.tif")


wwa_specialty_WP1[wwa_specialty_WP1 > 1] <- 1
ewa_specialty_WP1[ewa_specialty_WP1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_specialty_WP1 <- mosaic(wwa_specialty_WP1, ewa_specialty_WP1)

#WP2
wwa_specialty_WP2 <- rast("./data/Medical/W_WA_specialty_WP2.tif")
ewa_specialty_WP2 <- rast("./data/Medical/E_WA_specialty_WP2.tif")


wwa_specialty_WP2[wwa_specialty_WP2 > 1] <- 1
ewa_specialty_WP2[ewa_specialty_WP2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_specialty_WP2 <- mosaic(wwa_specialty_WP2, ewa_specialty_WP2)

#WP3
wwa_specialty_WP3 <- rast("./data/Medical/W_WA_specialty_WP3.tif")
ewa_specialty_WP3 <- rast("./data/Medical/E_WA_specialty_WP3.tif")


wwa_specialty_WP3[wwa_specialty_WP3 > 1] <- 1
ewa_specialty_WP3[ewa_specialty_WP3 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_specialty_WP3 <- mosaic(wwa_specialty_WP3, ewa_specialty_WP3)

#WP4
wwa_specialty_WP4 <- rast("./data/Medical/W_WA_specialty_WP4.tif")
ewa_specialty_WP4 <- rast("./data/Medical/E_WA_specialty_WP4.tif")


wwa_specialty_WP4[wwa_specialty_WP4 > 1] <- 1
ewa_specialty_WP4[ewa_specialty_WP4 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_specialty_WP4 <- mosaic(wwa_specialty_WP4, ewa_specialty_WP4)

#Car
wwa_specialty_car <- rast("./data/Medical/W_WA_specialty_car.tif")
ewa_specialty_car <- rast("./data/Medical/E_WA_specialty_car.tif")


wwa_specialty_car[wwa_specialty_car > 1] <- 1
ewa_specialty_car[ewa_specialty_car > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_specialty_car <- mosaic(wwa_specialty_car, ewa_specialty_car)

#walk1
wwa_specialty_walk1 <- rast("./data/Medical/W_WA_specialty_Walk1.tif")
ewa_specialty_walk1 <- rast("./data/Medical/E_WA_specialty_Walk1.tif")


wwa_specialty_walk1[wwa_specialty_walk1 > 1] <- 1
ewa_specialty_walk1[ewa_specialty_walk1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_specialty_walk1 <- mosaic(wwa_specialty_walk1, ewa_specialty_walk1)
wa_specialty_walk1

#walk2
wwa_specialty_walk2 <- rast("./data/Medical/W_WA_specialty_Walk2.tif")
ewa_specialty_walk2 <- rast("./data/Medical/E_WA_specialty_Walk2.tif")


wwa_specialty_walk2[wwa_specialty_walk2 > 1] <- 1
ewa_specialty_walk2[ewa_specialty_walk2 > 1] <- 1


#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_specialty_walk2 <- mosaic(wwa_specialty_walk2, ewa_specialty_walk2)
wa_specialty_walk2

#create layered datasets 
wa_specialty <- c(wa_specialty_WP1, wa_specialty_WP2, wa_specialty_WP3, wa_specialty_WP4, wa_specialty_car, wa_specialty_walk1, wa_specialty_walk2)


#vision ----
#WP1
wwa_vision_WP1 <- rast("./data/Medical/W_WA_vision_WP1.tif")
ewa_vision_WP1 <- rast("./data/Medical/E_WA_vision_WP1.tif")
wwa_vision_WP1[wwa_vision_WP1 > 1] <- 1
ewa_vision_WP1[ewa_vision_WP1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_vision_WP1 <- mosaic(wwa_vision_WP1, ewa_vision_WP1)

#WP2
wwa_vision_WP2 <- rast("./data/Medical/W_WA_vision_WP2.tif")
ewa_vision_WP2 <- rast("./data/Medical/E_WA_vision_WP2.tif")


wwa_vision_WP2[wwa_vision_WP2 > 1] <- 1
ewa_vision_WP2[ewa_vision_WP2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_vision_WP2 <- mosaic(wwa_vision_WP2, ewa_vision_WP2)

#WP3
wwa_vision_WP3 <- rast("./data/Medical/W_WA_vision_WP3.tif")
ewa_vision_WP3 <- rast("./data/Medical/E_WA_vision_WP3.tif")


wwa_vision_WP3[wwa_vision_WP3 > 1] <- 1
ewa_vision_WP3[ewa_vision_WP3 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_vision_WP3 <- mosaic(wwa_vision_WP3, ewa_vision_WP3)

#WP4
wwa_vision_WP4 <- rast("./data/Medical/W_WA_vision_WP4.tif")
ewa_vision_WP4 <- rast("./data/Medical/E_WA_vision_WP4.tif")


wwa_vision_WP4[wwa_vision_WP4 > 1] <- 1
ewa_vision_WP4[ewa_vision_WP4 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_vision_WP4 <- mosaic(wwa_vision_WP4, ewa_vision_WP4)

#Car
wwa_vision_car <- rast("./data/Medical/W_WA_vision_car.tif")
ewa_vision_car <- rast("./data/Medical/E_WA_vision_car.tif")


wwa_vision_car[wwa_vision_car > 1] <- 1
ewa_vision_car[ewa_vision_car > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_vision_car <- mosaic(wwa_vision_car, ewa_vision_car)

#walk1
wwa_vision_walk1 <- rast("./data/Medical/W_WA_vision_Walk1.tif")
ewa_vision_walk1 <- rast("./data/Medical/E_WA_vision_Walk1.tif")


wwa_vision_walk1[wwa_vision_walk1 > 1] <- 1
ewa_vision_walk1[ewa_vision_walk1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_vision_walk1 <- mosaic(wwa_vision_walk1, ewa_vision_walk1)
wa_vision_walk1

#walk2
wwa_vision_walk2 <- rast("./data/Medical/W_WA_vision_Walk2.tif")
ewa_vision_walk2 <- rast("./data/Medical/E_WA_vision_Walk2.tif")


wwa_vision_walk2[wwa_vision_walk2 > 1] <- 1
ewa_vision_walk2[ewa_vision_walk2 > 1] <- 1


#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_vision_walk2 <- mosaic(wwa_vision_walk2, ewa_vision_walk2)
wa_vision_walk2

#create layered datasets 
wa_vision <- c(wa_vision_WP1, wa_vision_WP2, wa_vision_WP3, wa_vision_WP4, wa_vision_car, wa_vision_walk1, wa_vision_walk2)



wa_vision
#correction for Conveyal skipping ----
#if a slower or more walking network constrained walking profile outperformed faster/less constrained walking profiles, origin raster skipping (origin moves towards closest raster with functional network w/n certain radius at walking pace) is implied. We correct for this by setting those values to 0, as the absence of a routable network segment within the raster implies that the raster should not be navigable. 
wa_vision[[2]][wa_vision[[4]] < wa_vision[[2]]]<- 0 #if walking profile 2 outperforms walking profile 4, the values is set to 0
wa_vision[[3]][wa_vision[[1]] < wa_vision[[3]]]<- 0  #if walking profile 3 outperforms walking profile 1, the values is set to 0
wa_vision[[7]][wa_vision[[6]] < wa_vision[[7]]]<- 0  #if walking profile 2 outperforms walking profile 1, the values is set to 0

wa_vision

wa_hospitals[[2]][wa_hospitals[[4]] < wa_hospitals[[2]]]<- 0
wa_hospitals[[3]][wa_hospitals[[1]] < wa_hospitals[[3]]]<- 0
wa_hospitals[[7]][wa_hospitals[[6]] < wa_hospitals[[3]]]<- 0

wa_pharm[[2]][wa_pharm[[4]] < wa_pharm[[2]]]<- 0
wa_pharm[[3]][wa_pharm[[1]] < wa_pharm[[3]]]<- 0
wa_pharm[[7]][wa_pharm[[6]] < wa_pharm[[7]]]<- 0

wa_primary[[2]][wa_primary[[4]] < wa_primary[[2]]]<- 0
wa_primary[[3]][wa_primary[[1]] < wa_primary[[3]]]<- 0
wa_primary[[7]][wa_primary[[6]] < wa_primary[[7]]]<- 0

wa_dental[[2]][wa_dental[[4]] < wa_dental[[2]]]<- 0
wa_dental[[3]][wa_dental[[1]] < wa_dental[[3]]]<- 0
wa_dental[[7]][wa_dental[[6]] < wa_dental[[7]]]<- 0

wa_specialty[[2]][wa_specialty[[4]] < wa_specialty[[2]]]<- 0
wa_specialty[[3]][wa_specialty[[1]] < wa_specialty[[3]]]<- 0
wa_specialty[[7]][wa_specialty[[6]] < wa_specialty[[7]]]<- 0


#Combining non-hospital medical locations by summing rasters together ----
den_phar <- sum(wa_pharm, wa_dental)
den_phar_vis <- sum(den_phar, wa_vision)
den_phar_vis_prim <- sum(den_phar_vis, wa_primary)
wa_non_hosp_med<- sum(den_phar_vis_prim, wa_specialty)

#Total Medical Category ----
wa_medical <- sum(wa_non_hosp_med, wa_hospitals)

freq(wa_medical)
#write output file as tiff 
writeRaster(
  wa_medical, 
  "./data/Medical/06wa_medical.tif",
  overwrite = TRUE
)

#option to load layered files ----

wa_hospitals <- rast("./data/Medical/0wa_hospitals.tif")
wa_dental <- rast("./data/Medical/0wa_dental.tif")
wa_vision <- rast("./data/Medical/0wa_vision.tif")
wa_pharm <- rast("./data/Medical/0wa_pharm.tif")
wa_primary <- rast("./data/Medical/0wa_primary.tif")
wa_specialty <- rast("./data/Medical/0wa_specialty.tif")

#layer number is aligned with walking profile
wa_medical <- rast("./data/Medical/06wa_medical.tif") #still out of 10


#correction for WP2 walk only skipping ----
summary(wa_medical)
wa_medical[[7]][wa_medical[[7]] > wa_medical[[2]]] <- 0

#Commenting out reload option for data layer number is aligned with walking profile - values are 0 - 5 or 0 - 1 whether access to med dest is available. 
# wa_non_hosp_med <- rast("./output/wa_non_hosp_med.tif")
# wa_medical <- rast("./data/Medical/wa_medical.tif")




remove(wa_pharm)
remove(wa_pharm_car)
remove(wa_pharm_walk1)
remove(wa_pharm_walk2)
remove(wa_pharm_WP1)
remove(wa_pharm_WP2)
remove(wa_pharm_WP3)
remove(wa_pharm_WP4)
remove(ewa_pharm_car)
remove(ewa_pharm_walk1)
remove(ewa_pharm_walk2)
remove(ewa_pharm_WP1)
remove(ewa_pharm_WP2)
remove(ewa_pharm_WP3)
remove(ewa_pharm_WP4)
remove(wwa_pharm_car)
remove(wwa_pharm_walk1)
remove(wwa_pharm_walk2)
remove(wwa_pharm_WP1)
remove(wwa_pharm_WP2)
remove(wwa_pharm_WP3)
remove(wwa_pharm_WP4)

remove(wa_primary)
remove(wa_primary_car)
remove(wa_primary_walk1)
remove(wa_primary_walk2)
remove(wa_primary_WP1)
remove(wa_primary_WP2)
remove(wa_primary_WP3)
remove(wa_primary_WP4)
remove(ewa_primary_car)
remove(ewa_primary_walk1)
remove(ewa_primary_walk2)
remove(ewa_primary_WP1)
remove(ewa_primary_WP2)
remove(ewa_primary_WP3)
remove(ewa_primary_WP4)
remove(wwa_primary_car)
remove(wwa_primary_walk1)
remove(wwa_primary_walk2)
remove(wwa_primary_WP1)
remove(wwa_primary_WP2)
remove(wwa_primary_WP3)
remove(wwa_primary_WP4)

remove(wa_specialty)
remove(wa_specialty_car)
remove(wa_specialty_walk1)
remove(wa_specialty_walk2)
remove(wa_specialty_WP1)
remove(wa_specialty_WP2)
remove(wa_specialty_WP3)
remove(wa_specialty_WP4)
remove(ewa_specialty_car)
remove(ewa_specialty_walk1)
remove(ewa_specialty_walk2)
remove(ewa_specialty_WP1)
remove(ewa_specialty_WP2)
remove(ewa_specialty_WP3)
remove(ewa_specialty_WP4)
remove(wwa_specialty_car)
remove(wwa_specialty_walk1)
remove(wwa_specialty_walk2)
remove(wwa_specialty_WP1)
remove(wwa_specialty_WP2)
remove(wwa_specialty_WP3)
remove(wwa_specialty_WP4)

remove(wa_vision)
remove(wa_vision_car)
remove(wa_vision_walk1)
remove(wa_vision_walk2)
remove(wa_vision_WP1)
remove(wa_vision_WP2)
remove(wa_vision_WP3)
remove(wa_vision_WP4)
remove(ewa_vision_car)
remove(ewa_vision_walk1)
remove(ewa_vision_walk2)
remove(ewa_vision_WP1)
remove(ewa_vision_WP2)
remove(ewa_vision_WP3)
remove(ewa_vision_WP4)
remove(wwa_vision_car)
remove(wwa_vision_walk1)
remove(wwa_vision_walk2)
remove(wwa_vision_WP1)
remove(wwa_vision_WP2)
remove(wwa_vision_WP3)
remove(wwa_vision_WP4)

remove(wa_dental)
remove(wa_dental_car)
remove(wa_dental_walk1)
remove(wa_dental_walk2)
remove(wa_dental_WP1)
remove(wa_dental_WP2)
remove(wa_dental_WP3)
remove(wa_dental_WP4)
remove(ewa_dental_car)
remove(ewa_dental_walk1)
remove(ewa_dental_walk2)
remove(ewa_dental_WP1)
remove(ewa_dental_WP2)
remove(ewa_dental_WP3)
remove(ewa_dental_WP4)
remove(wwa_dental_car)
remove(wwa_dental_walk1)
remove(wwa_dental_walk2)
remove(wwa_dental_WP1)
remove(wwa_dental_WP2)
remove(wwa_dental_WP3)
remove(wwa_dental_WP4)

remove(wa_hospitals)
remove(wa_hospitals_car)
remove(wa_hospitals_walk1)
remove(wa_hospitals_walk2)
remove(wa_hospitals_WP1)
remove(wa_hospitals_WP2)
remove(wa_hospitals_WP3)
remove(wa_hospitals_WP4)
remove(ewa_hospitals_car)
remove(ewa_hospitals_walk1)
remove(ewa_hospitals_walk2)
remove(ewa_hospitals_WP1)
remove(ewa_hospitals_WP2)
remove(ewa_hospitals_WP3)
remove(ewa_hospitals_WP4)
remove(wwa_hospitals_car)
remove(wwa_hospitals_walk1)
remove(wwa_hospitals_walk2)
remove(wwa_hospitals_WP1)
remove(wwa_hospitals_WP2)
remove(wwa_hospitals_WP3)
remove(wwa_hospitals_WP4)


