#This script loads in conveyal results for the WSDOT Community Support Services (CSS) destination category, merges results from eastern and western Washington into a single layer for each walking profile, and including each walking profile as a layer within one SpatRaster file. 

library(sf)
library(terra)
library(tidyverse)
library(mapview)

wa_CSS <- rast("./data/CSS/wa_CSS.tif")


setwd("~/Projects/R Explore/Conveyal Exports")
getwd()

#data checks
# freq(wa_hospitals_WP2)
# #plots the raster data
# plot(wa_hospitals_WP2)

#senior ----
#WP1
wwa_senior_WP1 <- rast("./data/CSS/W_WA_senior_WP1.tif")
ewa_senior_WP1 <- rast("./data/CSS/E_WA_senior_WP1.tif")

#convert count into values of 1 and 0 - set to 1 for later weigting 
wwa_senior_WP1[wwa_senior_WP1 > 1] <- 1
ewa_senior_WP1[ewa_senior_WP1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_senior_WP1 <- mosaic(wwa_senior_WP1, ewa_senior_WP1)

# #set binary mask for cropping other rasters to match
# binary_mask <- wa_senior_WP1
# binary_mask[binary_mask>0] <- TRUE 
# binary_mask[binary_mask == 0] <- FALSE
# 
# mapview(binary_mask)

#WP2
wwa_senior_WP2 <- rast("./data/CSS/W_WA_senior_WP2.tif")
ewa_senior_WP2 <- rast("./data/CSS/E_WA_senior_WP2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_senior_WP2[wwa_senior_WP2 > 1] <- 1
ewa_senior_WP2[ewa_senior_WP2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_senior_WP2 <- mosaic(wwa_senior_WP2, ewa_senior_WP2)

#WP3
wwa_senior_WP3 <- rast("./data/CSS/W_WA_senior_WP3.tif")
ewa_senior_WP3 <- rast("./data/CSS/E_WA_senior_WP3.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_senior_WP3[wwa_senior_WP3 > 1] <- 1
ewa_senior_WP3[ewa_senior_WP3 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_senior_WP3 <- mosaic(wwa_senior_WP3, ewa_senior_WP3)

#WP4
wwa_senior_WP4 <- rast("./data/CSS/W_WA_senior_WP4.tif")
ewa_senior_WP4 <- rast("./data/CSS/E_WA_senior_WP4.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_senior_WP4[wwa_senior_WP4 > 1] <- 1
ewa_senior_WP4[ewa_senior_WP4 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_senior_WP4 <- mosaic(wwa_senior_WP4, ewa_senior_WP4)

#Car - Senior
wwa_senior_car <- rast("./data/CSS/W_WA_senior_car.tif")
ewa_senior_car <- rast("./data/CSS/E_WA_senior_car.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_senior_car[wwa_senior_car > 1] <- 1
ewa_senior_car[ewa_senior_car > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_senior_car <- mosaic(wwa_senior_car, ewa_senior_car)

#walk1 - Senior
wwa_senior_Walk1 <- rast("./data/CSS/W_WA_senior_Walk1.tif")
ewa_senior_Walk1 <- rast("./data/CSS/E_WA_senior_Walk1.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_senior_Walk1[wwa_senior_Walk1 > 1] <- 1
ewa_senior_Walk1[ewa_senior_Walk1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_senior_Walk1 <- mosaic(wwa_senior_Walk1, ewa_senior_Walk1)

#walk2
wwa_senior_Walk2 <- rast("./data/CSS/W_WA_senior_Walk2.tif")
ewa_senior_Walk2 <- rast("./data/CSS/E_WA_senior_Walk2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_senior_Walk2[wwa_senior_Walk2 > 1] <- 1
ewa_senior_Walk2[ewa_senior_Walk2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_senior_Walk2 <- mosaic(wwa_senior_Walk2, ewa_senior_Walk2)


#crop to align raster sizes
wa_senior_WP2 <- crop(wa_senior_WP2, wa_senior_WP1, mask = TRUE)
wa_senior_WP3 <- crop(wa_senior_WP3, wa_senior_WP1, mask = TRUE)
wa_senior_WP4 <- crop(wa_senior_WP4, wa_senior_WP1, mask = TRUE)


#create layered datasets 
wa_senior <- c(wa_senior_WP1, wa_senior_WP2, wa_senior_WP3, wa_senior_WP4, wa_senior_car, wa_senior_Walk1, wa_senior_Walk2)



#bank ----
#WP1
wwa_bank_WP1 <- rast("./data/CSS/W_WA_bank_WP1.tif")
ewa_bank_WP1 <- rast("./data/CSS/E_WA_bank_WP1.tif")

#convert count into values of 1 and 0 - set to 1 for later weigting 
wwa_bank_WP1[wwa_bank_WP1 > 1] <- 1
ewa_bank_WP1[ewa_bank_WP1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_bank_WP1 <- mosaic(wwa_bank_WP1, ewa_bank_WP1)

#WP2
wwa_bank_WP2 <- rast("./data/CSS/W_WA_bank_WP2.tif")
ewa_bank_WP2 <- rast("./data/CSS/E_WA_bank_WP2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_bank_WP2[wwa_bank_WP2 > 1] <- 1
ewa_bank_WP2[ewa_bank_WP2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_bank_WP2 <- mosaic(wwa_bank_WP2, ewa_bank_WP2)

#WP3
wwa_bank_WP3 <- rast("./data/CSS/W_WA_bank_WP3.tif")
ewa_bank_WP3 <- rast("./data/CSS/E_WA_bank_WP3.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_bank_WP3[wwa_bank_WP3 > 1] <- 1
ewa_bank_WP3[ewa_bank_WP3 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_bank_WP3 <- mosaic(wwa_bank_WP3, ewa_bank_WP3)

#WP4
wwa_bank_WP4 <- rast("./data/CSS/W_WA_bank_WP4.tif")
ewa_bank_WP4 <- rast("./data/CSS/E_WA_bank_WP4.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_bank_WP4[wwa_bank_WP4 > 1] <- 1
ewa_bank_WP4[ewa_bank_WP4 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_bank_WP4 <- mosaic(wwa_bank_WP4, ewa_bank_WP4)

wa_bank_WP4

#Car - bank
wwa_bank_car <- rast("./data/CSS/W_WA_bank_car.tif")
ewa_bank_car <- rast("./data/CSS/E_WA_bank_car.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_bank_car[wwa_bank_car > 1] <- 1
ewa_bank_car[ewa_bank_car > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_bank_car <- mosaic(wwa_bank_car, ewa_bank_car)

#walk1 - 
wwa_bank_Walk1 <- rast("./data/CSS/W_WA_bank_Walk1.tif")
ewa_bank_Walk1 <- rast("./data/CSS/E_WA_bank_Walk1.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_bank_Walk1[wwa_bank_Walk1 > 1] <- 1
ewa_bank_Walk1[ewa_bank_Walk1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_bank_Walk1 <- mosaic(wwa_bank_Walk1, ewa_bank_Walk1)

#walk2
wwa_bank_Walk2 <- rast("./data/CSS/W_WA_bank_Walk2.tif")
ewa_bank_Walk2 <- rast("./data/CSS/E_WA_bank_Walk2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_bank_Walk2[wwa_bank_Walk2 > 1] <- 1
ewa_bank_Walk2[ewa_bank_Walk2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_bank_Walk2 <- mosaic(wwa_bank_Walk2, ewa_bank_Walk2)

#crop to align raster sizes
wa_bank_WP2 <- crop(wa_bank_WP2, wa_bank_WP1, mask = TRUE)
wa_bank_WP3 <- crop(wa_bank_WP3, wa_bank_WP1, mask = TRUE)
wa_bank_WP4 <- crop(wa_bank_WP4, wa_bank_WP1, mask = TRUE)

#create layered datasets 
wa_bank <- c(wa_bank_WP1, wa_bank_WP2, wa_bank_WP3, wa_bank_WP4, wa_bank_car, wa_bank_Walk1, wa_bank_Walk2)

#bill ----
#WP1
wwa_bill_WP1 <- rast("./data/CSS/W_WA_bill_WP1.tif")
ewa_bill_WP1 <- rast("./data/CSS/E_WA_bill_WP1.tif")

#convert count into values of 1 and 0 - set to 1 for later weigting 
wwa_bill_WP1[wwa_bill_WP1 > 1] <- 1
ewa_bill_WP1[ewa_bill_WP1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_bill_WP1 <- mosaic(wwa_bill_WP1, ewa_bill_WP1)

#WP2
wwa_bill_WP2 <- rast("./data/CSS/W_WA_bill_WP2.tif")
ewa_bill_WP2 <- rast("./data/CSS/E_WA_bill_WP2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_bill_WP2[wwa_bill_WP2 > 1] <- 1
ewa_bill_WP2[ewa_bill_WP2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_bill_WP2 <- mosaic(wwa_bill_WP2, ewa_bill_WP2)

#WP3
wwa_bill_WP3 <- rast("./data/CSS/W_WA_bill_WP3.tif")
ewa_bill_WP3 <- rast("./data/CSS/E_WA_bill_WP3.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_bill_WP3[wwa_bill_WP3 > 1] <- 1
ewa_bill_WP3[ewa_bill_WP3 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_bill_WP3 <- mosaic(wwa_bill_WP3, ewa_bill_WP3)

#WP4
wwa_bill_WP4 <- rast("./data/CSS/W_WA_bill_WP4.tif")
ewa_bill_WP4 <- rast("./data/CSS/E_WA_bill_WP4.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_bill_WP4[wwa_bill_WP4 > 1] <- 1
ewa_bill_WP4[ewa_bill_WP4 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_bill_WP4 <- mosaic(wwa_bill_WP4, ewa_bill_WP4)

wa_bill_WP4

#Car - bill
wwa_bill_car <- rast("./data/CSS/W_WA_bill_car.tif")
ewa_bill_car <- rast("./data/CSS/E_WA_bill_car.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_bill_car[wwa_bill_car > 1] <- 1
ewa_bill_car[ewa_bill_car > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_bill_car <- mosaic(wwa_bill_car, ewa_bill_car)

#walk1 - 
wwa_bill_Walk1 <- rast("./data/CSS/W_WA_bill_Walk1.tif")
ewa_bill_Walk1 <- rast("./data/CSS/E_WA_bill_Walk1.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_bill_Walk1[wwa_bill_Walk1 > 1] <- 1
ewa_bill_Walk1[ewa_bill_Walk1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_bill_Walk1 <- mosaic(wwa_bill_Walk1, ewa_bill_Walk1)

#walk2
wwa_bill_Walk2 <- rast("./data/CSS/W_WA_bill_Walk2.tif")
ewa_bill_Walk2 <- rast("./data/CSS/E_WA_bill_Walk2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_bill_Walk2[wwa_bill_Walk2 > 1] <- 1
ewa_bill_Walk2[ewa_bill_Walk2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_bill_Walk2 <- mosaic(wwa_bill_Walk2, ewa_bill_Walk2)

#crop to align raster sizes
wa_bill_WP2 <- crop(wa_bill_WP2, wa_bill_WP1, mask = TRUE)
wa_bill_WP3 <- crop(wa_bill_WP3, wa_bill_WP1, mask = TRUE)
wa_bill_WP4 <- crop(wa_bill_WP4, wa_bill_WP1, mask = TRUE)


#create layered datasets 
wa_bill <- c(wa_bill_WP1, wa_bill_WP2, wa_bill_WP3, wa_bill_WP4, wa_bill_car, wa_bill_Walk1, wa_bill_Walk2)

#child ----
#WP1
wwa_chld_WP1 <- rast("./data/CSS/W_WA_chld_WP1.tif")
ewa_chld_WP1 <- rast("./data/CSS/E_WA_chld_WP1.tif")

#convert count into values of 1 and 0 - set to 1 for later weigting 
wwa_chld_WP1[wwa_chld_WP1 > 1] <- 1
ewa_chld_WP1[ewa_chld_WP1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_chld_WP1 <- mosaic(wwa_chld_WP1, ewa_chld_WP1)

#WP2
wwa_chld_WP2 <- rast("./data/CSS/W_WA_chld_WP2.tif")
ewa_chld_WP2 <- rast("./data/CSS/E_WA_chld_WP2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_chld_WP2[wwa_chld_WP2 > 1] <- 1
ewa_chld_WP2[ewa_chld_WP2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_chld_WP2 <- mosaic(wwa_chld_WP2, ewa_chld_WP2)

#WP3
wwa_chld_WP3 <- rast("./data/CSS/W_WA_chld_WP3.tif")
ewa_chld_WP3 <- rast("./data/CSS/E_WA_chld_WP3.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_chld_WP3[wwa_chld_WP3 > 1] <- 1
ewa_chld_WP3[ewa_chld_WP3 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_chld_WP3 <- mosaic(wwa_chld_WP3, ewa_chld_WP3)

#WP4
wwa_chld_WP4 <- rast("./data/CSS/W_WA_chld_WP4.tif")
ewa_chld_WP4 <- rast("./data/CSS/E_WA_chld_WP4.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_chld_WP4[wwa_chld_WP4 > 1] <- 1
ewa_chld_WP4[ewa_chld_WP4 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_chld_WP4 <- mosaic(wwa_chld_WP4, ewa_chld_WP4)

wa_chld_WP4

#Car - chld
wwa_chld_car <- rast("./data/CSS/W_WA_chld_car.tif")
ewa_chld_car <- rast("./data/CSS/E_WA_chld_car.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_chld_car[wwa_chld_car > 1] <- 1
ewa_chld_car[ewa_chld_car > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_chld_car <- mosaic(wwa_chld_car, ewa_chld_car)

#walk1 - 
wwa_chld_Walk1 <- rast("./data/CSS/W_WA_chld_Walk1.tif")
ewa_chld_Walk1 <- rast("./data/CSS/E_WA_chld_Walk1.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_chld_Walk1[wwa_chld_Walk1 > 1] <- 1
ewa_chld_Walk1[ewa_chld_Walk1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_chld_Walk1 <- mosaic(wwa_chld_Walk1, ewa_chld_Walk1)

#walk2
wwa_chld_Walk2 <- rast("./data/CSS/W_WA_chld_Walk2.tif")
ewa_chld_Walk2 <- rast("./data/CSS/E_WA_chld_Walk2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_chld_Walk2[wwa_chld_Walk2 > 1] <- 1
ewa_chld_Walk2[ewa_chld_Walk2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_chld_Walk2 <- mosaic(wwa_chld_Walk2, ewa_chld_Walk2)

#crop to align raster sizes
wa_chld_WP2 <- crop(wa_chld_WP2, wa_chld_WP1, mask = TRUE)
wa_chld_WP3 <- crop(wa_chld_WP3, wa_chld_WP1, mask = TRUE)
wa_chld_WP4 <- crop(wa_chld_WP4, wa_chld_WP1, mask = TRUE)


#create layered datasets 
wa_chld <- c(wa_chld_WP1, wa_chld_WP2, wa_chld_WP3, wa_chld_WP4, wa_chld_car, wa_chld_Walk1, wa_chld_Walk2)

#col ----
#WP1
wwa_col_WP1 <- rast("./data/CSS/W_WA_col_WP1.tif")
ewa_col_WP1 <- rast("./data/CSS/E_WA_col_WP1.tif")

#convert count into values of 1 and 0 - set to 1 for later weigting 
wwa_col_WP1[wwa_col_WP1 > 1] <- 1
ewa_col_WP1[ewa_col_WP1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_col_WP1 <- mosaic(wwa_col_WP1, ewa_col_WP1)

#WP2
wwa_col_WP2 <- rast("./data/CSS/W_WA_col_WP2.tif")
ewa_col_WP2 <- rast("./data/CSS/E_WA_col_WP2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_col_WP2[wwa_col_WP2 > 1] <- 1
ewa_col_WP2[ewa_col_WP2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_col_WP2 <- mosaic(wwa_col_WP2, ewa_col_WP2)

#WP3
wwa_col_WP3 <- rast("./data/CSS/W_WA_col_WP3.tif")
ewa_col_WP3 <- rast("./data/CSS/E_WA_col_WP3.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_col_WP3[wwa_col_WP3 > 1] <- 1
ewa_col_WP3[ewa_col_WP3 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_col_WP3 <- mosaic(wwa_col_WP3, ewa_col_WP3)

#WP4
wwa_col_WP4 <- rast("./data/CSS/W_WA_col_WP4.tif")
ewa_col_WP4 <- rast("./data/CSS/E_WA_col_WP4.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_col_WP4[wwa_col_WP4 > 1] <- 1
ewa_col_WP4[ewa_col_WP4 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_col_WP4 <- mosaic(wwa_col_WP4, ewa_col_WP4)


#Car - col
wwa_col_car <- rast("./data/CSS/W_WA_col_car.tif")
ewa_col_car <- rast("./data/CSS/E_WA_col_car.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_col_car[wwa_col_car > 1] <- 1
ewa_col_car[ewa_col_car > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_col_car <- mosaic(wwa_col_car, ewa_col_car)

#walk1 - 
wwa_col_Walk1 <- rast("./data/CSS/W_WA_col_Walk1.tif")
ewa_col_Walk1 <- rast("./data/CSS/E_WA_col_Walk1.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_col_Walk1[wwa_col_Walk1 > 1] <- 1
ewa_col_Walk1[ewa_col_Walk1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_col_Walk1 <- mosaic(wwa_col_Walk1, ewa_col_Walk1)

#walk2
wwa_col_Walk2 <- rast("./data/CSS/W_WA_col_Walk2.tif")
ewa_col_Walk2 <- rast("./data/CSS/E_WA_col_Walk2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_col_Walk2[wwa_col_Walk2 > 1] <- 1
ewa_col_Walk2[ewa_col_Walk2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_col_Walk2 <- mosaic(wwa_col_Walk2, ewa_col_Walk2)

#crop to align raster sizes
wa_col_WP2 <- crop(wa_col_WP2, wa_col_WP1, mask = TRUE)
wa_col_WP3 <- crop(wa_col_WP3, wa_col_WP1, mask = TRUE)
wa_col_WP4 <- crop(wa_col_WP4, wa_col_WP1, mask = TRUE)

#create layered datasets 
wa_col <- c(wa_col_WP1, wa_col_WP2, wa_col_WP3, wa_col_WP4, wa_col_car, wa_col_Walk1, wa_col_Walk2)

#comc ----
#WP1
wwa_comc_WP1 <- rast("./data/CSS/W_WA_comc_WP1.tif")
ewa_comc_WP1 <- rast("./data/CSS/E_WA_comc_WP1.tif")

#convert count into values of 1 and 0 - set to 1 for later weigting 
wwa_comc_WP1[wwa_comc_WP1 > 1] <- 1
ewa_comc_WP1[ewa_comc_WP1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_comc_WP1 <- mosaic(wwa_comc_WP1, ewa_comc_WP1)

#WP2
wwa_comc_WP2 <- rast("./data/CSS/W_WA_comc_WP2.tif")
ewa_comc_WP2 <- rast("./data/CSS/E_WA_comc_WP2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_comc_WP2[wwa_comc_WP2 > 1] <- 1
ewa_comc_WP2[ewa_comc_WP2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_comc_WP2 <- mosaic(wwa_comc_WP2, ewa_comc_WP2)

#WP3
wwa_comc_WP3 <- rast("./data/CSS/W_WA_comc_WP3.tif")
ewa_comc_WP3 <- rast("./data/CSS/E_WA_comc_WP3.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_comc_WP3[wwa_comc_WP3 > 1] <- 1
ewa_comc_WP3[ewa_comc_WP3 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_comc_WP3 <- mosaic(wwa_comc_WP3, ewa_comc_WP3)

#WP4
wwa_comc_WP4 <- rast("./data/CSS/W_WA_comc_WP4.tif")
ewa_comc_WP4 <- rast("./data/CSS/E_WA_comc_WP4.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_comc_WP4[wwa_comc_WP4 > 1] <- 1
ewa_comc_WP4[ewa_comc_WP4 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_comc_WP4 <- mosaic(wwa_comc_WP4, ewa_comc_WP4)


#Car - comc
wwa_comc_car <- rast("./data/CSS/W_WA_comc_car.tif")
ewa_comc_car <- rast("./data/CSS/E_WA_comc_car.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_comc_car[wwa_comc_car > 1] <- 1
ewa_comc_car[ewa_comc_car > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_comc_car <- mosaic(wwa_comc_car, ewa_comc_car)

#walk1 - 
wwa_comc_Walk1 <- rast("./data/CSS/W_WA_comc_Walk1.tif")
ewa_comc_Walk1 <- rast("./data/CSS/E_WA_comc_Walk1.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_comc_Walk1[wwa_comc_Walk1 > 1] <- 1
ewa_comc_Walk1[ewa_comc_Walk1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_comc_Walk1 <- mosaic(wwa_comc_Walk1, ewa_comc_Walk1)

#walk2
wwa_comc_Walk2 <- rast("./data/CSS/W_WA_comc_Walk2.tif")
ewa_comc_Walk2 <- rast("./data/CSS/E_WA_comc_Walk2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_comc_Walk2[wwa_comc_Walk2 > 1] <- 1
ewa_comc_Walk2[ewa_comc_Walk2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_comc_Walk2 <- mosaic(wwa_comc_Walk2, ewa_comc_Walk2)

#crop to align raster sizes
wa_comc_WP2 <- crop(wa_comc_WP2, wa_comc_WP1, mask = TRUE)
wa_comc_WP3 <- crop(wa_comc_WP3, wa_comc_WP1, mask = TRUE)
wa_comc_WP4 <- crop(wa_comc_WP4, wa_comc_WP1, mask = TRUE)

#create layered datasets 
wa_comc <- c(wa_comc_WP1, wa_comc_WP2, wa_comc_WP3, wa_comc_WP4, wa_comc_car, wa_comc_Walk1, wa_comc_Walk2)

#gym ----
#WP1
wwa_gym_WP1 <- rast("./data/CSS/W_WA_gym_WP1.tif")
ewa_gym_WP1 <- rast("./data/CSS/E_WA_gym_WP1.tif")

#convert count into values of 1 and 0 - set to 1 for later weigting 
wwa_gym_WP1[wwa_gym_WP1 > 1] <- 1
ewa_gym_WP1[ewa_gym_WP1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_gym_WP1 <- mosaic(wwa_gym_WP1, ewa_gym_WP1)

#WP2
wwa_gym_WP2 <- rast("./data/CSS/W_WA_gym_WP2.tif")
ewa_gym_WP2 <- rast("./data/CSS/E_WA_gym_WP2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_gym_WP2[wwa_gym_WP2 > 1] <- 1
ewa_gym_WP2[ewa_gym_WP2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_gym_WP2 <- mosaic(wwa_gym_WP2, ewa_gym_WP2)

#WP3
wwa_gym_WP3 <- rast("./data/CSS/W_WA_gym_WP3.tif")
ewa_gym_WP3 <- rast("./data/CSS/E_WA_gym_WP3.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_gym_WP3[wwa_gym_WP3 > 1] <- 1
ewa_gym_WP3[ewa_gym_WP3 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_gym_WP3 <- mosaic(wwa_gym_WP3, ewa_gym_WP3)

#WP4
wwa_gym_WP4 <- rast("./data/CSS/W_WA_gym_WP4.tif")
ewa_gym_WP4 <- rast("./data/CSS/E_WA_gym_WP4.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_gym_WP4[wwa_gym_WP4 > 1] <- 1
ewa_gym_WP4[ewa_gym_WP4 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_gym_WP4 <- mosaic(wwa_gym_WP4, ewa_gym_WP4)


#Car - gym
wwa_gym_car <- rast("./data/CSS/W_WA_gym_car.tif")
ewa_gym_car <- rast("./data/CSS/E_WA_gym_car.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_gym_car[wwa_gym_car > 1] <- 1
ewa_gym_car[ewa_gym_car > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_gym_car <- mosaic(wwa_gym_car, ewa_gym_car)

#walk1 - 
wwa_gym_Walk1 <- rast("./data/CSS/W_WA_gym_Walk1.tif")
ewa_gym_Walk1 <- rast("./data/CSS/E_WA_gym_Walk1.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_gym_Walk1[wwa_gym_Walk1 > 1] <- 1
ewa_gym_Walk1[ewa_gym_Walk1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_gym_Walk1 <- mosaic(wwa_gym_Walk1, ewa_gym_Walk1)

#walk2
wwa_gym_Walk2 <- rast("./data/CSS/W_WA_gym_Walk2.tif")
ewa_gym_Walk2 <- rast("./data/CSS/E_WA_gym_Walk2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_gym_Walk2[wwa_gym_Walk2 > 1] <- 1
ewa_gym_Walk2[ewa_gym_Walk2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_gym_Walk2 <- mosaic(wwa_gym_Walk2, ewa_gym_Walk2)

#crop to align raster sizes
wa_gym_WP2 <- crop(wa_gym_WP2, wa_gym_WP1, mask = TRUE)
wa_gym_WP3 <- crop(wa_gym_WP3, wa_gym_WP1, mask = TRUE)
wa_gym_WP4 <- crop(wa_gym_WP4, wa_gym_WP1, mask = TRUE)

#create layered datasets 
wa_gym <- c(wa_gym_WP1, wa_gym_WP2, wa_gym_WP3, wa_gym_WP4, wa_gym_car, wa_gym_Walk1, wa_gym_Walk2)

#lib ----
#WP1
wwa_lib_WP1 <- rast("./data/CSS/W_WA_lib_WP1.tif")
ewa_lib_WP1 <- rast("./data/CSS/E_WA_lib_WP1.tif")

#convert count into values of 1 and 0 - set to 1 for later weigting 
wwa_lib_WP1[wwa_lib_WP1 > 1] <- 1
ewa_lib_WP1[ewa_lib_WP1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_lib_WP1 <- mosaic(wwa_lib_WP1, ewa_lib_WP1)

#WP2
wwa_lib_WP2 <- rast("./data/CSS/W_WA_lib_WP2.tif")
ewa_lib_WP2 <- rast("./data/CSS/E_WA_lib_WP2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_lib_WP2[wwa_lib_WP2 > 1] <- 1
ewa_lib_WP2[ewa_lib_WP2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_lib_WP2 <- mosaic(wwa_lib_WP2, ewa_lib_WP2)

#WP3
wwa_lib_WP3 <- rast("./data/CSS/W_WA_lib_WP3.tif")
ewa_lib_WP3 <- rast("./data/CSS/E_WA_lib_WP3.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_lib_WP3[wwa_lib_WP3 > 1] <- 1
ewa_lib_WP3[ewa_lib_WP3 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_lib_WP3 <- mosaic(wwa_lib_WP3, ewa_lib_WP3)

#WP4
wwa_lib_WP4 <- rast("./data/CSS/W_WA_lib_WP4.tif")
ewa_lib_WP4 <- rast("./data/CSS/E_WA_lib_WP4.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_lib_WP4[wwa_lib_WP4 > 1] <- 1
ewa_lib_WP4[ewa_lib_WP4 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_lib_WP4 <- mosaic(wwa_lib_WP4, ewa_lib_WP4)


#Car - lib
wwa_lib_car <- rast("./data/CSS/W_WA_lib_car.tif")
ewa_lib_car <- rast("./data/CSS/E_WA_lib_car.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_lib_car[wwa_lib_car > 1] <- 1
ewa_lib_car[ewa_lib_car > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_lib_car <- mosaic(wwa_lib_car, ewa_lib_car)

#walk1 - 
wwa_lib_Walk1 <- rast("./data/CSS/W_WA_lib_Walk1.tif")
ewa_lib_Walk1 <- rast("./data/CSS/E_WA_lib_Walk1.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_lib_Walk1[wwa_lib_Walk1 > 1] <- 1
ewa_lib_Walk1[ewa_lib_Walk1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_lib_Walk1 <- mosaic(wwa_lib_Walk1, ewa_lib_Walk1)

#walk2
wwa_lib_Walk2 <- rast("./data/CSS/W_WA_lib_Walk2.tif")
ewa_lib_Walk2 <- rast("./data/CSS/E_WA_lib_Walk2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_lib_Walk2[wwa_lib_Walk2 > 1] <- 1
ewa_lib_Walk2[ewa_lib_Walk2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_lib_Walk2 <- mosaic(wwa_lib_Walk2, ewa_lib_Walk2)

#crop to align raster sizes
wa_lib_WP2 <- crop(wa_lib_WP2, wa_lib_WP1, mask = TRUE)
wa_lib_WP3 <- crop(wa_lib_WP3, wa_lib_WP1, mask = TRUE)
wa_lib_WP4 <- crop(wa_lib_WP4, wa_lib_WP1, mask = TRUE)

#create layered datasets 
wa_lib <- c(wa_lib_WP1, wa_lib_WP2, wa_lib_WP3, wa_lib_WP4, wa_lib_car, wa_lib_Walk1, wa_lib_Walk2)

#PO ----
#WP1
wwa_PO_WP1 <- rast("./data/CSS/W_WA_PO_WP1.tif")
ewa_PO_WP1 <- rast("./data/CSS/E_WA_PO_WP1.tif")

#convert count into values of 1 and 0 - set to 1 for later weigting 
wwa_PO_WP1[wwa_PO_WP1 > 1] <- 1
ewa_PO_WP1[ewa_PO_WP1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_PO_WP1 <- mosaic(wwa_PO_WP1, ewa_PO_WP1)

#WP2
wwa_PO_WP2 <- rast("./data/CSS/W_WA_PO_WP2.tif")
ewa_PO_WP2 <- rast("./data/CSS/E_WA_PO_WP2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_PO_WP2[wwa_PO_WP2 > 1] <- 1
ewa_PO_WP2[ewa_PO_WP2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_PO_WP2 <- mosaic(wwa_PO_WP2, ewa_PO_WP2)

#WP3
wwa_PO_WP3 <- rast("./data/CSS/W_WA_PO_WP3.tif")
ewa_PO_WP3 <- rast("./data/CSS/E_WA_PO_WP3.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_PO_WP3[wwa_PO_WP3 > 1] <- 1
ewa_PO_WP3[ewa_PO_WP3 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_PO_WP3 <- mosaic(wwa_PO_WP3, ewa_PO_WP3)

#WP4
wwa_PO_WP4 <- rast("./data/CSS/W_WA_PO_WP4.tif")
ewa_PO_WP4 <- rast("./data/CSS/E_WA_PO_WP4.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_PO_WP4[wwa_PO_WP4 > 1] <- 1
ewa_PO_WP4[ewa_PO_WP4 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_PO_WP4 <- mosaic(wwa_PO_WP4, ewa_PO_WP4)


#Car - PO
wwa_PO_car <- rast("./data/CSS/W_WA_PO_car.tif")
ewa_PO_car <- rast("./data/CSS/E_WA_PO_car.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_PO_car[wwa_PO_car > 1] <- 1
ewa_PO_car[ewa_PO_car > 1] <- 1

#mosaic datasets - if working with older "raster" POrary - will uncompress files, rast alleviates this
wa_PO_car <- mosaic(wwa_PO_car, ewa_PO_car)

#walk1 - 
wwa_PO_Walk1 <- rast("./data/CSS/W_WA_PO_Walk1.tif")
ewa_PO_Walk1 <- rast("./data/CSS/E_WA_PO_Walk1.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_PO_Walk1[wwa_PO_Walk1 > 1] <- 1
ewa_PO_Walk1[ewa_PO_Walk1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_PO_Walk1 <- mosaic(wwa_PO_Walk1, ewa_PO_Walk1)

#walk2
wwa_PO_Walk2 <- rast("./data/CSS/W_WA_PO_Walk2.tif")
ewa_PO_Walk2 <- rast("./data/CSS/E_WA_PO_Walk2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_PO_Walk2[wwa_PO_Walk2 > 1] <- 1
ewa_PO_Walk2[ewa_PO_Walk2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_PO_Walk2 <- mosaic(wwa_PO_Walk2, ewa_PO_Walk2)

#crop to align raster sizes
wa_PO_WP2 <- crop(wa_PO_WP2, wa_PO_WP1, mask = TRUE)
wa_PO_WP3 <- crop(wa_PO_WP3, wa_PO_WP1, mask = TRUE)
wa_PO_WP4 <- crop(wa_PO_WP4, wa_PO_WP1, mask = TRUE)

#create layered datasets 
wa_PO <- c(wa_PO_WP1, wa_PO_WP2, wa_PO_WP3, wa_PO_WP4, wa_PO_car, wa_PO_Walk1, wa_PO_Walk2)

#POW ----
#WP1
wwa_POW_WP1 <- rast("./data/CSS/W_WA_POW_WP1.tif")
ewa_POW_WP1 <- rast("./data/CSS/E_WA_POW_WP1.tif")

#convert count into values of 1 and 0 - set to 1 for later weigting 
wwa_POW_WP1[wwa_POW_WP1 > 1] <- 1
ewa_POW_WP1[ewa_POW_WP1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_POW_WP1 <- mosaic(wwa_POW_WP1, ewa_POW_WP1)

#WP2
wwa_POW_WP2 <- rast("./data/CSS/W_WA_POW_WP2.tif")
ewa_POW_WP2 <- rast("./data/CSS/E_WA_POW_WP2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_POW_WP2[wwa_POW_WP2 > 1] <- 1
ewa_POW_WP2[ewa_POW_WP2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_POW_WP2 <- mosaic(wwa_POW_WP2, ewa_POW_WP2)

#WP3
wwa_POW_WP3 <- rast("./data/CSS/W_WA_POW_WP3.tif")
ewa_POW_WP3 <- rast("./data/CSS/E_WA_POW_WP3.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_POW_WP3[wwa_POW_WP3 > 1] <- 1
ewa_POW_WP3[ewa_POW_WP3 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_POW_WP3 <- mosaic(wwa_POW_WP3, ewa_POW_WP3)

#WP4
wwa_POW_WP4 <- rast("./data/CSS/W_WA_POW_WP4.tif")
ewa_POW_WP4 <- rast("./data/CSS/E_WA_POW_WP4.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_POW_WP4[wwa_POW_WP4 > 1] <- 1
ewa_POW_WP4[ewa_POW_WP4 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_POW_WP4 <- mosaic(wwa_POW_WP4, ewa_POW_WP4)


#Car - POW
wwa_POW_car <- rast("./data/CSS/W_WA_POW_car.tif")
ewa_POW_car <- rast("./data/CSS/E_WA_POW_car.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_POW_car[wwa_POW_car > 1] <- 1
ewa_POW_car[ewa_POW_car > 1] <- 1

#mosaic datasets - if working with older "raster" POWrary - will uncompress files, rast alleviates this
wa_POW_car <- mosaic(wwa_POW_car, ewa_POW_car)

#walk1 - 
wwa_POW_Walk1 <- rast("./data/CSS/W_WA_POW_Walk1.tif")
ewa_POW_Walk1 <- rast("./data/CSS/E_WA_POW_Walk1.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_POW_Walk1[wwa_POW_Walk1 > 1] <- 1
ewa_POW_Walk1[ewa_POW_Walk1 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_POW_Walk1 <- mosaic(wwa_POW_Walk1, ewa_POW_Walk1)

#walk2
wwa_POW_Walk2 <- rast("./data/CSS/W_WA_POW_Walk2.tif")
ewa_POW_Walk2 <- rast("./data/CSS/E_WA_POW_Walk2.tif")

#convert Hospital count into values of 5 and 0 - set to 5 for later weigting 
wwa_POW_Walk2[wwa_POW_Walk2 > 1] <- 1
ewa_POW_Walk2[ewa_POW_Walk2 > 1] <- 1

#mosaic datasets - if working with older "raster" library - will uncompress files, rast alleviates this
wa_POW_Walk2 <- mosaic(wwa_POW_Walk2, ewa_POW_Walk2)

#crop to align raster sizes
wa_POW_WP2 <- crop(wa_POW_WP2, wa_POW_WP1, mask = TRUE)
wa_POW_WP3 <- crop(wa_POW_WP3, wa_POW_WP1, mask = TRUE)
wa_POW_WP4 <- crop(wa_POW_WP4, wa_POW_WP1, mask = TRUE)

#create layered datasets 
wa_POW <- c(wa_POW_WP1, wa_POW_WP2, wa_POW_WP3, wa_POW_WP4, wa_POW_car, wa_POW_Walk1, wa_POW_Walk2)



#correction for Conveyal skipping ----
wa_senior[[2]][wa_senior[[4]] < wa_senior[[2]]]<- 0
wa_senior[[3]][wa_senior[[1]] < wa_senior[[3]]]<- 0
wa_senior[[7]][wa_senior[[6]] < wa_senior[[7]]]<- 0


wa_bank[[2]][wa_bank[[4]] < wa_bank[[2]]]<- 0
wa_bank[[3]][wa_bank[[1]] < wa_bank[[3]]]<- 0
wa_bank[[7]][wa_bank[[6]] < wa_bank[[7]]]<- 0

wa_bill[[2]][wa_bill[[4]] < wa_bill[[2]]]<- 0
wa_bill[[3]][wa_bill[[1]] < wa_bill[[3]]]<- 0
wa_bill[[7]][wa_bill[[6]] < wa_bill[[7]]]<- 0

wa_chld[[2]][wa_chld[[4]] < wa_chld[[2]]]<- 0
wa_chld[[3]][wa_chld[[1]] < wa_chld[[3]]]<- 0
wa_chld[[7]][wa_chld[[6]] < wa_chld[[7]]]<- 0

wa_col[[2]][wa_col[[4]] < wa_col[[2]]]<- 0
wa_col[[3]][wa_col[[1]] < wa_col[[3]]]<- 0
wa_col[[7]][wa_col[[6]] < wa_col[[7]]]<- 0

wa_comc[[2]][wa_comc[[4]] < wa_comc[[2]]]<- 0
wa_comc[[3]][wa_comc[[1]] < wa_comc[[3]]]<- 0
wa_comc[[7]][wa_comc[[6]] < wa_comc[[7]]]<- 0

wa_gym[[2]][wa_gym[[4]] < wa_gym[[2]]]<- 0
wa_gym[[3]][wa_gym[[1]] < wa_gym[[3]]]<- 0
wa_gym[[7]][wa_gym[[6]] < wa_gym[[7]]]<- 0

wa_lib[[2]][wa_lib[[4]] < wa_lib[[2]]]<- 0
wa_lib[[3]][wa_lib[[1]] < wa_lib[[3]]]<- 0
wa_lib[[7]][wa_lib[[6]] < wa_lib[[7]]]<- 0

wa_PO[[2]][wa_PO[[4]] < wa_PO[[2]]]<- 0
wa_PO[[3]][wa_PO[[1]] < wa_PO[[3]]]<- 0
wa_PO[[7]][wa_PO[[6]] < wa_PO[[7]]]<- 0

wa_POW[[2]][wa_POW[[4]] < wa_POW[[2]]]<- 0
wa_POW[[3]][wa_POW[[1]] < wa_POW[[3]]]<- 0
wa_POW[[7]][wa_POW[[6]] < wa_POW[[7]]]<- 0


#Combinining calc for non-hospital CSS locations ----
css1 <- sum(wa_senior, wa_bank)
css2 <- sum(css1, wa_bill)
css3 <- sum(css2, wa_chld)
css4<- sum(css3, wa_col)
css5<- sum(css4, wa_comc)
css6<- sum(css5, wa_gym)
css7<- sum(css6, wa_lib)
css8 <- sum(css7, wa_PO)
css <- sum(css8, wa_POW)

css <- css *10


remove(wwa_bank_car)
remove(wwa_bank_Walk1)
remove(wwa_bank_Walk2)
remove(wwa_bank_WP1)
remove(wwa_bank_WP2)
remove(wwa_bank_WP3)
remove(wwa_bank_WP4)
remove(ewa_bank_car)
remove(ewa_bank_Walk1)
remove(ewa_bank_Walk2)
remove(ewa_bank_WP1)
remove(ewa_bank_WP2)
remove(ewa_bank_WP3)
remove(ewa_bank_WP4)
remove(wa_bank_car)
remove(wa_bank_Walk1)
remove(wa_bank_Walk2)
remove(wa_bank_WP1)
remove(wa_bank_WP2)
remove(wa_bank_WP3)
remove(wa_bank_WP4)

remove(wwa_bill_car)
remove(wwa_bill_Walk1)
remove(wwa_bill_Walk2)
remove(wwa_bill_WP1)
remove(wwa_bill_WP2)
remove(wwa_bill_WP3)
remove(wwa_bill_WP4)
remove(ewa_bill_car)
remove(ewa_bill_Walk1)
remove(ewa_bill_Walk2)
remove(ewa_bill_WP1)
remove(ewa_bill_WP2)
remove(ewa_bill_WP3)
remove(ewa_bill_WP4)
remove(wa_bill_car)
remove(wa_bill_Walk1)
remove(wa_bill_Walk2)
remove(wa_bill_WP1)
remove(wa_bill_WP2)
remove(wa_bill_WP3)
remove(wa_bill_WP4)

remove(wwa_chld_car)
remove(wwa_chld_Walk1)
remove(wwa_chld_Walk2)
remove(wwa_chld_WP1)
remove(wwa_chld_WP2)
remove(wwa_chld_WP3)
remove(wwa_chld_WP4)
remove(ewa_chld_car)
remove(ewa_chld_Walk1)
remove(ewa_chld_Walk2)
remove(ewa_chld_WP1)
remove(ewa_chld_WP2)
remove(ewa_chld_WP3)
remove(ewa_chld_WP4)
remove(wa_chld_car)
remove(wa_chld_Walk1)
remove(wa_chld_Walk2)
remove(wa_chld_WP1)
remove(wa_chld_WP2)
remove(wa_chld_WP3)
remove(wa_chld_WP4)

remove(wwa_col_car)
remove(wwa_col_Walk1)
remove(wwa_col_Walk2)
remove(wwa_col_WP1)
remove(wwa_col_WP2)
remove(wwa_col_WP3)
remove(wwa_col_WP4)
remove(ewa_col_car)
remove(ewa_col_Walk1)
remove(ewa_col_Walk2)
remove(ewa_col_WP1)
remove(ewa_col_WP2)
remove(ewa_col_WP3)
remove(ewa_col_WP4)
remove(wa_col_car)
remove(wa_col_Walk1)
remove(wa_col_Walk2)
remove(wa_col_WP1)
remove(wa_col_WP2)
remove(wa_col_WP3)
remove(wa_col_WP4)

remove(wwa_comc_car)
remove(wwa_comc_Walk1)
remove(wwa_comc_Walk2)
remove(wwa_comc_WP1)
remove(wwa_comc_WP2)
remove(wwa_comc_WP3)
remove(wwa_comc_WP4)
remove(ewa_comc_car)
remove(ewa_comc_Walk1)
remove(ewa_comc_Walk2)
remove(ewa_comc_WP1)
remove(ewa_comc_WP2)
remove(ewa_comc_WP3)
remove(ewa_comc_WP4)
remove(wa_comc_car)
remove(wa_comc_Walk1)
remove(wa_comc_Walk2)
remove(wa_comc_WP1)
remove(wa_comc_WP2)
remove(wa_comc_WP3)
remove(wa_comc_WP4)

remove(wwa_gym_car)
remove(wwa_gym_Walk1)
remove(wwa_gym_Walk2)
remove(wwa_gym_WP1)
remove(wwa_gym_WP2)
remove(wwa_gym_WP3)
remove(wwa_gym_WP4)
remove(ewa_gym_car)
remove(ewa_gym_Walk1)
remove(ewa_gym_Walk2)
remove(ewa_gym_WP1)
remove(ewa_gym_WP2)
remove(ewa_gym_WP3)
remove(ewa_gym_WP4)
remove(wa_gym_car)
remove(wa_gym_Walk1)
remove(wa_gym_Walk2)
remove(wa_gym_WP1)
remove(wa_gym_WP2)
remove(wa_gym_WP3)
remove(wa_gym_WP4)

remove(wwa_lib_car)
remove(wwa_lib_Walk1)
remove(wwa_lib_Walk2)
remove(wwa_lib_WP1)
remove(wwa_lib_WP2)
remove(wwa_lib_WP3)
remove(wwa_lib_WP4)
remove(ewa_lib_car)
remove(ewa_lib_Walk1)
remove(ewa_lib_Walk2)
remove(ewa_lib_WP1)
remove(ewa_lib_WP2)
remove(ewa_lib_WP3)
remove(ewa_lib_WP4)
remove(wa_lib_car)
remove(wa_lib_Walk1)
remove(wa_lib_Walk2)
remove(wa_lib_WP1)
remove(wa_lib_WP2)
remove(wa_lib_WP3)
remove(wa_lib_WP4)

remove(wwa_PO_car)
remove(wwa_PO_Walk1)
remove(wwa_PO_Walk2)
remove(wwa_PO_WP1)
remove(wwa_PO_WP2)
remove(wwa_PO_WP3)
remove(wwa_PO_WP4)
remove(ewa_PO_car)
remove(ewa_PO_Walk1)
remove(ewa_PO_Walk2)
remove(ewa_PO_WP1)
remove(ewa_PO_WP2)
remove(ewa_PO_WP3)
remove(ewa_PO_WP4)
remove(wa_PO_car)
remove(wa_PO_Walk1)
remove(wa_PO_Walk2)
remove(wa_PO_WP1)
remove(wa_PO_WP2)
remove(wa_PO_WP3)
remove(wa_PO_WP4)

remove(wwa_POW_car)
remove(wwa_POW_Walk1)
remove(wwa_POW_Walk2)
remove(wwa_POW_WP1)
remove(wwa_POW_WP2)
remove(wwa_POW_WP3)
remove(wwa_POW_WP4)
remove(ewa_POW_car)
remove(ewa_POW_Walk1)
remove(ewa_POW_Walk2)
remove(ewa_POW_WP1)
remove(ewa_POW_WP2)
remove(ewa_POW_WP3)
remove(ewa_POW_WP4)
remove(wa_POW_car)
remove(wa_POW_Walk1)
remove(wa_POW_Walk2)
remove(wa_POW_WP1)
remove(wa_POW_WP2)
remove(wa_POW_WP3)
remove(wa_POW_WP4)

remove(wwa_senior_car)
remove(wwa_senior_Walk1)
remove(wwa_senior_Walk2)
remove(wwa_senior_WP1)
remove(wwa_senior_WP2)
remove(wwa_senior_WP3)
remove(wwa_senior_WP4)
remove(ewa_senior_car)
remove(ewa_senior_Walk1)
remove(ewa_senior_Walk2)
remove(ewa_senior_WP1)
remove(ewa_senior_WP2)
remove(ewa_senior_WP3)
remove(ewa_senior_WP4)
remove(wa_senior_car)
remove(wa_senior_Walk1)
remove(wa_senior_Walk2)
remove(wa_senior_WP1)
remove(wa_senior_WP2)
remove(wa_senior_WP3)
remove(wa_senior_WP4)

remove(wa_senior)
remove(wa_bank)
remove(wa_bill)
remove(wa_chld)
remove(wa_col)
remove(wa_comc)
remove(wa_PO)
remove(wa_POW)
remove(wa_gym)
remove(wa_lib)

remove(css1)
remove(css2)
remove(css3)
remove(css4)
remove(css5)
remove(css6)
remove(css7)
remove(css8)

#Total CSS Category ----
wa_CSS <- css / 10

#comparison analysis -----
wa_CSS_Wp2WP1 <- wa_CSS[[2]]/wa_CSS[[1]] 
wa_CSS_WP3WP1 <- wa_CSS[[3]]/wa_CSS[[1]] 
wa_CSS_WP4WP1 <- wa_CSS[[4]]/wa_CSS[[1]] 
wa_CSS_WP1Car <- wa_CSS[[1]]/wa_CSS[[5]] 
wa_CSS_WP1Car[wa_CSS_WP1Car == Inf] <- NaN

#final skip correction for walking profile 2 ----
wa_CSS[[7]][wa_CSS[[7]] > wa_CSS[[2]]] <- 0

writeRaster(
  wa_CSS, 
  "./data/CSS/06wa_CSS.tif",
  overwrite = TRUE
)

wa_CSS <- rast("./data/CSS/06wa_CSS.tif")


summary(wa_CSS)

