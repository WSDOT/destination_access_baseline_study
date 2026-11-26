#This scrips consolidates previous metrics in the Washington State Public Transportation Division's Destination Access analysis. 
#Original consolidation proposal as of 8/25/26 - https://wsdot.sharepoint.com/:w:/r/teams/HQ-PublicTransportation-All/Shared%20Documents/Engineering%20and%20Planning/4)%20Studies/FTSS2/DABS%20metric%20aggregation%20ideas.docx?d=w12372f07be844a9cb8be709c1545d685&csf=1&web=1&e=r3PEKq

library(tidyverse)
library(terra)
library(sf)
library(mapview)

#load in context classification -----
gdb_path <- "./data/context_classification/ContextClassification_Backups.gdb"
layers <- st_layers(gdb_path)
layer_name <- "WashingtonState_Blocks_test_06_04_2026"
print(layers)

tryCatch({
  context_class <- st_read(dsn = gdb_path, layer = layer_name, quiet = FALSE)
  print(context_class)
}, error = function(e) {
  message("Error reading layer: ", e$message)
})

#context classification only geographies ----
unique(context_class$CorrectedLandUseCode)

str(context_class)

context_class_Simple_Groups <-  context_class |> 
  mutate(
    simple_classification = if_else(
      CorrectedLandUseCode == "Urban Core", "Urban Core", 
      if_else(
        CorrectedLandUseCode == "Urban General" | CorrectedLandUseCode == "Urban Center", "Urban",
        if_else(
          CorrectedLandUseCode == "Suburban Commercial" & RuralGeneralFlag == 0 | CorrectedLandUseCode == "Suburban Residential" & RuralGeneralFlag == 0, "Suburban",
          if_else(
            CorrectedLandUseCode == "Suburban Commercial" & RuralGeneralFlag == 1 | CorrectedLandUseCode == "Suburban Residential" & RuralGeneralFlag == 1 | CorrectedLandUseCode == "Rural" | CorrectedLandUseCode == "Rural Town Center" , "Rural",
            CorrectedLandUseCode)
        )
      )
    )) |> select(GEOID, GEOID_int, P0010001, RuralGeneralFlag, CorrectedLandUseCode, simple_classification)

#PT Network 1.2 - WK4 vs WP4 ----
#running with equal walk time, no filter for PT impact required
ewa_leis_wk45 <- rast("./data/Consolidated_inputs/e_wa_leis_wk4_45.tif")
wwa_leis_wk45 <- rast("./data/Consolidated_inputs/w_wa_leis_wk4_45.tif")
ewa_leis_wp45 <- rast("./data/Consolidated_inputs/e_wa_leis_wp4_45.tif")
wwa_leis_wp45 <- rast("./data/Consolidated_inputs/w_wa_leis_wp4_45.tif")

ewa_jobs_wk45 <- rast("./data/Consolidated_inputs/e_wa_jobs_wk4_45.tif")
wwa_jobs_wk45 <- rast("./data/Consolidated_inputs/w_wa_jobs_wk4_45.tif")
ewa_jobs_wp45 <- rast("./data/Consolidated_inputs/e_wa_jobs_wp4_45.tif")
wwa_jobs_wp45 <- rast("./data/Consolidated_inputs/w_wa_jobs_wp4_45.tif")

plot(ewa_leis_wk45)

leis_wk45 <-  mosaic(ewa_leis_wk45, wwa_leis_wk45, fun = mean)
leis_wk45<- round(leis_wk45 / 3, digits = 0)

leis_wp45 <- mosaic(ewa_leis_wp45, wwa_leis_wp45, fun = mean)
leis_wp45 <- round(leis_wp45 / 3, digits = 0)

jobs_wk45 <- mosaic(ewa_jobs_wk45, wwa_jobs_wk45, fun = mean)
jobs_wp45 <- mosaic(ewa_jobs_wp45, wwa_jobs_wp45, fun = mean)

rm(ewa_leis_wk45, wwa_leis_wk45, ewa_leis_wp45, wwa_leis_wp45, ewa_jobs_wk45, wwa_jobs_wk45, ewa_jobs_wp45, wwa_jobs_wp45)

list(jobs_wp45)

#fitler rasters where walking access is equal to PT access | filter is now for any areas where <500 jobs are added from transit to limit noise from ped discrepancy in Conveyal
wa_jobs_PT_add <- jobs_wp45
wa_jobs_PT_add[jobs_wp45 <= jobs_wk45]<- NA

wa_jobs_PT_add

plot(wa_jobs_PT_add)

jobs_wk45[is.na(wa_jobs_PT_add)] <- NA
jobs_wp45[is.na(wa_jobs_PT_add)] <- NA
leis_wp45[is.na(wa_jobs_PT_add)] <- NA
leis_wk45[is.na(wa_jobs_PT_add)] <- NA

plot(jobs_wk45)
plot(jobs_wp45)

#Normalize Jobs
max <- minmax(jobs_wp45) |> 
  max()
#use max to calculate new raster values 
JobsWP4 <- (jobs_wp45 / max) * 100
#normalize WP4 without transit jobs
JobsWK4 <- (jobs_wk45 / max) * 100

#Normalize Leis 
#max value of Leis WP4 
max <- minmax(leis_wp45) |> 
  max()
leisWP4 <- (leis_wp45 / max) * 100
#normalize WP4 without transit leis
leisWK4 <- (leis_wk45 / max) * 100


freq(leisWP4)
freq(leisWK4)
freq(JobsWK4)


# e. Merge jobs and leisure values by averaging - should result in 2 rasters, one for w/ and one for w/o
Avg_WP45 <- mosaic(JobsWP4, leisWP4, fun = mean)
Avg_Wk45 <- mosaic(JobsWK4, leisWK4, fun = mean)

Wk_WP45 <- (Avg_WP45 - Avg_Wk45) / Avg_Wk45
Wk_WP45[Wk_WP45 == Inf] <-  Avg_WP45 

#save externally to port over to population distribution
writeRaster(Wk_WP45, "./data/Population/Wk_WP45.tif", overwrite = TRUE)

# h. Aggregate at block level - rm.na to not factor int NAs
load("./data/WA_blocks2020w_water.RData")

WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(leisWP4))
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())

#get block ID for each raster 
PT_blk45 = terra::extract(x = Wk_WP45, y = WA_block_tigris_reproj, na.rm=TRUE) 

#bring results into context classification and block geography
PT_blk_summaries45 <- group_by(PT_blk45, ID) |> 
  summarize( 
    n_rasters = n(), 
    avg_Wk_WP = mean(e_wa_jobs_wp4_45, na.rm = TRUE)
  ) |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID))

PT_blk_no_NaN <- na.omit(PT_blk_summaries45) |> 
  na.omit(simple_classification)


# i. Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.  
#quantile(PT_blk_no_NaN$avg_Wk_WP, probs = seq(0, 1, by = .1))
PT_blk_no_NaN$decile <- ntile(PT_blk_no_NaN$avg_Wk_WP, 10)

PT_blk_summaries45 <- PT_blk_summaries45 |> 
  left_join(
    PT_blk_no_NaN |> 
      select(ID, decile), 
    by = "ID"
  )

PT_blk_summaries45 <- PT_blk_summaries45 |> 
  mutate(
    decile = replace_na(decile, 0)
  ) |>
  select(GEOID20, TRACTCE20, simple_classification, P0010001, decile, avg_Wk_WP, Shape) |> 
  filter(!is.na(simple_classification))

summary <- PT_blk_summaries45 |> 
  group_by(simple_classification) |> 
  summarise(
    n = n(), 
    avg_in = mean(na.omit(avg_Wk_WP)), 
    avg_dec = mean(decile)
  )

#convert to simple feature to include geometry
sf_PT45 <- st_as_sf(PT_blk_summaries45)

#mapview(sf_PT, zcol = "decile")

st_write(sf_PT45, dsn = "./output/Summaries/PT_Network_1.2.shp", delete_layer = TRUE)

#create summary tables/graphs for measure
decile_land_use45 <- PT_blk_summaries45 |> 
  group_by(simple_classification, decile) |> 
  summarize(
    n = n()
  ) |> 
  pivot_wider(names_from = simple_classification, values_from = n) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

df_pct45 <- decile_land_use45 %>%
  filter(decile != 0) |> 
  mutate(across(everything(), prop.table, .names = "{col}_pct"))


#Only Jobs % change Wk vs WP1 (requires variables from PT Network 1.2 run before) ----
jobs_wk45[is.na(wa_jobs_PT_add)] <- NA
jobs_wp45[is.na(wa_jobs_PT_add)] <- NA

jobs_wk_wp <- jobs_wk45 / jobs_wp45

load("./data/WA_blocks2020w_water.RData")

WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(jobs_wk45))
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())

#get block ID for each raster 
jobs_wk_wp_blck = terra::extract(x = jobs_wk_wp, y = WA_block_tigris_reproj, na.rm=TRUE) 

jobs_wk_wp_blck

PT_blk_summaries_jobs <- group_by(jobs_wk_wp_blck, ID) |> 
  summarize( 
    n_rasters = n(), 
    avg_Wk_WP = mean(e_wa_jobs_wk4_45, na.rm = TRUE)
  ) |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID)) |> 
  filter(!is.na(simple_classification))


sf_Jobs_Wk_WP <- st_as_sf(PT_blk_summaries_jobs)
st_write(PT_blk_summaries_jobs, dsn = "./output/Summaries/Jobs_Percent_1.0.shp", delete_layer = TRUE)


#Pedestrian Network Quality - WP1 vs WP3 ----
# a. Load WP1 & WP3  Leis, Load WP1 & WP3 Jobs access;
wa_Leisure_normalized <- rast("./data/Leisure/06wa_Leisure_normalized.tif")
wa_jobs <- rast("./data/Jobs/all_WP_layered.tif")

wa_Jobs_WP1 <- wa_jobs[[1]]
wa_Jobs_WP3 <- wa_jobs[[5]]

wa_leis_WP1 <- wa_Leisure_normalized[[1]]
wa_leis_WP3 <- wa_Leisure_normalized[[3]]

# b. Create new sandbox raster by Normalize each destination type & WP combination: 
#   i. new raster value = (value of each square / raster max) * 100 
#Normalize Jobs
#WP1
max <- minmax(wa_Jobs_WP1) |> 
  max()
#use max to calculate new raster values 
wa_Jobs1 <- (wa_Jobs_WP1 / max)
#normalize WP3 jobs
wa_Jobs3 <- (wa_Jobs_WP3 / max)

#Normalize Leis 
#max value of Leis WP1 
max <- minmax(wa_leis_WP1) |> 
  max()
wa_leis1 <- (wa_leis_WP1 / max)
#normalize Leis WP3
wa_leis3 <- (wa_leis_WP3 / max)


# c. Merge jobs and leisure values by averaging - should result in 2 rasters, one for WP1 and one for WP3
Avg_WP1 <- mosaic(wa_Jobs1, wa_leis1, fun = mean)
Avg_WP3 <- mosaic(wa_Jobs3, wa_leis3, fun = mean)


# Skip this step, as 1:1 access would be better access in this situation d. Set all rasters with 0 diff b/w w/ and w/o transit to NA
# e. New raster - dividing WP3 access by WP1
Acc_3_1 <- Avg_WP3 / Avg_WP1
#Acc_3_1[Acc_3_1 > 1] <- 0
#here likely want to add the filter for rasters where WP1 does not have improved PT access(need values/variables from Metric 1)
Acc_3_1[is.na(wa_jobs_PT_add)] <- NA


# f. Aggregate at block level - rm.na to not factor int NAs
load("./data/WA_blocks2020w_water.RData")

WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(wa_Leisure_normalized))
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())

#get block ID for each raster 
PN_blk = terra::extract(x = Acc_3_1, y = WA_block_tigris_reproj, na.rm=TRUE)

PN_blk <- PN_blk |> 
  rename(
    "PN_Ratio" = ewa_jobs_AM_WP3_P50_C45
    )

#   Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.   
PN_blk_summaries <- group_by(PN_blk, ID) |> 
  summarize( 
    n_rasters = n(), 
    avg_WP3_WP1 = mean(PN_Ratio, na.rm = TRUE),
    no_WP3_WP1 = sum(is.na(PN_Ratio))
  ) |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID))

PN_blk_no_NaN <- PN_blk_summaries |> 
  na.omit(PN_Ratio) |> 
  na.omit(simple_classification)


#   i. Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.  
#quantile(PT_blk_no_NaN$avg_Wk_WP, probs = seq(0, 1, by = .1))
PN_blk_no_NaN$decile <- ntile(PN_blk_no_NaN$avg_WP3_WP1, 10)

PN_blk_summaries <- PN_blk_summaries |> 
  left_join(
    PN_blk_no_NaN |> 
      select(ID, decile), 
    by = "ID"
  )

PN_blk_summaries <- PN_blk_summaries |> 
  mutate(
   decile = replace_na(decile, 0)
  ) |>
  select(GEOID20, TRACTCE20, simple_classification, P0010001, decile, avg_WP3_WP1, Shape) |> 
  filter(!is.na(simple_classification))

summary <- PN_blk_summaries |> 
  group_by(simple_classification) |> 
  summarise(
    n = n(), 
    avg_in = mean(na.omit(avg_WP3_WP1))
  )

sf_PN <- st_as_sf(PN_blk_summaries)
#think this is about ready for shp conversion and exploration
#mapview(sf_PN, zcol = "decile")

st_write(sf_PN, dsn = "./output/Summaries/PN_Network_1.1.shp", delete_layer = TRUE)


#Access Potential - WK1 vs Car ----
#import jobs and leisure for uncongested driving and WP1 without transit
wa_Leisure_normalized <- rast("./data/Leisure/06wa_Leisure_normalized.tif")
wa_jobs_walk1 <- rast("./data/Jobs/wa_jobs_walk1.tif")
w_wa_jobs_driving <- rast("./data/Jobs/w_wa_jobs_car.tif")
e_wa_jobs_driving <- rast("./data/Jobs/e_wa_jobs_car.tif")
wa_jobs_driving <- mosaic(e_wa_jobs_driving, w_wa_jobs_driving, fun = mean)
wa_jobs_driving <- crop(wa_jobs_driving, wa_jobs_walk1, mask = TRUE)
wa_jobs_driving <- wa_jobs_driving[[1]]


wa_Jobs_Wk1 <- wa_jobs_walk1[[1]]
wa_leis_Wk1 <- wa_Leisure_normalized[[6]]
wa_leis_car <- wa_Leisure_normalized[[5]]

#Normalize in same way previous metrics have been 
#Normalize Jobs
#use max to calculate new raster values 

#normalize driving jobs
max <- minmax(wa_jobs_driving) |> 
  max()
wa_JobsCar <- (wa_jobs_driving / max)
max <- minmax(wa_Jobs_Wk1) |> 
  max()
wa_JobsWk1 <- (wa_Jobs_Wk1 / max) 


#Normalize Leis 
#normalize Leis WP3
max <- minmax(wa_leis_car) |> 
  max()
wa_leiscar <- (wa_leis_car / max)
#max value of Leis Wk1 
max <- minmax(wa_leis_Wk1) |> 
  max()
wa_leisWK1 <- (wa_leis_Wk1 / max)


# c. Merge jobs and leisure values by averaging - should result in 2 rasters, one for WP1 and one for WP3
Avg_Wk1 <- mosaic(wa_JobsWk1, wa_leisWK1, fun = mean)
Avg_Car <- mosaic(wa_JobsCar, wa_leiscar, fun = mean)

freq(Avg_Car)

# Skip this step, as 1:1 access would be better access in this situation d. Set all rasters with 0 diff b/w w/ and w/o transit to NA
# e. New raster - dividing WP3 access by WP1
Acc_Wk1_Car <-  mosaic(Avg_Wk1, Avg_Car, fun = mean)
#Acc_Wk1_Car[Acc_Wk1_Car == Inf] <-  Avg_Wk1
Acc_Wk1_Car[is.na(wa_jobs_PT_add)] <- NA

load("./data/WA_blocks2020w_water.RData")

WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(wa_Leisure_normalized))
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())

#get block ID for each raster 
AP_blk = terra::extract(x = Acc_Wk1_Car, y = WA_block_tigris_reproj, na.rm=TRUE)

list(AP_blk)

AP_blk <- AP_blk |> 
  rename(
    "AP_Ratio" = ewa_jobs_walk_WP1
  )

#   Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.   
AP_blk_summaries <- group_by(AP_blk, ID) |> 
  summarize( 
    n_rasters = n(), 
    avg_WP1_car = mean(AP_Ratio, na.rm = TRUE),
    no_WP1_car = sum(is.na(AP_Ratio))
  ) |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID))

AP_blk_no_NaN <- AP_blk_summaries |> 
  na.omit(AP_Ratio) |> 
  na.omit(simple_classification)


#   i. Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.  
#quantile(PT_blk_no_NaN$avg_Wk_WP, probs = seq(0, 1, by = .1))
AP_blk_no_NaN$decile <- ntile(AP_blk_no_NaN$avg_WP1_car, 10)

AP_blk_summaries <- AP_blk_summaries |> 
  left_join(
    AP_blk_no_NaN |> 
      select(ID, decile), 
    by = "ID"
  )

AP_blk_summaries <- AP_blk_summaries |> 
  mutate(
   decile = replace_na(decile, 0)
  ) |>
  select(GEOID20, TRACTCE20, simple_classification, P0010001, decile, avg_WP1_car, Shape) |> 
  filter(!is.na(simple_classification))

summary <- AP_blk_summaries |> 
  group_by(simple_classification) |> 
  summarise(
    n = n(), 
    avg_in = mean(na.omit(avg_WP1_car))
  )

sf_AP <- st_as_sf(AP_blk_summaries)
st_write(sf_AP, dsn = "./output/Summaries/AP_Network_1.1.shp", delete_layer = TRUE)

#Access Equity - WP1 vs Car -----
#import jobs and leisure for uncongested driving and WP1 without transit
wa_Leisure_normalized <- rast("./data/Leisure/06wa_Leisure_normalized.tif")
wa_jobs <- rast("./data/Jobs/all_WP_layered.tif")
wa_Jobs_WP1 <- wa_jobs[[1]]
wa_leis_WP1 <- wa_Leisure_normalized[[1]]
wa_leis_car <- wa_Leisure_normalized[[5]]

w_wa_jobs_driving <- rast("./data/Jobs/w_wa_jobs_car.tif")
e_wa_jobs_driving <- rast("./data/Jobs/e_wa_jobs_car.tif")
wa_jobs_driving <- mosaic(e_wa_jobs_driving, w_wa_jobs_driving, fun = mean)
wa_jobs_driving <- crop(wa_jobs_driving, wa_Jobs_WP1, mask = TRUE)

wa_jobs_driving

plot(w_wa_jobs_driving)
plot(wa_leis_car)


#Normalize in same way previous metrics have been 
#Normalize Jobs

#normalize WP3 jobs
max <- minmax(wa_jobs_driving) |> 
  max()
wa_JobsCar <- (wa_jobs_driving / max)
#use max to calculate new raster values 
wa_Jobs1 <- (wa_Jobs_WP1 / max)

#Normalize Leis 
#normalize Leis WP3
max <- minmax(wa_leis_car) |> 
  max()
wa_leiscar <- (wa_leis_car / max)
wa_leis1 <- (wa_leis_WP1 / max)

# c. Merge jobs and leisure values by averaging - should result in 2 rasters, one for WP1 and one for WP3
Avg_WP1 <- mosaic(wa_Jobs1, wa_leis1, fun = mean)
Avg_Car <- mosaic(wa_JobsCar, wa_leiscar, fun = mean)


# Skip this step, as 1:1 access would be better access in this situation d. Set all rasters with 0 diff b/w w/ and w/o transit to NA
# e. New raster - dividing WP3 access by WP1
Acc_1_Car <-  Avg_WP1 / Avg_Car
Acc_1_Car[Acc_1_Car == Inf] <-  Avg_WP1
Acc_1_Car[is.na(wa_jobs_PT_add)] <- NA
#Acc_3_1[Acc_3_1 > 1] <- 0

load("./data/WA_blocks2020w_water.RData")

WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(wa_Leisure_normalized))
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())

#get block ID for each raster 
AE_blk = terra::extract(x = Acc_1_Car, y = WA_block_tigris_reproj, na.rm=TRUE)

AE_blk <- AE_blk |> 
  rename(
    "AE_Ratio" = ewa_jobs_AM_WP1_P50_C45
  )

#   Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.   
AE_blk_summaries <- group_by(AE_blk, ID) |> 
  summarize( 
    n_rasters = n(), 
    avg_WP1_car = mean(AE_Ratio, na.rm = TRUE),
    no_WP1_car = sum(is.na(AE_Ratio))
  ) |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID))

AE_blk_no_NaN <- AE_blk_summaries |> 
  na.omit(AE_Ratio) |> 
  na.omit(simple_classification)


#   i. Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.  
#quantile(PT_blk_no_NaN$avg_Wk_WP, probs = seq(0, 1, by = .1))
AE_blk_no_NaN$decile <- ntile(AE_blk_no_NaN$avg_WP1_car, 10)

AE_blk_summaries <- AE_blk_summaries |> 
  left_join(
    AE_blk_no_NaN |> 
      select(ID, decile), 
    by = "ID"
  )

AE_blk_summaries <- AE_blk_summaries |> 
  mutate(
   decile = replace_na(decile, 0)
  ) |>
  select(GEOID20, TRACTCE20, simple_classification, P0010001, decile, avg_WP1_car, Shape) |> 
  filter(!is.na(simple_classification))

summary <- AE_blk_summaries |> 
  group_by(simple_classification) |> 
  summarise(
    n = n(), 
    avg_in = mean(na.omit(avg_WP1_car))
  )

sf_AE <- st_as_sf(AE_blk_summaries)
#think this is about ready for shp conversion and exploration
#mapview(sf_AE, zcol = "decile")

st_write(sf_AE, dsn = "./output/Summaries/AE_Network_1.1.shp", delete_layer = TRUE)


#combined metrics in one dataset ----
Aggregated <- PT_blk_summaries45 |> 
  rename(
    "PT_dec" = decile
) |> left_join(
  PN_blk_summaries |>  select(1, 5:6), join_by(GEOID20 == GEOID20)) |> 
  rename(
    "PN_dec" = decile) |> 
  left_join(
      AE_blk_summaries |>  select(1, 5:6), join_by(GEOID20 == GEOID20)) |> 
  rename(
    "AE_dec" = decile) |> 
  left_join(
    AP_blk_summaries |>  select(1, 5:6), join_by(GEOID20 == GEOID20)) |> 
  rename(
    "AP_dec" = decile) |> 
  rename(
    "PT_Avg" = avg_Wk_WP, 
    "PN_Avg" = avg_WP3_WP1, 
    "AE_Avg" = avg_WP1_car.x,
    "AP_Avg" = avg_WP1_car.y
  ) |> mutate(
    "Access_Avg" = (PT_dec + PN_dec + AE_dec + AP_dec) / 4
    )
  
sf_Agg <- st_as_sf(Aggregated)

st_write(sf_Agg, dsn = "./output/Summaries/Agg_Network_1.0.shp", delete_layer = TRUE)
  
  

list(Aggregated)


#statewide aggregation ----
#Ignore decile calculation step for both PT and PN
#Do not filter by blocks that do have access improvements by PT. 
#Each block has a PT (100% - Infinity) and PN (0-100%) score
#PT Network 1.2 without PT filter
#running with equal walk time, no filter for PT impact required
ewa_leis_wk45 <- rast("./data/Consolidated_inputs/e_wa_leis_wk4_45.tif")
wwa_leis_wk45 <- rast("./data/Consolidated_inputs/w_wa_leis_wk4_45.tif")
ewa_leis_wp45 <- rast("./data/Consolidated_inputs/e_wa_leis_wp4_45.tif")
wwa_leis_wp45 <- rast("./data/Consolidated_inputs/w_wa_leis_wp4_45.tif")

ewa_jobs_wk45 <- rast("./data/Consolidated_inputs/e_wa_jobs_wk4_45.tif")
wwa_jobs_wk45 <- rast("./data/Consolidated_inputs/w_wa_jobs_wk4_45.tif")
ewa_jobs_wp45 <- rast("./data/Consolidated_inputs/e_wa_jobs_wp4_45.tif")
wwa_jobs_wp45 <- rast("./data/Consolidated_inputs/w_wa_jobs_wp4_45.tif")

plot(ewa_leis_wk45)

leis_wk45 <-  mosaic(ewa_leis_wk45, wwa_leis_wk45, fun = mean)
leis_wk45<- round(leis_wk45 / 3, digits = 0)

leis_wp45 <- mosaic(ewa_leis_wp45, wwa_leis_wp45, fun = mean)
leis_wp45 <- round(leis_wp45 / 3, digits = 0)

jobs_wk45 <- mosaic(ewa_jobs_wk45, wwa_jobs_wk45, fun = mean)
jobs_wp45 <- mosaic(ewa_jobs_wp45, wwa_jobs_wp45, fun = mean)

rm(ewa_leis_wk45, wwa_leis_wk45, ewa_leis_wp45, wwa_leis_wp45, ewa_jobs_wk45, wwa_jobs_wk45, ewa_jobs_wp45, wwa_jobs_wp45)

list(jobs_wp45)

#fitler rasters where walking access is equal to PT access | filter is now for any areas where <500 jobs are added from transit to limit noise from ped discrepancy in Conveyal
# wa_jobs_PT_add <- jobs_wp45
# wa_jobs_PT_add[jobs_wp45 <= jobs_wk45]<- NA
# 
# wa_jobs_PT_add
# 
# plot(wa_jobs_PT_add)
# 
#jobs_wk45[is.na(wa_jobs_PT_add)] <- 0
#jobs_wp45[is.na(wa_jobs_PT_add)] <- 0
#leis_wp45[is.na(wa_jobs_PT_add)] <- 0
#leis_wk45[is.na(wa_jobs_PT_add)] <- 0
# 
# plot(jobs_wk45)
# plot(jobs_wp45)

#Normalize Jobs
max <- minmax(jobs_wp45) |> 
  max()
#use max to calculate new raster values 
JobsWP4 <- (jobs_wp45 / max)
#normalize WP4 without transit jobs
JobsWK4 <- (jobs_wk45 / max)

#Normalize Leis 
#max value of Leis WP4 
max <- minmax(leis_wp45) |> 
  max()
leisWP4 <- (leis_wp45 / max)
#normalize WP4 without transit leis
leisWK4 <- (leis_wk45 / max)


freq(leisWP4)
freq(leisWK4)
freq(JobsWK4)


# e. Merge jobs and leisure values by averaging - should result in 2 rasters, one for w/ and one for w/o
Avg_WP45 <- mosaic(JobsWP4, leisWP4, fun = mean)
Avg_Wk45 <- mosaic(JobsWK4, leisWK4, fun = mean)

freq(Avg_WP45)
freq(Avg_Wk45)

Wk_WP45 <- (Avg_WP45 - Avg_Wk45) / Avg_Wk45
Wk_WP45[Wk_WP45 == Inf] <-  Avg_WP45

Wk_WP45

plot(Wk_WP45)

# h. Aggregate at block level - rm.na to not factor int NAs
load("./data/WA_blocks2020w_water.RData")

WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(leisWP4))
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())

#get block ID for each raster 
PT_blk45 = terra::extract(x = Wk_WP45, y = WA_block_tigris_reproj, na.rm=TRUE) 

PT_blk45

#PT_blk[PT_blk == Inf] <- 22000

ls(PT_blk45)

#bring results into context classification and block geography
PT_blk_summaries45 <- group_by(PT_blk45, ID) |> 
  summarize( 
    n_rasters = n(), 
    avg_Wk_WP = mean(e_wa_jobs_wp4_45, na.rm = TRUE)
  ) |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID))

PT_blk_no_NaN <- na.omit(PT_blk_summaries45) |> 
  na.omit(simple_classification)


# i. Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.  
#quantile(PT_blk_no_NaN$avg_Wk_WP, probs = seq(0, 1, by = .1))
PT_blk_no_NaN$decile <- ntile(PT_blk_no_NaN$avg_Wk_WP, 10)

PT_blk_summaries45 <- PT_blk_summaries45 |> 
  left_join(
    PT_blk_no_NaN |> 
      select(ID, decile), 
    by = "ID"
  )

PT_blk_summaries45 <- PT_blk_summaries45 |> 
  mutate(
    decile = replace_na(decile, 0)
  ) |>
  select(GEOID20, TRACTCE20, simple_classification, P0010001, decile, avg_Wk_WP, Shape) |> 
  filter(!is.na(simple_classification))

summary <- PT_blk_summaries45 |> 
  group_by(simple_classification) |> 
  summarise(
    n = n(), 
    avg_in = mean(avg_Wk_WP), 
    avg_dec = mean(decile), 
    Pop = sum(P0010001)
  )



# PN ----
wa_Leisure_normalized <- rast("./data/Leisure/06wa_Leisure_normalized.tif")
wa_jobs <- rast("./data/Jobs/all_WP_layered.tif")

wa_jobs

wa_Jobs_WP1 <- wa_jobs[[1]]
wa_Jobs_WP3 <- wa_jobs[[5]]

wa_leis_WP1 <- wa_Leisure_normalized[[1]]
wa_leis_WP3 <- wa_Leisure_normalized[[3]]

list(wa_Jobs_WP3)

# b. Create new sandbox raster by Normalize each destination type & WP combination: 
#   i. new raster value = (value of each square / raster max) * 100 
#Normalize Jobs
#WP1
max <- minmax(wa_Jobs_WP1) |> 
  max()
#use max to calculate new raster values 
wa_Jobs1 <- (wa_Jobs_WP1 / max)
#normalize WP3 jobs
wa_Jobs3 <- (wa_Jobs_WP3 / max)


#Normalize Leis 
#max value of Leis WP1 
max <- minmax(wa_leis_WP1) |> 
  max()
wa_leis1 <- (wa_leis_WP1 / max)
#normalize Leis WP3
wa_leis3 <- (wa_leis_WP3 / max)


# c. Merge jobs and leisure values by averaging - should result in 2 rasters, one for WP1 and one for WP3
Avg_WP1 <- mosaic(wa_Jobs1, wa_leis1, fun = mean)
Avg_WP3 <- mosaic(wa_Jobs3, wa_leis3, fun = mean)

freq(Avg_WP1)

# Skip this step, as 1:1 access would be better access in this situation d. Set all rasters with 0 diff b/w w/ and w/o transit to NA
# e. New raster - dividing WP3 access by WP1
Acc_3_1 <- Avg_WP3 / Avg_WP1
#Acc_3_1[Acc_3_1 > 1] <- 0
freq(Acc_3_1)
Acc_3_1
#here likely want to add the filter for rasters where WP1 does not have improved PT access(need values/variables from Metric 1)
#Acc_3_1[is.na(wa_jobs_PT_add)] <- NA
Acc_3_1[is.na(Acc_3_1)] <- 0

plot(Acc_3_1)

# f. Aggregate at block level - rm.na to not factor int NAs
load("./data/WA_blocks2020w_water.RData")

WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(wa_Leisure_normalized))
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())



#get block ID for each raster 
PN_blk = terra::extract(x = Acc_3_1, y = WA_block_tigris_reproj, na.rm=TRUE)

PN_blk <- PN_blk |> 
  rename(
    "PN_Ratio" = ewa_jobs_AM_WP3_P50_C45
  )

#   Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.   
PN_blk_summaries <- group_by(PN_blk, ID) |> 
  summarize( 
    n_rasters = n(), 
    avg_WP3_WP1 = mean(PN_Ratio, na.rm = TRUE),
    no_WP3_WP1 = sum(is.na(PN_Ratio))
  ) |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID))

PN_blk_no_NaN <- PN_blk_summaries |> 
  na.omit(PN_Ratio) |> 
  na.omit(simple_classification)


#   i. Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.  
#quantile(PT_blk_no_NaN$avg_Wk_WP, probs = seq(0, 1, by = .1))
PN_blk_no_NaN$decile <- ntile(PN_blk_no_NaN$avg_WP3_WP1, 10)

PN_blk_summaries <- PN_blk_summaries |> 
  left_join(
    PN_blk_no_NaN |> 
      select(ID, decile), 
    by = "ID"
  )

PN_blk_summaries <- PN_blk_summaries |> 
  # mutate(
  #   decile = replace_na(decile, 0)
  # ) |>
  select(GEOID20, TRACTCE20, simple_classification, P0010001, decile, avg_WP3_WP1, Shape) |> 
  filter(!is.na(simple_classification)) 


  

summary <- PN_blk_summaries |> 
  group_by(simple_classification) |> 
  summarise(
    n = n(), 
    avg_in = mean(avg_WP3_WP1)
  )

#sf_PN <- st_as_sf(PN_blk_summaries)
#list(sf_PN)


#AP -----
#import jobs and leisure for uncongested driving and WP1 without transit
wa_Leisure_normalized <- rast("./data/Leisure/06wa_Leisure_normalized.tif")
wa_jobs_walk1 <- rast("./data/Jobs/wa_jobs_walk1.tif")
w_wa_jobs_driving <- rast("./data/Jobs/w_wa_jobs_car.tif")
e_wa_jobs_driving <- rast("./data/Jobs/e_wa_jobs_car.tif")
wa_jobs_driving <- mosaic(e_wa_jobs_driving, w_wa_jobs_driving, fun = mean)
wa_jobs_driving <- crop(wa_jobs_driving, wa_jobs_walk1, mask = TRUE)
wa_jobs_driving <- wa_jobs_driving[[1]]

wa_Leisure_normalized

wa_Jobs_Wk1 <- wa_jobs_walk1[[1]]
wa_leis_Wk1 <- wa_Leisure_normalized[[6]]
wa_leis_car <- wa_Leisure_normalized[[5]]

#Normalize in same way previous metrics have been 
#Normalize Jobs
#use max to calculate new raster values 

#normalize driving jobs
max <- minmax(wa_jobs_driving) |> 
  max()
wa_JobsCar <- (wa_jobs_driving / max)

max <- minmax(wa_Jobs_Wk1) |> 
  max()
wa_JobsWk1 <- (wa_Jobs_Wk1 / max)


#Normalize Leis 
#normalize Leis WP3
max <- minmax(wa_leis_car) |> 
  max()
wa_leiscar <- (wa_leis_car / max)
#max value of Leis Wk1 
max <- minmax(wa_leis_Wk1) |> 
  max()

wa_leisWK1 <- (wa_leis_Wk1 / max)


# c. Merge jobs and leisure values by averaging - should result in 2 rasters, one for WP1 and one for WP3
Avg_Wk1 <- mosaic(wa_JobsWk1, wa_leisWK1, fun = mean)
Avg_Car <- mosaic(wa_JobsCar, wa_leiscar, fun = mean)

freq(Avg_Car)

# Skip this step, as 1:1 access would be better access in this situation d. Set all rasters with 0 diff b/w w/ and w/o transit to NA
# e. New raster - dividing WP3 access by WP1
Acc_Wk1_Car <-  mosaic(Avg_Wk1, Avg_Car, fun = mean)
#Acc_Wk1_Car[Acc_Wk1_Car == Inf] <-  Avg_Wk1
#Acc_Wk1_Car[is.na(wa_jobs_PT_add)] <- NA

load("./data/WA_blocks2020w_water.RData")

WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(wa_Leisure_normalized))
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())

#get block ID for each raster 
AP_blk = terra::extract(x = Acc_Wk1_Car, y = WA_block_tigris_reproj, na.rm=TRUE)

list(AP_blk)

AP_blk <- AP_blk |> 
  rename(
    "AP_Ratio" = ewa_jobs_walk_WP1
  )

#   Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.   
AP_blk_summaries <- group_by(AP_blk, ID) |> 
  summarize( 
    n_rasters = n(), 
    avg_WP1_car = mean(AP_Ratio, na.rm = TRUE),
    no_WP1_car = sum(is.na(AP_Ratio))
  ) |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID))

AP_blk_no_NaN <- AP_blk_summaries |> 
  na.omit(AP_Ratio) |> 
  na.omit(simple_classification)


#   i. Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.  
#quantile(PT_blk_no_NaN$avg_Wk_WP, probs = seq(0, 1, by = .1))
AP_blk_no_NaN$decile <- ntile(AP_blk_no_NaN$avg_WP1_car, 10)

AP_blk_summaries <- AP_blk_summaries |> 
  left_join(
    AP_blk_no_NaN |> 
      select(ID, decile), 
    by = "ID"
  )

AP_blk_summaries <- AP_blk_summaries |> 
  # mutate(
  #   decile = replace_na(decile, 0)
  # ) |>
  select(GEOID20, TRACTCE20, simple_classification, P0010001, decile, avg_WP1_car, Shape) |> 
  filter(!is.na(simple_classification))

summary <- AP_blk_summaries |> 
  group_by(simple_classification) |> 
  summarise(
    n = n(), 
    avg_in = mean(na.omit(avg_WP1_car))
  )

#AE ----
wa_Leisure_normalized <- rast("./data/Leisure/06wa_Leisure_normalized.tif")
wa_jobs <- rast("./data/Jobs/all_WP_layered.tif")
wa_Jobs_WP1 <- wa_jobs[[1]]
wa_leis_WP1 <- wa_Leisure_normalized[[1]]
wa_leis_car <- wa_Leisure_normalized[[5]]

w_wa_jobs_driving <- rast("./data/Jobs/w_wa_jobs_car.tif")
e_wa_jobs_driving <- rast("./data/Jobs/e_wa_jobs_car.tif")
wa_jobs_driving <- mosaic(e_wa_jobs_driving, w_wa_jobs_driving, fun = mean)
wa_jobs_driving <- crop(wa_jobs_driving, wa_Jobs_WP1, mask = TRUE)

wa_jobs_driving

plot(w_wa_jobs_driving)
plot(wa_leis_car)


#Normalize in same way previous metrics have been 
#Normalize Jobs

#normalize WP3 jobs
max <- minmax(wa_jobs_driving) |> 
  max()
wa_JobsCar <- (wa_jobs_driving / max)
#use max to calculate new raster values 
wa_Jobs1 <- (wa_Jobs_WP1 / max)

#Normalize Leis 
#normalize Leis WP3
max <- minmax(wa_leis_car) |> 
  max()
wa_leiscar <- (wa_leis_car / max) 
wa_leis1 <- (wa_leis_WP1 / max)

# c. Merge jobs and leisure values by averaging - should result in 2 rasters, one for WP1 and one for WP3
Avg_WP1 <- mosaic(wa_Jobs1, wa_leis1, fun = mean)
Avg_Car <- mosaic(wa_JobsCar, wa_leiscar, fun = mean)

freq(Avg_Car)

Avg_WP1
Avg_Car

# Skip this step, as 1:1 access would be better access in this situation d. Set all rasters with 0 diff b/w w/ and w/o transit to NA
# e. New raster - dividing WP3 access by WP1
Acc_1_Car <-  Avg_WP1 / Avg_Car
Acc_1_Car[Acc_1_Car == Inf] <-  Avg_WP1
#Acc_1_Car[is.na(wa_jobs_PT_add)] <- NA
#Acc_3_1[Acc_3_1 > 1] <- 0

load("./data/WA_blocks2020w_water.RData")

WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(wa_Leisure_normalized))
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())

#get block ID for each raster 
AE_blk = terra::extract(x = Acc_1_Car, y = WA_block_tigris_reproj, na.rm=TRUE)

list(AE_blk)

AE_blk <- AE_blk |> 
  rename(
    "AE_Ratio" = ewa_jobs_AM_WP1_P50_C45
  )

#   Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.   
AE_blk_summaries <- group_by(AE_blk, ID) |> 
  summarize( 
    n_rasters = n(), 
    avg_WP1_car = mean(AE_Ratio, na.rm = TRUE),
    no_WP1_car = sum(is.na(AE_Ratio))
  ) |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID))

AE_blk_no_NaN <- AE_blk_summaries |> 
  na.omit(AE_Ratio) |> 
  na.omit(simple_classification)


#   i. Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.  
#quantile(PT_blk_no_NaN$avg_Wk_WP, probs = seq(0, 1, by = .1))
AE_blk_no_NaN$decile <- ntile(AE_blk_no_NaN$avg_WP1_car, 10)

AE_blk_summaries <- AE_blk_summaries |> 
  left_join(
    AE_blk_no_NaN |> 
      select(ID, decile), 
    by = "ID"
  )

AE_blk_summaries <- AE_blk_summaries |> 
  # mutate(
  #   decile = replace_na(decile, 0)
  # ) |>
  select(GEOID20, TRACTCE20, simple_classification, P0010001, decile, avg_WP1_car, Shape) |> 
  filter(!is.na(simple_classification))



#bring together into statewide ----
stwd1 <- PN_blk_summaries |> 
  left_join(
    PT_blk_summaries45 |> select(GEOID20, decile, avg_Wk_WP), 
    join_by(GEOID20 == GEOID20)) |> 
  left_join(
    AP_blk_summaries |> select(GEOID20, decile, avg_WP1_car), 
    join_by(GEOID20 == GEOID20)) |>
  left_join(AE_blk_summaries |> select(GEOID20, decile, avg_WP1_car), 
            join_by(GEOID20 == GEOID20)) |>
  mutate(
    wt_PN = avg_WP3_WP1 * P0010001,
    wt_PT = avg_Wk_WP * P0010001, 
    wt_AP =  avg_WP1_car.x * P0010001, 
    wt_AE =  avg_WP1_car.y * P0010001
  )

stwd1

stwd1[is.na(stwd1)] <- 0

stwd_values <- stwd1 |> 
  summarise(
  Statewide_PN = sum(wt_PN, rm.na = TRUE) / sum(P0010001), 
  Statewide_PT =  sum(wt_PT) / sum(P0010001), 
  Statewide_AP = sum(wt_AP) / sum(P0010001), 
  Statewide_AE =  sum(wt_AE) / sum(P0010001) 
)


statewide_classification <- stwd1 |> 
  group_by(simple_classification) |> 
  summarise(
    Avg_PN = sum(wt_PN, rm.na = TRUE) / sum(P0010001), 
    Avg_PT =  sum(wt_PT) / sum(P0010001), 
    Avg_AP = sum(wt_AP) / sum(P0010001), 
    Avg_AE =  sum(wt_AE) / sum(P0010001) 
  )


stwdec <- PN_blk_summaries |> 
  left_join(
    PT_blk_summaries45 |> select(GEOID20, decile, avg_Wk_WP), 
    join_by(GEOID20 == GEOID20)) |> 
  left_join(
    AP_blk_summaries |> select(GEOID20, decile, avg_WP1_car), 
    join_by(GEOID20 == GEOID20)) |>
  left_join(AE_blk_summaries |> select(GEOID20, decile, avg_WP1_car), 
            join_by(GEOID20 == GEOID20)) |>
  mutate(
    dec_PN = decile.x * P0010001,
    dec_PT = decile.y * P0010001, 
    dec_AP =  decile.x.x * P0010001, 
    dec_AE =  decile.y.y * P0010001
  )

stwdec[is.na(stwdec)] <- 0

stwd_deciles <- stwdec |> 
  summarise(
    Statewide_PNdec = sum(dec_PN) / sum(P0010001), 
    Statewide_PTdec =  sum(dec_PT) / sum(P0010001), 
    Statewide_APdec = sum(dec_AP) / sum(P0010001), 
    Statewide_AEdec =  sum(dec_AE) / sum(P0010001) 
  )

statewide_classification_dec <- stwdec |> 
  group_by(simple_classification) |> 
  summarise(
    Avg_PNdec = sum(dec_PN) / sum(P0010001), 
    Avg_PTdec =  sum(dec_PT) / sum(P0010001), 
    Avg_APdec = sum(dec_AP) / sum(P0010001), 
    Avg_AEdec =  sum(dec_AE) / sum(P0010001) 
  )



#stwd_PT_PN <- st_as_sf(PN_blk_summaries)
#st_write(stwd_PT_PN, dsn = "./output/Summaries/statewidePN_PT_Network_1.1.shp", delete_layer = TRUE)




#Archived ----
#Not in use - #PT Network 1.2 - WK4 vs WP4 ----
#running with equal walk time, no filter for PT impact required
ewa_leis_wk45 <- rast("./data/Consolidated_inputs/e_wa_leis_wk4_45.tif")
wwa_leis_wk45 <- rast("./data/Consolidated_inputs/w_wa_leis_wk4_45.tif")
ewa_leis_wp45 <- rast("./data/Consolidated_inputs/e_wa_leis_wp4_45.tif")
wwa_leis_wp45 <- rast("./data/Consolidated_inputs/w_wa_leis_wp4_45.tif")

ewa_jobs_wk45 <- rast("./data/Consolidated_inputs/e_wa_jobs_wk4_45.tif")
wwa_jobs_wk45 <- rast("./data/Consolidated_inputs/w_wa_jobs_wk4_45.tif")
ewa_jobs_wp45 <- rast("./data/Consolidated_inputs/e_wa_jobs_wp4_45.tif")
wwa_jobs_wp45 <- rast("./data/Consolidated_inputs/w_wa_jobs_wp4_45.tif")

plot(ewa_leis_wk45)

leis_wk45 <-  mosaic(ewa_leis_wk45, wwa_leis_wk45, fun = mean)
leis_wk45<- round(leis_wk45 / 3, digits = 0)

leis_wp45 <- mosaic(ewa_leis_wp45, wwa_leis_wp45, fun = mean)
leis_wp45 <- round(leis_wp45 / 3, digits = 0)

jobs_wk45 <- mosaic(ewa_jobs_wk45, wwa_jobs_wk45, fun = mean)
jobs_wp45 <- mosaic(ewa_jobs_wp45, wwa_jobs_wp45, fun = mean)

rm(ewa_leis_wk45, wwa_leis_wk45, ewa_leis_wp45, wwa_leis_wp45, ewa_jobs_wk45, wwa_jobs_wk45, ewa_jobs_wp45, wwa_jobs_wp45)

list(jobs_wp45)

#fitler rasters where walking access is equal to PT access | filter is now for any areas where <500 jobs are added from transit to limit noise from ped discrepancy in Conveyal
wa_jobs_PT_add <- jobs_wp45
wa_jobs_PT_add[jobs_wp45 <= jobs_wk45]<- NA

wa_jobs_PT_add

plot(wa_jobs_PT_add)

jobs_wk45[is.na(wa_jobs_PT_add)] <- NA
jobs_wp45[is.na(wa_jobs_PT_add)] <- NA
leis_wp45[is.na(wa_jobs_PT_add)] <- NA
leis_wk45[is.na(wa_jobs_PT_add)] <- NA

plot(jobs_wk45)
plot(jobs_wp45)

#Normalize Jobs
max <- minmax(jobs_wp45) |> 
  max()
#use max to calculate new raster values 
JobsWP4 <- (jobs_wp45 / max) * 100

#normalize WP4 without transit jobs
max <- minmax(jobs_wk45) |> 
  max()
JobsWK4 <- (jobs_wk45 / max) * 100



#Normalize Leis 
#max value of Leis WP4 
max <- minmax(leis_wp45) |> 
  max()
leisWP4 <- (leis_wp45 / max) * 100

rm(max)

#normalize WP4 without transit leis
max <- minmax(leis_wk45) |> 
  max()
leisWK4 <- (leis_wk45 / max) * 100


freq(leisWP4)
freq(JobsWP4)


# e. Merge jobs and leisure values by averaging - should result in 2 rasters, one for w/ and one for w/o
Avg_WP45 <- mosaic(JobsWP4, leisWP4, fun = mean)
Avg_Wk45 <- mosaic(JobsWK4, leisWK4, fun = mean)

freq(Avg_WP45)
freq(Avg_Wk45)

Wk_WP45 <- (Avg_WP45 - Avg_Wk45) / Avg_Wk45
Wk_WP45[Wk_WP45 == Inf] <-  Avg_WP45 

Wk_WP45

plot(Wk_WP45)

# h. Aggregate at block level - rm.na to not factor int NAs
load("./data/WA_blocks2020w_water.RData")

WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(wa_Leisure_normalized))
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())

#get block ID for each raster 
PT_blk45 = terra::extract(x = Wk_WP45, y = WA_block_tigris_reproj, na.rm=TRUE) 

PT_blk45

#PT_blk[PT_blk == Inf] <- 22000

ls(PT_blk45)

#bring results into context classification and block geography
PT_blk_summaries45 <- group_by(PT_blk45, ID) |> 
  summarize( 
    n_rasters = n(), 
    avg_Wk_WP = mean(e_wa_jobs_wp4_45, na.rm = TRUE)
  ) |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID))

PT_blk_no_NaN <- na.omit(PT_blk_summaries45) |> 
  na.omit(simple_classification)


#   i. Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.  
#quantile(PT_blk_no_NaN$avg_Wk_WP, probs = seq(0, 1, by = .1))
PT_blk_no_NaN$decile <- ntile(PT_blk_no_NaN$avg_Wk_WP, 10)

PT_blk_summaries45 <- PT_blk_summaries45 |> 
  left_join(
    PT_blk_no_NaN |> 
      select(ID, decile), 
    by = "ID"
  )

PT_blk_summaries45 <- PT_blk_summaries45 |> 
  mutate(
    decile = replace_na(decile, 0)
  ) |>
  select(GEOID20, TRACTCE20, simple_classification, P0010001, decile, avg_Wk_WP, Shape) |> 
  filter(!is.na(simple_classification))

summary <- PT_blk_summaries45 |> 
  group_by(simple_classification) |> 
  summarise(
    n = n(), 
    avg_in = mean(na.omit(avg_Wk_WP)), 
    avg_dec = mean(decile)
  )

sf_PT45 <- st_as_sf(PT_blk_summaries45)
#think this is about ready for shp conversion and exploration
#mapview(sf_PT, zcol = "decile")

st_write(sf_PT45, dsn = "./output/Summaries/PT_Network_1.2.shp", delete_layer = TRUE)

#create summary tables/graphs for measure
decile_land_use45 <- PT_blk_summaries45 |> 
  group_by(simple_classification, decile) |> 
  summarize(
    n = n()
  ) |> 
  pivot_wider(names_from = simple_classification, values_from = n) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

df_pct45 <- decile_land_use45 %>%
  filter(decile != 0) |> 
  mutate(across(everything(), prop.table, .names = "{col}_pct"))


#Only Jobs % change Wk vs WP1
jobs_wk45[is.na(wa_jobs_PT_add)] <- NA
jobs_wp45[is.na(wa_jobs_PT_add)] <- NA

jobs_wk_wp <- jobs_wk45 / jobs_wp45

load("./data/WA_blocks2020w_water.RData")

WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(jobs_wk45))
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())

#get block ID for each raster 
jobs_wk_wp_blck = terra::extract(x = jobs_wk_wp, y = WA_block_tigris_reproj, na.rm=TRUE) 

jobs_wk_wp_blck

PT_blk_summaries_jobs <- group_by(jobs_wk_wp_blck, ID) |> 
  summarize( 
    n_rasters = n(), 
    avg_Wk_WP = mean(e_wa_jobs_wk4_45, na.rm = TRUE)
  ) |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID)) |> 
  filter(!is.na(simple_classification))


sf_Jobs_Wk_WP <- st_as_sf(PT_blk_summaries_jobs)
st_write(PT_blk_summaries_jobs, dsn = "./output/Summaries/Jobs_Percent_1.0.shp", delete_layer = TRUE)
#Not in use - #Pedestrian Network Quality - WP1 vs WP3 ----
# a. Load WP1 & WP3  Leis, Load WP1 & WP3 Jobs access;
wa_Leisure_normalized <- rast("./data/Leisure/06wa_Leisure_normalized.tif")
wa_jobs <- rast("./data/Jobs/all_WP_layered.tif")

wa_jobs

wa_Jobs_WP1 <- wa_jobs[[1]]
wa_Jobs_WP3 <- wa_jobs[[5]]

wa_leis_WP1 <- wa_Leisure_normalized[[1]]
wa_leis_WP3 <- wa_Leisure_normalized[[3]]

list(wa_Jobs_WP3)

# b. Create new sandbox raster by Normalize each destination type & WP combination: 
#   i. new raster value = (value of each square / raster max) * 100 
#Normalize Jobs
#WP1
max <- minmax(wa_Jobs_WP1) |> 
  max()
#use max to calculate new raster values 
wa_Jobs1 <- (wa_Jobs_WP1 / max) * 100

#normalize WP3 jobs
max <- minmax(wa_Jobs_WP3) |> 
  max()
wa_Jobs3 <- (wa_Jobs_WP3 / max) * 100


#Normalize Leis 
#max value of Leis WP1 
max <- minmax(wa_leis_WP1) |> 
  max()
wa_leis1 <- (wa_leis_WP1 / max) * 100

#normalize Leis WP3
max <- minmax(wa_leis_WP3) |> 
  max()
wa_leis3 <- (wa_leis_WP3 / max) * 100


# c. Merge jobs and leisure values by averaging - should result in 2 rasters, one for WP1 and one for WP3
Avg_WP1 <- mosaic(wa_Jobs1, wa_leis1, fun = mean)
Avg_WP3 <- mosaic(wa_Jobs3, wa_leis3, fun = mean)

freq(Avg_WP3)

Avg_WP1
Avg_WP3

# Skip this step, as 1:1 access would be better access in this situation d. Set all rasters with 0 diff b/w w/ and w/o transit to NA
# e. New raster - dividing WP3 access by WP1
Acc_3_1 <- Avg_WP3 / Avg_WP1
#Acc_3_1[Acc_3_1 > 1] <- 0
freq(Acc_3_1)
Acc_3_1
#here likely want to add the filter for rasters where WP1 does not have improved PT access(need values/variables from Metric 1)
Acc_3_1[is.na(wa_jobs_PT_add)] <- NA
plot(Acc_3_1)

# f. Aggregate at block level - rm.na to not factor int NAs
load("./data/WA_blocks2020w_water.RData")

WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(wa_Leisure_normalized))
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())

#get block ID for each raster 
PN_blk = terra::extract(x = Acc_3_1, y = WA_block_tigris_reproj, na.rm=TRUE)

PN_blk <- PN_blk |> 
  rename(
    "PN_Ratio" = ewa_jobs_AM_WP3_P50_C45
  )

#   Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.   
PN_blk_summaries <- group_by(PN_blk, ID) |> 
  summarize( 
    n_rasters = n(), 
    avg_WP3_WP1 = mean(PN_Ratio, na.rm = TRUE),
    no_WP3_WP1 = sum(is.na(PN_Ratio))
  ) |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID))

PN_blk_no_NaN <- PN_blk_summaries |> 
  na.omit(PN_Ratio) |> 
  na.omit(simple_classification)


#   i. Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.  
#quantile(PT_blk_no_NaN$avg_Wk_WP, probs = seq(0, 1, by = .1))
PN_blk_no_NaN$decile <- ntile(PN_blk_no_NaN$avg_WP3_WP1, 10)

PN_blk_summaries <- PN_blk_summaries |> 
  left_join(
    PN_blk_no_NaN |> 
      select(ID, decile), 
    by = "ID"
  )

PN_blk_summaries <- PN_blk_summaries |> 
  # mutate(
  #   decile = replace_na(decile, 0)
  # ) |>
  select(GEOID20, TRACTCE20, simple_classification, P0010001, decile, avg_WP3_WP1, Shape) |> 
  filter(!is.na(simple_classification))

summary <- PN_blk_summaries |> 
  group_by(simple_classification) |> 
  summarise(
    n = n(), 
    avg_in = mean(na.omit(avg_WP3_WP1))
  )

sf_PN <- st_as_sf(PN_blk_summaries)
#think this is about ready for shp conversion and exploration
mapview(sf_PN, zcol = "decile")

st_write(sf_PN, dsn = "./output/Summaries/PN_Network_1.1.shp", delete_layer = TRUE)

list(PT_blk_summaries)
#Not in use - PT Network Quality 1.0 -  ----
#		b. Load WP4 Leis (w/ and w/o transit), Load WP4 Jobs (w/ and w/o transit) access;
wa_Leisure_normalized <- rast("./data/Leisure/06wa_Leisure_normalized.tif")
#wa_jobs <- rast("./data/Jobs/all_WP_layered.tif") #using updated no-decay v7.6, same jobs distribution
#first time loading in WP4, so have to replicate original steps in Conveyal file processing
ewa_leis_Wk4 <- rast("./data/Leisure/E_WA_Leisure_Walk4.tif")
wwa_leis_Wk4 <- rast("./data/Leisure/W_WA_Leisure_Walk4.tif")
Leisure_Walk4 <- mosaic(ewa_leis_Wk4, wwa_leis_Wk4, fun = mean)
Leisure_Wk4_nrm <- round(Leisure_Walk4 / 3, digits = 0)
rm(ewa_leis_Wk4, wwa_leis_Wk4, Leisure_Walk4)

ewa_jobs_Wk4 <- rast("./data/Jobs/e_wa_jobs_WK4.tif")
wwa_jobs_Wk4 <- rast("./data/Jobs/w_wa_jobs_WK4.tif")
ewa_jobs_WP4 <- rast("./data/Jobs/e_wa_jobs_WP4.tif")
wwa_jobs_WP4 <- rast("./data/Jobs/w_wa_jobs_WP4.tif")

wa_jobs_Wk4 <- mosaic(ewa_jobs_Wk4, wwa_jobs_Wk4, fun = mean)
wa_jobs_WP4 <- mosaic(ewa_jobs_WP4, wwa_jobs_WP4, fun = mean)

rm(ewa_jobs_Wk4, wwa_jobs_Wk4, ewa_jobs_WP4, wwa_jobs_WP4)

#wa_Jobs_WP4 <- wa_jobs[[7]] #replace with files once analysis is complete
wa_leis_WP4 <- wa_Leisure_normalized[[4]]

list(wa_jobs_Wk4)

#fitler rasters where walking access is equal to PT access | filter is now for any areas where <500 jobs are added from transit to limit noise from ped discrepancy in Conveyal
wa_jobs_PT_add <- wa_jobs_WP4
wa_jobs_PT_add[wa_jobs_WP4 <= wa_jobs_Wk4 + 500]<- NA

wa_jobs_WP4
wa_jobs_PT_add

plot(wa_jobs_PT_add)
plot(wa_jobs_WP4)

wa_jobs_Wk4[is.na(wa_jobs_PT_add)] <- NA
wa_jobs_WP4[is.na(wa_jobs_PT_add)] <- NA
wa_leis_WP4[is.na(wa_jobs_PT_add)] <- NA
Leisure_Wk4_nrm[is.na(wa_jobs_PT_add)] <- NA


plot(wa_leis_WP4)
plot(Leisure_Wk4_nrm)


#Normalize Jobs
#
max <- minmax(wa_jobs_WP4) |> 
  max()
#use max to calculate new raster values 
wa_Jobs4w <- (wa_jobs_WP4 / max) * 100

#normalize WP4 without transit jobs
max <- minmax(wa_jobs_Wk4) |> 
  max()
wa_Jobs4wo <- (wa_jobs_Wk4 / max) * 100



#Normalize Leis 
#max value of Leis WP4 
max <- minmax(wa_leis_WP4) |> 
  max()
wa_leis4w <- (wa_leis_WP4 / max) * 100

rm(max)

#normalize WP4 without transit leis
max <- minmax(Leisure_Wk4_nrm) |> 
  max()
wa_leis4wo <- (Leisure_Wk4_nrm / max) * 100


freq(wa_Jobs4wo)
freq(wa_leis4wo)


# e. Merge jobs and leisure values by averaging - should result in 2 rasters, one for w/ and one for w/o
Avg_WP4 <- mosaic(wa_Jobs4w, wa_leis4w, fun = mean)
Avg_Wk4 <- mosaic(wa_Jobs4wo, wa_leis4wo, fun = mean)

freq(Avg_WP4)
freq(Avg_Wk4)

Wk_WP <- (Avg_WP4 - Avg_Wk4) / Avg_Wk4
Wk_WP[Wk_WP == Inf] <-  Avg_WP4 

plot(Wk_WP)

# h. Aggregate at block level - rm.na to not factor int NAs
load("./data/WA_blocks2020w_water.RData")

WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(wa_Leisure_normalized))
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())

#get block ID for each raster 
PT_blk = terra::extract(x = Wk_WP, y = WA_block_tigris_reproj, na.rm=TRUE) 


#PT_blk[PT_blk == Inf] <- 22000
PT_blk |> rename(
  "Wk_WP" = e_wa_jobs_WP4
)

ls(PT_blk)

#bring results into context classification and block geography
PT_blk_summaries <- group_by(PT_blk, ID) |> 
  summarize( 
    n_rasters = n(), 
    avg_Wk_WP = mean(e_wa_jobs_WP4, na.rm = TRUE)
  ) |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID))

PT_blk_no_NaN <- na.omit(PT_blk_summaries) |> 
  na.omit(simple_classification)


#   i. Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.  
#quantile(PT_blk_no_NaN$avg_Wk_WP, probs = seq(0, 1, by = .1))
PT_blk_no_NaN$decile <- ntile(PT_blk_no_NaN$avg_Wk_WP, 10)

PT_blk_summaries <- PT_blk_summaries |> 
  left_join(
    PT_blk_no_NaN |> 
      select(ID, decile), 
    by = "ID"
  )

PT_blk_summaries <- PT_blk_summaries |> 
  mutate(
    decile = replace_na(decile, 0)
  ) |>
  select(GEOID20, TRACTCE20, simple_classification, P0010001, decile, avg_Wk_WP, Shape) |> 
  filter(!is.na(simple_classification))

summary <- PT_blk_summaries |> 
  group_by(simple_classification) |> 
  summarise(
    n = n(), 
    avg_in = mean(na.omit(avg_Wk_WP)), 
    avg_dec = mean(decile)
  )

sf_PT <- st_as_sf(PT_blk_summaries)
#think this is about ready for shp conversion and exploration
mapview(sf_PT, zcol = "decile")

st_write(sf_PT, dsn = "./output/Summaries/PT_Network_1.0.shp", delete_layer = TRUE)

#create summary tables/graphs for measure
decile_land_use <- PT_blk_summaries |> 
  group_by(simple_classification, decile) |> 
  summarize(
    n = n()
  ) |> 
  pivot_wider(names_from = simple_classification, values_from = n) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

df_pct <- decile_land_use %>%
  filter(decile != 0) |> 
  mutate(across(everything(), prop.table, .names = "{col}_pct"))


#Not in use - PT Network 1.1 diverging branch for alternate inf handling - now mostly aligned with 1.0 -----
Wk_WP <- (Avg_WP4 - Avg_Wk4) / Avg_Wk4 
Wk_WP[Wk_WP == Inf] <-  Avg_WP4
#Wk_WP[Wk_WP < 0] <- NA

freq(Wk_WP)

# h. Aggregate at block level - rm.na to not factor int NAs
load("./data/WA_blocks2020w_water.RData")

WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(wa_Leisure_normalized))
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())

#get block ID for each raster 
PT_blk = terra::extract(x = Wk_WP, y = WA_block_tigris_reproj, na.rm=TRUE) 

list(PT_blk)

PT_blk <- PT_blk |> rename(
  "Wk_WP" = e_wa_jobs_WP4
)


#bring results into context classification and block geography




PT_blk_summaries <- group_by(PT_blk, ID) |> 
  summarize( 
    n_rasters = n(), 
    avg_Wk_WP = mean(Wk_WP, na.rm = TRUE),
    no_Wk_WP = sum(is.na(Wk_WP))
  ) |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID))

PT_blk_no_NaN <- na.omit(PT_blk_summaries) |> 
  na.omit(simple_classification)

#   i. Assign decile score to blocks, from 1-10 based on comp to max - Assign blocks w/ NA a 0.  
#quantile(PT_blk_no_NaN$avg_Wk_WP, probs = seq(0, 1, by = .1))
PT_blk_no_NaN$decile <- ntile(PT_blk_no_NaN$avg_Wk_WP, 10)

PT_blk_summaries <- PT_blk_summaries |> 
  left_join(
    PT_blk_no_NaN |> 
      select(ID, decile), 
    by = "ID"
  ) 

summary <- PT_blk_summaries |> 
  group_by(simple_classification) |> 
  summarise(
    n = n(), 
    #avg_in = mean(na.omit(avg_Wk_WP)), 
    #avg_none =  mean(no_Wk_WP), 
    avg_decile = mean(decile, na.rm = TRUE)
  )

str(PT_blk_summaries)

PT_blk_summaries <- PT_blk_summaries |> 
  select(
    GEOID20, TRACTCE20, simple_classification, P0010001, decile, avg_Wk_WP, Shape
  ) |> 
  filter(!is.na(simple_classification))



sf_PT <- st_as_sf(PT_blk_summaries)
#think this is about ready for shp conversion and exploration
st_write(sf_PT, dsn = "./output/Summaries/PT_Network_1.2.shp", delete_layer = TRUE)

list(PT_blk_summaries)


#PT Network 1.2 - Like to like walking to travel time 
