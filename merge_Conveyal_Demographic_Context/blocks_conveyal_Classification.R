#unify context classification + demographics with Conveyal
#This script was designed to mostly run one destination category at a time, and thus there are functions that are repeatetive across the full script.   
#

library(tidyverse)
library(sf)
library(terra)
library(writexl)
library(stringr)

getwd()

#census geometries
load("./data/WA_block_groups2020w_water.RData")
load("./data/WA_tracts2020w_water.RData")
load("./data/WA_blocks2020w_water.RData")

#2024 ACS demo data
load("./data/Demographic/wa_bg_combined_demo.RDATA")
load("./data/Demographic/wa_tracts_combined_demo.RDATA")

block_group_demo <- wa_bg_combined_demo |> 
  mutate(
    POC = poc_bg / summary_est, 
    u200 = u200pov_bg / summary_est, 
    u100 = u100pov_bg / summary_est, 
    u18 = u18_bg / summary_est, 
    o65 = o65_bg / summary_est
  )

tract_demo <- wa_tracts_combined_demo |> 
  mutate(
    a_w_dis = disability_tr / summary_est
  )




#updated file pull when GDB has multiple layers in it - specify layer choice by name. 
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

#context classification only geographies
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

save(context_class_Simple_Groups, file = "./data/context_classification/simplified_context_classification.RDATA") 


#Leisure ----
#load processed leisure raster
wa_Leisure_normalized <- rast("./data/Leisure/06wa_Leisure_normalized.tif")

#reproject Washington state census blocks to match the coordinate reference system which the raster uses.
WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(wa_Leisure_normalized))
#add unique identifier via count function for each row/census block for reference by terra::extract function in next step
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())
#terra extract outputs each raster value and the spatially associated unique row/block identifier --output is table 4.5 mil rows long with raster value and block identifier. 
wa_leis_blck = terra::extract(x = wa_Leisure_normalized, y = WA_block_tigris_reproj) 

#dataset summary to get snippet of output from previous step
#summary(wa_leis_blck)

#summarize results by block, 0acc values count how many rasters are 0 (one of our summary is what % of rasters have no access) 
blk_lei_summaries_0acc <- group_by(wa_leis_blck, ID) |> 
  summarize( 
    n_rasters = n(), 
    WP_1_0Acc = sum(E_WA_Leisure_WP1 == 0),
    WP_2_0Acc = sum(E_WA_Leisure_WP2 == 0),
    WP_3_0Acc = sum(E_WA_Leisure_WP3 == 0),
    WP_4_0Acc = sum(E_WA_Leisure_WP4 == 0), 
    Car_0Acc = sum(E_WA_Leisure_Car == 0),
    Wlk1_0Acc = sum(E_WA_Leisure_Walk1  == 0),
    Wlk2_0Acc = sum(E_WA_Leisure_Walk2 == 0) 
  ) |> 
  pivot_longer(
    cols = ends_with("0Acc"),
    names_to = "WP",
    values_to = "ZeroAccess"
  ) |> 
  mutate(
    WP_ID = paste(substr(WP, 1, 4), ID, sep = "")
  )

#average raster values for each walking profile and mode within each census block
blk_leis_summaries <- group_by(wa_leis_blck, ID) |> 
  summarize( 
    n_rasters = n(), 
    WP_1_mean = mean(E_WA_Leisure_WP1), 
    WP_2_mean = mean(E_WA_Leisure_WP2),
    WP_3_mean = mean(E_WA_Leisure_WP3), 
    WP_4_mean = mean(E_WA_Leisure_WP4), 
    Car_mean = mean(E_WA_Leisure_Car),
    Wlk1_mean = mean(E_WA_Leisure_Walk1), 
    Wlk2_mean = mean(E_WA_Leisure_Walk2)
  ) |> 
  pivot_longer(
    cols = ends_with("mean"),
    names_to = "WP",
    values_to = "Avg_Access"
  ) |> 
  mutate(
    WP_ID = paste(substr(WP, 1, 4), ID, sep = ""), 
    WP = substr(WP, 1, 4)
  ) |> 
  left_join(
    blk_lei_summaries_0acc |> select(ZeroAccess, WP_ID), 
    join_by(WP_ID == WP_ID)) 

#combine calculated values from above with blocks and context classifications
blk_leis_context <- blk_leis_summaries |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID)) |> 
  mutate(ZeroAccess_Perc = ZeroAccess / n_rasters,
         za_perc_pr = ZeroAccess_Perc * P0010001,
         acc_pr = Avg_Access * P0010001) |> 
  select(1, WP, TRACTCE20, GEOID20, simple_classification, P0010001, n_rasters, ZeroAccess, ZeroAccess_Perc, za_perc_pr, Avg_Access, acc_pr)

save(blk_leis_summaries, file = "./output/Summaries/blk_leis_summaries.RData")

#summarize findings at block level - moves walking profiles and modes from individual rows to  column
blk_summaries <- blk_leis_context |> 
  group_by(simple_classification, WP) |> 
  summarise(
    population = sum(P0010001), 
    total_rast = sum(n_rasters), 
    Zero_Access = sum(za_perc_pr) / sum(P0010001), 
    Access_Avg = sum(acc_pr) / sum(P0010001)
  ) |> pivot_wider(
    names_from = WP,
    values_from = c(Access_Avg, Zero_Access)
  ) |> 
  mutate(
    demographic_group = "Total"
  ) 
#aggregates average values statewide
statewide_summary <- blk_leis_context |> 
  group_by(WP) |> 
  summarise(
    population = sum(P0010001), 
    total_rast = sum(n_rasters), 
    Zero_Access = sum(za_perc_pr) / sum(P0010001), 
    Access_Avg = sum(acc_pr) / sum(P0010001)
  ) |> pivot_wider(
    names_from = WP,
    values_from = c(Access_Avg, Zero_Access)
  ) |> 
  mutate(
    demographic_group = "Total"
  ) 

combined_summary <- do.call(rbind, 
        list(blk_summaries, statewide_summary)) 

#Adding demographic data from block groups and tracts to census blocks
blk_leis_context_demo <- blk_leis_context |>
  mutate(
    BG_GEOID = substr(GEOID20, 1, nchar(GEOID20) -3), 
    TR_GEOID = substr(GEOID20, 1, nchar(GEOID20) -4)
  ) |> 
  left_join(
    block_group_demo |> select(1, 3, 15:19), join_by(BG_GEOID == GEOID)
  ) |> 
  left_join(
    tract_demo |> select(1, 17), join_by(TR_GEOID == GEOID)
  )

names(blk_leis_context_demo)


#update population from 2020 census block populations scaled to increased 2024 ACS population counts and updated block group demographic percentages. Average access is then multiplied by population, grouped by context classification and walking profile, and divided by populations in order to get weighted average access for demographic groups at each context classification.  
blk_leis_context_demo_pop <- blk_leis_context_demo |> 
  group_by(BG_GEOID) |> 
  mutate(blk_pop = sum(P0010001) / 7, 
         pop_inc = summary_est/blk_pop) |> 
  ungroup() |> 
  mutate(
    updated_pop = P0010001 * pop_inc, 
    est_POC = updated_pop * POC,
    est_u200 = updated_pop * u200,
    est_u100 = updated_pop * u100,
    est_u18 = updated_pop * u18, 
    est_o65 = updated_pop * o65, 
    est_dis = updated_pop * a_w_dis,
    est_POC_acc = updated_pop * Avg_Access * POC,
    est_u200_acc = updated_pop * Avg_Access * u200,
    est_u100_acc = updated_pop * Avg_Access * u100,
    est_u18_acc = updated_pop * Avg_Access * u18, 
    est_o65_acc = updated_pop * Avg_Access * o65,
    est_dis_acc = updated_pop * Avg_Access * a_w_dis,
    est_POC_za = updated_pop * ZeroAccess_Perc * POC,
    est_u200_za = updated_pop * ZeroAccess_Perc * u200,
    est_u100_za = updated_pop * ZeroAccess_Perc * u100,
    est_u18_za = updated_pop * ZeroAccess_Perc * u18, 
    est_o65_za = updated_pop * ZeroAccess_Perc * o65,
    est_dis_za = updated_pop * ZeroAccess_Perc * a_w_dis,
  ) |> group_by(WP, simple_classification) |> 
  summarise(
    POC_Pop = sum(est_POC, na.rm = TRUE), 
    POC_Acc = sum(est_POC_acc, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    POC_za = sum(est_POC_za, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    u200_Pop = sum(est_u200, na.rm = TRUE), 
    u200_Acc = sum(est_u200_acc, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u200_za = sum(est_u200_za, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u100_Pop = sum(est_u100, na.rm = TRUE), 
    u100_Acc = sum(est_u100_acc, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u100_za = sum(est_u100_za, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u18_Pop = sum(est_u18, na.rm = TRUE), 
    u18_Acc = sum(est_u18_acc, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    u18_za = sum(est_u18_za, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    o65_Pop = sum(est_o65, na.rm = TRUE), 
    o65_Acc = sum(est_o65_acc, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    o65_za = sum(est_o65_za, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    dis_Pop = sum(est_dis, na.rm = TRUE), 
    dis_Acc = sum(est_dis_acc, na.rm = TRUE) / sum(est_dis, na.rm = TRUE), 
    dis_za = sum(est_dis_za, na.rm = TRUE) / sum(est_dis, na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = ends_with("Acc") | ends_with("za"), 
    names_to = "Demographic"
  ) |>
  mutate(
    Demo = substr(Demographic, 1, 4), 
    Measure = substr(Demographic, nchar(Demographic)-2, nchar(Demographic))
  ) |> 
  pivot_wider(
    names_from = Measure, 
    values_from = value, 
    values_fill = 0
  ) |> 
  select(-Demographic) |> 
  rename(
    Zero_Acc = "_za"
  ) |> 
  group_by(WP, simple_classification, Demo) |> 
  summarise(
    Access_Avg = sum(Acc), 
    Zero_Access = sum(Zero_Acc)
  ) |> ungroup() |>
  pivot_wider(
    names_from = WP, 
    values_from = c(Access_Avg, Zero_Access) 
  ) |> 
  filter(simple_classification != "Natural")

blk_leis_context_demo_pop$demographic_group <-  blk_leis_context_demo_pop$Demo |> 
  str_replace("dis_", "Adults with a Disability") |> 
  str_replace("o65_", "65 and Over") |> 
  str_replace("POC_", "People of Color") |>
  str_replace("u100", "Income under 100% of Poverty Level") |>
  str_replace("u200",  "Income under 200% of Poverty Level") |>
  str_replace("u18_", "Under 18") 
  
blk_leis_context_demo_pop_total <- blk_leis_context_demo |> 
  group_by(BG_GEOID) |> 
  mutate(blk_pop = sum(P0010001) / 7, 
         pop_inc = summary_est/blk_pop) |> 
  ungroup() |> 
  mutate(
    updated_pop = P0010001 * pop_inc, 
    est_POC = updated_pop * POC,
    est_u200 = updated_pop * u200,
    est_u100 = updated_pop * u100,
    est_u18 = updated_pop * u18, 
    est_o65 = updated_pop * o65, 
    est_dis = updated_pop * a_w_dis,
    est_POC_acc = updated_pop * Avg_Access * POC,
    est_u200_acc = updated_pop * Avg_Access * u200,
    est_u100_acc = updated_pop * Avg_Access * u100,
    est_u18_acc = updated_pop * Avg_Access * u18, 
    est_o65_acc = updated_pop * Avg_Access * o65,
    est_dis_acc = updated_pop * Avg_Access * a_w_dis,
    est_POC_za = updated_pop * ZeroAccess_Perc * POC,
    est_u200_za = updated_pop * ZeroAccess_Perc * u200,
    est_u100_za = updated_pop * ZeroAccess_Perc * u100,
    est_u18_za = updated_pop * ZeroAccess_Perc * u18, 
    est_o65_za = updated_pop * ZeroAccess_Perc * o65,
    est_dis_za = updated_pop * ZeroAccess_Perc * a_w_dis,
  ) |> 
  filter(simple_classification != "Natural") |> 
  group_by(WP) |> 
  summarise(
    POC_Pop = sum(est_POC, na.rm = TRUE), 
    POC_Acc = sum(est_POC_acc, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    POC_za = sum(est_POC_za, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    u200_Pop = sum(est_u200, na.rm = TRUE), 
    u200_Acc = sum(est_u200_acc, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u200_za = sum(est_u200_za, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u100_Pop = sum(est_u100, na.rm = TRUE), 
    u100_Acc = sum(est_u100_acc, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u100_za = sum(est_u100_za, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u18_Pop = sum(est_u18, na.rm = TRUE), 
    u18_Acc = sum(est_u18_acc, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    u18_za = sum(est_u18_za, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    o65_Pop = sum(est_o65, na.rm = TRUE), 
    o65_Acc = sum(est_o65_acc, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    o65_za = sum(est_o65_za, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    dis_Pop = sum(est_dis, na.rm = TRUE), 
    dis_Acc = sum(est_dis_acc, na.rm = TRUE) / sum(est_dis, na.rm = TRUE), 
    dis_za = sum(est_dis_za, na.rm = TRUE) / sum(est_dis, na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = ends_with("Acc") | ends_with("za"), 
    names_to = "Demographic"
  ) |>
  mutate(
    Demo = substr(Demographic, 1, 4), 
    Measure = substr(Demographic, nchar(Demographic)-2, nchar(Demographic))
  ) |> 
  pivot_wider(
    names_from = Measure, 
    values_from = value, 
    values_fill = 0
  ) |> 
  select(-Demographic) |> 
  rename(
    Zero_Acc = "_za"
  ) |> 
  group_by(WP, Demo) |> 
  summarise(
    Access_Avg = sum(Acc), 
    Zero_Access = sum(Zero_Acc)
  ) |> ungroup() |>
  pivot_wider(
    names_from = WP, 
    values_from = c(Access_Avg, Zero_Access) 
  ) |> 
  mutate(
    simple_classification = "Statewide"
  ) |> 
  select(simple_classification, everything())


blk_leis_context_demo_pop_total$demographic_group <-  blk_leis_context_demo_pop_total$Demo |> 
  str_replace("dis_", "Adults with a Disability") |> 
  str_replace("o65_", "65 and Over") |> 
  str_replace("POC_", "People of Color") |>
  str_replace("u100", "Income under 100% of Poverty Level") |>
  str_replace("u200",  "Income under 200% of Poverty Level") |>
  str_replace("u18_", "Under 18")

names(blk_leis_context_demo_pop_total)
names(blk_leis_context_demo_pop)

combined_demo_summary <- do.call(rbind, 
                            list(blk_leis_context_demo_pop, blk_leis_context_demo_pop_total))

names(combined_demo_summary)

export_summary <- do.call(rbind, 
                            list(combined_summary, combined_demo_summary)) |>
  select(1, 18, 9, 16, 10, 17, 5, 12, 6, 13, 7, 14, 8, 15, 4, 11, 2)

write_xlsx(export_summary, path = "./data/Leisure/leisure_summaries.xlsx")
names(export_summary)



#Jobs ----
wa_jobs <- rast("./data/Jobs/all_WP_layered.tif")
wa_jobs_driving <- rast("./data/Jobs/statewide_driving_jobs.tif")
wa_jobs_walk1 <- rast("./data/Jobs/wa_jobs_walk.tif")
wa_jobs_combined <- c(wa_jobs, wa_jobs_driving, wa_jobs_walk1)

#reporject the blocks dataset to align with the combined raster coordinate reference system - this allows calculation
WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(wa_jobs_combined))
#re-order blocks and give sequential unique row identifier by row count - preps dataset to match automatic block identifiers used by terra::extract in the next step. 
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())

#step aggregates rasters by blocks - will take at least 5-10 minutes - save for faster re-run.
wa_jobs_blck = terra::extract(x = wa_jobs_combined, y = WA_block_tigris_reproj) 

save(wa_jobs_blck, file = "wa_jobs_blck.RData")

summary(wa_jobs_blck)

#include count of rasters with zero access -
blk_jobs_summaries_0acc <- group_by(wa_jobs_blck, ID) |> 
  summarize( 
    n_rasters = n(), 
    WP1p_0Acc = sum(ewa_jobs_PM_WP1_P50_C45 == 0),
    WP2p_0Acc = sum(ewa_jobs_PM_WP2_P50_C45 == 0),
    WP3p_0Acc = sum(ewa_jobs_PM_WP3_P50_C45 == 0),
    WP4p_0Acc = sum(ewa_jobs_PM_WP4_P50_C45 == 0), 
    Car_0Acc = sum(ewa_jobs_car_P50_C45 == 0),
    Wlk1_0Acc = sum(ewa_jobs_walk_WP1 == 0),
    Wlk2_0Acc = sum(ewa_jobs_walk_WP2 == 0), 
    WP1a_0Acc = sum(ewa_jobs_AM_WP1_P50_C45 == 0),
    WP2a_0Acc = sum(ewa_jobs_AM_WP2_P50_C45 == 0),
    WP3a_0Acc = sum(ewa_jobs_AM_WP3_P50_C45 == 0),
    WP4a_0Acc = sum(ewa_jobs_AM_WP4_P50_C45 == 0),
  ) |> 
  pivot_longer(
    cols = ends_with("0Acc"),
    names_to = "WP",
    values_to = "ZeroAccess"
  ) |> 
  mutate(
    WP_ID = paste(substr(WP, 1, 4), ID, sep = "")
  )

#calculate average value within block
blk_jobs_summaries <- group_by(wa_jobs_blck, ID) |> 
  summarize( 
    n_rasters = n(), 
    WP1p_mean = mean(ewa_jobs_PM_WP1_P50_C45), 
    WP2p_mean = mean(ewa_jobs_PM_WP2_P50_C45),
    WP3p_mean = mean(ewa_jobs_PM_WP3_P50_C45), 
    WP4p_mean = mean(ewa_jobs_PM_WP4_P50_C45), 
    Car_mean = mean(ewa_jobs_car_P50_C45),
    Wlk1_mean = mean(ewa_jobs_walk_WP1), 
    Wlk2_mean = mean(ewa_jobs_walk_WP2),
    WP1a_mean = mean(ewa_jobs_AM_WP1_P50_C45), 
    WP2a_mean = mean(ewa_jobs_AM_WP2_P50_C45),
    WP3a_mean = mean(ewa_jobs_AM_WP3_P50_C45), 
    WP4a_mean = mean(ewa_jobs_AM_WP4_P50_C45),
  ) |> 
  pivot_longer(
    cols = ends_with("mean"),
    names_to = "WP",
    values_to = "Avg_Access"
  ) |> 
  mutate(
    WP_ID = paste(substr(WP, 1, 4), ID, sep = ""), 
    WP = substr(WP, 1, 4)
  ) |> 
  left_join(
    blk_jobs_summaries_0acc |> select(ZeroAccess, WP_ID), 
    join_by(WP_ID == WP_ID)) 

#assign 0 access and mean values to block
#calculate 0 access % for block, and give weighted products for zero access % and mean
blk_jobs_context <- blk_jobs_summaries |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID)) |> 
  mutate(ZeroAccess_Perc = ZeroAccess / n_rasters,
         za_perc_pr = ZeroAccess_Perc * P0010001,
         acc_pr = Avg_Access * P0010001) |> 
  select(1, WP, TRACTCE20, GEOID20, simple_classification, P0010001, n_rasters, ZeroAccess, ZeroAccess_Perc, za_perc_pr, Avg_Access, acc_pr)

save(blk_jobs_context, file = "./output/Summaries/blk_jobs_context.RDATA")

#summarize findings
blk_summaries_jobs <- blk_jobs_context |> 
  group_by(simple_classification, WP) |> 
  summarise(
    population = sum(P0010001), 
    total_rast = sum(n_rasters), 
    Zero_Access = sum(za_perc_pr) / sum(P0010001), 
    Access_Avg = sum(acc_pr) / sum(P0010001)
  ) |> pivot_wider(
    names_from = WP,
    values_from = c(Access_Avg, Zero_Access)
  ) |> 
  mutate(
    demographic_group = "Total"
  ) 


statewide_summary_jobs <- blk_jobs_context |> 
  group_by(WP) |> 
  summarise(
    population = sum(P0010001), 
    total_rast = sum(n_rasters), 
    Zero_Access = sum(za_perc_pr) / sum(P0010001), 
    Access_Avg = sum(acc_pr) / sum(P0010001)
  ) |> pivot_wider(
    names_from = WP,
    values_from = c(Access_Avg, Zero_Access)
  ) |> 
  mutate(
    demographic_group = "Total"
  ) 

combined_summary_jobs <- do.call(rbind, 
                            list(blk_summaries_jobs, statewide_summary_jobs)) 

names(combined_summary)
names(combined_demo_summary)

names(blk_leis_context)

#demographic pass to block level
blk_jobs_context_demo <- blk_jobs_context |>
  mutate(
    BG_GEOID = substr(GEOID20, 1, nchar(GEOID20) -3), 
    TR_GEOID = substr(GEOID20, 1, nchar(GEOID20) -4)
  ) |> 
  left_join(
    block_group_demo |> select(1, 3, 15:19), join_by(BG_GEOID == GEOID)
  ) |> 
  left_join(
    tract_demo |> select(1, 17), join_by(TR_GEOID == GEOID)
  )


blk_jobs_context_demo_pop <- blk_jobs_context_demo |> 
  group_by(BG_GEOID) |> 
  mutate(blk_pop = sum(P0010001) / 7, 
         pop_inc = summary_est/blk_pop) |> 
  ungroup() |> 
  mutate(
    updated_pop = P0010001 * pop_inc, 
    est_POC = updated_pop * POC,
    est_u200 = updated_pop * u200,
    est_u100 = updated_pop * u100,
    est_u18 = updated_pop * u18, 
    est_o65 = updated_pop * o65, 
    est_dis = updated_pop * a_w_dis,
    est_POC_acc = updated_pop * Avg_Access * POC,
    est_u200_acc = updated_pop * Avg_Access * u200,
    est_u100_acc = updated_pop * Avg_Access * u100,
    est_u18_acc = updated_pop * Avg_Access * u18, 
    est_o65_acc = updated_pop * Avg_Access * o65,
    est_dis_acc = updated_pop * Avg_Access * a_w_dis,
    est_POC_za = updated_pop * ZeroAccess_Perc * POC,
    est_u200_za = updated_pop * ZeroAccess_Perc * u200,
    est_u100_za = updated_pop * ZeroAccess_Perc * u100,
    est_u18_za = updated_pop * ZeroAccess_Perc * u18, 
    est_o65_za = updated_pop * ZeroAccess_Perc * o65,
    est_dis_za = updated_pop * ZeroAccess_Perc * a_w_dis,
  ) |> group_by(WP, simple_classification) |> 
  summarise(
    POC_Pop = sum(est_POC, na.rm = TRUE), 
    POC_Acc = sum(est_POC_acc, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    POC_za = sum(est_POC_za, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    u200_Pop = sum(est_u200, na.rm = TRUE), 
    u200_Acc = sum(est_u200_acc, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u200_za = sum(est_u200_za, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u100_Pop = sum(est_u100, na.rm = TRUE), 
    u100_Acc = sum(est_u100_acc, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u100_za = sum(est_u100_za, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u18_Pop = sum(est_u18, na.rm = TRUE), 
    u18_Acc = sum(est_u18_acc, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    u18_za = sum(est_u18_za, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    o65_Pop = sum(est_o65, na.rm = TRUE), 
    o65_Acc = sum(est_o65_acc, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    o65_za = sum(est_o65_za, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    dis_Pop = sum(est_dis, na.rm = TRUE), 
    dis_Acc = sum(est_dis_acc, na.rm = TRUE) / sum(est_dis, na.rm = TRUE), 
    dis_za = sum(est_dis_za, na.rm = TRUE) / sum(est_dis, na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = ends_with("Acc") | ends_with("za"), 
    names_to = "Demographic"
  ) |>
  mutate(
    Demo = substr(Demographic, 1, 4), 
    Measure = substr(Demographic, nchar(Demographic)-2, nchar(Demographic))
  ) |> 
  pivot_wider(
    names_from = Measure, 
    values_from = value, 
    values_fill = 0
  ) |> 
  select(-Demographic) |> 
  rename(
    Zero_Acc = "_za"
  ) |> 
  group_by(WP, simple_classification, Demo) |> 
  summarise(
    Access_Avg = sum(Acc), 
    Zero_Access = sum(Zero_Acc)
  ) |> ungroup() |>
  pivot_wider(
    names_from = WP, 
    values_from = c(Access_Avg, Zero_Access) 
  ) |> 
  filter(simple_classification != "Natural")

blk_jobs_context_demo_pop$demographic_group <-  blk_jobs_context_demo_pop$Demo |> 
  str_replace("dis_", "Adults with a Disability") |> 
  str_replace("o65_", "65 and Over") |> 
  str_replace("POC_", "People of Color") |>
  str_replace("u100", "Income under 100% of Poverty Level") |>
  str_replace("u200",  "Income under 200% of Poverty Level") |>
  str_replace("u18_", "Under 18") 



blk_jobs_context_demo_pop_total <- blk_jobs_context_demo |> 
  group_by(BG_GEOID) |> 
  mutate(blk_pop = sum(P0010001) / 7, 
         pop_inc = summary_est/blk_pop) |> 
  ungroup() |> 
  mutate(
    updated_pop = P0010001 * pop_inc, 
    est_POC = updated_pop * POC,
    est_u200 = updated_pop * u200,
    est_u100 = updated_pop * u100,
    est_u18 = updated_pop * u18, 
    est_o65 = updated_pop * o65, 
    est_dis = updated_pop * a_w_dis,
    est_POC_acc = updated_pop * Avg_Access * POC,
    est_u200_acc = updated_pop * Avg_Access * u200,
    est_u100_acc = updated_pop * Avg_Access * u100,
    est_u18_acc = updated_pop * Avg_Access * u18, 
    est_o65_acc = updated_pop * Avg_Access * o65,
    est_dis_acc = updated_pop * Avg_Access * a_w_dis,
    est_POC_za = updated_pop * ZeroAccess_Perc * POC,
    est_u200_za = updated_pop * ZeroAccess_Perc * u200,
    est_u100_za = updated_pop * ZeroAccess_Perc * u100,
    est_u18_za = updated_pop * ZeroAccess_Perc * u18, 
    est_o65_za = updated_pop * ZeroAccess_Perc * o65,
    est_dis_za = updated_pop * ZeroAccess_Perc * a_w_dis,
  ) |> 
  filter(simple_classification != "Natural") |> 
  group_by(WP) |> 
  summarise(
    POC_Pop = sum(est_POC, na.rm = TRUE), 
    POC_Acc = sum(est_POC_acc, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    POC_za = sum(est_POC_za, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    u200_Pop = sum(est_u200, na.rm = TRUE), 
    u200_Acc = sum(est_u200_acc, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u200_za = sum(est_u200_za, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u100_Pop = sum(est_u100, na.rm = TRUE), 
    u100_Acc = sum(est_u100_acc, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u100_za = sum(est_u100_za, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u18_Pop = sum(est_u18, na.rm = TRUE), 
    u18_Acc = sum(est_u18_acc, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    u18_za = sum(est_u18_za, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    o65_Pop = sum(est_o65, na.rm = TRUE), 
    o65_Acc = sum(est_o65_acc, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    o65_za = sum(est_o65_za, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    dis_Pop = sum(est_dis, na.rm = TRUE), 
    dis_Acc = sum(est_dis_acc, na.rm = TRUE) / sum(est_dis, na.rm = TRUE), 
    dis_za = sum(est_dis_za, na.rm = TRUE) / sum(est_dis, na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = ends_with("Acc") | ends_with("za"), 
    names_to = "Demographic"
  ) |>
  mutate(
    Demo = substr(Demographic, 1, 4), 
    Measure = substr(Demographic, nchar(Demographic)-2, nchar(Demographic))
  ) |> 
  pivot_wider(
    names_from = Measure, 
    values_from = value, 
    values_fill = 0
  ) |> 
  select(-Demographic) |> 
  rename(
    Zero_Acc = "_za"
  ) |> 
  group_by(WP, Demo) |> 
  summarise(
    Access_Avg = sum(Acc), 
    Zero_Access = sum(Zero_Acc)
  ) |> ungroup() |>
  pivot_wider(
    names_from = WP, 
    values_from = c(Access_Avg, Zero_Access) 
  ) |> 
  mutate(
    simple_classification = "Statewide"
  ) |> 
  select(simple_classification, everything())

blk_jobs_context_demo_pop_total$demographic_group <-  blk_jobs_context_demo_pop_total$Demo |> 
  str_replace("dis_", "Adults with a Disability") |> 
  str_replace("o65_", "65 and Over") |> 
  str_replace("POC_", "People of Color") |>
  str_replace("u100", "Income under 100% of Poverty Level") |>
  str_replace("u200",  "Income under 200% of Poverty Level") |>
  str_replace("u18_", "Under 18")


combined_demo_summary_jobs <- do.call(rbind, 
                                 list(blk_jobs_context_demo_pop, blk_jobs_context_demo_pop_total))

names(blk_jobs_context_demo_pop)
names(blk_jobs_context_demo_pop_total)


export_summary_jobs <- do.call(rbind, 
                          list(combined_summary_jobs, combined_demo_summary_jobs)) |>
  select(1, 26, 13, 24, 14, 25, 6, 17, 8, 19, 10, 21, 12, 23, 4, 15, 5, 16, 7, 18, 9, 20, 11, 22, 2)

names(export_summary_jobs)


write_xlsx(export_summary_jobs, path = "./data/Jobs/jobs_summaries.xlsx")
names(combined_summary_jobs)



#Medical ----
wa_medical <- rast("./data/Medical/06wa_medical.tif")

##normalize medical to scale of 0 to 1
updated_wa_medical <- wa_medical / 10



WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(updated_wa_medical))
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())
wa_med_blck = terra::extract(x = updated_wa_medical, y = WA_block_tigris_reproj) 

summary(wa_med_blck)

blk_med_summaries_0acc <- group_by(wa_med_blck, ID) |> 
  summarize( 
    n_rasters = n(), 
    WP_1_0Acc = sum(W_WA_pharm_WP1     == 0),
    WP_2_0Acc = sum(W_WA_pharm_WP2     == 0),
    WP_3_0Acc = sum(W_WA_pharm_WP3     == 0),
    WP_4_0Acc = sum(W_WA_pharm_WP4     == 0), 
    Car_0Acc = sum(W_WA_pharm_car    == 0),
    Wlk1_0Acc = sum(W_WA_pharm_Walk1      == 0),
    Wlk2_0Acc = sum(W_WA_pharm_Walk2     == 0) 
  ) |> 
  pivot_longer(
    cols = ends_with("0Acc"),
    names_to = "WP",
    values_to = "ZeroAccess"
  ) |> 
  mutate(
    WP_ID = paste(substr(WP, 1, 4), ID, sep = "")
  )

blk_med_summaries <- group_by(wa_med_blck, ID) |> 
  summarize( 
    n_rasters = n(), 
    WP_1_mean = mean(W_WA_pharm_WP1), 
    WP_2_mean = mean(W_WA_pharm_WP2),
    WP_3_mean = mean(W_WA_pharm_WP3), 
    WP_4_mean = mean(W_WA_pharm_WP4), 
    Car_mean = mean(W_WA_pharm_car),
    Wlk1_mean = mean(W_WA_pharm_Walk1), 
    Wlk2_mean = mean(W_WA_pharm_Walk2)
  ) |> 
  pivot_longer(
    cols = ends_with("mean"),
    names_to = "WP",
    values_to = "Avg_Access"
  ) |> 
  mutate(
    WP_ID = paste(substr(WP, 1, 4), ID, sep = ""), 
    WP = substr(WP, 1, 4)
  ) |> 
  left_join(
    blk_med_summaries_0acc |> select(ZeroAccess, WP_ID), 
    join_by(WP_ID == WP_ID)) 

blk_med_context <- blk_med_summaries |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID)) |> 
  mutate(ZeroAccess_Perc = ZeroAccess / n_rasters,
         za_perc_pr = ZeroAccess_Perc * P0010001,
         acc_pr = Avg_Access * P0010001) |> 
  select(1, WP, TRACTCE20, GEOID20, simple_classification, P0010001, n_rasters, ZeroAccess, ZeroAccess_Perc, za_perc_pr, Avg_Access, acc_pr)

save(blk_med_context, file = "./output/summaries/blk_med_summaries.RData")

#summarize findings
blk_summaries_med <- blk_med_context |> 
  group_by(simple_classification, WP) |> 
  summarise(
    population = sum(P0010001), 
    total_rast = sum(n_rasters), 
    Zero_Access = sum(za_perc_pr) / sum(P0010001), 
    Access_Avg = sum(acc_pr) / sum(P0010001)
  ) |> pivot_wider(
    names_from = WP,
    values_from = c(Access_Avg, Zero_Access)
  ) |> 
  mutate(
    demographic_group = "Total"
  ) 

statewide_summary_med <- blk_med_context |> 
  group_by(WP) |> 
  summarise(
    population = sum(P0010001), 
    total_rast = sum(n_rasters), 
    Zero_Access = sum(za_perc_pr) / sum(P0010001), 
    Access_Avg = sum(acc_pr) / sum(P0010001)
  ) |> pivot_wider(
    names_from = WP,
    values_from = c(Access_Avg, Zero_Access)
  ) |> 
  mutate(
    demographic_group = "Total"
  ) 

combined_summary_med <- do.call(rbind, 
                            list(blk_summaries_med, statewide_summary_med)) 

#demographic pass to block level
blk_med_context_demo <- blk_med_context |>
  mutate(
    BG_GEOID = substr(GEOID20, 1, nchar(GEOID20) -3), 
    TR_GEOID = substr(GEOID20, 1, nchar(GEOID20) -4)
  ) |> 
  left_join(
    block_group_demo |> select(1, 3, 15:19), join_by(BG_GEOID == GEOID)
  ) |> 
  left_join(
    tract_demo |> select(1, 17), join_by(TR_GEOID == GEOID)
  )

names(blk_med_context_demo)

blk_med_context_demo_pop <- blk_med_context_demo |> 
  group_by(BG_GEOID) |> 
  mutate(blk_pop = sum(P0010001) / 7, 
         pop_inc = summary_est/blk_pop) |> 
  ungroup() |> 
  mutate(
    updated_pop = P0010001 * pop_inc, 
    est_POC = updated_pop * POC,
    est_u200 = updated_pop * u200,
    est_u100 = updated_pop * u100,
    est_u18 = updated_pop * u18, 
    est_o65 = updated_pop * o65, 
    est_dis = updated_pop * a_w_dis,
    est_POC_acc = updated_pop * Avg_Access * POC,
    est_u200_acc = updated_pop * Avg_Access * u200,
    est_u100_acc = updated_pop * Avg_Access * u100,
    est_u18_acc = updated_pop * Avg_Access * u18, 
    est_o65_acc = updated_pop * Avg_Access * o65,
    est_dis_acc = updated_pop * Avg_Access * a_w_dis,
    est_POC_za = updated_pop * ZeroAccess_Perc * POC,
    est_u200_za = updated_pop * ZeroAccess_Perc * u200,
    est_u100_za = updated_pop * ZeroAccess_Perc * u100,
    est_u18_za = updated_pop * ZeroAccess_Perc * u18, 
    est_o65_za = updated_pop * ZeroAccess_Perc * o65,
    est_dis_za = updated_pop * ZeroAccess_Perc * a_w_dis,
  ) |> group_by(WP, simple_classification) |> 
  summarise(
    POC_Pop = sum(est_POC, na.rm = TRUE), 
    POC_Acc = sum(est_POC_acc, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    POC_za = sum(est_POC_za, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    u200_Pop = sum(est_u200, na.rm = TRUE), 
    u200_Acc = sum(est_u200_acc, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u200_za = sum(est_u200_za, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u100_Pop = sum(est_u100, na.rm = TRUE), 
    u100_Acc = sum(est_u100_acc, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u100_za = sum(est_u100_za, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u18_Pop = sum(est_u18, na.rm = TRUE), 
    u18_Acc = sum(est_u18_acc, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    u18_za = sum(est_u18_za, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    o65_Pop = sum(est_o65, na.rm = TRUE), 
    o65_Acc = sum(est_o65_acc, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    o65_za = sum(est_o65_za, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    dis_Pop = sum(est_dis, na.rm = TRUE), 
    dis_Acc = sum(est_dis_acc, na.rm = TRUE) / sum(est_dis, na.rm = TRUE), 
    dis_za = sum(est_dis_za, na.rm = TRUE) / sum(est_dis, na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = ends_with("Acc") | ends_with("za"), 
    names_to = "Demographic"
  ) |>
  mutate(
    Demo = substr(Demographic, 1, 4), 
    Measure = substr(Demographic, nchar(Demographic)-2, nchar(Demographic))
  ) |> 
  pivot_wider(
    names_from = Measure, 
    values_from = value, 
    values_fill = 0
  ) |> 
  select(-Demographic) |> 
  rename(
    Zero_Acc = "_za"
  ) |> 
  group_by(WP, simple_classification, Demo) |> 
  summarise(
    Access_Avg = sum(Acc), 
    Zero_Access = sum(Zero_Acc)
  ) |> ungroup() |>
  pivot_wider(
    names_from = WP, 
    values_from = c(Access_Avg, Zero_Access) 
  ) |> 
  filter(simple_classification != "Natural")

blk_med_context_demo_pop$demographic_group <-  blk_med_context_demo_pop$Demo |> 
  str_replace("dis_", "Adults with a Disability") |> 
  str_replace("o65_", "65 and Over") |> 
  str_replace("POC_", "People of Color") |>
  str_replace("u100", "Income under 100% of Poverty Level") |>
  str_replace("u200",  "Income under 200% of Poverty Level") |>
  str_replace("u18_", "Under 18") 



blk_med_context_demo_pop_total <- blk_med_context_demo |> 
  group_by(BG_GEOID) |> 
  mutate(blk_pop = sum(P0010001) / 7, 
         pop_inc = summary_est/blk_pop) |> 
  ungroup() |> 
  mutate(
    updated_pop = P0010001 * pop_inc, 
    est_POC = updated_pop * POC,
    est_u200 = updated_pop * u200,
    est_u100 = updated_pop * u100,
    est_u18 = updated_pop * u18, 
    est_o65 = updated_pop * o65, 
    est_dis = updated_pop * a_w_dis,
    est_POC_acc = updated_pop * Avg_Access * POC,
    est_u200_acc = updated_pop * Avg_Access * u200,
    est_u100_acc = updated_pop * Avg_Access * u100,
    est_u18_acc = updated_pop * Avg_Access * u18, 
    est_o65_acc = updated_pop * Avg_Access * o65,
    est_dis_acc = updated_pop * Avg_Access * a_w_dis,
    est_POC_za = updated_pop * ZeroAccess_Perc * POC,
    est_u200_za = updated_pop * ZeroAccess_Perc * u200,
    est_u100_za = updated_pop * ZeroAccess_Perc * u100,
    est_u18_za = updated_pop * ZeroAccess_Perc * u18, 
    est_o65_za = updated_pop * ZeroAccess_Perc * o65,
    est_dis_za = updated_pop * ZeroAccess_Perc * a_w_dis,
  ) |> 
  filter(simple_classification != "Natural") |> 
  group_by(WP) |> 
  summarise(
    POC_Pop = sum(est_POC, na.rm = TRUE), 
    POC_Acc = sum(est_POC_acc, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    POC_za = sum(est_POC_za, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    u200_Pop = sum(est_u200, na.rm = TRUE), 
    u200_Acc = sum(est_u200_acc, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u200_za = sum(est_u200_za, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u100_Pop = sum(est_u100, na.rm = TRUE), 
    u100_Acc = sum(est_u100_acc, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u100_za = sum(est_u100_za, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u18_Pop = sum(est_u18, na.rm = TRUE), 
    u18_Acc = sum(est_u18_acc, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    u18_za = sum(est_u18_za, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    o65_Pop = sum(est_o65, na.rm = TRUE), 
    o65_Acc = sum(est_o65_acc, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    o65_za = sum(est_o65_za, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    dis_Pop = sum(est_dis, na.rm = TRUE), 
    dis_Acc = sum(est_dis_acc, na.rm = TRUE) / sum(est_dis, na.rm = TRUE), 
    dis_za = sum(est_dis_za, na.rm = TRUE) / sum(est_dis, na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = ends_with("Acc") | ends_with("za"), 
    names_to = "Demographic"
  ) |>
  mutate(
    Demo = substr(Demographic, 1, 4), 
    Measure = substr(Demographic, nchar(Demographic)-2, nchar(Demographic))
  ) |> 
  pivot_wider(
    names_from = Measure, 
    values_from = value, 
    values_fill = 0
  ) |> 
  select(-Demographic) |> 
  rename(
    Zero_Acc = "_za"
  ) |> 
  group_by(WP, Demo) |> 
  summarise(
    Access_Avg = sum(Acc), 
    Zero_Access = sum(Zero_Acc)
  ) |> ungroup() |>
  pivot_wider(
    names_from = WP, 
    values_from = c(Access_Avg, Zero_Access) 
  ) |> 
  mutate(
    simple_classification = "Statewide"
  ) |> 
  select(simple_classification, everything())

blk_med_context_demo_pop_total$demographic_group <-  blk_med_context_demo_pop_total$Demo |> 
  str_replace("dis_", "Adults with a Disability") |> 
  str_replace("o65_", "65 and Over") |> 
  str_replace("POC_", "People of Color") |>
  str_replace("u100", "Income under 100% of Poverty Level") |>
  str_replace("u200",  "Income under 200% of Poverty Level") |>
  str_replace("u18_", "Under 18")


combined_demo_summary_med <- do.call(rbind, 
                                 list(blk_med_context_demo_pop, blk_med_context_demo_pop_total))

names(export_summary_med)

export_summary_med <- do.call(rbind, 
                          list(combined_summary_med, combined_demo_summary_med)) |>
  select(1, 18, 9, 16, 10, 17, 5, 12, 6, 13, 7, 14, 8, 15, 4, 11, 2)

write_xlsx(export_summary_med, path = "./data/Medical/medical_summaries.xlsx")

#CSS ----
wa_CSS <- rast("./data/CSS/wa_CSS.tif")  

wa_CSS <- wa_CSS / 10 #correcting CSS values to out of 1 vs out of 10

WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(wa_CSS))
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())
wa_css_blck = terra::extract(x = wa_CSS, y = WA_block_tigris_reproj) 

summary(wa_css_blck)

blk_css_summaries_0acc <- group_by(wa_css_blck, ID) |> 
  summarize( 
    n_rasters = n(), 
    WP_1_0Acc = sum(W_WA_senior_WP1     == 0),
    WP_2_0Acc = sum(W_WA_senior_WP2     == 0),
    WP_3_0Acc = sum(W_WA_senior_WP3    == 0),
    WP_4_0Acc = sum(W_WA_senior_WP4    == 0), 
    Car_0Acc = sum(W_WA_senior_car  == 0),
    Wlk1_0Acc = sum(W_WA_senior_Walk1      == 0),
    Wlk2_0Acc = sum(W_WA_senior_Walk2     == 0) 
  ) |> 
  pivot_longer(
    cols = ends_with("0Acc"),
    names_to = "WP",
    values_to = "ZeroAccess"
  ) |> 
  mutate(
    WP_ID = paste(substr(WP, 1, 4), ID, sep = "")
  )

blk_css_summaries <- group_by(wa_css_blck, ID) |> 
  summarize( 
    n_rasters = n(), 
    WP_1_mean = mean(W_WA_senior_WP1), 
    WP_2_mean = mean(W_WA_senior_WP2),
    WP_3_mean = mean(W_WA_senior_WP3), 
    WP_4_mean = mean(W_WA_senior_WP4), 
    Car_mean = mean(W_WA_senior_car),
    Wlk1_mean = mean(W_WA_senior_Walk1), 
    Wlk2_mean = mean(W_WA_senior_Walk2)
  ) |> 
  pivot_longer(
    cols = ends_with("mean"),
    names_to = "WP",
    values_to = "Avg_Access"
  ) |> 
  mutate(
    WP_ID = paste(substr(WP, 1, 4), ID, sep = ""), 
    WP = substr(WP, 1, 4)
  ) |> 
  left_join(
    blk_css_summaries_0acc |> select(ZeroAccess, WP_ID), 
    join_by(WP_ID == WP_ID)) 

blk_css_context <- blk_css_summaries |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID)) |> 
  mutate(ZeroAccess_Perc = ZeroAccess / n_rasters,
         za_perc_pr = ZeroAccess_Perc * P0010001,
         acc_pr = Avg_Access * P0010001) |> 
  select(1, WP, TRACTCE20, GEOID20, simple_classification, P0010001, n_rasters, ZeroAccess, ZeroAccess_Perc, za_perc_pr, Avg_Access, acc_pr)

save(blk_css_context, file = "./output/summaries/blk_css_summaries.RData")

#summarize findings
blk_summaries_css <- blk_css_context |> 
  group_by(simple_classification, WP) |> 
  summarise(
    population = sum(P0010001), 
    total_rast = sum(n_rasters), 
    Zero_Access = sum(za_perc_pr) / sum(P0010001), 
    Access_Avg = sum(acc_pr) / sum(P0010001)
  ) |> pivot_wider(
    names_from = WP,
    values_from = c(Access_Avg, Zero_Access)
  ) |> 
  mutate(
    demographic_group = "Total"
  ) 

statewide_summary_css <- blk_css_context |> 
  group_by(WP) |> 
  summarise(
    population = sum(P0010001), 
    total_rast = sum(n_rasters), 
    Zero_Access = sum(za_perc_pr) / sum(P0010001), 
    Access_Avg = sum(acc_pr) / sum(P0010001)
  ) |> pivot_wider(
    names_from = WP,
    values_from = c(Access_Avg, Zero_Access)
  ) |> 
  mutate(
    demographic_group = "Total"
  ) 

combined_summary_css <- do.call(rbind, 
                                list(blk_summaries_css, statewide_summary_css)) 

#demographic pass to block level
blk_css_context_demo <- blk_css_context |>
  mutate(
    BG_GEOID = substr(GEOID20, 1, nchar(GEOID20) -3), 
    TR_GEOID = substr(GEOID20, 1, nchar(GEOID20) -4)
  ) |> 
  left_join(
    block_group_demo |> select(1, 3, 15:19), join_by(BG_GEOID == GEOID)
  ) |> 
  left_join(
    tract_demo |> select(1, 17), join_by(TR_GEOID == GEOID)
  )

names(blk_css_context_demo)

blk_css_context_demo_pop <- blk_css_context_demo |> 
  group_by(BG_GEOID) |> 
  mutate(blk_pop = sum(P0010001) / 7, 
         pop_inc = summary_est/blk_pop) |> 
  ungroup() |> 
  mutate(
    updated_pop = P0010001 * pop_inc, 
    est_POC = updated_pop * POC,
    est_u200 = updated_pop * u200,
    est_u100 = updated_pop * u100,
    est_u18 = updated_pop * u18, 
    est_o65 = updated_pop * o65, 
    est_dis = updated_pop * a_w_dis,
    est_POC_acc = updated_pop * Avg_Access * POC,
    est_u200_acc = updated_pop * Avg_Access * u200,
    est_u100_acc = updated_pop * Avg_Access * u100,
    est_u18_acc = updated_pop * Avg_Access * u18, 
    est_o65_acc = updated_pop * Avg_Access * o65,
    est_dis_acc = updated_pop * Avg_Access * a_w_dis,
    est_POC_za = updated_pop * ZeroAccess_Perc * POC,
    est_u200_za = updated_pop * ZeroAccess_Perc * u200,
    est_u100_za = updated_pop * ZeroAccess_Perc * u100,
    est_u18_za = updated_pop * ZeroAccess_Perc * u18, 
    est_o65_za = updated_pop * ZeroAccess_Perc * o65,
    est_dis_za = updated_pop * ZeroAccess_Perc * a_w_dis,
  ) |> group_by(WP, simple_classification) |> 
  summarise(
    POC_Pop = sum(est_POC, na.rm = TRUE), 
    POC_Acc = sum(est_POC_acc, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    POC_za = sum(est_POC_za, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    u200_Pop = sum(est_u200, na.rm = TRUE), 
    u200_Acc = sum(est_u200_acc, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u200_za = sum(est_u200_za, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u100_Pop = sum(est_u100, na.rm = TRUE), 
    u100_Acc = sum(est_u100_acc, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u100_za = sum(est_u100_za, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u18_Pop = sum(est_u18, na.rm = TRUE), 
    u18_Acc = sum(est_u18_acc, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    u18_za = sum(est_u18_za, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    o65_Pop = sum(est_o65, na.rm = TRUE), 
    o65_Acc = sum(est_o65_acc, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    o65_za = sum(est_o65_za, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    dis_Pop = sum(est_dis, na.rm = TRUE), 
    dis_Acc = sum(est_dis_acc, na.rm = TRUE) / sum(est_dis, na.rm = TRUE), 
    dis_za = sum(est_dis_za, na.rm = TRUE) / sum(est_dis, na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = ends_with("Acc") | ends_with("za"), 
    names_to = "Demographic"
  ) |>
  mutate(
    Demo = substr(Demographic, 1, 4), 
    Measure = substr(Demographic, nchar(Demographic)-2, nchar(Demographic))
  ) |> 
  pivot_wider(
    names_from = Measure, 
    values_from = value, 
    values_fill = 0
  ) |> 
  select(-Demographic) |> 
  rename(
    Zero_Acc = "_za"
  ) |> 
  group_by(WP, simple_classification, Demo) |> 
  summarise(
    Access_Avg = sum(Acc), 
    Zero_Access = sum(Zero_Acc)
  ) |> ungroup() |>
  pivot_wider(
    names_from = WP, 
    values_from = c(Access_Avg, Zero_Access) 
  ) |> 
  filter(simple_classification != "Natural")

blk_css_context_demo_pop$demographic_group <-  blk_css_context_demo_pop$Demo |> 
  str_replace("dis_", "Adults with a Disability") |> 
  str_replace("o65_", "65 and Over") |> 
  str_replace("POC_", "People of Color") |>
  str_replace("u100", "Income under 100% of Poverty Level") |>
  str_replace("u200",  "Income under 200% of Poverty Level") |>
  str_replace("u18_", "Under 18") 



blk_css_context_demo_pop_total <- blk_css_context_demo |> 
  group_by(BG_GEOID) |> 
  mutate(blk_pop = sum(P0010001) / 7, 
         pop_inc = summary_est/blk_pop) |> 
  ungroup() |> 
  mutate(
    updated_pop = P0010001 * pop_inc, 
    est_POC = updated_pop * POC,
    est_u200 = updated_pop * u200,
    est_u100 = updated_pop * u100,
    est_u18 = updated_pop * u18, 
    est_o65 = updated_pop * o65, 
    est_dis = updated_pop * a_w_dis,
    est_POC_acc = updated_pop * Avg_Access * POC,
    est_u200_acc = updated_pop * Avg_Access * u200,
    est_u100_acc = updated_pop * Avg_Access * u100,
    est_u18_acc = updated_pop * Avg_Access * u18, 
    est_o65_acc = updated_pop * Avg_Access * o65,
    est_dis_acc = updated_pop * Avg_Access * a_w_dis,
    est_POC_za = updated_pop * ZeroAccess_Perc * POC,
    est_u200_za = updated_pop * ZeroAccess_Perc * u200,
    est_u100_za = updated_pop * ZeroAccess_Perc * u100,
    est_u18_za = updated_pop * ZeroAccess_Perc * u18, 
    est_o65_za = updated_pop * ZeroAccess_Perc * o65,
    est_dis_za = updated_pop * ZeroAccess_Perc * a_w_dis,
  ) |> 
  filter(simple_classification != "Natural") |> 
  group_by(WP) |> 
  summarise(
    POC_Pop = sum(est_POC, na.rm = TRUE), 
    POC_Acc = sum(est_POC_acc, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    POC_za = sum(est_POC_za, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    u200_Pop = sum(est_u200, na.rm = TRUE), 
    u200_Acc = sum(est_u200_acc, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u200_za = sum(est_u200_za, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u100_Pop = sum(est_u100, na.rm = TRUE), 
    u100_Acc = sum(est_u100_acc, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u100_za = sum(est_u100_za, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u18_Pop = sum(est_u18, na.rm = TRUE), 
    u18_Acc = sum(est_u18_acc, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    u18_za = sum(est_u18_za, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    o65_Pop = sum(est_o65, na.rm = TRUE), 
    o65_Acc = sum(est_o65_acc, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    o65_za = sum(est_o65_za, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    dis_Pop = sum(est_dis, na.rm = TRUE), 
    dis_Acc = sum(est_dis_acc, na.rm = TRUE) / sum(est_dis, na.rm = TRUE), 
    dis_za = sum(est_dis_za, na.rm = TRUE) / sum(est_dis, na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = ends_with("Acc") | ends_with("za"), 
    names_to = "Demographic"
  ) |>
  mutate(
    Demo = substr(Demographic, 1, 4), 
    Measure = substr(Demographic, nchar(Demographic)-2, nchar(Demographic))
  ) |> 
  pivot_wider(
    names_from = Measure, 
    values_from = value, 
    values_fill = 0
  ) |> 
  select(-Demographic) |> 
  rename(
    Zero_Acc = "_za"
  ) |> 
  group_by(WP, Demo) |> 
  summarise(
    Access_Avg = sum(Acc), 
    Zero_Access = sum(Zero_Acc)
  ) |> ungroup() |>
  pivot_wider(
    names_from = WP, 
    values_from = c(Access_Avg, Zero_Access) 
  ) |> 
  mutate(
    simple_classification = "Statewide"
  ) |> 
  select(simple_classification, everything())

blk_css_context_demo_pop_total$demographic_group <-  blk_css_context_demo_pop_total$Demo |> 
  str_replace("dis_", "Adults with a Disability") |> 
  str_replace("o65_", "65 and Over") |> 
  str_replace("POC_", "People of Color") |>
  str_replace("u100", "Income under 100% of Poverty Level") |>
  str_replace("u200",  "Income under 200% of Poverty Level") |>
  str_replace("u18_", "Under 18")


combined_demo_summary_css <- do.call(rbind, 
                                     list(blk_css_context_demo_pop, blk_css_context_demo_pop_total))

names(combined_demo_summary_css)



export_summary_css <- do.call(rbind, 
                              list(combined_summary_css, combined_demo_summary_css)) |>
  select(1, 18, 9, 16, 10, 17, 5, 12, 6, 13, 7, 14, 8, 15, 4, 11, 2)

names(export_summary_css)

write_xlsx(export_summary_css, path = "./data/CSS/CSS_summaries.xlsx")


#Parks ----
wa_Parks <- rast("./data/Parks/wa_Parks.tif")

WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(wa_Parks))
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())
wa_Parks_blck = terra::extract(x = wa_Parks, y = WA_block_tigris_reproj) 

summary(wa_Parks_blck)

blk_Parks_summaries_0acc <- group_by(wa_Parks_blck, ID) |> 
  summarize( 
    n_rasters = n(), 
    WP_1_0Acc = sum(E_WA_parks_WP1 == 0),
    WP_2_0Acc = sum(E_WA_parks_WP2 == 0),
    WP_3_0Acc = sum(E_WA_parks_WP3 == 0),
    WP_4_0Acc = sum(E_WA_parks_WP4 == 0), 
    Car_0Acc = sum(E_WA_Parks_car == 0),
    Wlk1_0Acc = sum(W_WA_Parks_Walk1 == 0),
    Wlk2_0Acc = sum(W_WA_Parks_Walk2 == 0) 
  ) |> 
  pivot_longer(
    cols = ends_with("0Acc"),
    names_to = "WP",
    values_to = "ZeroAccess"
  ) |> 
  mutate(
    WP_ID = paste(substr(WP, 1, 4), ID, sep = "")
  )

blk_Parks_summaries <- group_by(wa_Parks_blck, ID) |> 
  summarize( 
    n_rasters = n(), 
    WP_1_mean = mean(E_WA_parks_WP1), 
    WP_2_mean = mean(E_WA_parks_WP2),
    WP_3_mean = mean(E_WA_parks_WP3), 
    WP_4_mean = mean(E_WA_parks_WP4), 
    Car_mean = mean(E_WA_Parks_car),
    Wlk1_mean = mean(W_WA_Parks_Walk1), 
    Wlk2_mean = mean(W_WA_Parks_Walk2)
  ) |> 
  pivot_longer(
    cols = ends_with("mean"),
    names_to = "WP",
    values_to = "Avg_Access"
  ) |> 
  mutate(
    WP_ID = paste(substr(WP, 1, 4), ID, sep = ""), 
    WP = substr(WP, 1, 4)
  ) |> 
  left_join(
    blk_Parks_summaries_0acc |> select(ZeroAccess, WP_ID), 
    join_by(WP_ID == WP_ID)) 

blk_Parks_context <- blk_Parks_summaries |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID)) |> 
  mutate(ZeroAccess_Perc = ZeroAccess / n_rasters,
         za_perc_pr = ZeroAccess_Perc * P0010001,
         acc_pr = Avg_Access * P0010001) |> 
  select(1, WP, TRACTCE20, GEOID20, simple_classification, P0010001, n_rasters, ZeroAccess, ZeroAccess_Perc, za_perc_pr, Avg_Access, acc_pr)

save(blk_Parks_context, file = "./output/summaries/blk_Parks_context.RData")

#summarize findings
blk_summaries_Parks <- blk_Parks_context |> 
  group_by(simple_classification, WP) |> 
  summarise(
    population = sum(P0010001), 
    total_rast = sum(n_rasters), 
    Zero_Access = sum(za_perc_pr) / sum(P0010001), 
    Access_Avg = sum(acc_pr) / sum(P0010001)
  ) |> pivot_wider(
    names_from = WP,
    values_from = c(Access_Avg, Zero_Access)
  ) |> 
  mutate(
    demographic_group = "Total"
  ) 

statewide_summary_Parks <- blk_Parks_context |> 
  group_by(WP) |> 
  summarise(
    population = sum(P0010001), 
    total_rast = sum(n_rasters), 
    Zero_Access = sum(za_perc_pr) / sum(P0010001), 
    Access_Avg = sum(acc_pr) / sum(P0010001)
  ) |> pivot_wider(
    names_from = WP,
    values_from = c(Access_Avg, Zero_Access)
  ) |> 
  mutate(
    demographic_group = "Total"
  ) 

combined_summary_Parks <- do.call(rbind, 
                                list(blk_summaries_Parks, statewide_summary_Parks)) 

#demographic pass to block level
blk_Parks_context_demo <- blk_Parks_context |>
  mutate(
    BG_GEOID = substr(GEOID20, 1, nchar(GEOID20) -3), 
    TR_GEOID = substr(GEOID20, 1, nchar(GEOID20) -4)
  ) |> 
  left_join(
    block_group_demo |> select(1, 3, 15:19), join_by(BG_GEOID == GEOID)
  ) |> 
  left_join(
    tract_demo |> select(1, 17), join_by(TR_GEOID == GEOID)
  )

names(blk_Parks_context_demo)

blk_Parks_context_demo_pop <- blk_Parks_context_demo |> 
  group_by(BG_GEOID) |> 
  mutate(blk_pop = sum(P0010001) / 7, 
         pop_inc = summary_est/blk_pop) |> 
  ungroup() |> 
  mutate(
    updated_pop = P0010001 * pop_inc, 
    est_POC = updated_pop * POC,
    est_u200 = updated_pop * u200,
    est_u100 = updated_pop * u100,
    est_u18 = updated_pop * u18, 
    est_o65 = updated_pop * o65, 
    est_dis = updated_pop * a_w_dis,
    est_POC_acc = updated_pop * Avg_Access * POC,
    est_u200_acc = updated_pop * Avg_Access * u200,
    est_u100_acc = updated_pop * Avg_Access * u100,
    est_u18_acc = updated_pop * Avg_Access * u18, 
    est_o65_acc = updated_pop * Avg_Access * o65,
    est_dis_acc = updated_pop * Avg_Access * a_w_dis,
    est_POC_za = updated_pop * ZeroAccess_Perc * POC,
    est_u200_za = updated_pop * ZeroAccess_Perc * u200,
    est_u100_za = updated_pop * ZeroAccess_Perc * u100,
    est_u18_za = updated_pop * ZeroAccess_Perc * u18, 
    est_o65_za = updated_pop * ZeroAccess_Perc * o65,
    est_dis_za = updated_pop * ZeroAccess_Perc * a_w_dis,
  ) |> group_by(WP, simple_classification) |> 
  summarise(
    POC_Pop = sum(est_POC, na.rm = TRUE), 
    POC_Acc = sum(est_POC_acc, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    POC_za = sum(est_POC_za, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    u200_Pop = sum(est_u200, na.rm = TRUE), 
    u200_Acc = sum(est_u200_acc, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u200_za = sum(est_u200_za, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u100_Pop = sum(est_u100, na.rm = TRUE), 
    u100_Acc = sum(est_u100_acc, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u100_za = sum(est_u100_za, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u18_Pop = sum(est_u18, na.rm = TRUE), 
    u18_Acc = sum(est_u18_acc, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    u18_za = sum(est_u18_za, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    o65_Pop = sum(est_o65, na.rm = TRUE), 
    o65_Acc = sum(est_o65_acc, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    o65_za = sum(est_o65_za, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    dis_Pop = sum(est_dis, na.rm = TRUE), 
    dis_Acc = sum(est_dis_acc, na.rm = TRUE) / sum(est_dis, na.rm = TRUE), 
    dis_za = sum(est_dis_za, na.rm = TRUE) / sum(est_dis, na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = ends_with("Acc") | ends_with("za"), 
    names_to = "Demographic"
  ) |>
  mutate(
    Demo = substr(Demographic, 1, 4), 
    Measure = substr(Demographic, nchar(Demographic)-2, nchar(Demographic))
  ) |> 
  pivot_wider(
    names_from = Measure, 
    values_from = value, 
    values_fill = 0
  ) |> 
  select(-Demographic) |> 
  rename(
    Zero_Acc = "_za"
  ) |> 
  group_by(WP, simple_classification, Demo) |> 
  summarise(
    Access_Avg = sum(Acc), 
    Zero_Access = sum(Zero_Acc)
  ) |> ungroup() |>
  pivot_wider(
    names_from = WP, 
    values_from = c(Access_Avg, Zero_Access) 
  ) |> 
  filter(simple_classification != "Natural")

blk_Parks_context_demo_pop$demographic_group <-  blk_Parks_context_demo_pop$Demo |> 
  str_replace("dis_", "Adults with a Disability") |> 
  str_replace("o65_", "65 and Over") |> 
  str_replace("POC_", "People of Color") |>
  str_replace("u100", "Income under 100% of Poverty Level") |>
  str_replace("u200",  "Income under 200% of Poverty Level") |>
  str_replace("u18_", "Under 18") 



blk_Parks_context_demo_pop_total <- blk_Parks_context_demo |> 
  group_by(BG_GEOID) |> 
  mutate(blk_pop = sum(P0010001) / 7, 
         pop_inc = summary_est/blk_pop) |> 
  ungroup() |> 
  mutate(
    updated_pop = P0010001 * pop_inc, 
    est_POC = updated_pop * POC,
    est_u200 = updated_pop * u200,
    est_u100 = updated_pop * u100,
    est_u18 = updated_pop * u18, 
    est_o65 = updated_pop * o65, 
    est_dis = updated_pop * a_w_dis,
    est_POC_acc = updated_pop * Avg_Access * POC,
    est_u200_acc = updated_pop * Avg_Access * u200,
    est_u100_acc = updated_pop * Avg_Access * u100,
    est_u18_acc = updated_pop * Avg_Access * u18, 
    est_o65_acc = updated_pop * Avg_Access * o65,
    est_dis_acc = updated_pop * Avg_Access * a_w_dis,
    est_POC_za = updated_pop * ZeroAccess_Perc * POC,
    est_u200_za = updated_pop * ZeroAccess_Perc * u200,
    est_u100_za = updated_pop * ZeroAccess_Perc * u100,
    est_u18_za = updated_pop * ZeroAccess_Perc * u18, 
    est_o65_za = updated_pop * ZeroAccess_Perc * o65,
    est_dis_za = updated_pop * ZeroAccess_Perc * a_w_dis,
  ) |> 
  filter(simple_classification != "Natural") |> 
  group_by(WP) |> 
  summarise(
    POC_Pop = sum(est_POC, na.rm = TRUE), 
    POC_Acc = sum(est_POC_acc, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    POC_za = sum(est_POC_za, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    u200_Pop = sum(est_u200, na.rm = TRUE), 
    u200_Acc = sum(est_u200_acc, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u200_za = sum(est_u200_za, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u100_Pop = sum(est_u100, na.rm = TRUE), 
    u100_Acc = sum(est_u100_acc, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u100_za = sum(est_u100_za, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u18_Pop = sum(est_u18, na.rm = TRUE), 
    u18_Acc = sum(est_u18_acc, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    u18_za = sum(est_u18_za, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    o65_Pop = sum(est_o65, na.rm = TRUE), 
    o65_Acc = sum(est_o65_acc, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    o65_za = sum(est_o65_za, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    dis_Pop = sum(est_dis, na.rm = TRUE), 
    dis_Acc = sum(est_dis_acc, na.rm = TRUE) / sum(est_dis, na.rm = TRUE), 
    dis_za = sum(est_dis_za, na.rm = TRUE) / sum(est_dis, na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = ends_with("Acc") | ends_with("za"), 
    names_to = "Demographic"
  ) |>
  mutate(
    Demo = substr(Demographic, 1, 4), 
    Measure = substr(Demographic, nchar(Demographic)-2, nchar(Demographic))
  ) |> 
  pivot_wider(
    names_from = Measure, 
    values_from = value, 
    values_fill = 0
  ) |> 
  select(-Demographic) |> 
  rename(
    Zero_Acc = "_za"
  ) |> 
  group_by(WP, Demo) |> 
  summarise(
    Access_Avg = sum(Acc), 
    Zero_Access = sum(Zero_Acc)
  ) |> ungroup() |>
  pivot_wider(
    names_from = WP, 
    values_from = c(Access_Avg, Zero_Access) 
  ) |> 
  mutate(
    simple_classification = "Statewide"
  ) |> 
  select(simple_classification, everything())

blk_Parks_context_demo_pop_total$demographic_group <-  blk_Parks_context_demo_pop_total$Demo |> 
  str_replace("dis_", "Adults with a Disability") |> 
  str_replace("o65_", "65 and Over") |> 
  str_replace("POC_", "People of Color") |>
  str_replace("u100", "Income under 100% of Poverty Level") |>
  str_replace("u200",  "Income under 200% of Poverty Level") |>
  str_replace("u18_", "Under 18")


combined_demo_summary_Parks <- do.call(rbind, 
                                     list(blk_Parks_context_demo_pop, blk_Parks_context_demo_pop_total))

names(combined_demo_summary_Parks)



export_summary_Parks <- do.call(rbind, 
                              list(combined_summary_Parks, combined_demo_summary_Parks)) |>
  select(1, 18, 9, 16, 10, 17, 5, 12, 6, 13, 7, 14, 8, 15, 4, 11, 2)

names(export_summary_Parks)

write_xlsx(export_summary_Parks, path = "./data/Parks/Parks_summaries.xlsx")

#Food ----
wa_food <- rast("./data/Food/wa_food.tif")
WA_block_tigris_reproj <- st_transform(WA_block_tigris, crs(wa_food))
WA_block_tigris_reproj <- sort_by(WA_block_tigris_reproj, WA_block_tigris$GEOID20) |> 
  mutate(
    row = 1:n())
wa_food_blck = terra::extract(x = wa_food, y = WA_block_tigris_reproj) 

summary(wa_food_blck)

freq(wa_food[[1]])

blk_food_summaries_0acc <- group_by(wa_food_blck, ID) |> 
  summarize( 
    n_rasters = n(), 
    WP_1_0Acc = sum(is.na(E_WA_food_WP1)),
    WP_2_0Acc = sum(is.na(E_WA_food_WP2)),
    WP_3_0Acc = sum(is.na(E_WA_food_WP3)),
    WP_4_0Acc = sum(is.na(E_WA_food_WP4)), 
    Car_0Acc = sum(is.na(E_WA_food_car))
    # Wlk1_0Acc = sum(W_WA_food_Walk1 == 0),
    # Wlk2_0Acc = sum(W_WA_food_Walk2 == 0) 
  ) |> 
  pivot_longer(
    cols = ends_with("0Acc"),
    names_to = "WP",
    values_to = "ZeroAccess"
  ) |> 
  mutate(
    WP_ID = paste(substr(WP, 1, 4), ID, sep = "")
  )

blk_food_summaries <- group_by(wa_food_blck, ID) |> 
  summarize( 
    n_rasters = n(), 
    WP_1_mean = ifelse(all(is.na(E_WA_food_WP1)), NA, mean(E_WA_food_WP1, na.rm = TRUE)), 
    WP_2_mean = ifelse(all(is.na(E_WA_food_WP2)), NA, mean(E_WA_food_WP2, na.rm = TRUE)),
    WP_3_mean = ifelse(all(is.na(E_WA_food_WP3)), NA, mean(E_WA_food_WP3, na.rm = TRUE)), 
    WP_4_mean = ifelse(all(is.na(E_WA_food_WP4)), NA, mean(E_WA_food_WP4, na.rm = TRUE)), 
    Car_mean = ifelse(all(is.na(E_WA_food_car)), NA, mean(E_WA_food_car, na.rm = TRUE))
    # Wlk1_mean = mean(W_WA_food_Walk1), 
    # Wlk2_mean = mean(W_WA_food_Walk2)
  ) |> 
  pivot_longer(
    cols = ends_with("mean"),
    names_to = "WP",
    values_to = "Avg_Access"
  ) |> 
  mutate(
    WP_ID = paste(substr(WP, 1, 4), ID, sep = ""), 
    WP = substr(WP, 1, 4)
  ) |> 
  left_join(
    blk_food_summaries_0acc |> select(ZeroAccess, WP_ID), 
    join_by(WP_ID == WP_ID)) 

blk_food_context <- blk_food_summaries |> 
  left_join(st_drop_geometry(WA_block_tigris_reproj) |> 
              select(3, 5, row) , join_by(ID == row)) |> 
  left_join(context_class_Simple_Groups, join_by(GEOID20 == GEOID)) |> 
  mutate(ZeroAccess_Perc = ZeroAccess / n_rasters,
         za_perc_pr = ZeroAccess_Perc * P0010001,
         acc_pr = Avg_Access * P0010001) |> 
  select(1, WP, TRACTCE20, GEOID20, simple_classification, P0010001, n_rasters, ZeroAccess, ZeroAccess_Perc, za_perc_pr, Avg_Access, acc_pr)

save(blk_food_summaries, file = "./output/summaries/blk_food_summaries.RData")


#summarize findings
blk_summaries_food <- blk_food_context |> 
  group_by(simple_classification, WP) |> 
  summarise(
    population = sum(P0010001), 
    total_rast = sum(n_rasters), 
    Zero_Access = sum(za_perc_pr) / sum(P0010001), 
    Access_Avg = sum(acc_pr, na.rm = TRUE) / sum(P0010001)
  ) |> pivot_wider(
    names_from = WP,
    values_from = c(Access_Avg, Zero_Access)
  ) |> 
  mutate(
    demographic_group = "Total"
  ) 

statewide_summary_food <- blk_food_context |> 
  group_by(WP) |> 
  summarise(
    population = sum(P0010001), 
    total_rast = sum(n_rasters), 
    Zero_Access = sum(za_perc_pr) / sum(P0010001), 
    Access_Avg = sum(acc_pr, na.rm = TRUE) / sum(P0010001)
  ) |> pivot_wider(
    names_from = WP,
    values_from = c(Access_Avg, Zero_Access)
  ) |> 
  mutate(
    demographic_group = "Total"
  ) 

combined_summary_food <- do.call(rbind, 
                                  list(blk_summaries_food, statewide_summary_food)) 

#demographic pass to block level
blk_food_context_demo <- blk_food_context |>
  mutate(
    BG_GEOID = substr(GEOID20, 1, nchar(GEOID20) -3), 
    TR_GEOID = substr(GEOID20, 1, nchar(GEOID20) -4)
  ) |> 
  left_join(
    block_group_demo |> select(1, 3, 15:19), join_by(BG_GEOID == GEOID)
  ) |> 
  left_join(
    tract_demo |> select(1, 17), join_by(TR_GEOID == GEOID)
  )

names(blk_food_context_demo)

blk_food_context_demo_pop <- blk_food_context_demo |> 
  group_by(BG_GEOID) |> 
  mutate(blk_pop = sum(P0010001) / 7, 
         pop_inc = summary_est/blk_pop) |> 
  ungroup() |> 
  mutate(
    updated_pop = P0010001 * pop_inc, 
    est_POC = updated_pop * POC,
    est_u200 = updated_pop * u200,
    est_u100 = updated_pop * u100,
    est_u18 = updated_pop * u18, 
    est_o65 = updated_pop * o65, 
    est_dis = updated_pop * a_w_dis,
    est_POC_acc = updated_pop * Avg_Access * POC,
    est_u200_acc = updated_pop * Avg_Access * u200,
    est_u100_acc = updated_pop * Avg_Access * u100,
    est_u18_acc = updated_pop * Avg_Access * u18, 
    est_o65_acc = updated_pop * Avg_Access * o65,
    est_dis_acc = updated_pop * Avg_Access * a_w_dis,
    est_POC_za = updated_pop * ZeroAccess_Perc * POC,
    est_u200_za = updated_pop * ZeroAccess_Perc * u200,
    est_u100_za = updated_pop * ZeroAccess_Perc * u100,
    est_u18_za = updated_pop * ZeroAccess_Perc * u18, 
    est_o65_za = updated_pop * ZeroAccess_Perc * o65,
    est_dis_za = updated_pop * ZeroAccess_Perc * a_w_dis,
  ) |> group_by(WP, simple_classification) |> 
  summarise(
    POC_Pop = sum(est_POC, na.rm = TRUE), 
    POC_Acc = sum(est_POC_acc, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    POC_za = sum(est_POC_za, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    u200_Pop = sum(est_u200, na.rm = TRUE), 
    u200_Acc = sum(est_u200_acc, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u200_za = sum(est_u200_za, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u100_Pop = sum(est_u100, na.rm = TRUE), 
    u100_Acc = sum(est_u100_acc, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u100_za = sum(est_u100_za, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u18_Pop = sum(est_u18, na.rm = TRUE), 
    u18_Acc = sum(est_u18_acc, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    u18_za = sum(est_u18_za, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    o65_Pop = sum(est_o65, na.rm = TRUE), 
    o65_Acc = sum(est_o65_acc, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    o65_za = sum(est_o65_za, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    dis_Pop = sum(est_dis, na.rm = TRUE), 
    dis_Acc = sum(est_dis_acc, na.rm = TRUE) / sum(est_dis, na.rm = TRUE), 
    dis_za = sum(est_dis_za, na.rm = TRUE) / sum(est_dis, na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = ends_with("Acc") | ends_with("za"), 
    names_to = "Demographic"
  ) |>
  mutate(
    Demo = substr(Demographic, 1, 4), 
    Measure = substr(Demographic, nchar(Demographic)-2, nchar(Demographic))
  ) |> 
  pivot_wider(
    names_from = Measure, 
    values_from = value, 
    values_fill = 0
  ) |> 
  select(-Demographic) |> 
  rename(
    Zero_Acc = "_za"
  ) |> 
  group_by(WP, simple_classification, Demo) |> 
  summarise(
    Access_Avg = sum(Acc), 
    Zero_Access = sum(Zero_Acc)
  ) |> ungroup() |>
  pivot_wider(
    names_from = WP, 
    values_from = c(Access_Avg, Zero_Access) 
  ) |> 
  filter(simple_classification != "Natural")

blk_food_context_demo_pop$demographic_group <-  blk_food_context_demo_pop$Demo |> 
  str_replace("dis_", "Adults with a Disability") |> 
  str_replace("o65_", "65 and Over") |> 
  str_replace("POC_", "People of Color") |>
  str_replace("u100", "Income under 100% of Poverty Level") |>
  str_replace("u200",  "Income under 200% of Poverty Level") |>
  str_replace("u18_", "Under 18") 



blk_food_context_demo_pop_total <- blk_food_context_demo |> 
  group_by(BG_GEOID) |> 
  mutate(blk_pop = sum(P0010001) / 7, 
         pop_inc = summary_est/blk_pop) |> 
  ungroup() |> 
  mutate(
    updated_pop = P0010001 * pop_inc, 
    est_POC = updated_pop * POC,
    est_u200 = updated_pop * u200,
    est_u100 = updated_pop * u100,
    est_u18 = updated_pop * u18, 
    est_o65 = updated_pop * o65, 
    est_dis = updated_pop * a_w_dis,
    est_POC_acc = updated_pop * Avg_Access * POC,
    est_u200_acc = updated_pop * Avg_Access * u200,
    est_u100_acc = updated_pop * Avg_Access * u100,
    est_u18_acc = updated_pop * Avg_Access * u18, 
    est_o65_acc = updated_pop * Avg_Access * o65,
    est_dis_acc = updated_pop * Avg_Access * a_w_dis,
    est_POC_za = updated_pop * ZeroAccess_Perc * POC,
    est_u200_za = updated_pop * ZeroAccess_Perc * u200,
    est_u100_za = updated_pop * ZeroAccess_Perc * u100,
    est_u18_za = updated_pop * ZeroAccess_Perc * u18, 
    est_o65_za = updated_pop * ZeroAccess_Perc * o65,
    est_dis_za = updated_pop * ZeroAccess_Perc * a_w_dis,
  ) |> 
  filter(simple_classification != "Natural") |> 
  group_by(WP) |> 
  summarise(
    POC_Pop = sum(est_POC, na.rm = TRUE), 
    POC_Acc = sum(est_POC_acc, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    POC_za = sum(est_POC_za, na.rm = TRUE) / sum(est_POC, na.rm = TRUE), 
    u200_Pop = sum(est_u200, na.rm = TRUE), 
    u200_Acc = sum(est_u200_acc, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u200_za = sum(est_u200_za, na.rm = TRUE) / sum(est_u200, na.rm = TRUE), 
    u100_Pop = sum(est_u100, na.rm = TRUE), 
    u100_Acc = sum(est_u100_acc, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u100_za = sum(est_u100_za, na.rm = TRUE) / sum(est_u100, na.rm = TRUE), 
    u18_Pop = sum(est_u18, na.rm = TRUE), 
    u18_Acc = sum(est_u18_acc, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    u18_za = sum(est_u18_za, na.rm = TRUE) / sum(est_u18, na.rm = TRUE), 
    o65_Pop = sum(est_o65, na.rm = TRUE), 
    o65_Acc = sum(est_o65_acc, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    o65_za = sum(est_o65_za, na.rm = TRUE) / sum(est_o65, na.rm = TRUE), 
    dis_Pop = sum(est_dis, na.rm = TRUE), 
    dis_Acc = sum(est_dis_acc, na.rm = TRUE) / sum(est_dis, na.rm = TRUE), 
    dis_za = sum(est_dis_za, na.rm = TRUE) / sum(est_dis, na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = ends_with("Acc") | ends_with("za"), 
    names_to = "Demographic"
  ) |>
  mutate(
    Demo = substr(Demographic, 1, 4), 
    Measure = substr(Demographic, nchar(Demographic)-2, nchar(Demographic))
  ) |> 
  pivot_wider(
    names_from = Measure, 
    values_from = value, 
    values_fill = 0
  ) |> 
  select(-Demographic) |> 
  rename(
    Zero_Acc = "_za"
  ) |> 
  group_by(WP, Demo) |> 
  summarise(
    Access_Avg = sum(Acc), 
    Zero_Access = sum(Zero_Acc)
  ) |> ungroup() |>
  pivot_wider(
    names_from = WP, 
    values_from = c(Access_Avg, Zero_Access) 
  ) |> 
  mutate(
    simple_classification = "Statewide"
  ) |> 
  select(simple_classification, everything())

blk_food_context_demo_pop_total$demographic_group <-  blk_food_context_demo_pop_total$Demo |> 
  str_replace("dis_", "Adults with a Disability") |> 
  str_replace("o65_", "65 and Over") |> 
  str_replace("POC_", "People of Color") |>
  str_replace("u100", "Income under 100% of Poverty Level") |>
  str_replace("u200",  "Income under 200% of Poverty Level") |>
  str_replace("u18_", "Under 18")


combined_demo_summary_food <- do.call(rbind, 
                                       list(blk_food_context_demo_pop, blk_food_context_demo_pop_total))

names(combined_demo_summary_food)



export_summary_food <- do.call(rbind, 
                                list(combined_summary_food, combined_demo_summary_food))|>
  select(1, 14, 5, 10, 6, 11, 7, 12, 8, 13, 4, 9, 2)

names(export_summary_food)

write_xlsx(export_summary_food, path = "./data/Food/food_summaries.xlsx")



#aggregate results into one table by block ----
rm(blk_jobs_summaries, 
   blk_lei_summaries_0acc, blk_leis_summaries,
   blk_css_summaries_0acc, blk_css_summaries,
   blk_food_summaries_0acc, blk_food_summaries,
   blk_med_summaries_0acc, blk_med_summaries,
   blk_Parks_summaries_0acc, blk_Parks_summaries, 
   gdb_path, layer_name, layers)



load("./output/Summaries/blk_food_summaries.RData")
load("./output/Summaries/blk_css_summaries.RData")
load("./output/Summaries/blk_med_summaries.RData")
load("./output/Summaries/blk_leis_summaries.RData")
load("./output/Summaries/blk_Parks_context.RData")
load("./output/Summaries/blk_jobs_context.RDATA")

str(blk_jobs_context)
str(agg_jbs_WP)


names(blk_leis_context)

#may want to try to move formatting number change to this section - or save for the end
# Seems like numbers will come in in all kinds of different formats, doing in bulk at the end likely best.


renameDF <- function(dataF, colnm){
  dataF |> 
  select(- 8, - 9, -10, -12) |> 
    pivot_wider(
      names_from = 2,
      values_from = 8, 
      names_prefix =  colnm
    )
}

#create baseline df to then be able to left join others to
block_join_start <- renameDF(blk_leis_context, "leis_")


names(blk_food_context)
names(blk_css_context)
#variables we want 
# 3, 7-End

renameDF_min <- function(dataF, colnm){ 
  dataF |> 
    select(- 8, - 9, -10, -12) |> 
    pivot_wider(
      names_from = 2,
      values_from = 8, 
      names_prefix =  colnm
    ) |> select(
     -1, -2, -4, -5, -6
    )
}

smpl <- renameDF_min(blk_css_context, "css_")


join_cus <- function(dataF1, dataF2) {
  dataF1 |> left_join(dataF2, join_by(GEOID20 == GEOID20))
}

#starter join
Combo_Access_Avg <- join_cus(block_join_start, smpl) 

#update to DF and "XXXX_" dest name
smpl <- renameDF_min(blk_jobs_context, "jbs_")

#updated sample with
#leis in base #
#css #
#food #
#jobs 
#med #
#Parks
Combo_Access_Avg <- join_cus(Combo_Access_Avg, smpl) 

save(Combo_Access_Avg, file = "./output/Summaries/Combo_Access_Avg.RDATA")

names(Combo_Access_Avg)


#set formats for 
Com <- Combo_Access_Avg |> (
    format(c(7:50), nsmall = 2, scientific = FALSE))
  

Access_Comparisons <- Combo_Access_Avg |> 
  mutate(
    leis_wk11 = leis_Wlk1 / leis_WP_1,
    leis_wk21 = leis_Wlk2 / leis_WP_1,
    leis_21 = leis_WP_2 / leis_WP_1,
    leis_31 = leis_WP_3 / leis_WP_1, 
    leis_41 = leis_WP_4 / leis_WP_1,
    leis_41 = leis_WP_4 / leis_WP_1,
    leis_1car = leis_WP_1 / leis_Car_, 
    
    css_wk11 = css_Wlk1 / css_WP_1,
    css_wk21 = css_Wlk2 / css_WP_1,
    css_21 = css_WP_2 / css_WP_1,
    css_31 = css_WP_3 / css_WP_1, 
    css_41 = css_WP_4 / css_WP_1,
    css_41 = css_WP_4 / css_WP_1,
    css_1car = css_WP_1 / css_Car_, 
    
    food_21 = food_WP_2 / food_WP_1,
    food_31 = food_WP_3 / food_WP_1, 
    food_41 = food_WP_4 / food_WP_1,
    food_41 = food_WP_4 / food_WP_1,
    food_1car = food_WP_1 / food_Car_, 
    
    med_wk11 = med_Wlk1 / med_WP_1,
    med_wk21 = med_Wlk2 / med_WP_1,
    med_21 = med_WP_2 / med_WP_1,
    med_31 = med_WP_3 / med_WP_1, 
    med_41 = med_WP_4 / med_WP_1,
    med_41 = med_WP_4 / med_WP_1,
    med_1car = med_WP_1 / med_Car_, 
    
    prk_wk11 = prk_Wlk1 / prk_WP_1,
    prk_wk21 = prk_Wlk2 / prk_WP_1,
    prk_21 = prk_WP_2 / prk_WP_1,
    prk_31 = prk_WP_3 / prk_WP_1, 
    prk_41 = prk_WP_4 / prk_WP_1,
    prk_41 = prk_WP_4 / prk_WP_1,
    prk_1car = prk_WP_1 / prk_Car_, 
    
    jbs_wk11 = jbs_Wlk1 / jbs_WP1a,
    jbs_wk21 = jbs_Wlk2 / jbs_WP1a,
    jbs_21 = jbs_WP2a / jbs_WP1a,
    jbs_31 = jbs_WP3a / jbs_WP1a, 
    jbs_41 = jbs_WP4a / jbs_WP1a,
    jbs_41 = jbs_WP4a / jbs_WP1a,
    jbs_1car = jbs_WP1a / jbs_Car_, 
    jbs_1avp = jbs_WP1p / jbs_WP1a
  ) |> select(1:6, 51:85) 
  
Access_Comparisons <- Access_Comparisons |> 
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)), 
         across(everything(), ~ ifelse(is.infinite(.), NA, .))
         )

save(Access_Comparisons, file = "./output/Summaries/Access_Comparisons.RDATA")

