#census check 
#https://walker-data.com/census-r/an-introduction-to-tidycensus.html

library(tidycensus)
library(tigris)

library(tidyverse)
library(mapview)
library(sf)
library(knitr)
library(terra)

#option to re-load demographic only datasets extracted from this script - 
# load("./data/Demographic/wa_bg_combined_demo.RDATA")
# load("./data/Demographic/wa_tracts_combined_demo.RDATA")

#get unique API key via Census website - instructions available in walker-data link referenced above
census_api_key("288c7796bc5c3155cdc7c011f923240305448718", install = TRUE)


#2020 data available from a redistricting summary with sumfile "pl" - P1_005N prints the number of American Indians in each state
aian_2020 <- get_decennial(
  geography = "state",
  variables = "P1_005N",
  year = 2020,
  sumfile = "pl"
)

#geographic boundaries pulled from tigris ----

#load in pre-loaded file already instead - water area threshold means that only water areas > than the threshold are removed. 0.9 indicates the largest 10% areas are removed. 
Wa_counties_tigris_water <- counties("Washington") |> 
    erase_water(area_threshold = 0.95) #remove water for now, as it ads shape complexity and can break calculations 
save(Wa_counties_tigris, file = "./data/WA_counties2020w_water.RData")
load("./data/WA_counties2020w_water.RData")

#block groups without with water
WA_block_groups_tigris <- block_groups("Washington") #|> 
#   erase_water(area_threshold = 0.9)
save(WA_block_groups_tigris, file = "./data/WA_block_groups2020w_water.RData")
WA_block_tigris <- blocks("Washington") 
save(WA_block_tigris, file = "./data/WA_blocks2020w_water.RData")
load("./data/WA_block_groups2020w_water.RData")

#load in pre-loaded file already instead 
WA_tracts_tigris <- tracts("Washington") #|> 
#   erase_water(area_threshold = 0.9)
save(WA_tracts_tigris, file = "./data/WA_tracts2020w_water.RData")
load("./data/WA_tracts2020w_water.RData")


#Race to get calculate People of Color - available at the block-group level ----
race_vars <- c(
  White = "B03002_003",
  Black = "B03002_004",
  Native = "B03002_005",
  Asian = "B03002_006",
  HIPI = "B03002_007",
  SomeOtherRaceAlone = "B03002_008",
  TwoOrMoreRaces = "B03002_009",
  Hispanic = "B03002_012"
)

#total population count variable "B03002_001" - folks can fall under multiple categories, so this table is not best for aggregate
wa_race_populations_bg <- get_acs("block group", 
        state = "WA",
        variables = race_vars,
        summary_var = "B03002_001",
        year = 2024)


wa_people_of_color_bg <- wa_race_populations_bg |> 
  filter(variable != "White") |> 
  group_by(GEOID, NAME) |> 
  mutate(
    block_group_poc = sum(estimate), 
    block_group_MOE_sum = round(moe_sum(moe, estimate = estimate, na.rm = TRUE), digits = 0),
    ) |> 
  dplyr::select(GEOID, NAME, summary_est, summary_moe, block_group_poc, block_group_MOE_sum) |> 
  distinct(GEOID, .keep_all = TRUE)

#save people of color
save(wa_people_of_color_bg, file = "./data/demographic/POC_by_BG.RDATA")
#load people of color by tract
wa_people_of_color_bg <- load("./data/demographic/POC_by_BG.RDATA")

#people of color by Tract ----
wa_race_populations_tract <- get_acs("tract", 
                               state = "WA",
                               variables = race_vars,
                               summary_var = "B03002_001",
                               year = 2024)

wa_people_of_color_tract <- wa_race_populations_tract |> 
  filter(variable != "White") |> 
  group_by(GEOID, NAME) |> 
  mutate(
    tract_poc = sum(estimate), 
    tract_MOE_sum = round(moe_sum(moe, estimate = estimate, na.rm = TRUE), digits = 0)
  ) |> 
  dplyr::select(GEOID, NAME, summary_est, summary_moe, tract_poc, tract_MOE_sum) |> 
  distinct(GEOID, .keep_all = TRUE)

sum(wa_people_of_color_tract$tract_poc)
sum(wa_race_populations_tract$estimate)

wa_race_populations_tract2020 <- get_acs("tract", 
                                     state = "WA",
                                     variables = race_vars,
                                     summary_var = "B03002_001",
                                     year = 2020)

# sum(wa_race_populations_tract2020$estimate)
# 
# wa_people_of_color_tract2020 <- wa_race_populations_tract |> 
#   filter(variable != "White") |> 
#   group_by(GEOID, NAME) |> 
#   mutate(
#     tract_poc = sum(estimate), 
#     tract_MOE_sum = round(moe_sum(moe, estimate = estimate, na.rm = TRUE), digits = 0),
#   ) |> 
#   dplyr::select(GEOID, NAME, summary_est, summary_moe, tract_poc, tract_MOE_sum) |> 
#   distinct(GEOID, .keep_all = TRUE)
# 
# hispanic_pop_white <- wa_race_populations_tract2020 |> 
#   filter(variable == "Hispanic_white")


#save people of color
save(wa_people_of_color_tract, file = "./data/demographic/POC_by_tract.RDATA")
#load people of color by tract
load("./data/demographic/POC_by_tract.RDATA")


# wa_race_percent <- wa_race_populations |> 
#   mutate(percent = 0.01 * (estimate / summary_est)) |> 
#   select(NAME, variable, percent)


#disability status and age----- 
#(match formatting with others)
#set variables to be queried from ACS data
sex_by_age_disability_status <- c(
  dis_mu5 = "B18101_004", #male under 5 with disability
  dis_m5_17 = "B18101_007", #5 to 17 years
  dis_m18_34 = "B18101_010", #18 to 34 years
  dis_m35_64 = "B18101_013", #35 to 64 years
  dis_m65_74 = "B18101_016", #65 to 74 years
  dis_mo75 = "B18101_019", #75 years and over
  dis_f_u5 = "B18101_023",
  dis_f5_17 = "B18101_026",
  dis_f18_34 = "B18101_029",
  dis_f35_64 = "B18101_032",
  dis_f65_74 = "B18101_035",
  dis_fo75 = "B18101_038"
)


adults_with_a_disability <- c(
  dis_m18_34 = "B18101_010", #18 to 34 years
  dis_m35_64 = "B18101_013", #35 to 64 years
  dis_m65_74 = "B18101_016", #65 to 74 years
  dis_mo75 = "B18101_019", #75 years and over
  dis_f18_34 = "B18101_029",
  dis_f35_64 = "B18101_032",
  dis_f65_74 = "B18101_035",
  dis_fo75 = "B18101_038"
)


#data for full population - includes <18
wa_disability_populations <- get_acs("tract", 
                                     state = "WA",
                                     variables = sex_by_age_disability_status,
                                     summary_var = "B18101_001",
                                     year = 2024)
#alternative query for only adults
wa_adults_with_disability <- get_acs("tract", 
                                     state = "WA",
                                     variables = adults_with_a_disability,
                                     summary_var = "B18101_001",
                                     year = 2024)

#update data source for need
wa_disability_populations_consolidate <- wa_adults_with_disability |> #can update to adult or full population depending on need
  group_by(GEOID, NAME) |> 
  mutate(
    tract_disabilities = sum(estimate), 
    tract_disMOE_sum = moe_sum(moe, estimate = estimate, na.rm = TRUE)) |> 
  dplyr::select(GEOID, NAME, summary_est, summary_moe, tract_disabilities, tract_disMOE_sum) |> 
  distinct(GEOID, .keep_all = TRUE)

save(wa_disability_populations_consolidate, file = "./data/Demographic/adultw_disability_tract.RDATA")


# incomes below 200 percent of the federal poverty line ------
u200pov <- c(
  u0.5 = "C17002_002",
  o.50_0.994 = "C17002_003",
  o1.0_1.24 = "C17002_004",
  o1.25_1.49 = "C17002_005",
  o1.50_1.84 = "C17002_006",
  o1.85_1.99 = "C17002_007"
)
u200pov_bg <- get_acs("block group", 
                      state = "WA",
                      variables = u200pov,
                      summary_var = "C17002_001",
                      year = 2024)

u200pov_consol_bg <- u200pov_bg |> #can update to adult or full population depending on need
  group_by(GEOID, NAME) |> 
  mutate(
    u200pov_bg = sum(estimate), 
    u200pov_bg_moe = moe_sum(moe, estimate = estimate, na.rm = TRUE)) |> 
  dplyr::select(GEOID, NAME, summary_est, summary_moe, u200pov_bg, u200pov_bg_moe) |> 
  distinct(GEOID, .keep_all = TRUE)


u200pov_tract <- get_acs("tract", 
                         state = "WA",
                         variables = u200pov,
                         summary_var = "C17002_001",
                         year = 2024)

u200pov_consol_tract <- u200pov_tract |> #can update to adult or full population depending on need
  group_by(GEOID, NAME) |> 
  mutate(
    u200pov_tr = sum(estimate), 
    u200pov_tr_moe = moe_sum(moe, estimate = estimate, na.rm = TRUE)) |> 
  dplyr::select(GEOID, NAME, summary_est, summary_moe, u200pov_tr, u200pov_tr_moe) |> 
  distinct(GEOID, .keep_all = TRUE)

save(u200pov_consol_tract, file = "./data/Demographic/u200pov_tract.RDATA")
save(u200pov_consol_bg, file = "./data/Demographic/u200pov_BG.RDATA")

load("./data/Demographic/u200pov_tract.RDATA")
load("./data/Demographic/u200pov_BG.RDATA")

# incomes below 100 percent of the federal poverty line -----
u100pov <- c(
  u0.5 = "C17002_002",
  o.50_0.99 = "C17002_003"
)

u100pov_bg <- get_acs("block group", 
                      state = "WA",
                      variables = u100pov,
                      summary_var = "C17002_001",
                      year = 2024)

u100pov_consol_bg <- u100pov_bg |> #can update to adult or full population depending on need
  group_by(GEOID, NAME) |> 
  mutate(
    u100pov_bg = sum(estimate), 
    u100pov_bg_moe = moe_sum(moe, estimate = estimate, na.rm = TRUE)) |> 
  dplyr::select(GEOID, NAME, summary_est, summary_moe, u100pov_bg, u100pov_bg_moe) |> 
  distinct(GEOID, .keep_all = TRUE)


u100pov_tract <- get_acs("tract", 
                         state = "WA",
                         variables = u100pov,
                         summary_var = "C17002_001",
                         year = 2024)

u100pov_consol_tract <- u100pov_tract |> #can update to adult or full population depending on need
  group_by(GEOID, NAME) |> 
  mutate(
    u100pov_tr = sum(estimate), 
    u100pov_tr_moe = moe_sum(moe, estimate = estimate, na.rm = TRUE)) |> 
  dplyr::select(GEOID, NAME, summary_est, summary_moe, u100pov_tr, u100pov_tr_moe) |> 
  distinct(GEOID, .keep_all = TRUE)

save(u100pov_consol_tract, file = "./data/Demographic/u100pov_tract.RDATA")
save(u100pov_consol_bg, file = "./data/Demographic/u100pov_BG.RDATA")

load("./data/Demographic/u100pov_tract.RDATA")
load("./data/Demographic/u100pov_BG.RDATA")

#under 18 -----
under18 <- c(
  mu5 = "B01001_003", #male under 5
  m5_9 = "B01001_004", #5 to 9 years
  m10_14 = "B01001_005", #10 to 14 years
  m15_17 = "B01001_006", #15 to 17 years
  fu5 = "B01001_027", #female under 5
  f5_9 = "B01001_028", #5 to 9 years
  f10_14 = "B01001_029", #10 to 14 years
  f15_17 = "B01001_030" #15 to 17 years
)

under18_bg <- get_acs("block group", 
                      state = "WA",
                      variables = under18,
                      summary_var = "B01001_001",
                      year = 2024)

under18_consol <- under18_bg |> #can update to adult or full population depending on need
  group_by(GEOID, NAME) |> 
  mutate(
    u18_bg = sum(estimate), 
    u18_bg_moe = moe_sum(moe, estimate = estimate, na.rm = TRUE)) |> 
  dplyr::select(GEOID, NAME, summary_est, summary_moe, u18_bg, u18_bg_moe) |> 
  distinct(GEOID, .keep_all = TRUE)


under18_tract <- get_acs("tract", 
                      state = "WA",
                      variables = under18,
                      summary_var = "B01001_001",
                      year = 2024)

under18_consol_tract <- under18_tract |> #can update to adult or full population depending on need
  group_by(GEOID, NAME) |> 
  mutate(
    u18_tr = sum(estimate), 
    u18_tr_moe = moe_sum(moe, estimate = estimate, na.rm = TRUE)) |> 
  dplyr::select(GEOID, NAME, summary_est, summary_moe, u18_tr, u18_tr_moe) |> 
  distinct(GEOID, .keep_all = TRUE)


save(under18_consol_tract, file = "./data/Demographic/u18_tract.RDATA")
save(under18_consol, file = "./data/Demographic/u18_BG.RDATA")

load("./data/Demographic/u18_tract.RDATA")
load("./data/Demographic/u18_BG.RDATA")

wa_demo_Tract <- left_join(wa_demo_Tract, under18_consol_tract, join_by(GEOID))
wa_demo_BG <- left_join(wa_people_of_color_bg, under18_consol, join_by(GEOID))
#Over 65 -----
over65 <- c(
  m65_66 = "B01001_020", #male under 5
  m6769 = "B01001_021", #5 to 9 years
  m7074 = "B01001_022", #10 to 14 years
  m7579 = "B01001_023", #15 to 17 years
  m8084 = "B01001_024", #15 to 17 years
  m85o = "B01001_025", #15 to 17 years
  f65_66 = "B01001_044", #male under 5
  f6769 = "B01001_045", #5 to 9 years
  f7074 = "B01001_046", #10 to 14 years
  f7579 = "B01001_047", #15 to 17 years
  f8084 = "B01001_048", #15 to 17 years
  f85o = "B01001_049" #15 to 17 years
)

over65_bg <- get_acs("block group", 
                      state = "WA",
                      variables = over65,
                      summary_var = "B01001_001",
                      year = 2024) 

over65_consol_bg <- over65_bg |> #can update to adult or full population depending on need
  group_by(GEOID, NAME) |> 
  mutate(
    o65_bg = sum(estimate), 
    o65_bg_moe = moe_sum(moe, estimate = estimate, na.rm = TRUE)) |> 
  dplyr::select(GEOID, NAME, summary_est, summary_moe, o65_bg, o65_bg_moe) |> 
  distinct(GEOID, .keep_all = TRUE)


over65_tract <- get_acs("tract", 
                         state = "WA",
                         variables = over65,
                         summary_var = "B01001_001",
                         year = 2024)

over65_consol_tract <- over65_tract |> #can update to adult or full population depending on need
  group_by(GEOID, NAME) |> 
  mutate(
    o65_tr = sum(estimate), 
    o65_tr_moe = moe_sum(moe, estimate = estimate, na.rm = TRUE)) |> 
  dplyr::select(GEOID, NAME, summary_est, summary_moe, o65_tr, o65_tr_moe) |> 
  distinct(GEOID, .keep_all = TRUE)

save(over65_consol_tract, file = "./data/Demographic/o65_tract.RDATA")
save(over65_consol_bg, file = "./data/Demographic/o65_BG.RDATA")

load("./data/Demographic/o65_tract.RDATA")
load("./data/Demographic/o65_BG.RDATA")


#combined demographics file -----
wa_tracts_combined_demo <- wa_people_of_color_tract |> 
  left_join(wa_disability_populations_consolidate |> select(tract_disabilities, tract_disMOE_sum), join_by(GEOID, NAME)) |> 
  left_join(u200pov_consol_tract |> select(u200pov_tr, u200pov_tr_moe), join_by(GEOID, NAME)) |> 
  left_join(u100pov_consol_tract |> select(u100pov_tr, u100pov_tr_moe), join_by(GEOID, NAME)) |> 
  left_join(under18_consol_tract |> select(u18_tr, u18_tr_moe), join_by(GEOID, NAME)) |> 
  left_join(over65_consol_tract |> select(o65_tr, o65_tr_moe), join_by(GEOID, NAME)) |> 
  rename(
    poc_tr_MOE = tract_MOE_sum, 
    poc_tr = tract_poc, 
    disability_tr = tract_disabilities,
    disability_tr_moe = tract_disMOE_sum
  )

save(wa_tracts_combined_demo, file = "./data/Demographic/wa_tracts_combined_demo.RDATA")

wa_bg_combined_demo <- wa_people_of_color_bg |> 
  left_join(u200pov_consol_bg |> select(u200pov_bg, u200pov_bg_moe), join_by(GEOID, NAME)) |> 
  left_join(u100pov_consol_bg |> select(u100pov_bg, u100pov_bg_moe), join_by(GEOID, NAME)) |> 
  left_join(under18_consol |> select(u18_bg, u18_bg_moe), join_by(GEOID, NAME)) |> 
  left_join(over65_consol_bg |> select(o65_bg, o65_bg_moe), join_by(GEOID, NAME)) |> 
  rename(
    poc_bg_MOE = block_group_MOE_sum, 
    poc_bg = block_group_poc
  )

save(wa_bg_combined_demo, file = "./data/Demographic/wa_bg_combined_demo.RDATA")

#load combined demographic data ----
load("./data/Demographic/wa_tracts_combined_demo.RDATA")
load("./data/Demographic/wa_bg_combined_demo.RDATA")

#total population ACS 5-year 7,816,116
#runs a little bit below the 1-year ests. 

#geometry with populations ----
st_geometry(WA_tracts_tigris)

#vector conversion to raster projection ----
WA_tracts_tigris_reproj <- st_transform(WA_tracts_tigris, crs(wa_jobs))
st_crs(wa_jobs)
ggplot(WA_tracts_tigris_reproj) +
  geom_sf()

