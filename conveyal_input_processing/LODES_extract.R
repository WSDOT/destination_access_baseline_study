#script to test functionality and implement LODES data extraction using lehdr
#

library(lehdr)
library(tigris)
library(tidyverse)
library(sf)

options(lehdr_use_cache = TRUE)

#creates df with all od jobs in state - defaults to state_part main - may want to include OR and ID for jobs that may be reachable 
wa_or_id_od <- grab_lodes(
  state = c("wa", "or", "id"),
  year = 2023, 
  version = "LODES8", 
  segment = "S000",
  lodes_type = "od",
  agg_geo = "block",
  state_part = "main",
  download_dir = "./data/LODES"
)



#tigris download of block groups
wa_blocks <- blocks(state = "wa", 
                    year = 2020)
  
or_blocks <- blocks(state = "or", 
         year = 2020, 
         filter_by = c(-124.76074, 45.440863, -116.88904, 49.02886))


id_blocks <- blocks(state = "id", 
         year = 2020,
         filter_by = c(-124.76074, 45.53714, -116.88904, 49.02886))

wa_blocks_tidy <- st_drop_geometry(wa_blocks) |> 
  select(GEOID20, INTPTLAT20, INTPTLON20)

or_blocks_tidy <- st_drop_geometry(or_blocks) |> 
  select(GEOID20, INTPTLAT20, INTPTLON20)

id_blocks_tidy <- st_drop_geometry(id_blocks) |> 
  select(GEOID20, INTPTLAT20, INTPTLON20)

wa_or_blocks_tidy <- rbind(wa_blocks_tidy, or_blocks_tidy) 

wa_or_id_blocks_tidy <- rbind(wa_or_blocks_tidy, id_blocks_tidy)

#shapefile upload attempt - assumes distributed population w/n conveyal instead of shapefile centroid
wa_or_blocks <- rbind(wa_blocks, or_blocks) 
wa_or_id_blocks <- rbind(wa_or_blocks, id_blocks)


str(wa_or_id_blocks)
str(wa_od)
str(wa_od_work_only)

wa_or_id_od_work_only <- wa_or_id_od |> 
  select(w_geocode, S000) |> 
  group_by(w_geocode) |> 
  summarise(sum(S000)) |> 
  rename(
    "total_jobs" = `sum(S000)`
  )

wa_or_id_live <- wa_or_id_od |> 
  select(h_geocode, S000) |> 
  group_by(h_geocode) |> 
  summarise(sum(S000)) |> 
  rename(
    "workers_living" = `sum(S000)`
  )


#join LODES and center lat/lon
wa_plus_block_jobs_geom <- left_join(wa_or_id_blocks |> select(GEOID20, geometry), wa_or_id_od_work_only, join_by(GEOID20 == w_geocode))

wa_plus_block_jobs_geom


#ewa-bounding box ----
#example equally distributes jobs across area instead of centroid points
bbox <- matrix(
  c(-121.35223, 49.02706, 
    -116.88904, 49.02706,
    -116.88904, 45.56983,
    -121.35223, 45.56983, 
    -121.35223, 49.02706
  ), 
  ncol = 2, 
  byrow = TRUE
)
bbox_poly <- st_polygon(list(bbox))
bbox_sf <- st_sfc(bbox_poly, crs = "NAD83")
plot(bbox_sf)

e_wa_jobs_poly <- st_filter(wa_plus_block_jobs_geom, bbox_sf)

st_write(e_wa_jobs_poly, "./output/ewa_2023_LODES.shp")

e_wa_jobs_poly <- read_sf("./output/ewa_2023_LODES.shp")


#wwa-bounding box -----
#xmin = -124.76074 , xmax = -121.07758, ymin = 45.440863, ymax = 49.02886
#
wbbox <- matrix(
  c(-124.76074, 49.02886, 
    -121.07758, 49.02886,
    -121.07758, 45.440863,
    -124.76074, 45.440863, 
    -124.76074, 49.02886
  ), 
  ncol = 2, 
  byrow = TRUE
)
wbbox_poly <- st_polygon(list(wbbox))
wbbox_sf <- st_sfc(wbbox_poly, crs = "NAD83")
plot(wbbox_sf)

w_wa_jobs_poly <- st_filter(wa_plus_block_jobs_geom, wbbox_sf)

st_write(w_wa_jobs_poly, "./output/wwa_2023_LODES.shp")
w_wa_jobs_poly <- read_sf("./output/wwa_2023_LODES.shp")

#block centroid -----
wa__plus_block_jobs_centroid <- wa_plus_block_jobs_geom |> 
  mutate(
    latitude = parse_number(lat),
    longitude = parse_number(lon)
  ) |> 
  select(-lat, -lon)


#east_ref xmin = -121.35223 , xmax = -116.88904, ymin = 45.56983, ymax = 49.02706

#west_ref xmin = -124.76074 , xmax = -121.07758, ymin = 45.440863, ymax = 49.02886
  

#crop dataset to align with conveyal regions
east_plus_LODES_total_jobs <- wa__plus_block_jobs_centroid |> 
  filter(longitude > -121.35223 &
           latitude > 45.53714 &
           latitude < 49.02886) |> 
  select(-w_geocode)

west_plus_LODES_total_jobs <- wa__plus_block_jobs_centroid |> 
  filter(longitude < -121.07758,
         latitude > 45.440863 &
           latitude < 49.02886)|> 
  select(-w_geocode)

write.csv(east_plus_LODES_total_jobs, file = "./output/2023_lodes_ewa_plus_blocks_centroid.csv")

write.csv(west_plus_LODES_total_jobs, file = "./output/2023_lodes_wwa_plus_blocks_centroid.csv")
 


