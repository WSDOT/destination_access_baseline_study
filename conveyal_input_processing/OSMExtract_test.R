#working out of package - https://cran.r-project.org/web/packages/osmextract/vignettes/osmextract.html

#able to extract data well enoght - have full dataset of WA locally - can query from here, but would rather query on data pull for smaller/workable files, so osmdata package uses more familiar...friendly overpass API. Will test process in new script

library(osmextract)
library(sf)
library(tidyverse)

getwd()

#only load in OSM data once - do have full WA state extract at "./data/geofabrik_washington-latest.osm.pbf" locally
# GDAL are usually categorized into 5 layers, named points, lines, multilinestrings, multipolygons and other_relations5. The oe_vectortranslate() function can covert only one layer at a time.

#shows the different sources available for extract----
oe_providers()

#sf function that I assume sets up mapping process
par(mar = rep(0.1, 4))
#plots the zones that geofabrik providers use to divide datasets spatially
plot(st_geometry(geofabrik_zones))

#search function that tries to find matching dataset for input string, returns us/washington
oe_match("bellingham washington")

#allows download .pbf files representing OSM extracts - runs checks for download prior to download - full DL can be expensive - >300 Mb for WA state
oe_download(
  file_url = "https://download.geofabrik.de/north-america/us/washington-latest.osm.pbf", 
  file_size = 3.12e+08, 
  download_directory = "./data")

#get initial download of OSM dat
# oe_get("us/washington", download_only = TRUE, skip_vectortranslate = TRUE)

#shows where default download will land
oe_download_directory()

#provides metadata for specified file
st_layers("./data/geofabrik_washington-latest.osm.pbf", do_count = TRUE) 

#decompression as a gkpkg file is created  with only lines
wa_state_lines <- oe_vectortranslate("./data/geofabrik_washington-latest.osm.pbf")
#add points to gpkg file - now 1.019 GB
wa_stae_points <- oe_vectortranslate("./data/geofabrik_washington-latest.osm.pbf", layer = "points")

st_layers("./data/geofabrik_washington-latest.gpkg", do_count = TRUE)

#gives commonly used keys used in file
oe_get_keys("./data/geofabrik_washington-latest.gpkg")
#adds top occurring and a $ of NAs
oe_get_keys("./data/geofabrik_washington-latest.gpkg", values = TRUE)

#query practice with smaller area

wa_state_lin <- oe_read("./data/geofabrik_washington-latest.gpkg", skip_vectortranslate = TRUE)
wa_state_point <- oe_read("./data/geofabrik_washington-latest.gpkg", layer = "points", skip_vectortranslate = TRUE)

#start here -----
lines_v1 <- oe_get(
  place = "us/washington", 
  layer = "lines", 
  query =  "SELECT * FROM lines WHERE highway IN ('primary', 'secondary', 'tertiary', 'residential', 'unclassified', 'trunk', 'motorway', 'motorway_link', 'trunk_link', 'primary_link', 'secondary_link', 'tertiary_link', 'living_street', 'service', 'footway', 'path', 'cycleway')", 
  download_directory = "./data"
)

plot(lines_v1$geometry)


#goals are to crop and filter by tags, likely export as geojson
str(lines_v1)

#attempt at regex - would just put results in random columns, seems more challenging to sort 
# crop_lines <- lines_v1 |> 
#   select(osm_id, name, highway, other_tags, geometry) |> 
#   separate_wider_regex(
#     other_tags, 
#     patterns = c(
#       '"', 
#       tag = "[A-Za-z]", 
#       '"=>"', 
#       value = "[A-Za-z]"
#     )
#     delim  = ","
#   )


#gets each tag - value pair into it's own row, but duplicate elements now exist and remove geometry to work with smaller file for now - can left-join to original file and get rid of other tags column. 
crop_lines <- lines_v1 |> 
  select(osm_id, name, highway, other_tags) |> 
  separate_longer_delim(
    other_tags, 
    delim  = ','
  )

#get each tag and value separated
crop_lines <- crop_lines |>
  select(-geometry) |> 
  separate_wider_delim(
    other_tags, 
    delim = '=>', 
    names = c("tag", "value"), 
    too_few = "align_start"
  )

#filter to only needed tags - can add tag using below format if interested. 734426 features have at least one of the below
crop_lines_fil <- crop_lines |> 
  filter(
  str_detect(tag, '\"maxspeed\"|\"access\"|\"surface\"|\"incline\"|\"foot\"')
  )


pivot_try <- crop_lines_fil |> 
  pivot_wider(
  id_cols = osm_id, 
  names_from = tag, 
  values_from = value) |> 
  rename(
    maxspeed = '\"maxspeed\"', 
    surface = '\"surface\"',
    foot = '\"foot\"', 
    access = '\"access\"',
    incline ='\"incline\"'
    )

#cleaning and removal of quotation marks in 
pivot_quote <- pivot_try |> 
  separate_wider_delim(
  maxspeed,
  delim = '\"', 
  names = c("quote", "maxspeed", "quote2")
  ) |> 
  separate_wider_delim(
    access,
    delim = '\"', 
    names = c("quote1", "access", "quote12")
  ) |> 
  separate_wider_delim(
    surface,
    delim = '\"', 
    names = c("quote21", "surface", "quote22"), 
    too_few = "align_start"
  )|> 
  separate_wider_delim(
    incline,
    delim = '\"', 
    names = c("quote3", "incline", "quote32"), 
    too_few = "align_start")|> 
  separate_wider_delim(
    foot,
    delim = '\"', 
    names = c("quote4", "foot", "quote42")
  ) |> 
  select(osm_id, maxspeed,access, surface, incline, foot)
  
#tag metadata summaries - and converting slope - extracting numbers only and converting into decimal value
incline_review <- pivot_quote |> 
  distinct(incline) |> 
  mutate(
    "incline2" = parse_number(incline)
  ) |> 
  mutate(
    "incline2" = incline2 * 0.01
  )

#tag metadata summaries
access_review <- pivot_quote |> 
  group_by(access) |>  
  summarise(n())

#join_decimal incline values to rest of tags
pivot_quote_decimals <- left_join(pivot_quote, incline_review, join_by(incline == incline)) |> 
  select(-incline) |> 
  rename(incline = incline2)

#filter our unnecessary columns - including grouped tags column
wa_osm_paths <- lines_v1 |> 
  select(name, osm_id, highway, geometry)

#removing paths without access for now, will be retaining a number of unpaved paths this way; limiting the amount we filter here to align with walking profiles 1 & 4 that indicate navigation regardless of infrastructure or grade
wa_osm_paths_filtered <- left_join(wa_osm_paths, pivot_quote_decimals, by = c("osm_id" = "osm_id")) |>
  filter(
    !access %in% c("no", "private")
  ) |> 
  relocate(
    osm_id, .before = name
  )



#tag metadata summaries
surface_review <- st_drop_geometry(wa_osm_paths_filtered) |>
  group_by(surface) |>  
  summarise(n())

highway_surface_review <- st_drop_geometry(wa_osm_paths_filtered) |>
  group_by(highway, surface) |>  
  summarise(n())

foot_review <- st_drop_geometry(wa_osm_paths_filtered) |>
  group_by(foot) |>  
  summarise(n())

str(surface_review)

#will want to crop into east and west files at a minimum

#Conveyal Regional settings
#East	N: 49.02706, S: 45.56983, E: -116.88904, W: -121.35223 OR (45.56983, -121.35223, 49.02706, -116.88904)
#Half lat - 47.29845
#West	N: 49.02886, S: 45.53714, E: -121.07758, W: -124.76074 OR (45.53714, 124.76074, 49.02886, -121.07758)

east_osm <- st_crop(wa_osm_paths_filtered, xmin = -121.35223 , xmax = -116.88904, ymin = 45.56983, ymax = 49.02706)

nw_osm <- st_crop(wa_osm_paths_filtered, xmin = -124.76074 , xmax = -121.07758, ymin = 47.29845, ymax = 49.02886)

sw_osm <- st_crop(wa_osm_paths_filtered, xmin = -124.76074 , xmax = -121.07758, ymin = 45.53714, ymax = 47.29845) 

west_osm <- st_crop(wa_osm_paths_filtered, xmin = -124.76074 , xmax = -121.07758, ymin = 45.53714, ymax = 49.02886)

#saving dataset post-filter and tag conversion
save(wa_osm_paths_filtered, file ="./data/wa_osm_paths.Rdata")

#writing file as geojson
st_write(wa_osm_paths_filtered, "./output/wa_osm_paths.geojson")


#west files at least initially are possibly too big, causing load failure
#write east file as geojson
st_write(east_osm, "./output/e_wa_osm.geojson")
st_write(nw_osm, "./output/nw_wa_osm.geojson")
st_write(sw_osm, "./output/sw_wa_osm.geojson")
st_write(west_osm, "./output/w_wa_osm.geojson")
#experimenting with writing as shapefile - got warning   GDAL Message 1: One or several characters couldn't be converted correctly from UTF-8 to ISO-8859-1.  This warning will not be emitted anymore.
st_write(east_osm, "./output/e_wa_osm.shp")


st_drivers()

#portland export ----
#


oe_match("portland Oregon")

oe_download(
  file_url = "https://download.geofabrik.de/north-america/us/oregon-latest.osm.pbf", 
  file_size = 2.24e+08, 
  download_directory = "./data")

lines_or <- oe_get(
  place = "us/oregon", 
  layer = "lines", 
  query =  "SELECT * FROM lines WHERE highway IN ('primary', 'secondary', 'tertiary', 'residential', 'unclassified', 'trunk', 'motorway', 'motorway_link', 'trunk_link', 'primary_link', 'secondary_link', 'tertiary_link', 'living_street', 'service', 'footway', 'path', 'cycleway')", 
  download_directory = "./data"
)

#gets each tag - value pair into it's own row, but duplicate elements now exist and remove geometry to work with smaller file for now - can left-join to original file and get rid of other tags column. 
crop_lines_or <- lines_or |> 
  select(osm_id, name, highway, other_tags) |> 
  separate_longer_delim(
    other_tags, 
    delim  = ','
  )

#get each tag and value separated
crop_lines_or <- crop_lines_or |>
  select(-geometry) |> 
  separate_wider_delim(
    other_tags, 
    delim = '=>', 
    names = c("tag", "value"), 
    too_few = "align_start"
  )

#filter to only needed tags - can add tag using below format if interested. 734426 features have at least one of the below
crop_lines_or_fil <- crop_lines_or |> 
  filter(
    str_detect(tag, '\"maxspeed\"|\"access\"|\"surface\"|\"incline\"|\"foot\"')
  )


pivot_try_or <- crop_lines_or_fil |> 
  pivot_wider(
    id_cols = osm_id, 
    names_from = tag, 
    values_from = value) |> 
  rename(
    maxspeed = '\"maxspeed\"', 
    surface = '\"surface\"',
    foot = '\"foot\"', 
    access = '\"access\"',
    incline ='\"incline\"'
  )

#cleaning and removal of quotation marks in tag fields
pivot_quote <- pivot_try_or |> 
  separate_wider_delim(
    maxspeed,
    delim = '\"', 
    names = c("quote", "maxspeed", "quote2")
  ) |> 
  separate_wider_delim(
    access,
    delim = '\"', 
    names = c("quote1", "access", "quote12")
  ) |> 
  separate_wider_delim(
    surface,
    delim = '\"', 
    names = c("quote21", "surface", "quote22"), 
    too_few = "align_start"
  )|> 
  separate_wider_delim(
    incline,
    delim = '\"', 
    names = c("quote3", "incline", "quote32"), 
    too_few = "align_start")|> 
  separate_wider_delim(
    foot,
    delim = '\"', 
    names = c("quote4", "foot", "quote42")
  ) |> 
  select(osm_id, maxspeed,access, surface, incline, foot)

#tag metadata summaries - and converting slope - extracting numbers only and converting into decimal value
incline_review <- pivot_quote |> 
  distinct(incline) |> 
  mutate(
    "incline2" = parse_number(incline)
  ) |> 
  mutate(
    "incline2" = incline2 * 0.01
  )

#tag metadata summaries
access_review <- pivot_quote |> 
  group_by(access) |>  
  summarise(n())

#join_decimal incline values to rest of tags
pivot_quote_decimals <- left_join(pivot_quote, incline_review, join_by(incline == incline)) |> 
  select(-incline) |> 
  rename(incline = incline2)

#filter our unnecessary columns - including grouped tags column
or_osm_paths <- lines_or |> 
  select(name, osm_id, highway, geometry)

#removing paths without access for now, will be retaining a number of unpaved paths this way; limiting the amount we filter here to align with walking profiles 1 & 4 that indicate navigation regardless of infrastructure or grade
or_osm_paths_filtered <- left_join(or_osm_paths, pivot_quote_decimals, by = c("osm_id" = "osm_id")) |>
  filter(
    !access %in% c("no", "private")
  ) |> 
  relocate(
    osm_id, .before = name
  )



#tag metadata summaries
# surface_review <- st_drop_geometry(wa_osm_paths_filtered) |>
#   group_by(surface) |>  
#   summarise(n())
# 
# highway_surface_review <- st_drop_geometry(wa_osm_paths_filtered) |>
#   group_by(highway, surface) |>  
#   summarise(n())
# 
# foot_review <- st_drop_geometry(wa_osm_paths_filtered) |>
#   group_by(foot) |>  
#   summarise(n())
# 
# str(surface_review)

#will want to crop into east and west files at a minimum

#PDX Regional settings
##bbox - Southwest: 45.375875, -122.911606 
#Northeast: 45.670294, -122.381516
pdx_osm <- st_crop(or_osm_paths_filtered, xmin = -122.911606 , xmax = -122.381516, ymin = 45.375875, ymax = 45.670294)


#saving dataset post-filter and tag conversion
save(pdx_osm, file ="./data/pdx_osm_paths.Rdata")

#writing file as geojson
st_write(pdx_osm, "./output/pdx_osm_paths.geojson")

