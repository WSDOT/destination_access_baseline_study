#This script utilizes duckDB to download places of interest data from overture maps foundation. The core function takes coordinates for a bounding box and exports each point of overture type ("place" in our use case), and the output file name. 
#We then read in the parquet as a dataframe, convert to a simple feature with geometry and tidy by filtering unneeded fields and unnest list-type category fields for simplified data exploration, summarization, and filtering by place type. 


# Load required libraries
library(tidyverse)
library(sf)
library(mapview) 
library(DBI)
library(duckdb)
library(arrow)

# Ties variables from script to Overture - native variable names. Mostly a remnant from https://ar-puuk.github.io/posts/overture-data-download/ - 
map_themes <- list(
  "locality" = "admins",
  "locality_area" = "admins",
  "administrative_boundary" = "admins",
  "building" = "buildings",
  "building_part" = "buildings",
  "place" = "places",
  "segment" = "transportation",
  "connector" = "transportation",
  "infrastructure" = "base",
  "land" = "base",
  "land_use" = "base",
  "water" = "base"
)

#Overture download function ----
overture_data <- function(bbox, overture_type, dst_parquet) {
  
  # Validate overture_type
  if (!overture_type %in% names(map_themes)) {
    stop(paste("Valid Overture types are:", paste(names(map_themes), collapse = ", ")))
  }
  
  # Configure S3 path
  s3_region <- "us-west-2"
  base_url <- sprintf("s3://overturemaps-%s/release", s3_region)
  version <- "2026-03-18.0" #manually update release date - https://docs.overturemaps.org/release-calendar/
  theme <- map_themes[[overture_type]]
  remote_path <- sprintf("%s/%s/theme=%s/type=%s/*", base_url, version, theme, overture_type)
  
  # Connect to DuckDB and install extensions
  conn <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(conn, "INSTALL httpfs;")
  DBI::dbExecute(conn, "INSTALL spatial;")
  DBI::dbExecute(conn, "LOAD httpfs;")
  DBI::dbExecute(conn, "LOAD spatial;")
  DBI::dbExecute(conn, sprintf("SET s3_region='%s';", s3_region))
  
  # Create view and execute spatial query
  read_parquet <- sprintf("read_parquet('%s', filename=TRUE, hive_partitioning=1);", remote_path)
  DBI::dbExecute(conn, sprintf("CREATE OR REPLACE VIEW data_view AS SELECT * FROM %s", read_parquet))
  
  query <- sprintf("
    SELECT data.*
    FROM data_view AS data
    WHERE data.bbox.xmin <= %f AND data.bbox.xmax >= %f
    AND data.bbox.ymin <= %f AND data.bbox.ymax >= %f
  ", bbox[3], bbox[1], bbox[4], bbox[2])
  
  # Save results to Parquet file
  file <- normalizePath(dst_parquet, mustWork = FALSE)
  DBI::dbExecute(conn, sprintf("COPY (%s) TO '%s' WITH (FORMAT 'parquet');", query, file))
  DBI::dbDisconnect(conn, shutdown = TRUE)
}

#manually input bbox parameters for function
manual_bbox <- st_bbox(c(xmin = -124.76074, xmax = -116.88904, ymax = 49.02886, ymin = 45.53714), crs = st_crs(4326))
print(manual_bbox)

# Execute function to Download places data for CUSTOMIZE PARQUET NAME ------
if (!file.exists("output/wa_places_r.parquet")) {
  overture_data(manual_bbox, "place", "output/wa_places_r.parquet") # need to make sure an output file exists in order to create file in location
}

# Read the downloaded Parquet file to R data frame 
wa_places <- arrow::read_parquet("output/wa_places_r.parquet")


# Convert to sf object for spatial operations
wa_places_sf <- sf::st_as_sf(
  wa_places |> dplyr::select(-sources),
  geometry = wa_places$geometry,
  crs = "EPSG:4326"
)


#need to identify list or other data types and unnest into individual columns
#head(wa_places_sf)

#unnest list variables into new columns, and drop unneccessary fields with select -----
wa_tidy1 <- wa_places_sf |> select(
  -websites, -socials, -emails, -phones
) |> 
  unnest_wider(
    bbox, 
    names_sep = "$"
  ) |> unnest_wider(
    names,
    names_sep = "$"
  ) |> unnest_wider(
    categories,
    names_sep = "$"
  )|> unnest_wider(
    brand,
    names_sep = "$"
  ) |> unnest_wider(
    addresses,
    names_sep = ","
  ) |> unnest_wider(
    `brand$names`,
    names_sep = "$"
  ) |> select(-`names$common`, -`names$rules`, -`categories$alternate`, -`brand$names$common`, -`brand$names$rules`)

#convert the point value from sfc_point formatting to two lat/lon columns for simplified data handling and file size -----
wa_tidy_geom <- wa_tidy1 |> 
  mutate(lon = sf::st_coordinates(geometry)[,1],
         lat = sf::st_coordinates(geometry)[,2],) |> 
  select(-geometry)

# export as csv
write.csv(wwa_tidy_geom, file = "./output/wwa_tidy.csv", row.names = FALSE)

#save R data file for future use
save(wwa_tidy_geom, file = "./output/wwa_places.Rdata")
