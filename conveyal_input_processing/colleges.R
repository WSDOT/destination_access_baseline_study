
library(osmextract)
library(sf)
library(tidyverse)
library(data.table)
library(units)
library(mapview)

#colleges ----
college_poly <- oe_get(
  place = "us/washington", 
  layer = "multipolygons", 
  query =  "SELECT * FROM multipolygons WHERE amenity IN ('college', 'university')",
  download_directory = "./data"
) 
sf_use_s2(FALSE)
college_poly <- college_poly |> 
  distinct(name, .keep_all = TRUE) |> 
  filter(!grepl('Steve Ballmer|State Patrol Academy|Washington State Criminal Justice Training Center|The Seattle School of Theology and Psychology|Olympic Building|Alderleaf Wilderness College|CBC X|Northwest University Graduate|Evergreen Beauty College - Yakima|EvCC |Angela Bowen|MITAGS', name)) |> 
  mutate(
    centroid = st_centroid(geometry)
  )

str(college_lat_lon)

college_lat_lon <- college_poly |> 
  mutate(lon = sf::st_coordinates(centroid)[,1],
         lat = sf::st_coordinates(centroid)[,2],) |> 
  st_drop_geometry() |> 
  select(name, lat, lon)

west_college <- filter(college_lat_lon, college_lat_lon$lon < -121.07758)
east_college <- filter(college_lat_lon, college_lat_lon$lon > -121.35223)

#save csv for conveyal input
write.csv(west_college, file = "./output/west_college.csv")
write.csv(east_college, file = "./output/east_college.csv")