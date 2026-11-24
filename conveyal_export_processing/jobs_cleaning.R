

library(terra)
library(tidyverse)

make_statewide <- function(profile, period, type = "jobs") {
  ewa <- rast(sprintf("./data/Jobs/ewa_%s_%s_%s_P50_C45.tif",
                      type, period, profile))
  
  wwa <- rast(sprintf("./data/Jobs/wwa_%s_%s_%s_P50_C45.tif",
                      type, period, profile))
  
  mosaic(ewa, wwa)
}

profiles <- c("WP1", "WP2", "WP3", "WP4")
periods  <- c("AM", "PM")

jobs <- list()

for (p in profiles) {
  for (tm in periods) {
    jobs[[paste(tm, p, sep = "_")]] <-
      make_statewide(p, tm)
  }
}

jobs$AM_WP3 <- ifel(jobs$AM_WP3 > jobs$AM_WP1, 0, jobs$AM_WP3)
jobs$PM_WP3 <- ifel(jobs$PM_WP3 > jobs$PM_WP1, 0, jobs$PM_WP3)

jobs$AM_WP2 <- ifel(jobs$AM_WP2 > jobs$AM_WP4, 0, jobs$AM_WP2)
jobs$PM_WP2 <- ifel(jobs$PM_WP2 > jobs$PM_WP4, 0, jobs$PM_WP2)

wa_jobs <- rast(jobs)

#driving raster
wa_jobs_driving <- mosaic(
  rast("./data/Jobs/ewa_jobs_car_P50_C45.tif"),
  rast("./data/Jobs/wwa_jobs_car_P50_C45.tif")
)

#walking-only rasters
make_walk <- function(profile) {
  mosaic(
    rast(sprintf("./data/Jobs/ewa_jobs_walk_%s.tif", profile)),
    rast(sprintf("./data/Jobs/wwa_jobs_walk_%s.tif", profile))
  )
}

wa_jobs_walk1 <- make_walk("WP1")
wa_jobs_walk2 <- make_walk("WP2")
