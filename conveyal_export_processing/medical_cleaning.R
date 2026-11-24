#test- copilot consolidation
#
##This script loads in and transforms Conveyal geotiff regional analysis export data for WSDOT analyses for Hospitals and other medical category destinations. The files are loaded in as spatrasters using terra. Each medical destination category is then weighted to a binary value of 1 if the destination is reachable or a 0 is retained (additional counts of reachable destinations are not retained). Next, each category is assigned a weighted value to align with the aggregation value, 5 for hospitals and 1 for each of the other destination types. East and west regional analyses are merged together using terra mosaic. 
#This could have been done more efficiently with functions and aggregating layers that would undergo the same transformation earlier on. Retaining original code for visibility. 


library(terra)
library(purrr)

setwd("~/Projects/R Explore/Conveyal Exports")
getwd()

# Travel scenarios in layer order
profiles <- c(
  "WP1",
  "WP2",
  "WP3",
  "WP4",
  "car",
  "Walk1",
  "Walk2"
)

# Load and mosaic east/west rasters for one destination type
load_destination <- function(dest_type, weight = 1) {
  
  layers <- map(
    profiles,
    function(profile) {
      
      west <- rast(
        sprintf("./data/Medical/W_WA_%s_%s.tif",
                dest_type,
                profile)
      )
      
      east <- rast(
        sprintf("./data/Medical/E_WA_%s_%s.tif",
                dest_type,
                profile)
      )
      
      mosaic(west, east)
    }
  )
  
  out <- rast(layers)
  
  # binary accessibility
  out[out > 1] <- 1
  
  # destination weighting
  out <- out * weight
  
  out
}


apply_skipping_correction <- function(x) {
  
  # WP2 should not outperform WP4
  x[[2]][x[[4]] < x[[2]]] <- 0
  
  # WP3 should not outperform WP1
  x[[3]][x[[1]] < x[[3]]] <- 0
  
  # Walk2 should not outperform Walk1
  x[[7]][x[[6]] < x[[7]]] <- 0
  
  x
}


medical_layers <- list(
  
  hospitals = load_destination(
    "hospitals",
    weight = 5
  ),
  
  dental = load_destination("dental"),
  
  pharm = load_destination("pharm"),
  
  primary = load_destination("primary"),
  
  specialty = load_destination("specialty"),
  
  vision = load_destination("vision")
)



medical_layers <- map(
  medical_layers,
  apply_skipping_correction
)



wa_non_hosp_med <- Reduce(
    function(x, y) x + y,
    medical_layers[c(
      "dental",
      "pharm",
      "primary",
      "specialty",
      "vision"
    )]
  )


wa_medical <- sum(
  wa_non_hosp_med,
  medical_layers$hospitals
)


writeRaster(
  wa_medical,
  "./data/Medical/wa_medical_test.tif",
  overwrite = TRUE
)

wa_medical <- rast("./data/Medical/wa_medical_test.tif")




#diff appears to be due to typo in original code - re-running to confirm? 
med_diff <- wa_medical - wa_medical1
diff <- med_diff[[5]] - #934 differences concentrated in one area - max at 3 - may be a mosaic difference
diff2 <- med_diff[[7]] #>1200 differences scattered across state - additional filter in original - filtered WP2 by walking to never exceed WP2 w/ transit
  #example wa_medical[[7]][wa_medical[[7]] > wa_medical[[2]]] <- 0 ---should be corrected with updated conveyal run. Do not need to address now

freq(diff)
freq(diff2)

plot(diff2)

summary(med_diff)
