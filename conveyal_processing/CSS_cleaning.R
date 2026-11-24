
library(terra)

load_css_category <- function(category,
                              path = "./data/CSS") {
  
  layers <- c(
    "WP1",
    "WP2",
    "WP3",
    "WP4",
    "car",
    "Walk1",
    "Walk2"
  )
  
  rasters <- lapply(layers, function(layer) {
    
    west <- rast(file.path(path,
                           sprintf("W_WA_%s_%s.tif", category, layer)))
    
    east <- rast(file.path(path,
                           sprintf("E_WA_%s_%s.tif", category, layer)))
    
    # binary conversion
    west <- ifel(west > 0, 1, 0)
    east <- ifel(east > 0, 1, 0)
    
    mosaic(west, east)
  })
  
  # align to WP1
  rasters[2:4] <- lapply(
    rasters[2:4],
    crop,
    y = rasters[[1]],
    mask = TRUE
  )
  
  stack <- rast(rasters)
  
  # Conveyal skip corrections
  stack[[2]][stack[[4]] < stack[[2]]] <- 0
  stack[[3]][stack[[1]] < stack[[3]]] <- 0
  stack[[7]][stack[[6]] < stack[[7]]] <- 0
  
  stack
}


categories <- c(
  "senior",
  "bank",
  "bill",
  "chld",
  "col",
  "comc",
  "gym",
  "lib",
  "PO",
  "POW"
)

css_layers <- lapply(categories, load_css_category)

css <- Reduce(`+`, css_layers)

css <- css / 10

css[[7]][css[[7]] > css[[2]]] <- 0

summary(css)


