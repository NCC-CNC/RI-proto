library(shiny)
library(leaflet)
library(terra)
library(raster)
library(sf)
library(dplyr)
library(leafgl)
library(purrr)
library(exactextractr)
library(shinyBS)
library(shinyjs)

# Rasters for Equation
RI_READY <- rast(list.files("data/_RIREADY",  pattern = ".tif$", full.names = TRUE)) 

# Read in 10km points
pts <- read_sf("data/xy/points10km_RI.shp") %>%
  mutate(ID = row_number())

# Buffer for extractions
pts_buf <- st_buffer(pts, 10)

# Points to map
pts_wgs <- st_transform(pts, crs = 4326) %>%
  dplyr::mutate(
    lon = sf::st_coordinates(.)[,1],
    lat = sf::st_coordinates(.)[,2]
  ) %>% mutate(RI = round(RI, 4))

# Rasters for Map
## RI
RI <- rast("data/_RI/RI.tif")
## Protection
pa <- RI_READY[[14]] 
pa[pa == 0] <- NA 

# Palettes for map
RI_pal <- colorNumeric(palette = "viridis", domain = c(0, 1), na.color = "transparent")
RI_lpal <- colorNumeric(palette = "viridis", domain = c(0, 1), na.color = "transparent", reverse = TRUE)
carbon_pal <- colorNumeric(palette = "YlOrBr", domain = c(0, 1), na.color = "transparent")
carbon_lpal <- colorNumeric(palette = "YlOrBr", domain = c(0, 1), na.color = "transparent", reverse = TRUE)
climate_pal <- colorNumeric(palette = "magma", domain = c(0, 1), na.color = "transparent")
climate_lpal <- colorNumeric(palette = "magma", domain = c(0, 1), na.color = "transparent", reverse = TRUE)
connectivity_lpal <- colorNumeric(palette = "PRGn", domain = c(0, 1), na.color = "transparent", reverse = TRUE)
connectivity_pal <- colorNumeric(palette = "PRGn", domain = c(0, 1), na.color = "transparent")
freshwater_pal <- colorNumeric(palette = "BrBG", domain = c(0, 1), na.color = "transparent")
freshwater_lpal <- colorNumeric(palette = "BrBG", domain = c(0, 1), na.color = "transparent", reverse = TRUE)
hfi_pal <- colorNumeric(palette = "RdYlBu", domain = c(0, 1), na.color = "transparent")
hfi_lpal <- colorNumeric(palette = "RdYlBu", domain = c(0, 1), na.color = "transparent", reverse = TRUE)
ch_pal <- colorNumeric(palette = "YlOrRd", domain = c(0, 1), na.color = "transparent")
ch_lpal <- colorNumeric(palette = "YlOrRd", domain = c(0, 1), na.color = "transparent", reverse = TRUE)
sar_pal <- colorNumeric(palette = "Reds", domain = c(0, 1), na.color = "transparent")
sar_lpal <- colorNumeric(palette = "Reds", domain = c(0, 1), na.color = "transparent", reverse = TRUE)
forest_pal <- colorNumeric(palette = "YlGn", domain = c(0, 1), na.color = "transparent")
forest_lpal <- colorNumeric(palette = "YlGn", domain = c(0, 1), na.color = "transparent", reverse = TRUE)
grass_pal <- colorNumeric(palette = "Oranges", domain = c(0, 1), na.color = "transparent")
grass_lpal <- colorNumeric(palette = "Oranges", domain = c(0, 1), na.color = "transparent", reverse = TRUE)
wet_pal <- colorNumeric(palette = "BuPu", domain = c(0, 1), na.color = "transparent")
wet_lpal <- colorNumeric(palette = "BuPu", domain = c(0, 1), na.color = "transparent", reverse = TRUE)
pa_pal <- colorNumeric(palette = "BuGn", domain = c(0.0001, 1), na.color = "transparent")
pa_lpal <- colorNumeric(palette = "BuGn", domain = c(0.0001, 1), na.color = "transparent", reverse = TRUE)

# Normalize layers between 0-1
normalize_between_0_and_1 <- function(rast) {
  mm <- terra::minmax(rast)
  min_val <- mm[1]
  max_val <- mm[2]
  normalized_data <- (rast - min_val) / (max_val - min_val)
  normalized_data <- round(normalized_data, 2) # round to 2 decimal places
  return(normalized_data)
}

# Layer cache
base_group_cache <- list(
  (`Resilience Index` = c(TRUE, 1, RI_pal, RI_lpal)),
  (`Critical Habitat` = c(FALSE, 3, ch_pal, ch_lpal)), 
  (`Range Map: Endangered` = c(FALSE, 8, sar_pal, sar_lpal)),
  (`Range Map: Special Concern` = c(FALSE, 15, sar_pal, sar_lpal)), 
  (`Range Map: Threatened` = c(FALSE, 16, sar_pal, sar_lpal)),
  (`Carbon Potential` = c(FALSE, 1, carbon_pal, carbon_lpal)), 
  (`Carbon Storage` = c(FALSE, 2, carbon_pal, carbon_lpal)),
  (`Climate Extremes` = c(FALSE, 4, climate_pal, climate_lpal)),
  (`Climate Refugia` = c(FALSE, 5, climate_pal, climate_lpal)),
  (`Climate Velocity` = c(FALSE, 6, climate_pal, climate_lpal)),
  (`Connectivity` = c(FALSE, 7, connectivity_pal, connectivity_lpal)),
  (`Freshwater Provision` = c(FALSE, 10, freshwater_pal, freshwater_lpal)), 
  (`Forest Landcover` = c(FALSE, 9, forest_pal, forest_lpal)),
  (`Grassland` = c(FALSE, 11, grass_pal, grass_lpal)),
  (`Wetland` = c(FALSE, 17, wet_pal, wet_lpal)),
  (`Human Footprint Index` = c(FALSE, 12, hfi_pal, hfi_lpal)), 
  (`Off` = TRUE)
)

names(base_group_cache) <- c(
  "Resilience Index", 
  "Critical Habitat", "Range Map: Endangered", "Range Map: Special Concern", "Range Map: Threatened",
  "Carbon Potential", "Carbon Storage",
  "Climate Extremes", "Climate Refugia", "Climate Velocity",
  "Connectivity", "Freshwater Provision", 
  "Forest Landcover", "Grassland", "Wetland",
  "Human Footprint Index", "Off"
)
