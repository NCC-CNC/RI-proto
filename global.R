library(shiny)
library(leaflet)
library(terra)
library(raster)
library(sf)
library(dplyr)
library(leafgl)
library(purrr)
library(exactextractr)

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
# ## Biodiversity
# kba <- rast("data/biodiversity/W_Key_biodiversity_areas.tif")
# ch <- rast("data/biodiversity/ECCC_CH_ALL_HA_SUM0.tif")
# end <- rast("data/biodiversity/ECCC_SAR_END_N.tif")
# spc <- rast("data/biodiversity/ECCC_SAR_SPC_N.tif")
# thr <- rast("data/biodiversity/ECCC_SAR_THR_N.tif")
# ## Carbon
# carbon_p <- rast("data/carbon/W_Carbon_potential.tif")
# carbon_s <- rast("data/carbon/W_Carbon_storage.tif")
# ## Climate
# climate_e <- rast("data/climate/W_Climate_extremes.tif")
# climate_r <- rast("data/climate/W_Climate_refugia.tif")
# climate_v <- rast("data/climate/W_Climate_shortest_path.tif")
# ## Connectivity
# connect <- rast("data/connectivity/W_Connectivity.tif")
# ## eServices
# freshwater <- rast("data/eservices/W_Freshwater.tif")
# ## Habitat
# forest <- rast("data/habitat/T_LC_Forest-lc.tif")
# grass <- rast("data/habitat/T_LC_Grassland.tif")
# wet <- rast("data/habitat/T_LC_Wetlands.tif")
## Protection
pa <- rast("data/protection/Existing_Conservation_ha.tif")
pa[pa == 0] <- NA 

# Palettes for map
RI_pal <- colorNumeric(palette = "viridis", domain = c(0, 1), na.color = "transparent")
RI_pal <- colorNumeric(palette = "viridis", domain = c(0, 1), na.color = "transparent")
carbon_pal <- colorNumeric(palette = "YlOrBr", domain = c(0, 1), na.color = "transparent")
climate_pal <- colorNumeric(palette = "magma", domain = c(0, 1), na.color = "transparent")
connectivity_pal <- colorNumeric(palette = "PRGn", domain = c(0, 1), na.color = "transparent")
freshwater_pal <- colorNumeric(palette = "BrBG", domain = c(0, 1), na.color = "transparent")
hfi_pal <- colorNumeric(palette = "RdYlBu", domain = c(0, 1), na.color = "transparent")
ch_pal <- colorNumeric(palette = "YlOrRd", domain = c(0, 1), na.color = "transparent")
sar_pal <- colorNumeric(palette = "Reds", domain = c(0, 1), na.color = "transparent")
forest_pal <- colorNumeric(palette = "YlGn", domain = c(0, 1), na.color = "transparent")
grass_pal <- colorNumeric(palette = "Oranges", domain = c(0, 1), na.color = "transparent")
wet_pal <- colorNumeric(palette = "BuPu", domain = c(0, 1), na.color = "transparent")
pa_pal <- colorNumeric(palette = "BuGn", domain = c(0.01, 1), na.color = "transparent")

# Normalize layers between 0-1
normalize_between_0_and_1 <- function(rast) {
  mm <- terra::minmax(rast)
  min_val <- mm[1]
  max_val <- mm[2]
  normalized_data <- (rast - min_val) / (max_val - min_val)
  normalized_data <- round(normalized_data, 2) # round to 2 decimal places
  return(normalized_data)
}
