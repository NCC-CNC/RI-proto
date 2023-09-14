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
library(readxl)
library(xlsx)
library(shinycssloaders)

# Rasters for Equation
kba <- rast("data/_RIREADY/kba.tif")              # key biodiversity areas
ch <- rast("data/_RIREADY/ch.tif")                # critical habitat
sar_rich <- rast("data/_RIREADY/sar_rich.tif")    # SAR richness
end_rich <- rast("data/_RIREADY/end_rich.tif")    # END richness
biod_rich <- rast("data/_RIREADY/biod_rich.tif")  # BIOD richness
sar_goal <- rast("data/_RIREADY/sar_goal.tif")    # SAR goal
end_goal <- rast("data/_RIREADY/end_goal.tif")    # END goal
biod_goal <- rast("data/_RIREADY/biod_goal.tif")  # BIOD goal
climate_c <-  rast("data/_RIREADY/climate_c.tif") # climate centrality
climate_e <-  rast("data/_RIREADY/climate_e.tif") # climate extremes
climate_r <-  rast("data/_RIREADY/climate_r.tif") # climate refugia
connect <- rast("data/_RIREADY/connectivity.tif") # connectivity
forest <- rast("data/_RIREADY/forest.tif")        # forest
grass <- rast("data/_RIREADY/grass.tif")          # grassland
wet <- rast("data/_RIREADY/wet.tif")              # wetland
river <- rast("data/_RIREADY/river.tif")          # rivers
pa <- rast("data/_RIREADY/pa.tif")                # existing conservation
hfi <- rast("data/_RIREADY/hfi.tif")               # hfi

# Rasters for Map
kba_map <- rast("data/National/Biodiversity/W_Key_biodiversity_areas.tif") * 100  # key biodiversity areas
ch_map <- rast("data/National/Biodiversity/ECCC_CH_ALL_HA_SUM0.tif")        # critical habitat
sar_rich_map <- rast("data/National/Biodiversity/ECCC_SAR_SUM_N.tif")       # SAR richness
end_rich_map <- rast("data/National/Biodiversity/NSC_END_SUM_N.tif")        # END richness
biod_rich_map <- rast("data/National/Biodiversity/BIOD_N.tif")          # BIOD richness
sar_goal_map <- sar_goal        # SAR goal
end_goal_map <- end_goal        # END goal
biod_goal_map <- biod_goal      # BIOD goal
climate_c_map <-  rast("data/National/Climate/W_Climate_shortest_path.tif") # climate centrality
climate_e_map <-  rast("data/National/Climate/W_Climate_extremes.tif")      # climate extremes
climate_r_map <-  rast("data/National/Climate/W_Climate_refugia.tif")       # climate refugia
connect_map <- rast("data/National/Connectivity/W_Connectivity.tif")        # connectivity
forest_map <- rast("data/National/Habitat/T_LC_Forest-lc.tif")              # forest
grass_map <- rast("data/National/Habitat/T_LC_Grassland.tif")               # grassland
wet_map <- rast("data/National/Habitat/T_LC_Wetlands.tif")                  # wetland
river_map <- rast("data/National/Habitat/T_KM_River_length.tif")            # rivers
river_map[river_map > 10] <- 10 # FOR DISPLAY PURPOSE
hfi_map <- rast("data/National/Threats/W_Human_footprint.tif")               # hfi
## protection
pa_map <- pa
pa_map[pa_map== 0] <- NA # FOR DISPLAY PURPOSE

# Weights excel
weights_tbl <- read_xlsx("data/VALUES.xlsx")

# Read in points
pts <- read_sf("data/xy/points10km_RI.shp") %>%
  mutate(ID = row_number())

# Buffer for extractions
pts_buf <- st_buffer(pts, 1)

# Points to map
pts_wgs <- st_transform(pts, crs = 4326) %>%
  dplyr::mutate(
    lon = sf::st_coordinates(.)[,1],
    lat = sf::st_coordinates(.)[,2]
  ) 

# Palettes for map
## RI
RI_pal <- colorNumeric(palette = "viridis", domain = c(0, 1), na.color = "transparent")
RI_lpal <- colorNumeric(palette = "viridis", domain = c(0, 1), na.color = "transparent", reverse = TRUE)
## ch
ch_pal <- colorNumeric(
  palette = "YlOrRd", 
  domain = c(min(ch_map[], na.rm = TRUE), max(ch_map[], na.rm = TRUE)), 
  na.color = "transparent"
)
ch_lpal <- colorNumeric(
  palette = "YlOrRd", 
  domain = c(min(ch_map[], na.rm = TRUE), max(ch_map[], na.rm = TRUE)), 
  na.color = "transparent",
  reverse = TRUE
)
## SAR richness
sar_rich_pal <- colorNumeric(
  palette = "Reds", 
  domain = c(min(sar_rich_map[], na.rm = TRUE), max(sar_rich_map[], na.rm = TRUE)),  
  na.color = "transparent"
)
sar_rich_lpal <- colorNumeric(
  palette = "Reds", 
  domain = c(min(sar_rich_map[], na.rm = TRUE), max(sar_rich_map[], na.rm = TRUE)),
  na.color = "transparent", 
  reverse = TRUE
)
## END richness
end_rich_pal <- colorNumeric(
  palette = "Reds", 
  domain = c(min(end_rich_map[], na.rm = TRUE), max(end_rich_map[], na.rm = TRUE)),  
  na.color = "transparent"
)
end_rich_lpal <- colorNumeric(
  palette = "Reds", 
  domain = c(min(end_rich_map[], na.rm = TRUE), max(end_rich_map[], na.rm = TRUE)),
  na.color = "transparent", 
  reverse = TRUE
)
## Common richness
biod_rich_pal <- colorNumeric(
  palette = "Reds", 
  domain = c(min(biod_rich_map[], na.rm = TRUE), max(biod_rich_map[], na.rm = TRUE)),  
  na.color = "transparent"
)
biod_rich_lpal <- colorNumeric(
  palette = "Reds", 
  domain = c(min(biod_rich_map[], na.rm = TRUE), max(biod_rich_map[], na.rm = TRUE)),
  na.color = "transparent", 
  reverse = TRUE
)
## SAR goal
sar_goal_pal <- colorNumeric(
  palette = "RdPu", 
  domain = c(min(sar_goal_map[], na.rm = TRUE), max(sar_goal_map[], na.rm = TRUE)),
  na.color = "transparent"
)
sar_goal_lpal <- colorNumeric(
  palette = "RdPu", 
  domain = c(min(sar_goal_map[], na.rm = TRUE), max(sar_goal_map[], na.rm = TRUE)),
  na.color = "transparent", 
  reverse = TRUE
)


## END goal
end_goal_pal <- colorNumeric(
  palette = "RdPu", 
  domain = c(min(end_goal_map[], na.rm = TRUE), max(end_goal_map[], na.rm = TRUE)),
  na.color = "transparent"
)
end_goal_lpal <- colorNumeric(
  palette = "RdPu", 
  domain = c(min(end_goal_map[], na.rm = TRUE), max(end_goal_map[], na.rm = TRUE)),
  na.color = "transparent", 
  reverse = TRUE
)
## Common goal
biod_goal_pal <- colorNumeric(
  palette = "RdPu", 
  domain = c(min(biod_goal_map[], na.rm = TRUE), max(biod_goal_map[], na.rm = TRUE)),
  na.color = "transparent"
)
biod_goal_lpal <- colorNumeric(
  palette = "RdPu", 
  domain = c(min(biod_goal_map[], na.rm = TRUE), max(biod_goal_map[], na.rm = TRUE)),
  na.color = "transparent", 
  reverse = TRUE
)
## climate refugia
climate_r_pal <- colorNumeric(
  palette = "magma", 
  domain = c(min(climate_r_map[], na.rm = TRUE), max(climate_r_map[], na.rm = TRUE)),  
  na.color = "transparent"
)
climate_r_lpal <- colorNumeric(
  palette = "magma", 
  domain = c(min(climate_r_map[], na.rm = TRUE), max(climate_r_map[], na.rm = TRUE)),  
  na.color = "transparent",
  reverse = TRUE
)
## climate centrality
climate_c_pal <- colorNumeric(
  palette = "magma", 
  domain = c(min(climate_c_map[], na.rm = TRUE), max(climate_c_map[], na.rm = TRUE)),  
  na.color = "transparent"
)
climate_c_lpal <- colorNumeric(
  palette = "magma", 
  domain = c(min(climate_c_map[], na.rm = TRUE), max(climate_c_map[], na.rm = TRUE)),  
  na.color = "transparent",
  reverse = TRUE
)
## climate extremes
climate_e_pal <- colorNumeric(
  palette = "magma", 
  domain = c(min(climate_e_map[], na.rm = TRUE), max(climate_e_map[], na.rm = TRUE)),  
  na.color = "transparent"
)
climate_e_lpal <- colorNumeric(
  palette = "magma", 
  domain = c(min(climate_e_map[], na.rm = TRUE), max(climate_e_map[], na.rm = TRUE)),  
  na.color = "transparent",
  reverse = TRUE
)
## connectivity
connect_pal <- colorNumeric(
  palette = "PRGn", 
  domain = c(min(connect_map[], na.rm = TRUE), max(connect_map[], na.rm = TRUE)),
  na.color = "transparent", 
)
connect_lpal <- colorNumeric(
  palette = "PRGn", 
  domain = c(min(connect_map[], na.rm = TRUE), max(connect_map[], na.rm = TRUE)),
  na.color = "transparent", 
  reverse = TRUE
)
## Forest
forest_pal <- colorNumeric(
  palette = "YlGn", 
  domain = c(min(forest_map[], na.rm = TRUE), max(forest_map[], na.rm = TRUE)),
  na.color = "transparent"
)
forest_lpal <- colorNumeric(
  palette = "YlGn", 
  domain = c(min(forest_map[], na.rm = TRUE), max(forest_map[], na.rm = TRUE)),
  na.color = "transparent", 
  reverse = TRUE
)
## Grassland
grass_pal <- colorNumeric(
  palette = "Oranges", 
  domain = c(min(grass_map[], na.rm = TRUE), max(grass_map[], na.rm = TRUE)),
  na.color = "transparent"
)
grass_lpal <- colorNumeric(
  palette = "Oranges", 
  domain = c(min(grass_map[], na.rm = TRUE), max(grass_map[], na.rm = TRUE)),
  na.color = "transparent", 
  reverse = TRUE
)
## wetland
wet_pal <- colorNumeric(
  palette = "BuPu", 
  domain = c(min(wet_map[], na.rm = TRUE), max(wet_map[], na.rm = TRUE)), 
  na.color = "transparent"
)
wet_lpal <- colorNumeric(
  palette = "BuPu", 
  domain = c(min(wet_map[], na.rm = TRUE), max(wet_map[], na.rm = TRUE)),
  na.color = "transparent", 
  reverse = TRUE
)
## rivers
river_pal <- colorNumeric(
  palette = "Blues", 
  domain = c(0, 10),
  na.color = "transparent"
)
river_lpal <- colorNumeric(
  palette = "Blues", 
  domain = range(0, 10),
  na.color = "transparent", 
  reverse = TRUE
)
## HFI
hfi_pal <- colorNumeric(
  palette = "RdYlBu", 
  domain = c(min(hfi_map[], na.rm = TRUE), max(hfi_map[], na.rm = TRUE)), 
  na.color = "transparent", 
  reverse = TRUE
)
hfi_lpal <- colorNumeric(
  palette = "RdYlBu", 
  domain = c(min(hfi_map[], na.rm = TRUE), max(hfi_map[], na.rm = TRUE)), 
  na.color = "transparent"
)
## protected
pa_pal <- colorNumeric(
  palette = c("#78c679", "#41ab5d", "#238443", "#005a32"), 
  domain = c(min(pa_map[], na.rm = TRUE), max(pa_map[], na.rm = TRUE)), 
  na.color = "transparent"
)
pa_lpal <- colorNumeric(
  palette = c("#78c679", "#41ab5d", "#238443", "#005a32"), 
  domain = c(min(pa_map[], na.rm = TRUE), max(pa_map[], na.rm = TRUE)),
  na.color = "transparent", 
  reverse = TRUE
)

# Normalize layers between 0-1 function
normalize_between_0_and_1 <- function(rast) {
  mm <- terra::minmax(rast)
  min_val <- mm[1]
  max_val <- mm[2]
  normalized_data <- (rast - min_val) / (max_val - min_val)
  normalized_data <- round(normalized_data, 2) # round to 2 decimal places
  return(normalized_data)
}

# links
ch_link <<- "https://open.canada.ca/data/en/dataset/47caa405-be2b-4e9e-8f53-c478ade2ca74"
sar_link <<- "https://open.canada.ca/data/en/dataset/d00f8e8c-40c4-435a-b790-980339ce3121"
climate_v_link <<- "https://adaptwest.databasin.org/pages/climate-connectivity-north-america/"
climate_r_link <<- "https://adaptwest.databasin.org/pages/climatic-macrorefugia-for-trees-and-songbirds/"
climate_e_link <<- "https://link.springer.com/article/10.1007/s10584-021-03094-0"
pa_link <<- "https://www.canada.ca/en/environment-climate-change/services/national-wildlife-areas/protected-conserved-areas-database.html" 
grass_link <<- "https://open.canada.ca/data/en/dataset/fa84a70f-03ad-4946-b0f8-a3b481dd5248"
wet_link <<- "https://open.canada.ca/data/en/dataset/80aa8ec6-4947-48de-bc9c-7d09d48b4cad"
forest_link <<- "https://opendata.nfis.org/mapserver/nfis-change_eng.html"
riv_link <<- "https://open.canada.ca/data/en/dataset/9d96e8c9-22fe-4ad2-b5e8-94a6991b744b"
shore_link <<- "https://open.canada.ca/data/en/dataset/80aa8ec6-4947-48de-bc9c-7d09d48b4cad"
hfi_link <<- "https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP2/EVKAVL"
kba_link <<- "https://ibacanada.com/explore_how.jsp?lang=EN"
connect_link <<- "https://osf.io/z2qs3/"
