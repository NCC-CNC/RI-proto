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
RI_READY <- rast(list.files("data/_RIREADY",  pattern = ".tif$", full.names = TRUE)) 

# Weights excel
weights_tbl <- read_xlsx("data/WEIGHTS.xlsx")

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

# Rasters for Map
## RI
RI <<- rast("data/_RI/RI.tif")
## protection
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
eservice_pal <- colorNumeric(palette = "BrBG", domain = c(0, 1), na.color = "transparent")
eservice_lpal <- colorNumeric(palette = "BrBG", domain = c(0, 1), na.color = "transparent", reverse = TRUE)
hfi_pal <- colorNumeric(palette = "RdYlBu", domain = c(0, 1), na.color = "transparent", reverse = TRUE)
hfi_lpal <- colorNumeric(palette = "RdYlBu", domain = c(0, 1), na.color = "transparent")
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

ch_link <- "https://open.canada.ca/data/en/dataset/47caa405-be2b-4e9e-8f53-c478ade2ca74"
sar_link <- "https://open.canada.ca/data/en/dataset/d00f8e8c-40c4-435a-b790-980339ce3121"
climate_v_link <- "https://adaptwest.databasin.org/pages/climate-connectivity-north-america/"
climate_r_link <- "https://adaptwest.databasin.org/pages/climatic-macrorefugia-for-trees-and-songbirds/"
climate_e_link <- "https://link.springer.com/article/10.1007/s10584-021-03094-0"
pa_link <- "https://www.canada.ca/en/environment-climate-change/services/national-wildlife-areas/protected-conserved-areas-database.html" 
grass_link <- "https://open.canada.ca/data/en/dataset/fa84a70f-03ad-4946-b0f8-a3b481dd5248"
wet_link <- "https://open.canada.ca/data/en/dataset/80aa8ec6-4947-48de-bc9c-7d09d48b4cad"
forest_link <- "https://opendata.nfis.org/mapserver/nfis-change_eng.html"
freshwater_link <- "https://iopscience.iop.org/article/10.1088/1748-9326/abc121"
rec_link <- "https://iopscience.iop.org/article/10.1088/1748-9326/abc121"
carbon_s_link <- "https://iopscience.iop.org/article/10.1088/1748-9326/abc121"
carbon_p_link <- "https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990"
hfi_link <- "https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP2/EVKAVL"
kba_link <- "https://ibacanada.com/explore_how.jsp?lang=EN"
connect_link <- "https://osf.io/z2qs3/"
