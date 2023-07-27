
# Define server logic required to draw a histogram
function(input, output, session) {
  
  # init map
  output$RI_MAP <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addGlPoints(data = pts_wgs,
                  radius = 5,
                  group = "Points",
                  popup = ~RI,
                  color = "#2b8cbe") %>%
      hideGroup("Points") %>%
      # RI
      addRasterImage(
        raster(RI), 
        colors = RI_pal,
        layerId = "RI",
        group = "Resilience Index") %>%
      # # Biodiversity
      # ## KBA
      # addRasterImage(
      #   raster(RI_READY[[13]]), 
      #   colors = c('#00000000','#ef3b2c'),
      #   group = "KBA") %>% 
      # hideGroup("KBA") %>%
      # ## Critical habitat
      # addRasterImage(
      #   raster(RI_READY[[3]]), 
      #   colors = ch_pal,
      #   group = "Critical Habitat") %>% 
      # hideGroup("Critical Habitat") %>%
      # ## Range: END
      # addRasterImage(
      #   raster(RI_READY[[8]]), 
      #   colors = sar_pal,
      #   group = "Range Map: Endangered") %>% 
      # hideGroup("Range Map: Endangered") %>%
      # ## Range: SPC
      # addRasterImage(
      #   raster(RI_READY[[15]]), 
      #   colors = sar_pal,
      #   group = "Range Map: Special Concern") %>% 
      # hideGroup("Range Map: Special Concern") %>%
      # # Range: THR
      # addRasterImage(
      #   raster(RI_READY[[16]]), 
      #   colors = sar_pal,
      #   group = "Range Map: Threatened") %>% 
      # hideGroup("Range Map: Threatened") %>%
      # # Carbon potential
      # addRasterImage(
      #   raster(RI_READY[[1]]), 
      #   colors = carbon_pal,
      #   group = "Carbon Potential") %>% 
      # hideGroup("Carbon Potential") %>%
      # # Carbon storage
      # addRasterImage(
      #   raster(RI_READY[[2]]),
      #   colors = carbon_pal,
      #   group = "Carbon Storage") %>% 
      # hideGroup("Carbon Storage") %>% 
      # # Climate
      # ## Extremes
      # addRasterImage(
      #   raster(RI_READY[[4]]),
      #   colors = climate_pal,
      #   group = "Climate Extremes") %>% 
      # hideGroup("Climate Extremes") %>% 
      # ## Refugia
      # addRasterImage(
      #   raster(RI_READY[[5]]), 
      #   colors = climate_pal,
      #   group = "Climate Refugia") %>% 
      # hideGroup("Climate Refugia") %>%
      # ## Velocity
      # addRasterImage(
      #   raster(RI_READY[[6]]), 
      #   colors = climate_pal,
      #   group = "Climate Velocity") %>% 
      # hideGroup("Climate Velocity") %>%
      # # Connectivity
      # ## Connectivity
      # addRasterImage(
      #   raster(RI_READY[[7]]), 
      #   colors = connectivity_pal,
      #   group = "Connectivity") %>% 
      # hideGroup("Connectivity") %>%
      # # eServices
      # ## Freshwater provision
      # addRasterImage(
      #   raster(RI_READY[[10]]), 
      #   colors = freshwater_pal,
      #   group = "Freshwater Provision") %>% 
      # hideGroup("Freshwater Provision") %>%
      # # Threats
      # ## Human footprint index
      # addRasterImage(
      #   raster(RI_READY[[12]]),
      #   colors = hfi_pal,
      #   group = "Human Footprint Index") %>% 
      # hideGroup("Human Footprint Index") %>%
      # # Habitat
      # ## Forest landcover
      # addRasterImage(
      #   raster(RI_READY[[9]]), 
      #   colors = forest_pal,
      #   group = "Forest Landcover") %>% 
      # hideGroup("Forest Landcover") %>%
      # # Grassland
      # addRasterImage(
      #   raster(RI_READY[[11]]), 
      #   colors = grass_pal,
      #   group = "Grassland") %>% 
      # hideGroup("Grassland") %>%
      # # Wetland
      # addRasterImage(
      #   raster(RI_READY[[17]]), 
      #   colors = wet_pal,
      #   group = "Wetland") %>% 
      # hideGroup("Wetland") %>%  
      # # Protected
      # addRasterImage(
      #   raster(pa), 
      #   colors = pa_pal,
      #   method = "ngb",
      #   group = "Protected") %>% 
      # hideGroup("Protected") %>%  
      
      # Layer controls
      addLayersControl(
        overlayGroups = c("Protected", "KBA", "Points"),
        baseGroups = c("Resilience Index", 
                       "Critical Habitat", "Range Map: Endangered", "Range Map: Special Concern", "Range Map: Threatened",
                       "Carbon Potential", "Carbon Storage",
                       "Climate Extremes", "Climate Refugia", "Climate Velocity",
                       "Connectivity", "Freshwater Provision", 
                       "Forest Landcover", "Grassland", "Wetland",
                       "Human Footprint Index", "Off"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      htmlwidgets::onRender("
      function(el, x) {
        var myMap = this;
        myMap.on('baselayerchange',
          function (e) {
            Shiny.onInputChange('RI_MAP_tile', e.layer.groupname)
        })
    }")
      
  })
  
  # update map with themes
  observeEvent(input$RI_MAP_tile, {
    
    if (!base_group_cache[[input$RI_MAP_tile]][1][[1]]) {
      # update variable to TRUE
      base_group_cache[[input$RI_MAP_tile]][1][[1]] <<- TRUE
      # update map
      leafletProxy("RI_MAP") %>%
      addRasterImage(
           raster(RI_READY[[base_group_cache[[input$RI_MAP_tile]][2][[1]]]]),
           colors = base_group_cache[[input$RI_MAP_tile]][3][[1]],
           group = input$RI_MAP_tile) %>%
         showGroup(input$RI_MAP_tile)
    }
    
  })
  
  # RI equation
  observeEvent(input$ri_update, {
    
  RI <- (
     (RI_READY$W_Carbon_potential  * input$carbon_p) # carbon potential
    + (RI_READY$W_Carbon_storage * input$carbon_s)  # + carbon storage
    + (RI_READY$ECCC_CH_ALL_HA_SUM0 * input$ch)  # + critical habitat
    - (RI_READY$W_Climate_extremes * input$climate_e)  # - climate extremes
    + (RI_READY$W_Climate_refugia * input$climate_r)  # + climate refugia
    + (RI_READY$W_Climate_shortest_path * input$climate_v)  # + climate velocity
    + (RI_READY$W_Connectivity * input$connect)  # + connectivity
    + (RI_READY$ECCC_SAR_END_N * input$range_end) # + range map END
    + (RI_READY$`T_LC_Forest-lc` * input$forest) # + forest land cover
    + (RI_READY$W_Freshwater * input$freshwater)  # + freshwater provision
    + (RI_READY$T_LC_Grassland * input$grass) # + grassland
    - (RI_READY$W_Human_footprint * input$hfi)  # - human footprint index
    + (RI_READY$W_Key_biodiversity_areas * input$kba)            # + key biodiversity areas
    + (RI_READY$Existing_Conservation_ha * input$pa)             # + protected areas
    + (RI_READY$ECCC_SAR_SPC_N * input$range_spc) # + range map SPC
    + (RI_READY$ECCC_SAR_THR_N * input$range_thr) # + range map THR
    + (RI_READY$T_LC_Wetlands * input$wet) # + wetland
  )

  RI <-  normalize_between_0_and_1(RI)
  
  # Update map
  leafletProxy("RI_MAP") %>%
    removeImage("RI") %>%
    addRasterImage(
      raster(RI), 
      colors = RI_pal,
      group = "Resilience Index")
  
  # Extract points
  ri_df <- exactextractr::exact_extract(RI, pts_buf, "mean", force_df = TRUE) %>%
    rename("RI" = mean) %>%
    mutate(ID = row_number()) %>%
    mutate(RI = round(RI, 2))
  
  # Replace value
  pts_wgs['RI'] <- ri_df['RI']
  
  # Update points
  leafletProxy("RI_MAP") %>%
    removeGlPoints("RI_Points") %>%
    addGlPoints(data = pts_wgs,
                radius = 5,
                group = "Points",
                layerId = "RI_Points",
                popup = ~RI,
                color = "#2b8cbe") %>%
    showGroup("Points")
 })
  
  # equation text
    output$equation <-  renderText({
      HTML(paste0(
        "(<p class=var-bio>key biodiversity areas</p> * <span>", input$kba, "</span>) ",
        " + (<p class=var-bio>critical habitat</p> * <span>",  input$ch, "</span>)",
        " + (<p class=var-bio>endangered specices</p> * <span>", input$range_end, "</span>)",
        " + (<p class=var-bio>special concern species</p> * <span>", input$range_spc, "</span>)",
        " + (<p class=var-bio>threatened species</p> * <span>", input$range_thr, "</span>)",
        " + (<p class=var-carbon>carbon potential</p> * <span>", input$carbon_p, "</span>)",
        " + (<p class=var-carbon>carbon storage</p> * <span>", input$carbon_s, "</span>)",
        " <p class=minus>-</p> (<p class=var-climate>climate extremes</p> * <span>", input$climate_e, "</span>)",
        " + (<p class=var-climate>climate refugia</p> * <span>", input$climate_r, "</span>)",
        " + (<p class=var-climate>climate velocity</p> * <span>", input$climate_v, "</span>)",
        " + (<p class=var-connect>connectivity</p> * <span>", input$connect, "</span>)",
        " + (<p class=var-eservice>freshwater provision</p> * <span>", input$freshwater, "</span>)",
        " + (<p class=var-habitat>forest landcover</p> * <span>", input$forest, "</span>)",
        " + (<p class=var-habitat>grassland</p> * <span>", input$grass, "</span>)",
        " + (<p class=var-habitat>wetland </p> * <span>", input$wet, "</span>)",
        " + (<p class=var-protection>existing conservation </p> * <span>", input$pa, "</span>)",
        " <p class=minus>-</p> (<p class=var-threat>human footprint index </p> * <span>", input$hfi, "</span>)"
      ))
    })

}
