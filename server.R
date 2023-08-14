
# Define server logic required to draw a histogram
function(input, output, session) {
  
  # init map
  output$RI_MAP <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addGlPoints(data = pts_wgs,
                  radius = 5,
                  group = "Points",
                  popup = paste0("<b>RI: </b>", pts_wgs$RI),
                  color = "#2b8cbe") %>%
      hideGroup("Points") %>%
      # RI
      addRasterImage(
        raster(RI), 
        colors = RI_pal,
        layerId = "RI",
        group = "Resilience Index") %>%
      addLegend(pal=RI_lpal, 
                values=values(raster(RI)), 
                position="bottomleft", 
                opacity=1,
                title="values",
                layerId="ri-legend",
                labFormat=labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      ## KBA
      addRasterImage(
        raster(RI_READY[[13]]),
        colors = c('#00000000','#ef3b2c'),
        group = "KBA") %>%
      hideGroup("KBA") %>%
      # Protected
      addRasterImage(
        raster(pa),
        colors = pa_pal,
        method = "ngb",
        group = "Protected") %>%
      hideGroup("Protected") %>%
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
    # add map spinner
    shinyjs::runjs(
     "const spinner = document.querySelector('.spinner');
     spinner.style.display = 'block'")
    
    # remove legend if off
    if (input$RI_MAP_tile == "Off") {
      leafletProxy("RI_MAP") %>%
        removeControl("ri-legend")
    } else {
      # add legend based on layer
      leafletProxy("RI_MAP") %>%
        removeControl("ri-legend") %>%
        addLegend(pal=base_group_cache[[input$RI_MAP_tile]][4][[1]], 
                  values=values(raster(RI_READY[[base_group_cache[[input$RI_MAP_tile]][2][[1]]]])), 
                  position="bottomleft", 
                  opacity=1,
                  title="values", 
                  layerId="ri-legend",
                  labFormat=labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    }
    # lazy load
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
    
    # remove spinner
    shinyjs::runjs(
      "const spinner = document.querySelector('.spinner');
      spinner.style.display = 'none'")  
  })
  
  # Reset RI to CP&P recommendation 
  observeEvent(input$ri_reset, {
    updateNumericInput(session, "kba", value = 15)
    updateNumericInput(session, "ch", value = 10)
    updateNumericInput(session, "range_end", value = 5)
    updateNumericInput(session, "range_spc", value = 1)
    updateNumericInput(session, "range_thr", value = 3)
    updateNumericInput(session, "carbon_p", value = 1)
    updateNumericInput(session, "carbon_s", value = 1)
    updateNumericInput(session, "climate_e", value = 10)
    updateNumericInput(session, "climate_r", value = 15)
    updateNumericInput(session, "climate_v", value = 15)
    updateNumericInput(session, "connect", value = 20)
    updateNumericInput(session, "freshwater", value = 1)
    updateNumericInput(session, "forest", value = 1)
    updateNumericInput(session, "grass", value = 1)
    updateNumericInput(session, "wet", value = 1)
    updateNumericInput(session, "pa", value = 10)
    updateNumericInput(session, "hfi", value = 10)
  })
  
  # Update RI: RI equation
  observeEvent(input$ri_update, {
    
    # add map spinner
    shinyjs::runjs(
      "const spinner = document.querySelector('.spinner');
     spinner.style.display = 'block'")    
    
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
                popup = paste0("<b>RI: </b>", pts_wgs$RI),
                color = "#2b8cbe") %>%
    showGroup("Points")
  
  # remove spinner
  shinyjs::runjs(
    "const spinner = document.querySelector('.spinner');
      spinner.style.display = 'none'")    
  
 })
  
  # equation text for download
  RI_equ <- reactive({ as.character(
    paste0(
      "(key biodiversity areas * ", input$kba, ")",
      " + (critical habitat * ",  input$ch, ")",
      " + (endangered specices * ", input$range_end, ")",
      " + (special concern species * ", input$range_spc, ")",
      " + (threatened species * ", input$range_thr, ")",
      " + (carbon potential * ", input$carbon_p, ")",
      " + (carbon storage * ", input$carbon_s, ")",
      " - (climate extremes * ", input$climate_e, ")",
      " + (climate refugia * ", input$climate_r, ")",
      " + (climate velocity * ", input$climate_v, ")",
      " + (connectivity * ", input$connect, ")",
      " + (freshwater provision * ", input$freshwater, ")",
      " + (forest landcover * ", input$forest, ")",
      " + (grassland * ", input$grass, ")",
      " + (wetland * ", input$wet, ")",
      " + (existing conservation * ", input$pa, ")",
      " - (human footprint index * ", input$hfi, ")"
    )) 
  })

  # RI equation for display
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
    
    # Download RI
    download_SERVER(id = "download_mod1")
    
    # Info modal
    toggleModal(session, modalId="info-modal", toggle="close")
    
}
