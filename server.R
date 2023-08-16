
# Define server logic required to draw a histogram
function(input, output, session) {
  
  # make weights df reactive
  weights_tbl <- reactiveVal(weights_tbl)
  
  # init map
  output$RI_MAP <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addGlPoints(data = pts_wgs,
                  radius = 5,
                  group = "Points",
                  popup = 
                    paste0("<b>RI: </b>", pts_wgs$RI 
                           # "<hr>",
                           # "<b>Critical Habitat: </b>", pts_wgs$ch, "<br>",
                           # "<b>Endangered: </b>", pts_wgs$end, "<br>",
                           # "<b>Special Concern: </b>", pts_wgs$spc, "<br>",
                           # "<b>Threatened: </b>", pts_wgs$thr, "<br>", 
                           # "<b>Endangered: </b>", pts_wgs$end, "<br>",
                           # "<b>Carbon Potential: </b>", pts_wgs$carbon_p, "<br>",
                           # "<b>Carbon Storage: </b>", pts_wgs$carbon_s, "<br>",
                           # "<b>Climate Extremes: </b>", pts_wgs$climate_e, "<br>",
                           # "<b>Climate Refugia: </b>", pts_wgs$climate_r, "<br>",
                           # "<b>Climate Velocity: </b>", pts_wgs$climate_v, "<br>",
                           # "<b>Connectivity: </b>", pts_wgs$connectivi, "<br>",
                           # "<b>Freshwater: </b>", pts_wgs$freshwater, "<br>",
                           # "<b>Recreation: </b>", pts_wgs$rec, "<br>",
                           # "<b>Forest Landcover: </b>", pts_wgs$forset, "<br>",
                           # "<b>Grassland: </b>", pts_wgs$grass, "<br>",
                           # "<b>Wetland: </b>", pts_wgs$wet, "<br>",
                           # "<b>Human Footprint: </b>", pts_wgs$hfi
                           ),
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
                       "Connectivity", 
                       "Freshwater Provision", "Recreation", 
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
    updateNumericInput(session, "ch", value = 9)
    updateNumericInput(session, "range_end", value = 8)
    updateNumericInput(session, "range_spc", value = 6)
    updateNumericInput(session, "range_thr", value = 7)
    updateNumericInput(session, "carbon_p", value = 5)
    updateNumericInput(session, "carbon_s", value = 5)
    updateNumericInput(session, "climate_e", value = 12)
    updateNumericInput(session, "climate_r", value = 6)
    updateNumericInput(session, "climate_v", value = 6)
    updateNumericInput(session, "connect", value = 13)
    updateNumericInput(session, "freshwater", value = 1)
    updateNumericInput(session, "rec", value = 1)
    updateNumericInput(session, "forest", value = 2)
    updateNumericInput(session, "grass", value = 2)
    updateNumericInput(session, "wet", value = 2)
    updateNumericInput(session, "pa", value = 12)
    updateNumericInput(session, "hfi", value = 38)
  })
  
  # Update RI: RI equation
  observeEvent(input$ri_update, {
    
    # add map spinner
    shinyjs::runjs(
      "const spinner = document.querySelector('.spinner');
     spinner.style.display = 'block'")
    
  # update weights table
  weight_values <- c(input$kba, input$ch, 
               input$range_end, input$range_spc, input$range_thr,
               input$carbon_p, input$carbon_s, 
               input$climate_e, input$climate_r, input$climate_v,
               input$connect, input$freshwater, input$rec,
               input$forest, input$grass, input$wet,
               input$pa, input$hfi)
   weights_tbl(weights_tbl() %>% mutate(WEIGHTS = weight_values))
    
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
    + (RI_READY$W_Recreation * input$rec)  # + recreation
    + (RI_READY$T_LC_Grassland * input$grass) # + grassland
    - (RI_READY$W_Human_footprint * input$hfi)  # - human footprint index
    + (RI_READY$W_Key_biodiversity_areas * input$kba)            # + key biodiversity areas
    + (RI_READY$Existing_Conservation_ha * input$pa)             # + protected areas
    + (RI_READY$ECCC_SAR_SPC_N * input$range_spc) # + range map SPC
    + (RI_READY$ECCC_SAR_THR_N * input$range_thr) # + range map THR
    + (RI_READY$T_LC_Wetlands * input$wet) # + wetland
  )

  RI <- normalize_between_0_and_1(RI)
  
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
    mutate(RI = round(RI, 4))
  
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
      " + (recreation * ", input$rec, ")",
      " + (forest landcover * ", input$forest, ")",
      " + (grassland * ", input$grass, ")",
      " + (wetland * ", input$wet, ")",
      " + (existing conservation * ", input$pa, ")",
      " - (human footprint index * ", input$hfi, ")"
    )) 
  })

  # RI weight tally
  output$pos_weights <-  renderText({
    
    positive_weight_tally <- (input$kba + input$ch + input$range_end + input$range_spc +
      input$range_thr + input$carbon_p + input$carbon_s +
      input$climate_r + input$climate_v + input$connect + input$freshwater +
      input$rec + input$forest + input$grass + input$wet + input$pa) 
  
    HTML(paste0("<b> Positive Weight tally:</b> ", positive_weight_tally))
  })
  
  output$neg_weights <-  renderText({ 
    negative_weight_tally <- -input$hfi + -input$climate_e
    HTML(paste0("<b>Negative Weight tally:</b> ", negative_weight_tally))
  })    
  
  # RI equation for display
  output$equation <-  renderText({
    positive_ri_inputs <- list(
      list("feature" = "key biodiversity areas", "weight" = input$kba, "class" = "var-bio"),
      list("feature" = "critical habitat", "weight" = input$ch, "class" = "var-bio"),
      list("feature" = "endangered specices", "weight" = input$range_end, "class" = "var-bio"),
      list("feature" = "special concern species", "weight" = input$range_spc, "class" = "var-bio"),
      list("feature" = "threatened species", "weight" = input$range_thr, "class" = "var-bio"),
      list("feature" = "carbon potential", "weight" = input$carbon_p, "class" = "var-carbon"),
      list("feature" = "carbon storage", "weight" = input$carbon_s, "class" = "var-carbon"),
      list("feature" = "climate refugia", "weight" = input$climate_r, "class" = "var-climate"),
      list("feature" = "climate velocity", "weight" = input$climate_v, "class" = "var-climate"),
      list("feature" = "connectivity", "weight" = input$connect, "class" = "var-connect"),
      list("feature" = "freshwater provision", "weight" = input$freshwater, "class" = "var-eservice"),
      list("feature" = "recreation", "weight" = input$rec, "class" = "var-eservice"),
      list("feature" = "forest landcover", "weight" = input$forest, "class" = "var-habitat"),
      list("feature" = "grassland", "weight" = input$grass, "class" = "var-habitat"),
      list("feature" = "wetland", "weight" = input$wet, "class" = "var-habitat"),
      list("feature" = "existing conservation", "weight" = input$pa, "class" = "var-protection")
      )
    
    negative_ri_inputs <- list(
      list("feature" = "climate extremes", "weight" = input$climate_e, "class" = "var-climate"),
      list("feature" = "human footprint", "weight" = input$hfi, "class" = "var-threat")
    )  
    
    sorted_p <- positive_ri_inputs[order(-sapply(positive_ri_inputs, function(x) x$weight))]
    sorted_n <- negative_ri_inputs[order(-sapply(negative_ri_inputs, function(x) x$weight))]
    
    HTML(paste0(
      "(<p class=", sorted_p[[1]]$class, ">", sorted_p[[1]]$feature, "</p> * <span>", sorted_p[[1]]$weight, "</span>)",
      " + (<p class=", sorted_p[[2]]$class, ">", sorted_p[[2]]$feature, "</p> * <span>", sorted_p[[2]]$weight, "</span>)",
      " + (<p class=", sorted_p[[3]]$class, ">", sorted_p[[3]]$feature, "</p> * <span>", sorted_p[[3]]$weight, "</span>)",
      " + (<p class=", sorted_p[[4]]$class, ">", sorted_p[[4]]$feature, "</p> * <span>", sorted_p[[4]]$weight, "</span>)",
      " + (<p class=", sorted_p[[5]]$class, ">", sorted_p[[5]]$feature, "</p> * <span>", sorted_p[[5]]$weight, "</span>)",
      " + (<p class=", sorted_p[[6]]$class, ">", sorted_p[[6]]$feature, "</p> * <span>", sorted_p[[6]]$weight, "</span>)",
      " + (<p class=", sorted_p[[7]]$class, ">", sorted_p[[7]]$feature, "</p> * <span>", sorted_p[[7]]$weight, "</span>)",
      " + (<p class=", sorted_p[[8]]$class, ">", sorted_p[[8]]$feature, "</p> * <span>", sorted_p[[8]]$weight, "</span>)",
      " + (<p class=", sorted_p[[9]]$class, ">", sorted_p[[9]]$feature, "</p> * <span>", sorted_p[[9]]$weight, "</span>)",
      " + (<p class=", sorted_p[[10]]$class, ">", sorted_p[[10]]$feature, "</p> * <span>", sorted_p[[10]]$weight, "</span>)",
      " + (<p class=", sorted_p[[11]]$class, ">", sorted_p[[11]]$feature, "</p> * <span>", sorted_p[[11]]$weight, "</span>)",
      " + (<p class=", sorted_p[[12]]$class, ">", sorted_p[[12]]$feature, "</p> * <span>", sorted_p[[12]]$weight, "</span>)",
      " + (<p class=", sorted_p[[13]]$class, ">", sorted_p[[13]]$feature, "</p> * <span>", sorted_p[[13]]$weight, "</span>)",
      " + (<p class=", sorted_p[[14]]$class, ">", sorted_p[[14]]$feature, "</p> * <span>", sorted_p[[14]]$weight, "</span>)",
      " + (<p class=", sorted_p[[15]]$class, ">", sorted_p[[15]]$feature, "</p> * <span>", sorted_p[[15]]$weight, "</span>)",
      " + (<p class=", sorted_p[[16]]$class, ">", sorted_p[[16]]$feature, "</p> * <span>", sorted_p[[16]]$weight, "</span>)",
      " - (<p class=", sorted_n[[1]]$class, ">", sorted_n[[1]]$feature, "</p> * <span>", sorted_n[[1]]$weight, "</span>)",
      " - (<p class=", sorted_n[[2]]$class, ">", sorted_n[[2]]$feature, "</p> * <span>", sorted_n[[2]]$weight, "</span>)"
    ))
  })
  
    # Download RI
  observe(
    download_SERVER(id = "download_mod1", RI = RI, weights_tbl = weights_tbl)
  )
    
  # Info modal
  toggleModal(session, modalId="info-modal", toggle="close")
  
  # Tool tips
  addTooltip(session, id = "ri_reset", title = "Reset weights back to CP&P Recommendation",
             placement = "top", trigger = "hover")
  
  addTooltip(session, id = "ri_update", title = "Update RI using new weights",
             placement = "top", trigger = "hover")
  
  addTooltip(session, id = "info", title = "View PowerPoint",
             placement = "bottom", trigger = "hover")  
  
}
