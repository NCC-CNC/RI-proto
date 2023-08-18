# UI.R
fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),  

    # SIDE PANNEL
    sidebarLayout(
      # Sidebar header
        sidebarPanel(class="ri-builder",
          fluidRow(class="ri-header", 
            column(11, h2(class="ri-title", "RESILIENCE INDEX BUILDER")),
            column(1, actionButton(class="info-btn", inputId="info", label="", icon = icon('info')))
          ),
          
          # Weight builder:
          ## biodiversity
          tags$div(class="theme biodiversity",
          h4("Biodiversity"),
          fluidRow(
            column(6,  numericInput("kba", label = a("Key Biodiversity Areas", href=kba_link, target="_blank"), value = 15, min = 0, max = 50)),
            column(6,  numericInput("ch", label = a("Critical Habitat", href=ch_link, target="_blank"),  value = 9, min = 0, max = 50))
          ),
          fluidRow(
            column(4,  numericInput("range_end", label = a("Endangered", href=sar_link, target="_blank"), value = 8, min = 0, max = 50)),
            column(4,  numericInput("range_spc", label = a("Special Concern", href=sar_link, target="_blank"), value = 6, min = 0, max = 50)),
            column(4,  numericInput("range_thr", label = a("Threatened", href=sar_link, target="_blank"), value = 7, min = 0, max = 50))
          )),             
          ## carbon
          tags$div(class="theme carbon",
          h4("Carbon"),
          fluidRow(
            column(6,  numericInput("carbon_p", label = a("Potential", href=carbon_p_link, target="_blank"), value = 5, min = 0, max = 50)),
            column(6,  numericInput("carbon_s", label = a("Storage", href=carbon_s_link, target="_blank"), value = 5, min = 0, max = 50))
          )),
          ## climate
          tags$div(class="theme climate",
          h4("Climate"),
          fluidRow(
            column(4,  numericInput("climate_e", label = a("Extremes", href=climate_e_link, target="_blank"), value = 12, min = 0, max = 50)),
            column(4,  numericInput("climate_r", label = a("Refugia", href=climate_r_link, target="_blank"),  value = 6, min = 0, max = 50)), 
            column(4,  numericInput("climate_v", label = a("Centrality", href=climate_v_link, target="_blank"), value = 6, min = 0, max = 50))
          )),
          ## connectivity / eServices
          tags$div(class="theme connectivity",
          fluidRow(
            column(4, h4("Connectivity")),
            column(8, h4("eServices")),
          ),
          fluidRow(
            column(4,  numericInput("connect", label = a("Connectivity", href=connect_link, target="_blank"), value = 13, min = 0, max = 50)),
            column(4,  numericInput("freshwater", label = a("Freshwater Provision", href=freshwater_link, target="_blank"), value = 1, min = 0, max = 50)),
            column(4,  numericInput("rec", label = a("Recreation", href=rec_link, target="_blank"), value = 1, min = 0, max = 50)), 
          )),
          ## habitat
          tags$div(class="theme habitat",
          h4("Habitat"),
          fluidRow(
            column(4,  numericInput("forest", label = a("Forest Landcover", href=forest_link, target="_blank"), value = 2, min = 0, max = 50)),
            column(4,  numericInput("grass", label = a("Grassland", href=grass_link, target="_blank"), value = 2, min = 0, max = 50)), 
            column(4,  numericInput("wet", label = a("Wetland", href=wet_link, target="_blank"), value = 2, min = 0, max = 50)), 
          )),
          ## protection / threat
          tags$div(class="theme threat",
           fluidRow(
             column(6, h4("Protection")),
             column(6, h4("Threats")),
           ),
           fluidRow(
             column(6,  numericInput("pa", label = a("Existing Conservation", href=pa_link, target="_blank"), value = 12, min = 0, max = 50)),
             column(6,  numericInput("hfi", label = a("Human Footprint Index", href=hfi_link, target="_blank"), value = 38, min = 0, max = 50)), 
           )),
          
          # Weight tally
          tags$div(class="ri-weights",
           fluidRow(
             column(6, htmlOutput("pos_weights")),
             column(6, htmlOutput("neg_weights"))
           )),
          
          # Reset and update buttons
          tags$div(class="ri-btn",
          fluidRow(
            column(4, actionButton("ri_reset", "RESET RI", width = "100%")),
            column(8, actionButton("ri_update", "UPDATE RI", width = "100%"))
          ))
          
        # CLOSE SIDEBAR PANNEL 
        ),
        
        # MAIN PANEL
        mainPanel(
          # Info modal
          bsModal(id="info-modal", title="RESILIENCE INDEX GUIDE", trigger="info", size = "large",
            tags$iframe(style="height:600px; width:100%", src="RI_GUIDE.pdf")),
          
          # Map
          tags$div(class="map-container",
          tags$div(class="spinner"),
          withSpinner(proxy.height = "calc(100vh - 200px)", color = "#33862B",
          leafletOutput(outputId = "RI_MAP", height = "calc(100vh - 200px)", width = "100%"))),
          
          # Download RI button
          absolutePanel(
            top = 0,
            left = 40,
            width = 250,
            tags$div(class="ri-dwn",
            fluidRow(download_UI(id = "download_mod1")))),
          
          # Equation
          tags$div(class="equation",
          h4("Resilience Index Equation:"),
          fluidRow(htmlOutput("equation")))
          
        # CLOSE MAIN PANEL  
        )
    # CLOSE SIDE BAR LAYOUT 
    )
# CLOSE FLUID PAGE
)
