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
            column(11, h2(class="ri-title", tags$img(class="logo", src = "ncc_logo.png", width = "8%"), HTML("LANDSCAPE RESILIENCE <span>BUILDER<span/>"))),
            column(1, actionButton(class="info-btn", inputId="info", label="", icon = icon('info')))
          ),
          
          # Weight builder:
          tags$div(class="weight-builder",
          ## biodiversity
          tags$div(class="theme biodiversity",
          h4("Biodiversity"),
          fluidRow(
            column(6,  numericInput("kba", label = a("Key Biodiversity Areas", href=kba_link, target="_blank"), value = 1, min = 0, max = 50)),
            column(6,  numericInput("ch", label = a("Species at Risk, Critical Habitat", href=ch_link, target="_blank"),  value = 1, min = 0, max = 50)),
          ),
          fluidRow(
            column(4,  numericInput("sar_rich", label = a("Species at Risk, Richness", href=sar_link, target="_blank"), value = 1, min = 0, max = 50)),
            column(4,  numericInput("end_rich", label = "Endemic Species, Richness", value = 1, min = 0, max = 50)),
            column(4,  numericInput("biod_rich", label = "Common Species, Richness", value = 1, min = 0, max = 50))
          ),
          fluidRow(
            column(4,  numericInput("sar_goal", label = a("Species at Risk, Goal", href=sar_link, target="_blank"), value = 1, min = 0, max = 50)),
            column(4,  numericInput("end_goal", "Endemic Species, Goal", value = 1, min = 0, max = 50)),
            column(4,  numericInput("biod_goal", "Common Species, Goal", value = 1, min = 0, max = 50))
          )),

          ## connectivity / climate
          tags$div(class="theme",
          fluidRow(
           column(4, h4("Connectivity")),
           column(8, h4("Climate")),
           ),          
          
          tags$div(class="climate",
          fluidRow(
            column(4,  numericInput("connect", label = a("Connectivity", href=connect_link, target="_blank"), value = 1, min = 0, max = 50)),
            column(4,  numericInput("climate_r", label = a("Refugia", href=climate_r_link, target="_blank"),  value = 1, min = 0, max = 50)), 
            column(4,  numericInput("climate_c", label = a("Centrality", href=climate_v_link, target="_blank"), value = 1, min = 0, max = 50))
          ))),

          ## habitat
          tags$div(class="theme habitat",
          h4("Habitat"),
          fluidRow(
            column(6,  numericInput("forest", label = a("Forest", href=forest_link, target="_blank"), value = 1, min = 0, max = 50)),
            column(6,  numericInput("grass", label = a("Grassland", href=grass_link, target="_blank"), value = 1, min = 0, max = 50))),
          fluidRow(
            column(4,  numericInput("wet", label = a("Wetland", href=wet_link, target="_blank"), value = 1, min = 0, max = 50)),
            column(4,  numericInput("river", label = a("Rivers", href=riv_link, target="_blank"), value = 1, min = 0, max = 50)),
            column(4,  numericInput("shore", label = a("Shoreline", href=shore_link, target="_blank"), value = 1, min = 0, max = 50))),
          ),
          
          ## protection / threat
          tags$div(class="theme threat",
           fluidRow(
             column(4, h4("Protection")),
             column(8, h4("Threats")),
           ),
           fluidRow(
             column(4,  numericInput("pa", label = a("Existing Conservation", href=pa_link, target="_blank"), value = 1, min = 0, max = 50)),
             column(4,  numericInput("hfi", label = a("Human Footprint Index", href=hfi_link, target="_blank"), value = 1, min = 0, max = 50)),
             column(4,  numericInput("climate_e", label = a("Climate Extremes", href=climate_e_link, target="_blank"), value = 1, min = 0, max = 50))
           )),
          
          # Weight tally
          # tags$div(class="ri-weights",
          #  fluidRow(
          #    column(6, htmlOutput("pos_weights")),
          #    column(6, htmlOutput("neg_weights"))
          #  ))
          # CLOSE WEIGHT BUILDER
          ),
          
          # Reset and update buttons
          tags$div(class="ri-btn",
          fluidRow(
            column(4, actionButton("ri_reset", "RESET VALUES", width = "100%")),
            column(8, actionButton("ri_update", "UPDATE SCORES", width = "100%"))
          ))
          
        # CLOSE SIDEBAR PANNEL 
        ),
        
        # MAIN PANEL
        mainPanel(
          # Info modal
          bsModal(id="info-modal", title="LANDSCAPE RESILIENCE", trigger="info", size = "large",
            tags$iframe(style="height:600px; width:100%", src="LANDSCAPE_RESILIENCE.pdf")),
          
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
          h4("Landscape Resilience Score:"),
          fluidRow(htmlOutput("equation")))
          
        # CLOSE MAIN PANEL  
        )
    # CLOSE SIDE BAR LAYOUT 
    )
# CLOSE FLUID PAGE
)
