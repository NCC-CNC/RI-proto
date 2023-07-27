
fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),  

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          h2(class="ri-title", "RESLIENCE INDEX BUILDER"),
          # Biodiversity
          tags$div(class="theme biodiversity",
          h4("Biodiversity"),
          fluidRow(
            column(6,  numericInput("kba", label = "Key Biodiversity Areas", value = 15, min = 0, max = 50)),
            column(6,  numericInput("ch", label = "Critical Habitat", value = 10, min = 0, max = 50))
          ),
          fluidRow(
            column(4,  numericInput("range_end", label = "Endangered Species", value = 5, min = 0, max = 50)),
            column(4,  numericInput("range_spc", label = "Special Concern Species", value = 1, min = 0, max = 50)),
            column(4,  numericInput("range_thr", label = "Threatened Species", value = 3, min = 0, max = 50))
          )),             
          # Carbon
          tags$div(class="theme carbon",
          h4("Carbon"),
          fluidRow(
            column(6,  numericInput("carbon_p", label = "Potential", value = 1, min = 0, max = 50)),
            column(6,  numericInput("carbon_s", label = "Storage", value = 1, min = 0, max = 50))
          )),
          # Climate
          tags$div(class="theme climate",
          h4("Climate"),
          fluidRow(
            column(4,  numericInput("climate_e", label = "Extremes", value = 10, min = 0, max = 50)),
            column(4,  numericInput("climate_r", label = "Refugia", value = 15, min = 0, max = 50)), 
            column(4,  numericInput("climate_v", label = "Velocity", value = 15, min = 0, max = 50))
          )),
          # Connectivity / Freshwater
          tags$div(class="theme connectivity",
          fluidRow(
            column(6, h4("Connectivity")),
            column(6, h4("eServices")),
          ),
          fluidRow(
            column(6,  numericInput("connect", label = "Connectivity", value = 20, min = 0, max = 50)),
            column(6,  numericInput("freshwater", label = "Freshwater Provision", value = 1, min = 0, max = 50)), 
          )),
          # Habitat
          tags$div(class="theme habitat",
          h4("Habitat"),
          fluidRow(
            column(4,  numericInput("forest", label = "Forest Landcover", value = 1, min = 0, max = 50)),
            column(4,  numericInput("grass", label = "Grassland", value = 1, min = 0, max = 50)), 
            column(4,  numericInput("wet", label = "Wetland", value = 1, min = 0, max = 50)), 
          )),
          # Protection / Threat
          tags$div(class="theme threat",
           fluidRow(
             column(6, h4("Protection")),
             column(6, h4("Threats")),
           ),
           fluidRow(
             column(6,  numericInput("pa", label = "Existing Conservation", value = 0.1, min = 0, max = 50)),
             column(6,  numericInput("hfi", label = "Human Footprint Index", value = 10, min = 0, max = 50)), 
           )),
          
          # Update Map
          tags$div(class="ri-btn",
          fluidRow(
            column(12, actionButton("ri_update", "UPDATE RESILIENCE INDEX", width = "100%"))
          ))
          
        # CLOSE SIDEBAR PANNEL 
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          # Map
          leafletOutput(outputId = "RI_MAP", height = "calc(100vh - 200px)", width = "100%"),
          
          absolutePanel(
            top = 0,
            left = 40,
            width = 250,
            tags$div(class="ri-dwn",
                     fluidRow(actionButton("ri_update", "DOWNLOAD RESILIENCE INDEX", width = "100%")))),
          
        
          
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
