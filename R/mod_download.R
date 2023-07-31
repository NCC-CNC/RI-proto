# Download UI
download_UI <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("download"), "DOWNLOAD RESILIENCE INDEX", width = "100%")
  )
}

# Download server
download_SERVER <- function(id, user_pmp_mean) {
  moduleServer(id, function(input, output, session) {
    
    
    # Time stamp for output folder
    datetime <- format(Sys.time(),"%Y%m%d%H%M%S")
    
    # Create temporary directory to save data
    td <- tempfile()
    dir.create(td, recursive = FALSE, showWarnings = FALSE)
    
    # Save shapefile to tmp director
    writeRaster(RI, paste0(td, "/RI.tif"))
    
    # Zip
    files2zip <- list.files(td, full.names = TRUE, recursive = FALSE)
    utils::zip(zipfile = file.path(td, paste0("RI_BUILDER_", datetime, ".zip")),
               files = files2zip,
               flags = '-r9Xj') # flag so it does not take parent folders
    
    # set download button behavior
    output$download <- shiny::downloadHandler(
      filename <- function() {
        paste0("RI_BUILDER_", datetime, ".zip", sep="")
      },
      content <- function(file) {
        file.copy(file.path(td, paste0("RI_BUILDER_", datetime, ".zip", sep="")), file)
      },
      contentType = "application/zip"
    )
  })
}