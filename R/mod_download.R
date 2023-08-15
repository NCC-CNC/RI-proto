# Download UI
download_UI <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("download"), "DOWNLOAD RI", width = "100%")
  )
}

# Download server
download_SERVER <- function(id, RI, weights_tbl) {
  moduleServer(id, function(input, output, session) {
    
    # Time stamp for output folder
    datetime <- format(Sys.time(),"%Y%m%d%H%M%S")
    
    # Create temporary directory to save data
    td <- tempfile()
    dir.create(td, recursive = FALSE, showWarnings = FALSE)
    
    # Save shapefile to tmp director
    writeRaster(RI, paste0(td, "/RI.tif"))
    
    # Save weights csv
    write.xlsx(as.data.frame(weights_tbl()), paste0(td, "/WEIGHTS.xlsx"), row.names = FALSE)
    
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