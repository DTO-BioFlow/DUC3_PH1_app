library(shiny)
library(leaflet)
library(sf)
library(shinycssloaders)
library(fs)

pletUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        width = 6,
        h4("OSPAR Common Procedure Assessment Units"),
        leafletOutput(ns("map"), height = "500px") %>% withSpinner(),
        br(),
        verbatimTextOutput(ns("selected_info"))
      )
    )
  )
}

pletServer <- function(id, reset_trigger = NULL, wfs_url = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(features = NULL, selected_ID = NULL)
    
    # ------------------------
    # Load WFS with disk cache
    # ------------------------
    cache_file <- "cache/ospar_features.rds"
    if (!dir.exists("cache")) dir.create("cache")
    
    observe({
      req(wfs_url)
      if (file.exists(cache_file)) {
        rv$features <- readRDS(cache_file)
      } else {
        rv$features <- tryCatch({
          sf::st_read(wfs_url, quiet = TRUE)
        }, error = function(e) {
          message("Failed to load WFS:", e)
          NULL
        })
        if (!is.null(rv$features)) saveRDS(rv$features, cache_file)
      }
    })
    
    # ------------------------
    # Render map
    # ------------------------
    output$map <- renderLeaflet({
      req(rv$features)
      leaflet(rv$features) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          layerId = ~as.character(ID),
          color = "blue", weight = 2, fillOpacity = 0.25,
          highlightOptions = highlightOptions(color = "orange", weight = 3, bringToFront = TRUE),
          label = ~paste0("ID: ", ID)
        )
    })
    
    # ------------------------
    # Polygon click
    # ------------------------
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      req(click$id)
      rv$selected_ID <- click$id
      
      leafletProxy("map", session) %>%
        clearShapes() %>%
        addPolygons(
          data = rv$features,
          layerId = ~as.character(ID),
          color = ~ifelse(ID == rv$selected_ID, "red", "blue"),
          weight = 2,
          fillOpacity = ~ifelse(ID == rv$selected_ID, 0.5, 0.25),
          highlightOptions = highlightOptions(color = "orange", weight = 3, bringToFront = TRUE),
          label = ~paste0("ID: ", ID)
        )
    })
    
    # ------------------------
    # Show selected polygon info
    # ------------------------
    output$selected_info <- renderPrint({
      req(rv$selected_ID)
      poly <- rv$features[rv$features$ID == rv$selected_ID, ]
      if (!is.null(poly)) {
        cat("Selected Region ID:", poly$ID, "\n")
        cat("Columns:", paste(names(poly), collapse=", "))
      }
    })
    
    # ------------------------
    # Reset on tab switch
    # ------------------------
    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        rv$selected_ID <- NULL
        if (!is.null(rv$features)) {
          leafletProxy("map", session) %>%
            clearShapes() %>%
            addPolygons(
              data = rv$features,
              layerId = ~as.character(ID),
              color = "blue", weight = 2, fillOpacity = 0.25,
              highlightOptions = highlightOptions(color = "orange", weight = 3, bringToFront = TRUE),
              label = ~paste0("ID: ", ID)
            )
        }
      })
    }
  })
}
