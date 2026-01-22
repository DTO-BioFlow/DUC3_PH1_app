library(shiny)
library(leaflet)
library(sf)
library(shinycssloaders)
library(fs)
library(arrow)
library(dplyr)
library(tidyr)

log_msg <- function(msg) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

pletUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        width = 6,
        h4("OSPAR Common Procedure Assessment Units"),
        leafletOutput(ns("map"), height = "500px") %>% withSpinner(),
        br(),
        verbatimTextOutput(ns("selected_info")),
        br(),
        uiOutput(ns("dataset_selector")),
        br(),
        uiOutput(ns("lifeform1_selector")),
        textOutput(ns("lifeform1_value")),
        uiOutput(ns("lifeform2_selector")),
        textOutput(ns("lifeform2_value")),
        br(),
        tableOutput(ns("lifeform_table")) %>% withSpinner()
      )
    )
  )
}

pletServer <- function(id, reset_trigger = NULL, wfs_url = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      features = NULL,
      selected_ID = NULL,
      assessment = NULL,
      dataset_choices = NULL,
      lifeforms = c("uto_and_mix_dinos","carniv_zoo","ciliates","copepods",
                    "crustacean","diatom","dinoflagellate","fish_larvae",
                    "gelatinous","holoplankton","lg_copepods","lg_phyto",
                    "meroplankton","non_carniv_zoo","pelagic_diatoms",
                    "phytoplankton","potentially_toxic_nuisance_diatoms",
                    "potentially_toxic_nuisance_dinos","sm_copepods",
                    "sm_phyto","tychopelagic_diatoms")
    )
    
    # ------------------------
    # Load WFS
    # ------------------------
    cache_file <- "cache/ospar_features.rds"
    if (!dir.exists("cache")) dir.create("cache")
    observe({
      req(wfs_url)
      if (file.exists(cache_file)) {
        rv$features <- readRDS(cache_file)
      } else {
        rv$features <- tryCatch({ sf::st_read(wfs_url, quiet = TRUE) }, error = function(e) { log_msg(paste("Failed WFS:", e)); NULL })
        if (!is.null(rv$features)) saveRDS(rv$features, cache_file)
      }
    })
    
    # ------------------------
    # Load Parquet
    # ------------------------
    observe({
      fs <- S3FileSystem$create(anonymous = TRUE, scheme = "https", endpoint_override = "minio.dive.edito.eu")
      parquet_path <- "oidc-willemboone/PLET/assessment_data.parquet"
      rv$assessment <- read_parquet(fs$path(parquet_path))
      log_msg("Assessment data loaded")
    })
    
    # ------------------------
    # Render map
    # ------------------------
    output$map <- renderLeaflet({
      req(rv$features)
      leaflet(rv$features) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(layerId = ~as.character(ID), color = "blue", weight = 2, fillOpacity = 0.25,
                    highlightOptions = highlightOptions(color="orange", weight=3, bringToFront=TRUE),
                    label = ~paste0("ID: ", ID))
    })
    
    # ------------------------
    # Polygon click
    # ------------------------
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      req(click$id)
      rv$selected_ID <- click$id
      log_msg(paste("Polygon clicked:", click$id))
      
      # Update map colors
      leafletProxy("map", session) %>%
        clearShapes() %>%
        addPolygons(data=rv$features, layerId=~as.character(ID),
                    color = ~ifelse(ID==rv$selected_ID,"red","blue"),
                    weight=2, fillOpacity = ~ifelse(ID==rv$selected_ID,0.5,0.25),
                    highlightOptions=highlightOptions(color="orange", weight=3, bringToFront=TRUE),
                    label=~paste0("ID: ",ID))
      
      # Update dataset choices
      req(rv$assessment)
      filtered_region <- rv$assessment %>% filter(region_id == rv$selected_ID)
      rv$dataset_choices <- c("All", unique(filtered_region$dataset_name))
      log_msg(paste("Dataset choices updated:", paste(rv$dataset_choices, collapse=", ")))
    })
    
    # ------------------------
    # Dataset selector
    # ------------------------
    output$dataset_selector <- renderUI({
      req(rv$dataset_choices)
      selectInput(ns("dataset_name_filter"), "Select dataset_name:", choices = rv$dataset_choices, selected = "All")
    })
    
    # ------------------------
    # Lifeform selectors
    # ------------------------
    output$lifeform1_selector <- renderUI({
      req(rv$selected_ID)
      current <- ifelse(!is.null(input$lifeform1_filter), input$lifeform1_filter, "None")
      choices <- c("None", setdiff(rv$lifeforms, input$lifeform2_filter))
      selectInput(ns("lifeform1_filter"), "Select Lifeform 1:", choices = choices, selected = current)
    })
    
    output$lifeform2_selector <- renderUI({
      req(rv$selected_ID)
      current <- ifelse(!is.null(input$lifeform2_filter), input$lifeform2_filter, "None")
      choices <- c("None", setdiff(rv$lifeforms, input$lifeform1_filter))
      selectInput(ns("lifeform2_filter"), "Select Lifeform 2:", choices = choices, selected = current)
    })
    
    # ------------------------
    # Show polygon info
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
    # Lifeform debug prints
    # ------------------------
    output$lifeform1_value <- renderText({ paste("Lifeform 1 selected:", input$lifeform1_filter) })
    output$lifeform2_value <- renderText({ paste("Lifeform 2 selected:", input$lifeform2_filter) })
    
    # ------------------------
    # Convert and collapse duplicates
    # ------------------------
    convert_lifeform_df <- function(df, lf1, lf2) {
      df <- df %>%
        mutate(period = as.character(period)) %>%
        filter(grepl("^\\d{4}-\\d{2}$", period)) %>%
        select(period, numSamples, all_of(c(lf1, lf2))) %>%
        pivot_longer(cols = all_of(c(lf1, lf2)), names_to="lifeform", values_to="abundance") %>%
        rename(num_samples = numSamples) %>%
        filter(!is.na(abundance) & num_samples > 0)
      
      # Weighted average if duplicate period + lifeform
      df %>%
        group_by(period, lifeform) %>%
        summarise(
          abundance = sum(abundance * num_samples) / sum(num_samples),
          num_samples = sum(num_samples),
          .groups = "drop"
        ) %>%
        arrange(period, lifeform)
    }
    
    # ------------------------
    # Lifeform table
    # ------------------------
    output$lifeform_table <- renderTable({
      req(rv$assessment, rv$selected_ID, input$dataset_name_filter)
      lf1 <- input$lifeform1_filter
      lf2 <- input$lifeform2_filter
      if (is.null(lf1) || is.null(lf2) || lf1=="None" || lf2=="None") {
        log_msg("Waiting for both lifeforms to be selected...")
        return(NULL)
      }
      
      log_msg(paste("Generating lifeform table for", lf1, lf2))
      df_filtered <- rv$assessment %>% filter(region_id == rv$selected_ID)
      if (input$dataset_name_filter != "All") {
        df_filtered <- df_filtered %>% filter(dataset_name == input$dataset_name_filter)
      }
      
      convert_lifeform_df(df_filtered, lf1, lf2)
    })
    
    # ------------------------
    # Reset trigger
    # ------------------------
    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        rv$selected_ID <- NULL
        rv$dataset_choices <- NULL
        log_msg("Reset triggered")
        if (!is.null(rv$features)) {
          leafletProxy("map", session) %>% clearShapes() %>%
            addPolygons(data=rv$features, layerId=~as.character(ID),
                        color="blue", weight=2, fillOpacity=0.25,
                        highlightOptions=highlightOptions(color="orange", weight=3, bringToFront=TRUE),
                        label=~paste0("ID: ",ID))
        }
      })
    }
    
  })
}
