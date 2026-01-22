library(shiny)
library(leaflet)
library(sf)
library(shinycssloaders)
library(fs)
library(arrow)
library(dplyr)
library(tidyr)

# Source PH1 analysis functions
source("modules/PH1_function.R")

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
        tableOutput(ns("lifeform_table")) %>% withSpinner(),
        br(),
        uiOutput(ns("timesliders_ui")),
        br(),
        plotOutput(ns("analysis_plot"))
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
                    "sm_phyto","tychopelagic_diatoms"),
      min_year = NULL,
      max_year = NULL
    )
    
    mon_thr <- 8  # PH1 monthly threshold
    
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
    # Polygon click -> reset dataset and lifeforms
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
      
      # Reset dataset and lifeforms
      updateSelectInput(session, "dataset_name_filter", selected = "All")
      updateSelectInput(session, "lifeform1_filter", selected = "None")
      updateSelectInput(session, "lifeform2_filter", selected = "None")
      
      # Update dataset choices
      req(rv$assessment)
      filtered_region <- rv$assessment %>% filter(region_id == rv$selected_ID)
      rv$dataset_choices <- c("All", unique(filtered_region$dataset_name))
      
      # Determine min/max year for sliders
      years <- as.integer(substr(filtered_region$period, 1, 4))
      rv$min_year <- min(years, na.rm = TRUE)
      rv$max_year <- max(years, na.rm = TRUE)
    })
    
    # ------------------------
    # Dataset selector -> reset lifeforms
    # ------------------------
    output$dataset_selector <- renderUI({
      req(rv$dataset_choices)
      selectInput(ns("dataset_name_filter"), "Select dataset_name:", choices = rv$dataset_choices, selected = "All")
    })
    
    observeEvent(input$dataset_name_filter, {
      req(rv$selected_ID)
      # Reset lifeforms when dataset changes
      updateSelectInput(session, "lifeform1_filter", selected = "None")
      updateSelectInput(session, "lifeform2_filter", selected = "None")
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
    # Lifeform dataframe (reactive)
    # Only generate if both lifeforms are selected
    # ------------------------
    lifeform_df <- reactive({
      req(rv$assessment, rv$selected_ID,
          input$dataset_name_filter,
          input$lifeform1_filter != "None",
          input$lifeform2_filter != "None")
      
      df_filtered <- rv$assessment %>% filter(region_id == rv$selected_ID)
      if (input$dataset_name_filter != "All") {
        df_filtered <- df_filtered %>% filter(dataset_name == input$dataset_name_filter)
      }
      
      convert_lifeform_df(df_filtered, input$lifeform1_filter, input$lifeform2_filter)
    })
    
    output$lifeform_table <- renderTable({
      req(lifeform_df())
      lifeform_df()
    })
    
    # ------------------------
    # Time sliders for PH1
    # ------------------------
    output$timesliders_ui <- renderUI({
      req(lifeform_df(), rv$min_year, rv$max_year)
      
      mid <- floor((rv$min_year + rv$max_year) / 2)
      
      tagList(
        sliderInput(
          ns("time_slider_1"),
          "Reference Period",
          min = rv$min_year,
          max = rv$max_year,
          value = c(rv$min_year, mid),
          step = 1,
          sep = ""
        ),
        sliderInput(
          ns("time_slider_2"),
          "Comparison Period",
          min = rv$min_year,
          max = rv$max_year,
          value = c(mid, rv$max_year),
          step = 1,
          sep = ""
        )
      )
    })
    
    # ------------------------
    # PH1 analysis plot
    # Only runs if all inputs are set
    # ------------------------
    analysis_data <- reactive({
      req(lifeform_df(), input$lifeform1_filter, input$lifeform2_filter,
          input$time_slider_1, input$time_slider_2)
      
      run_ph1_analysis(
        df = lifeform_df() %>% filter(lifeform %in% c(input$lifeform1_filter, input$lifeform2_filter)),
        ref_years = input$time_slider_1,
        comp_years = input$time_slider_2,
        mon_thr = mon_thr
      )
    })
    
    output$analysis_plot <- renderPlot({
      req(analysis_data())
      print(analysis_data()$env_plots[[1]])
    })
    
    # ------------------------
    # Reset trigger
    # ------------------------
    if (!is.null(reset_trigger)) {
      observeEvent(reset_trigger(), {
        rv$selected_ID <- NULL
        rv$dataset_choices <- NULL
        updateSelectInput(session, "dataset_name_filter", selected = "All")
        updateSelectInput(session, "lifeform1_filter", selected = "None")
        updateSelectInput(session, "lifeform2_filter", selected = "None")
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
