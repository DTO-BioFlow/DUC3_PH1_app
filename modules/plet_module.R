library(shiny)
library(leaflet)
library(sf)
library(shinycssloaders)
library(fs)
library(arrow)
library(dplyr)
library(tidyr)
library(purrr)
library(yaml)
library(jsonlite)

# Source PH1 analysis functions
source("modules/PH1_function.R")

config <- yaml::read_yaml("config.yml")

log_msg <- function(msg) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

# ======================
# UI
# ======================
pletUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        width = 6,
        h4("OSPAR Common Procedure Assessment using PLET data"),
        
        uiOutput(ns("plet_file_selector")),
        br(),
        
        uiOutput(ns("plet_info_panel")),
        br(),
        
        leafletOutput(ns("map"), height = "500px") %>% withSpinner(),
        br(),
        
        uiOutput(ns("dataset_selector")),
        br(),
        
        uiOutput(ns("lifeform_pair_selector")),
        br(),
        
        tableOutput(ns("lifeform_table")) %>% withSpinner(),
        br(),
        
        uiOutput(ns("timesliders_ui")),
        br(),
        
        plotOutput(
          ns("analysis_plot"), 
          width = "1200",   # or "100%" for full width
          height = "600px"   # fixed height in px
        ),
        br(),
        # Download results
        downloadButton(ns("download_results"), "Download Analysis Results")
      )
    )
  )
}

# ======================
# SERVER
# ======================
pletServer <- function(id, reset_trigger = NULL, wfs_url = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ------------------------
    # Reactive state
    # ------------------------
    rv <- reactiveValues(
      features = NULL,
      selected_ID = NULL,
      assessment = NULL,
      dataset_choices = NULL,
      min_year = NULL,
      max_year = NULL
    )
    
    mon_thr <- 8
    
    # ------------------------
    # Load WFS once at startup
    # ------------------------
    cache_file <- "cache/ospar_features.rds"
    if (!dir.exists("cache")) dir.create("cache")
    observe({
      req(wfs_url)
      if (file.exists(cache_file)) {
        rv$features <- readRDS(cache_file)
      } else {
        rv$features <- tryCatch(
          sf::st_read(wfs_url, quiet = TRUE),
          error = function(e) { log_msg(paste("Failed WFS:", e$message)); NULL }
        )
        if (!is.null(rv$features)) saveRDS(rv$features, cache_file)
      }
      log_msg("WFS loaded")
    })
    
    # ------------------------
    # Load PLET catalog
    # ------------------------
    plet_catalog <- reactiveVal(NULL)
    observe({
      req(config$plet_catalog)
      cat_json <- tryCatch(
        jsonlite::fromJSON(config$plet_catalog, simplifyVector = TRUE),
        error = function(e) {
          log_msg(paste("Failed loading PLET catalog:", e$message))
          NULL
        }
      )
      plet_catalog(cat_json)
      log_msg("PLET catalog loaded")
    })
    
    # ------------------------
    # PLET selector
    # ------------------------
    output$plet_file_selector <- renderUI({
      req(plet_catalog())
      selectInput(
        ns("plet_file"),
        "Select PLET file:",
        choices = c(
          "None selected" = "",
          setNames(plet_catalog()$id, plet_catalog()$name)
        ),
        selected = ""
      )
    })
    
    selected_plet <- reactive({
      req(input$plet_file)
      if (input$plet_file == "") return(NULL)
      plet_catalog() %>% filter(id == input$plet_file)
    })
    
    # ------------------------
    # PLET metadata panel
    # ------------------------
    output$plet_info_panel <- renderUI({
      req(selected_plet())
      tagList(
        tags$div(
          style = "background:#f7f7f7; border:1px solid #ddd; border-radius:6px; padding:10px;",
          tags$b("Description:"), tags$p(selected_plet()$description),
          tags$b("Creation date:"), tags$p(selected_plet()$creation_date),
          tags$b("Data store:"), tags$p(tags$code(selected_plet()$sources$data_store)),
          tags$b("Flow:"), tags$p(tags$a(href = selected_plet()$sources$flow, target="_blank", selected_plet()$sources$flow))
        )
      )
    })
    
    # ------------------------
    # Reset reactive state except WFS
    # ------------------------
    reset_all <- function() {
      rv$selected_ID <- NULL
      rv$assessment <- NULL
      rv$dataset_choices <- NULL
      rv$min_year <- NULL
      rv$max_year <- NULL
      
      updateSelectInput(session, "dataset_name_filter", selected = "All")
      updateSelectInput(session, "lifeform_pair", selected = "None selected")
    }
    
    observeEvent(input$plet_file, {
      reset_all()
      log_msg(paste("PLET changed:", input$plet_file))
    })
    
    # ------------------------
    # Load parquet dynamically after PLET selection
    # ------------------------
    observeEvent(selected_plet(), {
      req(selected_plet())
      
      fs <- S3FileSystem$create(
        anonymous = TRUE,
        scheme = "https",
        endpoint_override = "minio.dive.edito.eu"
      )
      
      parquet_url <- selected_plet()$sources$data_store
      parquet_path <- sub("^https://minio.dive.edito.eu/", "", parquet_url)
      
      rv$assessment <- read_parquet(fs$path(parquet_path))
      log_msg(paste("Loaded parquet:", parquet_url))
    })
    
    # ------------------------
    # Render map + polygons
    # ------------------------
    output$map <- renderLeaflet({
      req(rv$features)
      leaflet(rv$features) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          layerId = ~as.character(ID),
          color = "blue",
          weight = 2,
          fillOpacity = 0.25,
          highlightOptions = highlightOptions(color = "orange", weight = 3, bringToFront = TRUE),
          label = ~paste0("ID: ", ID)
        )
    })
    
    # ------------------------
    # Polygon click → cascading downstream
    # ------------------------
    observeEvent(input$map_shape_click, {
      req(rv$assessment)
      click <- input$map_shape_click
      req(click$id)
      
      rv$selected_ID <- click$id
      
      leafletProxy(ns("map"), session) %>%
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
      
      filtered_region <- rv$assessment %>% filter(region_id == rv$selected_ID)
      rv$dataset_choices <- c("All", unique(filtered_region$dataset_name))
      
      years <- as.integer(substr(filtered_region$period, 1, 4))
      rv$min_year <- min(years, na.rm = TRUE)
      rv$max_year <- max(years, na.rm = TRUE)
    })
    
    # ------------------------
    # Dataset selector
    # ------------------------
    output$dataset_selector <- renderUI({
      req(rv$dataset_choices)
      selectInput(ns("dataset_name_filter"), "Select dataset:", choices = rv$dataset_choices, selected = "All")
    })
    
    # ------------------------
    # Lifeform pairs
    # ------------------------
    PH1_ALLOWED_PAIRS <- list(
      "Diatom vs Dinoflagellate" = c("diatom","dinoflagellate"),
      "Tychopelagic vs Pelagic diatoms" = c("tychopelagic_diatoms","pelagic_diatoms"),
      "LG vs SM Copepods" = c("lg_copepods","sm_copepods"),
      "Holoplankton vs Meroplankton" = c("holoplankton","meroplankton"),
      "LG vs SM Phyto" = c("lg_phyto","sm_phyto"),
      "Phytoplankton vs Non-carniv zoo" = c("phytoplankton","non_carniv_zoo"),
      "Crustacean vs Gelatinous" = c("crustacean","gelatinous"),
      "Gelatinous vs Fish larvae" = c("gelatinous","fish_larvae")
    )
    
    valid_lifeform_pairs <- reactive({
      req(rv$assessment, rv$selected_ID, input$dataset_name_filter)
      df <- rv$assessment %>% filter(region_id == rv$selected_ID)
      if(input$dataset_name_filter != "All") df <- df %>% filter(dataset_name == input$dataset_name_filter)
      
      keep <- map_lgl(PH1_ALLOWED_PAIRS, function(pair){
        all(pair %in% names(df)) && all(map_lgl(pair, ~ any(!is.na(df[[.x]]))))
      })
      PH1_ALLOWED_PAIRS[keep]
    })
    
    output$lifeform_pair_selector <- renderUI({
      req(valid_lifeform_pairs())
      selectInput(
        ns("lifeform_pair"),
        "Select Lifeform Pair:",
        choices = c("None selected", names(valid_lifeform_pairs())),
        selected = "None selected"
      )
    })
    
    lf1 <- reactive({
      if(input$lifeform_pair == "None selected") return(NULL)
      valid_lifeform_pairs()[[input$lifeform_pair]][1]
    })
    
    lf2 <- reactive({
      if(input$lifeform_pair == "None selected") return(NULL)
      valid_lifeform_pairs()[[input$lifeform_pair]][2]
    })
    
    convert_lifeform_df <- function(df, lf1, lf2){
      df %>%
        mutate(period = as.character(period)) %>%
        filter(grepl("^\\d{4}-\\d{2}$", period)) %>%
        rename(num_samples = numSamples) %>%
        group_by(period) %>%
        summarise(
          !!lf1 := weighted.mean(.data[[lf1]], num_samples, na.rm = TRUE),
          !!lf2 := weighted.mean(.data[[lf2]], num_samples, na.rm = TRUE),
          num_samples = sum(num_samples, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(num_samples > 0)
    }
    
    lifeform_df <- reactive({
      req(rv$assessment, rv$selected_ID, lf1(), lf2())
      df <- rv$assessment %>% filter(region_id == rv$selected_ID)
      if(input$dataset_name_filter != "All") df <- df %>% filter(dataset_name == input$dataset_name_filter)
      df <- df %>% select(period, numSamples, all_of(c(lf1(), lf2())))
      convert_lifeform_df(df, lf1(), lf2())
    })
    
    # ------------------------
    # Table + CSV
    # ------------------------
    output$lifeform_table <- renderUI({
      req(lifeform_df())
      tagList(
        tags$p(strong(paste("Total records:", nrow(lifeform_df())))),
        tags$div(style="max-height:400px; overflow-y:auto;", tableOutput(ns("lifeform_table_inner"))),
        downloadButton(ns("download_csv"), "Download CSV")
      )
    })
    
    output$lifeform_table_inner <- renderTable({
      lifeform_df()
    })
    
    output$download_csv <- downloadHandler(
      filename = function() paste0("lifeform_data_", Sys.Date(), ".csv"),
      content = function(file) write.csv(lifeform_df(), file, row.names = FALSE)
    )
    
    # ------------------------
    # Time sliders
    # ------------------------
    output$timesliders_ui <- renderUI({
      req(lifeform_df())
      years <- as.integer(substr(lifeform_df()$period, 1, 4))
      min_year <- min(years, na.rm=TRUE)
      max_year <- max(years, na.rm=TRUE)
      mid <- floor((min_year + max_year)/2)
      
      tagList(
        sliderInput(ns("time_slider_1"), "Reference Period", min=min_year, max=max_year,
                    value=c(min_year, mid), step=1, sep=""),
        sliderInput(ns("time_slider_2"), "Comparison Period", min=min_year, max=max_year,
                    value=c(mid+1, max_year), step=1, sep="")
      )
    })
    
    # ------------------------
    # PH1 analysis
    # ------------------------
    analysis_data <- reactive({
      req(lifeform_df(), lf1(), lf2(), input$time_slider_1, input$time_slider_2)
      run_ph1_analysis(
        df = lifeform_df(),
        lf1 = lf1(),
        lf2 = lf2(),
        ref_years = input$time_slider_1,
        comp_years = input$time_slider_2,
        mon_thr = mon_thr
      )
    })
    
    output$analysis_plot <- renderPlot({
      req(analysis_data())
      library(patchwork)
      
      # Extract actual ggplot objects
      p_env <- analysis_data()$env_plots[[1]][[1]]
      p_ts1 <- analysis_data()$ts_plots[[1]][[1]]
      p_ts2 <- analysis_data()$ts_plots[[2]][[1]]
      
      # Combine with patchwork
      p_env | (p_ts1 / p_ts2)
    },
    width = 1200,   # pixels
    height = 600   # pixels
    )
    output$download_results <- downloadHandler(
      filename = function() "PH1_results.xlsx",
      content = function(file) {
        openxlsx::write.xlsx(analysis_data()$datasets, file)
      }
    )
    
  })
}

