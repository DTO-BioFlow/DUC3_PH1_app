library(shiny)
library(jsonlite)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(openxlsx)

# Analysis packages
library(data.table)
library(janitor)
library(pracma)
library(broom)
library(EnvStats)
library(patchwork)
library(rnaturalearth)
library(zoo)
library(purrr)

# -------------------------------------------------------------------------
# Source analysis functions
# -------------------------------------------------------------------------
source("modules/PH1_function.R")
source("modules/Supporting_scripts/PI_functions_v1.R")
source("modules/Supporting_scripts/Supporting_functions_v2.R")

# -------------------------------------------------------------------------
# UI
# -------------------------------------------------------------------------
customUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Custom Data Workflow"),
    
    radioButtons(
      ns("data_source"),
      "Select data source",
      choices = c(
        "Upload CSV file" = "upload",
        "Select online dataset" = "catalog"
      ),
      selected = "upload"
    ),
    
    conditionalPanel(
      condition = sprintf("input['%s'] == 'upload'", ns("data_source")),
      fileInput(ns("file"), "Upload CSV file", accept = ".csv")
    ),
    
    conditionalPanel(
      condition = sprintf("input['%s'] == 'catalog'", ns("data_source")),
      selectizeInput(
        ns("dataset_id"),
        "Available datasets",
        choices = NULL,
        options = list(placeholder = "Select a dataset...")
      ),
      uiOutput(ns("dataset_info"))
    ),
    
    hr(),
    uiOutput(ns("timesliders_ui")),
    verbatimTextOutput(ns("time_ranges")),
    hr(),
    
    plotOutput(ns("analysis_plot")),
    hr(),
    
    downloadButton(ns("download_results"), "Download Analysis Results")
  )
}

# -------------------------------------------------------------------------
# SERVER
# -------------------------------------------------------------------------
customServer <- function(id, reset_trigger) {
  moduleServer(id, function(input, output, session) {
    
    state <- reactiveValues(
      df_custom = NULL,
      df_lf = NULL,
      datasets = NULL,
      min_year = NULL,
      max_year = NULL,
      ref_years = NULL,
      comp_years = NULL,
      file_lf = NULL,
      analysis_results = NULL
    )
    
    required_cols <- c("period", "lifeform", "abundance", "num_samples")
    mon_thr <- 8
    
    # ---------------------------------------------------------------------
    # Initialize
    # ---------------------------------------------------------------------
    initialize <- function() {
      state$df_custom <- NULL
      state$df_lf <- NULL
      state$ref_years <- NULL
      state$comp_years <- NULL
      state$analysis_results <- NULL
    }
    initialize()
    
    # ---------------------------------------------------------------------
    # Load catalog
    # ---------------------------------------------------------------------
    observeEvent(TRUE, {
      catalog_url <- "https://raw.githubusercontent.com/DTO-BioFlow/DUC3_dataset_inventory/refs/heads/main/data_catalog/data_catalog_PH1.json"
      datasets <- fromJSON(catalog_url, simplifyDataFrame = FALSE)
      state$datasets <- datasets
      
      choices <- setNames(
        vapply(datasets, function(x) x$id, character(1)),
        vapply(datasets, function(x) x$name, character(1))
      )
      
      updateSelectizeInput(session, "dataset_id", choices = choices)
    }, once = TRUE)
    
    # ---------------------------------------------------------------------
    # Dataset info
    # ---------------------------------------------------------------------
    output$dataset_info <- renderUI({
      req(input$dataset_id, state$datasets)
      dataset <- state$datasets[
        vapply(state$datasets, function(x) x$id, character(1)) == input$dataset_id
      ][[1]]
      
      tagList(
        tags$p(tags$strong("Description:"), dataset$description),
        tags$p(tags$strong("Region:"), dataset$region),
        tags$p(tags$strong("Lifeforms:"), paste(dataset$lifeforms, collapse = ", "))
      )
    })
    
    # ---------------------------------------------------------------------
    # CSV processing (NO period separation!)
    # ---------------------------------------------------------------------
    process_file <- function(path, name = NULL) {
      
      df <- read_csv(path, show_col_types = FALSE)
      
      if (!all(required_cols %in% names(df))) {
        showModal(modalDialog(
          title = "Invalid file format",
          "CSV must contain: period, lifeform, abundance, num_samples",
          easyClose = TRUE
        ))
        return(NULL)
      }
      
      df <- df %>% mutate(period = gsub('"', "", period))
      
      state$df_custom <- df
      state$df_lf <- unique(df$lifeform)
      state$file_lf <- ifelse(is.null(name), basename(path), name)
      
      years <- as.integer(substr(df$period, 1, 4))
      state$min_year <- min(years, na.rm = TRUE)
      state$max_year <- max(years, na.rm = TRUE)
    }
    
    # ---------------------------------------------------------------------
    # Upload / catalog handlers
    # ---------------------------------------------------------------------
    observeEvent(input$file, {
      req(input$data_source == "upload", input$file)
      process_file(input$file$datapath, input$file$name)
    })
    
    observeEvent(input$dataset_id, {
      req(input$data_source == "catalog")
      dataset <- state$datasets[
        vapply(state$datasets, function(x) x$id, character(1)) == input$dataset_id
      ][[1]]
      
      tmp <- tempfile(fileext = ".csv")
      download.file(dataset$sources$data, tmp, quiet = TRUE)
      process_file(tmp, basename(dataset$sources$data))
    })
    
    # ---------------------------------------------------------------------
    # Time sliders
    # ---------------------------------------------------------------------
    output$timesliders_ui <- renderUI({
      req(state$min_year, state$max_year)
      mid <- floor((state$min_year + state$max_year) / 2)
      
      tagList(
        sliderInput(session$ns("time_slider_1"), "Reference Period",
                    min = state$min_year, max = state$max_year,
                    value = c(state$min_year, mid), step = 1),
        sliderInput(session$ns("time_slider_2"), "Comparison Period",
                    min = state$min_year, max = state$max_year,
                    value = c(mid, state$max_year), step = 1)
      )
    })
    
    output$time_ranges <- renderPrint({
      req(input$time_slider_1, input$time_slider_2)
      state$ref_years <- input$time_slider_1
      state$comp_years <- input$time_slider_2
      cat("Reference:", state$ref_years, "\n")
      cat("Comparison:", state$comp_years, "\n")
    })
    
    # ---------------------------------------------------------------------
    # ANALYSIS (single call)
    # ---------------------------------------------------------------------
    analysis_data <- reactive({
      req(state$df_custom, state$df_lf, state$ref_years, state$comp_years)
      
      # required by PH1 function
      df_lf <- state$df_lf
      
      results <- run_ph1_analysis(
        df         = state$df_custom,
        ref_years  = state$ref_years,
        comp_years = state$comp_years,
        mon_thr    = mon_thr
      )
      
      state$analysis_results <- results$datasets
      results
    })
    
    # ---------------------------------------------------------------------
    # Plot
    # ---------------------------------------------------------------------
    output$analysis_plot <- renderPlot({
      req(analysis_data())
      print(analysis_data()$env_plots[[1]])
    })
    
    # ---------------------------------------------------------------------
    # Download
    # ---------------------------------------------------------------------
    output$download_results <- downloadHandler(
      filename = function() {
        paste0(tools::file_path_sans_ext(state$file_lf), "_PH1_results.xlsx")
      },
      content = function(file) {
        openxlsx::write.xlsx(state$analysis_results, file, overwrite = TRUE)
      }
    )
    
    observeEvent(reset_trigger(), initialize())
  })
}
