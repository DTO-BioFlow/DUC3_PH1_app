library(shiny)
library(jsonlite)
library(dplyr)
library(readr)
library(openxlsx)

library(data.table)
library(janitor)
library(pracma)
library(broom)
library(EnvStats)
library(patchwork)
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
        choices = c("- None selected -" = "NONE"),
        selected = "NONE"
      ),
      uiOutput(ns("dataset_info"))
    ),
    
    hr(),
    uiOutput(ns("lifeform_selectors_ui")),
    hr(),
    
    uiOutput(ns("timesliders_ui")),
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
      df = NULL,
      datasets = NULL,
      lifeform_levels = NULL,
      min_year = NULL,
      max_year = NULL,
      catalog_loaded = FALSE
    )
    
    mon_thr <- 8
    
    # ---------------------------------------------------------------------
    # RESET FUNCTIONS (SCOPED)
    # ---------------------------------------------------------------------
    reset_after_data <- function() {
      updateSelectizeInput(session, "lifeform_1", selected = "NONE")
      updateSelectizeInput(session, "lifeform_2", selected = "NONE")
    }
    
    reset_after_lifeform <- function() {
      # sliders + plot auto-reset via reactivity
      NULL
    }
    
    # ---------------------------------------------------------------------
    # Load catalog once
    # ---------------------------------------------------------------------
    observeEvent(input$data_source, {
      
      reset_after_data()
      
      if (input$data_source == "catalog" && !state$catalog_loaded) {
        url <- "https://raw.githubusercontent.com/DTO-BioFlow/DUC3_dataset_inventory/refs/heads/main/data_catalog/data_catalog_PH1.json"
        state$datasets <- fromJSON(url, simplifyDataFrame = FALSE)
        state$catalog_loaded <- TRUE
        
        updateSelectizeInput(
          session,
          "dataset_id",
          choices = c(
            "- None selected -" = "NONE",
            setNames(
              vapply(state$datasets, `[[`, character(1), "id"),
              vapply(state$datasets, `[[`, character(1), "name")
            )
          ),
          selected = "NONE"
        )
      }
    }, ignoreInit = TRUE)
    
    # ---------------------------------------------------------------------
    # Dataset info
    # ---------------------------------------------------------------------
    output$dataset_info <- renderUI({
      req(input$dataset_id != "NONE", state$datasets)
      
      idx <- which(vapply(state$datasets, `[[`, character(1), "id") == input$dataset_id)
      if (!length(idx)) return(NULL)
      
      d <- state$datasets[[idx]]
      
      tagList(
        tags$p(tags$strong("Description:"), d$description),
        tags$p(tags$strong("Region:"), d$region),
        tags$p(tags$strong("Lifeforms:"), paste(d$lifeforms, collapse = ", ")),
        tags$p(tags$strong("Data preview:"), tags$a(href = d$sources$data_store, d$sources$data_store)),
        tags$p(tags$strong("Source dataset:"), tags$a(href = d$sources$source_link, d$sources$source_link)),
        tags$p(tags$strong("Lifeform extraction script:"), tags$a(href = d$sources$flow, d$sources$flow))
      )
    })
    
    # ---------------------------------------------------------------------
    # Process CSV (UPLOAD OR CATALOG)
    # ---------------------------------------------------------------------
    process_df <- function(df) {
      state$df <- df
      state$lifeform_levels <- sort(unique(df$lifeform))
      
      years <- as.integer(substr(df$period, 1, 4))
      state$min_year <- min(years, na.rm = TRUE)
      state$max_year <- max(years, na.rm = TRUE)
      
      reset_after_data()
    }
    
    observeEvent(input$file, {
      req(input$data_source == "upload", input$file)
      process_df(read_csv(input$file$datapath, show_col_types = FALSE))
    })
    
    observeEvent(input$dataset_id, {
      req(input$data_source == "catalog", input$dataset_id != "NONE")
      
      idx <- which(vapply(state$datasets, `[[`, character(1), "id") == input$dataset_id)
      if (!length(idx)) return()
      
      tmp <- tempfile(fileext = ".csv")
      download.file(state$datasets[[idx]]$sources$data_store, tmp, quiet = TRUE)
      process_df(read_csv(tmp, show_col_types = FALSE))
    }, ignoreInit = TRUE)
    
    # ---------------------------------------------------------------------
    # Lifeform selectors (NOW APPEAR CORRECTLY)
    # ---------------------------------------------------------------------
    output$lifeform_selectors_ui <- renderUI({
      req(state$lifeform_levels)
      
      tagList(
        selectizeInput(
          session$ns("lifeform_1"),
          "Lifeform 1",
          choices = c("- None selected -" = "NONE", state$lifeform_levels),
          selected = "NONE"
        ),
        selectizeInput(
          session$ns("lifeform_2"),
          "Lifeform 2",
          choices = c("- None selected -" = "NONE", state$lifeform_levels),
          selected = "NONE"
        )
      )
    })
    
    observeEvent(input$lifeform_1, reset_after_lifeform(), ignoreInit = TRUE)
    observeEvent(input$lifeform_2, reset_after_lifeform(), ignoreInit = TRUE)
    
    # ---------------------------------------------------------------------
    # Time sliders (DEPEND ON LIFEFORMS)
    # ---------------------------------------------------------------------
    output$timesliders_ui <- renderUI({
      req(input$lifeform_1 != "NONE", input$lifeform_2 != "NONE")
      
      mid <- floor((state$min_year + state$max_year) / 2)
      
      tagList(
        sliderInput(session$ns("time_slider_1"), "Reference Period",
                    min = state$min_year, max = state$max_year,
                    value = c(state$min_year, mid), step=1, sep=""),
        sliderInput(session$ns("time_slider_2"), "Comparison Period",
                    min = state$min_year, max = state$max_year,
                    value = c(mid, state$max_year), step=1, sep="")
      )
    })
    
    # ---------------------------------------------------------------------
    # ANALYSIS (AUTO INVALIDATED & RERUN)
    # ---------------------------------------------------------------------
    analysis_data <- reactive({
      req(
        state$df,
        input$lifeform_1 != "NONE",
        input$lifeform_2 != "NONE",
        input$time_slider_1,
        input$time_slider_2
      )
      
      run_ph1_analysis(
        df = state$df %>% filter(lifeform %in% c(input$lifeform_1, input$lifeform_2)),
        ref_years = input$time_slider_1,
        comp_years = input$time_slider_2,
        mon_thr = mon_thr
      )
    })
    
    output$analysis_plot <- renderPlot({
      req(analysis_data())
      print(analysis_data()$env_plots[[1]])
    })
    
    output$download_results <- downloadHandler(
      filename = function() "PH1_results.xlsx",
      content = function(file) {
        openxlsx::write.xlsx(analysis_data()$datasets, file)
      }
    )
  })
}
