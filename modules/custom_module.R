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
# Source analysis & validation functions
# -------------------------------------------------------------------------
source("modules/PH1_function.R")
source("modules/test_PH1_data.R")  # provides check_data_PH1()
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
    
    # Lifeform selectors
    uiOutput(ns("lifeform_selectors_ui")),
    hr(),
    
    # Time sliders
    uiOutput(ns("timesliders_ui")),
    hr(),
    
    # Analysis plot
    plotOutput(ns("analysis_plot")),
    hr(),
    
    # Download results
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
    
    reset_after_data <- function() {
      updateSelectizeInput(session, "lifeform_1", selected = "NONE")
      updateSelectizeInput(session, "lifeform_2", selected = "NONE")
    }
    
    validate_and_process <- function(df, source_label = "dataset") {
      
      res <- check_data_PH1(df)
      
      if (!isTRUE(res$pass)) {
        showModal(
          modalDialog(
            title = "Invalid dataset",
            paste(
              "The selected", source_label, "does not comply with the PH1 data specification.",
              "",
              paste("Reason:", res$message),
              sep = "\n"
            ),
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
        return(FALSE)
      }
      
      # ---- Valid data: proceed ----
      state$df <- df
      # Select lifeform columns (all except period and num_samples)
      state$lifeform_levels <- setdiff(names(df), c("period", "num_samples"))
      
      years <- as.integer(substr(df$period, 1, 4))
      state$min_year <- min(years, na.rm = TRUE)
      state$max_year <- max(years, na.rm = TRUE)
      
      reset_after_data()
      TRUE
    }
    
    # ---------------------------------------------------------------------
    # Load catalog
    # ---------------------------------------------------------------------
    observeEvent(input$data_source, {
      reset_after_data()
      
      if (input$data_source == "catalog" && !state$catalog_loaded) {
        url <- "https://raw.githubusercontent.com/DTO-BioFlow/DUC3_dataset_inventory/refs/heads/main/data_catalog/data_catalog_PH1.json"
        state$datasets <- fromJSON(url, simplifyVector = FALSE)
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
    # Dataset info panel
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
    # Upload CSV
    # ---------------------------------------------------------------------
    observeEvent(input$file, {
      req(input$data_source == "upload", input$file)
      
      df <- tryCatch(
        read_csv(input$file$datapath, show_col_types = FALSE),
        error = function(e) NULL
      )
      
      if (is.null(df)) {
        showModal(modalDialog(title = "File read error",
                              "The uploaded file could not be read as a valid CSV.",
                              easyClose = TRUE))
        return()
      }
      
      validate_and_process(df, source_label = "uploaded file")
    })
    
    # ---------------------------------------------------------------------
    # Catalog CSV
    # ---------------------------------------------------------------------
    observeEvent(input$dataset_id, {
      req(input$data_source == "catalog", input$dataset_id != "NONE")
      
      idx <- which(vapply(state$datasets, `[[`, character(1), "id") == input$dataset_id)
      if (!length(idx)) return()
      
      tmp <- tempfile(fileext = ".csv")
      download.file(state$datasets[[idx]]$sources$data_store, tmp, quiet = TRUE)
      
      df <- read_csv(tmp, show_col_types = FALSE)
      validate_and_process(df, source_label = "catalog dataset")
      
    }, ignoreInit = TRUE)
    
    # ---------------------------------------------------------------------
    # Lifeform selectors
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
    
    #observeEvent(input$lifeform_1, reset_after_data(), ignoreInit = TRUE)
    #observeEvent(input$lifeform_2, reset_after_data(), ignoreInit = TRUE)
    
    # ---------------------------------------------------------------------
    # Time sliders
    # ---------------------------------------------------------------------
    output$timesliders_ui <- renderUI({
      req(input$lifeform_1 != "NONE", input$lifeform_2 != "NONE")
      
      mid <- floor((state$min_year + state$max_year) / 2)
      
      tagList(
        sliderInput(
          session$ns("time_slider_1"),
          "Reference Period",
          min = state$min_year,
          max = state$max_year,
          value = c(state$min_year, mid),
          step = 1,
          sep = ""
        ),
        sliderInput(
          session$ns("time_slider_2"),
          "Comparison Period",
          min = state$min_year,
          max = state$max_year,
          value = c(mid, state$max_year),
          step = 1,
          sep = ""
        )
      )
    })
    
    # ---------------------------------------------------------------------
    # ANALYSIS
    # ---------------------------------------------------------------------
    analysis_data <- reactive({
      req(
        state$df,
        input$lifeform_1 != "NONE",
        input$lifeform_2 != "NONE",
        input$time_slider_1,
        input$time_slider_2
      )
      
      # Run PH1 analysis
      results <- run_ph1_analysis(
        df = state$df,
        lf1 = input$lifeform_1,
        lf2 = input$lifeform_2,
        ref_years = input$time_slider_1,
        comp_years = input$time_slider_2,
        mon_thr = mon_thr
      )
      
      return(results)
    })
    
    output$analysis_plot <- renderPlot({
      req(analysis_data())
      print(analysis_data()$env_plots[[1]])
    })
    
    # ---------------------------------------------------------------------
    # DOWNLOAD
    # ---------------------------------------------------------------------
    output$download_results <- downloadHandler(
      filename = function() "PH1_results.xlsx",
      content = function(file) {
        openxlsx::write.xlsx(analysis_data()$datasets, file)
      }
    )
    
  })
}
