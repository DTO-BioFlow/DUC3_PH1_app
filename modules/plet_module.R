library(shiny)
library(leaflet)
library(sf)
library(shinycssloaders)
library(fs)
library(arrow)
library(dplyr)
library(tidyr)
library(purrr)

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
        h4("OSPAR Common Procedure Assessment using PLET data"),
        leafletOutput(ns("map"), height = "500px") %>% withSpinner(),
        br(),
        verbatimTextOutput(ns("selected_info")),
        br(),
        uiOutput(ns("dataset_selector")),
        br(),
        uiOutput(ns("lifeform_pair_selector")),
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
    
    all_lifeforms <- c("auto_and_mix_dinos","carniv_zoo","ciliates","copepods",
                       "crustacean","diatom","dinoflagellate","fish_larvae",
                       "gelatinous","holoplankton","lg_copepods","lg_phyto",
                       "meroplankton","non_carniv_zoo","pelagic_diatoms",
                       "phytoplankton","potentially_toxic_nuisance_diatoms",
                       "potentially_toxic_nuisance_dinos","sm_copepods",
                       "sm_phyto","tychopelagic_diatoms")
    
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
    
    rv <- reactiveValues(
      features = NULL,
      selected_ID = NULL,
      assessment = NULL,
      dataset_choices = NULL,
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
    # Polygon click
    # ------------------------
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      req(click$id)
      rv$selected_ID <- click$id
      log_msg(paste("Polygon clicked:", click$id))
      
      leafletProxy("map", session) %>%
        clearShapes() %>%
        addPolygons(data=rv$features, layerId=~as.character(ID),
                    color = ~ifelse(ID==rv$selected_ID,"red","blue"),
                    weight=2, fillOpacity = ~ifelse(ID==rv$selected_ID,0.5,0.25),
                    highlightOptions=highlightOptions(color="orange", weight=3, bringToFront=TRUE),
                    label=~paste0("ID: ",ID))
      
      # Reset dataset
      updateSelectInput(session, "dataset_name_filter", selected = "All")
      updateSelectInput(session, "lifeform_pair", selected = "None selected")
      
      # Update dataset choices
      req(rv$assessment)
      filtered_region <- rv$assessment %>% filter(region_id == rv$selected_ID)
      rv$dataset_choices <- c("All", unique(filtered_region$dataset_name))
      
      # min/max year for sliders
      years <- as.integer(substr(filtered_region$period,1,4))
      rv$min_year <- min(years, na.rm = TRUE)
      rv$max_year <- max(years, na.rm = TRUE)
    })
    
    # ------------------------
    # Dataset selector
    # ------------------------
    output$dataset_selector <- renderUI({
      req(rv$dataset_choices)
      selectInput(ns("dataset_name_filter"), "Select dataset_name:", choices = rv$dataset_choices, selected = "All")
    })
    
    # ------------------------
    # Valid lifeform pairs
    # ------------------------
    valid_lifeform_pairs <- reactive({
      req(rv$assessment, rv$selected_ID, input$dataset_name_filter)
      df <- rv$assessment %>% filter(region_id == rv$selected_ID)
      if(input$dataset_name_filter != "All") df <- df %>% filter(dataset_name == input$dataset_name_filter)
      
      keep <- map_lgl(PH1_ALLOWED_PAIRS, function(pair){
        all(pair %in% names(df)) && all(sapply(pair, function(lf) any(!is.na(df[[lf]]))))
      })
      PH1_ALLOWED_PAIRS[keep]
    })
    
    # ------------------------
    # Lifeform pair selector
    # ------------------------
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
      req(input$lifeform_pair)
      if(input$lifeform_pair == "None selected") return(NULL)
      valid_lifeform_pairs()[[input$lifeform_pair]][1]
    })
    
    lf2 <- reactive({
      req(input$lifeform_pair)
      if(input$lifeform_pair == "None selected") return(NULL)
      valid_lifeform_pairs()[[input$lifeform_pair]][2]
    })
    
    # ------------------------
    # Convert lifeform dataframe
    # ------------------------
    convert_lifeform_df <- function(df, lf1, lf2) {
      df <- df %>%
        mutate(period = as.character(period)) %>%
        filter(grepl("^\\d{4}-\\d{2}$", period)) %>%
        filter(!is.na(.data[[lf1]]) | !is.na(.data[[lf2]])) %>%
        rename(num_samples = numSamples)
      
      df_wide <- df %>%
        group_by(period) %>%
        summarise(
          !!lf1 := sum(.data[[lf1]] * num_samples, na.rm = TRUE)/sum(num_samples, na.rm = TRUE),
          !!lf2 := sum(.data[[lf2]] * num_samples, na.rm = TRUE)/sum(num_samples, na.rm = TRUE),
          num_samples = sum(num_samples, na.rm = TRUE),
          .groups = "drop"
        ) %>% arrange(period)
      df_wide
    }
    
    # ------------------------
    # Lifeform dataframe
    # ------------------------
    lifeform_df <- reactive({
      req(rv$assessment, rv$selected_ID)
      if(is.null(lf1()) || is.null(lf2())) return(NULL)
      
      df <- rv$assessment %>% filter(region_id == rv$selected_ID)
      if(input$dataset_name_filter != "All") df <- df %>% filter(dataset_name == input$dataset_name_filter)
      df <- df %>% select(period, numSamples, all_of(c(lf1(), lf2())))
      convert_lifeform_df(df, lf1(), lf2())
    })
    
    # ------------------------
    # Render scrollable table + CSV
    # ------------------------
    output$lifeform_table <- renderUI({
      req(lifeform_df())
      df <- lifeform_df()
      ns <- session$ns
      
      tagList(
        tags$p(strong(paste("Total records in dataframe:", nrow(df)))),
        tags$div(style="max-height:400px; overflow-y:auto;", tableOutput(ns("lifeform_table_inner"))),
        downloadButton(ns("download_csv"), "Download CSV")
      )
    })
    
    output$lifeform_table_inner <- renderTable({
      req(lifeform_df())
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
      df <- lifeform_df()
      years <- as.integer(substr(df$period,1,4))
      min_year <- min(years, na.rm=TRUE)
      max_year <- max(years, na.rm=TRUE)
      mid <- floor((min_year + max_year)/2)
      ns <- session$ns
      
      tagList(
        sliderInput(ns("time_slider_1"), "Reference Period", min=min_year, max=max_year,
                    value=c(min_year, mid), step=1, sep=""),
        sliderInput(ns("time_slider_2"), "Comparison Period", min=min_year, max=max_year,
                    value=c(mid+1, max_year), step=1, sep=""),
        uiOutput(ns("slider_counts"))
      )
    })
    
    output$slider_counts <- renderUI({
      req(lifeform_df(), input$time_slider_1, input$time_slider_2)
      df <- lifeform_df()
      years <- as.integer(substr(df$period,1,4))
      ref_count <- sum(years >= input$time_slider_1[1] & years <= input$time_slider_1[2])
      comp_count <- sum(years >= input$time_slider_2[1] & years <= input$time_slider_2[2])
      
      tagList(
        tags$p(strong(paste0("Reference period: ", input$time_slider_1[1], "-", input$time_slider_1[2],
                             " (observations: ", ref_count, ")"))),
        tags$p(strong(paste0("Comparison period: ", input$time_slider_2[1], "-", input$time_slider_2[2],
                             " (observations: ", comp_count, ")")))
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
      print(analysis_data()$env_plots[[1]])
    })
    
    output$download_results <- downloadHandler(
      filename = function() "PH1_results.xlsx",
      content = function(file) openxlsx::write.xlsx(analysis_data()$datasets, file)
    )
    
    # ------------------------
    # Reset trigger
    # ------------------------
    if(!is.null(reset_trigger)){
      observeEvent(reset_trigger(),{
        rv$selected_ID <- NULL
        rv$dataset_choices <- NULL
        updateSelectInput(session, "dataset_name_filter", selected = "All")
        updateSelectInput(session, "lifeform_pair", selected = "None selected")
        log_msg("Reset triggered")
        if(!is.null(rv$features)){
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
