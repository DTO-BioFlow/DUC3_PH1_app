library(shiny)
library(yaml)

# ---------- Source modules ----------
source("modules/plet_module.R")
source("modules/custom_module.R")

# ---------- Load config ----------
config <- yaml::read_yaml("config.yml")
wfs_url <- config$wfs_ospar_comp

# ---------- UI ----------
ui <- navbarPage(
  title = "PH1: Changes in Plankton Communities",
  id = "tabs",
  
  tabPanel(
    "Welcome",
    value = "Start",
    tags$iframe(
      src = "https://dto-bioflow.github.io/DUC3_PH1_app/",
      style = "width:100%; height:85vh; border:none;"
    )
  ),
  
  tabPanel(
    "PLET data",
    value = "plet",
    pletUI("plet_mod")
  ),
  
  tabPanel(
    "Custom data",
    value = "custom",
    customUI("custom_mod")
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  # Central reset signal for workflow tabs
  resetSignal <- reactiveVal(0)
  
  observeEvent(input$tabs, {
    if (input$tabs %in% c("plet", "custom")) {
      resetSignal(resetSignal() + 1)
    }
  }, ignoreInit = TRUE)
  
  # Initialize modules
  pletServer(
    "plet_mod",
    reset_trigger = resetSignal,
    wfs_url = wfs_url
  )
  
  customServer(
    "custom_mod",
    reset_trigger = resetSignal
  )
}

shinyApp(ui, server)
