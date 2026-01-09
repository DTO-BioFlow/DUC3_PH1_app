library(shiny)
library(yaml)

# ---------- Source modules ----------
source("modules/plet_module.R")
source("modules/custom_module.R") # your existing custom module

# ---------- Load config ----------
config <- yaml::read_yaml("config.yml")
wfs_url <- config$plet_wfs_url

# ---------- JavaScript for tab switch confirmation ----------
jsCode <- "
var lastTab = 'welcome';

$(document).on('shiny:inputchanged', function(event) {
  if (event.name === 'tabs') {

    // If coming FROM welcome, allow silently
    if (lastTab === 'welcome') {
      lastTab = event.value;
      return;
    }

    // If switching BETWEEN workflow tabs or leaving workflow
    if (lastTab !== event.value) {
      var ok = confirm('Leaving workflow - reset will happen');
      if (!ok) {
        Shiny.setInputValue('tabs', lastTab, {priority: 'event'});
      } else {
        Shiny.setInputValue(
          'confirmed_tab_change',
          {from: lastTab, to: event.value},
          {priority: 'event'}
        );
        lastTab = event.value;
      }
    }
  }
});
"

# ---------- UI ----------
ui <- navbarPage(
  title = "Shiny Workflow App",
  id = "tabs",
  header = tagList(tags$script(HTML(jsCode))),
  
  tabPanel("Welcome", value = "welcome", h3("Welcome to the workflow app")),
  
  tabPanel("PLET data", value = "plet", pletUI("plet_mod")),
  
  tabPanel("Custom data", value = "custom", customUI("custom_mod"))
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  # Central reset signal for workflow tabs
  resetSignal <- reactiveVal(0)
  observeEvent(input$confirmed_tab_change, {
    resetSignal(resetSignal() + 1)
  })
  
  # Initialize modules
  pletServer("plet_mod", reset_trigger = resetSignal, wfs_url = wfs_url)
  customServer("custom_mod", reset_trigger = resetSignal)
}

shinyApp(ui, server)
