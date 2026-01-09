library(shiny)

customUI <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Custom Data Workflow"),
    actionButton(ns("press"), "Press me"),
    br(), br(),
    textOutput(ns("counter_text")),
    br(),
    actionButton(ns("reset"), "Reset workflow")
  )
}

customServer <- function(id, reset_trigger) {
  moduleServer(id, function(input, output, session) {

    state <- reactiveValues()

    initialize <- function() {
      state$counter <- 0
    }

    initialize()

    observeEvent(input$press, {
      state$counter <- state$counter + 1
    })

    output$counter_text <- renderText({
      paste("You pressed me", state$counter, "times")
    })

    observeEvent(input$reset, {
      initialize()
    })

    observeEvent(reset_trigger(), {
      initialize()
    })
  })
}
