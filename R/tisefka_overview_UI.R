
#' Saldae Dashboard Data Overview Server UI
#' @description Saldae Dashboard module UI :  value boxes  in a row containing
#' @author Farid Azouaou
#' @param id  server module ID
#' @return n  value boxes objects
#' @export

SA_tisefka_overview_UI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("SA_tisefka_overview"))
  )

}

#' Saldae Dashboard Data Overview Server module
#' @description Saldae Dashboard module SERVER : render and generate 3 value boxes output
#' @param input  input shinydashboard elements containing information to use for output generation
#' @param output output shinydashboard element
#' @param session shiny session
#' @param tisefka reactive object containing  value data
#' @return output objects to be displayed in corresponding UI module
#' @export

SA_tisefka_overview_server <- function(input, output, session,tisefka) {
  output$SA_tisefka_overview <- renderUI({
    fluidRow(
      bs4Dash::valueBoxOutput(session$ns("SA_overview1")),
      bs4Dash::infoBoxOutput(session$ns("SA_overview2")),
      bs4Dash::infoBoxOutput(session$ns("SA_overview3"))

    )
  })
  output$SA_overview1 <- bs4Dash::renderInfoBox({
    bs4Dash::infoBox(title = "Info",
                            value = 22,subtitle = "Saldae Info Box",color = "olive",
                            shiny::icon("fas fa-chart-bar")
    )
  })
  output$SA_overview2 <- bs4Dash::renderInfoBox({
    bs4Dash::infoBox(title = "Info",
                            value = 22,subtitle = "Saldae Info Box",color = "green",
                            shiny::icon("fas fa-chart-bar")
    )
  })
  output$SA_overview3 <- bs4Dash::renderInfoBox({
    bs4Dash::infoBox(title = "Info",
                            value = 22,subtitle = "Saldae Info Box",color = "navy",
                            shiny::icon("fas fa-chart-bar")
    )
  })
}
