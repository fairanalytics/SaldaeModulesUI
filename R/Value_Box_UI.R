
#' Saldae Dashboard Module UI: value boxes
#' @description Saldae Dashboard module UI :  value boxes  in a row containing
#' @author Farid Azouaou
#' @param id  server module ID
#' @return n  value boxes objects
#' @export

SA_Value_box_UI <- function(id){
  ns <- NS(id)
  uiOutput(ns("SA_info_value_boxes"))
}

#' Saldae Dashboard Module Server
#' @description Saldae Dashboard module SERVER : render and generate 3 value boxes output
#' @param input  input shinydashboard elements containing information to use for output generation
#' @param output output shinydashboard element
#' @param session shiny session
#' @param tisefka reactive object containing  value data
#' @return output objects to be displayed in corresponding UI module
#' @export
SA_Value_box_server <- function(input, output, session,tisefka) {

  output$SA_info_value_boxes <- renderUI({
    fluidRow(
      bs4Dash::valueBoxOutput(session$ns("SA_valuebox")),
      bs4Dash::infoBoxOutput(session$ns("SA_infobox"))
    )
  })
output$SA_infobox <- bs4Dash::renderInfoBox({
  bs4Dash::infoBox(title = "Info",
    value = 22,subtitle = "Saldae Info Box",color = "maroon",
    shiny::icon("bar-chart")
  )
})

output$SA_valuebox <- bs4Dash::renderValueBox({
  bs4Dash::valueBox(
    value = 23,subtitle = "Saldae Value Box",
    color = "info",
    icon = icon("users")
  )
})
}
