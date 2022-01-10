
#' Saldae Dashboard Module UI: value boxes
#' @description Saldae Dashboard module UI :  value boxes  in a row containing
#' @author Farid Azouaou
#' @param id  server module ID
#' @return n  value boxes objects
#' @export

SA_key_figures_UI <- function(id){
  ns <- NS(id)
  shiny::tagList(
    uiOutput(ns("SA_key_figure_select")),
    uiOutput(ns("SA_key_figure"))
  )

}

#' Saldae Dashboard Module Server
#' @description Saldae Dashboard module SERVER : render and generate 3 value boxes output
#' @param input  input shinydashboard elements containing information to use for output generation
#' @param output output shinydashboard element
#' @param session shiny session
#' @param tisefka reactive object containing  value data
#' @return output objects to be displayed in corresponding UI module
#' @export
SA_key_figures_Server <- function(input, output, session,tisefka) {

  output$SA_key_figure_select <- renderUI({
    key_figures_choices <- c("None","Average","Sum","Maximum","Minimum","First Value","Last Value")
   shinyWidgets::pickerInput(
     inputId = session$ns("SA_key_figure_select"),
     label = "Saldae Key Numbers:",
     choices = key_figures_choices,
     multiple = FALSE
   )
  })
  output$SA_key_figure <- renderUI({
    if(input$SA_key_figure_select=="None")return(NULL)
    bs4Dash::infoBoxOutput(session$ns("SA_infor_box"))
  })
  output$SA_infor_box <- bs4Dash::renderInfoBox({
    if(input$SA_key_figure_select=="None")return(NULL)
    bs4Dash::infoBox(title = input$SA_key_figure_select,
                            value = rnorm(1,1,0),color = "maroon",
                            shiny::icon("bar-chart")
    )
  })

}
