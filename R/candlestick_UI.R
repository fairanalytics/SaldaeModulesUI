#' Saldae CandleStick Graph
#' @author Farid Azouaou
#' @param tisefka  data containing low,high,close,open  variables
#' @return plotly candlestick object
#' @export

sekned_taftilt_tisefka_f <- function(tisefka = NULL) {

  i <- list(line = list(color = "#FFD700"))
  d <- list(line = list(color = "#0000ff"))
  # tisefka <- tisefka %>% dplyr::mutate(Date = as.POSIXct(rownames(tisefka), tz = "CET"))
  p <- tisefka %>%
    plotly::plot_ly(
      x = ~date, type = "candlestick",
      open = ~open, close = ~close,
      high = ~high, low = ~low
    ) %>%
    plotly::layout(
      title = "Basic Candlestick Chart",
      xaxis = list(rangeslider = list(visible = F))
    )%>%plotly::config(displaylogo = F)
  return(p)
}

#' Saldae Dashboard Module UI (CandleStick)
#' @description Saldae Dashboard module UI : CandleStick View
#' @author Farid Azouaou
#' @param id  server module ID
#' @param mod_title module title (default NULL)
#' @return UI module
#' @export

Saldae_taftilt_UI <- function(id = NULL,mod_title = NULL){
  ns <- NS(id)
  fluidPage(
    plotly::plotlyOutput(ns("taftilt"))
  )
}

#' Saldae Dashboard Module Server CandleStick
#' @description Saldae Dashboard module SERVER : render and generate multiple output objects (CandleStick)
#' @author Farid Azouaou
#' @param input  input shinydashboard elements containing information to use for output generation
#' @param output output shinydashboard element
#' @param session shiny session
#' @param tisefka reactive object containing data
#' @param div_width dimension information about the framework(html object)
#' @return output objects to be displayed in corresponding UI module
#' @export
Saldae_taftilt_mod <- function(input, output, session,tisefka,div_width = "col-xs-6 col-sm-12 col-md-6"){

  tisefka_tizegzawin <- reactive({
    tisefka()$tisefka_tizegzawin
  })

  tella_taftilt <- reactive({
    req(tisefka_tizegzawin())
    if ("open" %in% colnames(tisefka_tizegzawin()) & "close" %in% colnames(tisefka_tizegzawin()) & "high" %in% colnames(tisefka_tizegzawin()) & "low" %in% colnames(tisefka_tizegzawin()) & "open" %in% colnames(tisefka_tizegzawin())) {
      return(TRUE)
    } else {
      return(NULL)
    }
  })
  output$taftilt <- plotly::renderPlotly({
    req(tella_taftilt())
    taftilt_plot_f <- sekned_taftilt_tisefka_f(tisefka = tisefka_tizegzawin())
  })
}


