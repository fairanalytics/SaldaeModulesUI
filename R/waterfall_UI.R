#' Saldae Waterfall Graph
#' @author Farid Azouaou
#' @param tisefka  data
#' @param kefrida_variables variables to use
#' @param aggregation aggregation logic
#' @return plotly waterfall object
#' @export
sekned_kefrida_f <- function(tisefka = NULL, kefrida_variables = NULL, aggregation = "sum") {
  measure_kefrida <- rep("relative", length(kefrida_variables))
  if (is.null(kefrida_variables)) kefrida_variables <- vector()
  if ("aggr." %in% kefrida_variables) {
    kefrida_aggregates <- which(kefrida_variables == "aggr.")
    measure_kefrida <- rep("relative", length(kefrida_variables))
    measure_kefrida[kefrida_aggregates] <- "total"
    # kefrida_variables<- kefrida_variables[-kefrida_aggregates]

    k <- 0
    start_index <- 1
    for (agg in 1:length(kefrida_aggregates)) {
      if (kefrida_aggregates[agg] > 1) {
        k <- k + 1
        aggr_indices <- start_index:(kefrida_aggregates[agg] - 1)
        aggr_indices <- kefrida_variables[aggr_indices]
        kefrida_variables[kefrida_aggregates[agg]] <- paste0("aggregate_", k)
        tisefka[, paste0("aggregate_", k)] <- apply(tisefka[, aggr_indices, drop = F], 1, sum, na.rm = TRUE)
        start_index <- kefrida_aggregates[agg] + 1
      } else {
        measure_kefrida[kefrida_aggregates[agg]] <- NULL
      }
    }
  }
  if (length(kefrida_variables) < 2) {
    return(NULL)
  }
  x <- apply(tisefka[, kefrida_variables, drop = F], 2, function(x) sum(x, na.rm = TRUE))
  y <- kefrida_variables

  data <- data.frame(x, y = factor(y, levels = y), measure = measure_kefrida)

  kefrida <- plotly::plot_ly(data,
                             x = ~x, y = ~y, measure = ~measure, type = "waterfall", name = "2018",
                             orientation = "h", connector = list(mode = "between", line = list(width = 4, color = "rgb(0, 0, 0)", dash = 0))
  ) %>%
    plotly::layout(
      title = "Profit and loss statement : <br>waterfall chart displaying positive and negative",
      xaxis = list(title = "", tickfont = "16", ticks = "outside"),
      yaxis = list(title = "", type = "category", autorange = "reversed"),
      xaxis = list(title = "", type = "linear"),
      margin = c(l = 150),
      showlegend = TRUE
    )%>%plotly::config(displaylogo = F)

  return(kefrida)
}


#' Saldae Dashboard Module UI (Waterfall)
#' @description Saldae Dashboard module UI : Waterfall View
#' @author Farid Azouaou
#' @param id  server module ID
#' @param mod_title module title (default NULL)
#' @return UI module
#' @export

Saldae_kefrida_UI <- function(id = NULL,mod_title = NULL){
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("kefrida")),
    fluidRow(
      uiOutput(ns("keferida_numericals")),
      uiOutput(ns("kefrida_aggregate")),
      uiOutput(ns("keferida_go"))
    ),
    plotly::plotlyOutput(ns("kefrida_plot"))
  )
}

#' Saldae Dashboard Module Server Waterfall
#' @description Saldae Dashboard module SERVER : render and generate multiple output objects (Waterfall)
#' @author Farid Azouaou
#' @param input  input shinydashboard elements containing information to use for output generation
#' @param output output shinydashboard element
#' @param session shiny session
#' @param tisefka reactive object containing data
#' @param div_width dimension information about the framework(html object)
#' @return output objects to be displayed in corresponding UI module
#' @export
Saldae_kefrida_mod <- function(input, output, session,tisefka,div_width = "col-xs-6 col-sm-12 col-md-6"){

  tisefka_tizegzawin <- reactive({
    tisefka()$tisefka_tizegzawin
  })
  numeric_variables <- reactive({
    tisefka()$numeric_variables
  })

  output$kefrida <- renderUI({
    req(numeric_variables())
    shinyWidgets::prettySwitch(
      inputId = session$ns("kefrida"),
      label = "Waterfall",
      status = "info",
      fill = TRUE
    )
  })
  output$keferida_numericals <- renderUI({
    #
    req(input$kefrida)
    if (input$kefrida == FALSE){
      return(NULL)
    }
    shinyWidgets::checkboxGroupButtons(
      inputId = session$ns("keferida_numericals"),
      label = "Choose Waterfall components:",
      choices = numeric_variables(),
      justified = TRUE,
      checkIcon = list(
        yes = shiny::icon("ok",
                          lib = "glyphicon"
        )
      )
    )
  })
  output$keferida_go <- renderUI({
    #
    req(input$kefrida_aggregate)
    if (is.null(input$kefrida_aggregate)) {
      aggregates_kefrida <- NULL
    } else {
      num_aggregates <- min(input$kefrida_aggregate, floor(length(input$keferida_numericals) / 2))
      aggregates_kefrida <- rep("aggr.", times = num_aggregates)
    }
    shinyjqui::orderInput(inputId = session$ns("keferida_go"), label = "Waterfall", items = c(input$keferida_numericals, aggregates_kefrida))
  })



  output$kefrida_aggregate <- renderUI({
    req(input$keferida_numericals)
    shinyWidgets::actionBttn(
      inputId = session$ns("kefrida_aggregate"),
      label = "Add aggregate:",
      style = "stretch",
      color = "warning"
    )
  })



  output$kefrida_plot <- plotly::renderPlotly({
    req(input$keferida_go_order)
    if (length(input$keferida_go_order) < 2) {
      return(NULL)
    }
    kefrida_plot <- sekned_kefrida_f(tisefka = tisefka_tizegzawin(), kefrida_variables = input$keferida_go_order)
  })
}


