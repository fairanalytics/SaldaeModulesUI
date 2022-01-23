#' Saldae Dashboard Module Anomaly Detection Advanced
#' @description
#' @author Farid Azouaou
#' @param id t.b.d
#' @export
SA_anomaly_UI <- function(id,mod_title = NULL ,div_width = "col-xs-12 col-sm-12 col-md-12") {
  ns <- NS(id)
  bs4Dash::tabBox(width = 12, title = mod_title,
                  tabPanel(icon("table"),
                           uiOutput(ns("anomaly_board_box")),
                           DT::dataTableOutput(ns("tisefka_table"))
                  ),
                  tabPanel(icon("fas fa-chart-bar"),
                           uiOutput(ns("variable_anomaly_view")),
                           dygraphs::dygraphOutput(ns("tisefka_plot"),height = "700px")
                  )

  )
}

#' Saldae Dashboard Module Server
#' @description Saldae Dashboard module SERVER : render and generate object to be displayed data(chart/table)
#' @author Farid Azouaou
#' @param input  input shinydashboard elements containing information to use for output generation
#' @param output output shinydashboard element
#' @param session shiny session
#' @param tisefka reactive object containing data
#' @return output objects to be displayed in corresponding UI module
#' @export

SA_anomaly_mod <- function(input, output, session,tisefka) {

  tisefka_choices <- reactive({
    req(tisefka())
    tisefka()$numeric_variables
  })
  tisefka_tizegzawin <- reactive({
    req(tisefka())
    tisefka()$tisefka_tizegzawin
  })
  non_numeric_variables <- reactive({
    req(tisefka())
    tisefka()$non_numeric_variables
  })
  categoricals_unique_values <- reactive({
    req(tisefka())
    tisefka()$categoricals_unique_values
  })
  ts_time_units <- reactive({
    tisefka()$ts_time_units
  })
  output$anomaly_board_box <- renderUI({
    bs4Dash::box(title = "Anomaly Board",collapsible = TRUE,
                        status = "success",width = 12,
                        #-----HEADER CONTENT
                        fluidRow(
                          column(width = 3,uiOutput(session$ns("select_element")))    ,
                          column(width = 2,uiOutput(session$ns("var_granularity"))),
                          column(width = 2,uiOutput(session$ns("aggregation_metric"))),
                          column(width = 2,uiOutput(session$ns("submit")))
                        ),
                 uiOutput(session$ns("non_numeric_variables_inputs"))

    )
  })

  output$submit <- renderUI({
    shinyWidgets::actionBttn(
      inputId = session$ns("submit"),
      style = "stretch",
      color = "success",
      label = "Start")
  })

  observeEvent(eventExpr=non_numeric_variables(),handlerExpr= {
    non_numeric_variables()%>%purrr::imap( ~{
      output_name_app <- paste0("non_numeric_variables_", .x)
      output[[output_name_app]] <- renderUI({
        ml_choices <- tisefka()$var_factors[[.x]]
        shinyWidgets::pickerInput(
          inputId = session$ns(output_name_app),
          label = gsub("_"," ",.x),
          choices = categoricals_unique_values()[[.x]],
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = TRUE
        )
      })
    })
  })

  output$non_numeric_variables_inputs <- renderUI({
    req(non_numeric_variables())
    fluidRow(
      purrr::map(non_numeric_variables(), ~{
        column(width = 2, uiOutput(session$ns(paste0("non_numeric_variables_",.x))))
      })
    )
  })

  output$select_element <- renderUI({
    req(tisefka_tizegzawin())
    shinyWidgets::pickerInput(inputId = session$ns("variable_picker"),
                              label = "Target elements:",
                              multiple = TRUE,
                              choices = tisefka_choices(),
                              selected = NULL)
  })
  output$var_granularity <- renderUI({
    req(non_numeric_variables())
    shinyWidgets::pickerInput(inputId = session$ns("var_granularity"),
                              label = "Granularity:",
                              multiple = TRUE,
                              choices = non_numeric_variables(),
                              selected = NULL
    )
  })
  # aggregation metric
  output$aggregation_metric <- renderUI({
    req(non_numeric_variables())
    aggregation_choices <- c("Average","Sum","Min","Max","Median")
    shinyWidgets::pickerInput(inputId = session$ns("aggregation_metric"),
                              label = "Aggregation Metric:",
                              multiple = FALSE,
                              selected = aggregation_choices[1],
                              choices = aggregation_choices
    )
  })


  tisefka_iheggan <- reactive({
    req(tisefka_tizegzawin())
    req(input$variable_picker)
    aggreg_fun <- SA_aggregation_funs(aggregation_metric = input$aggregation_metric )
    tisefka_iheggan <- tisefka_tizegzawin()
    if(length(non_numeric_variables())>0){
      categ_input_filter <-non_numeric_variables()%>%purrr::map(~input[[paste0("non_numeric_variables_",.x)]])%>%
        stats::setNames(non_numeric_variables())
      categ_input_filter <- categ_input_filter[!unlist(lapply(categ_input_filter, is.null))]
      for(cat_input in names(categ_input_filter)){
        tisefka_iheggan <- tisefka_iheggan%>%dplyr::filter(!!rlang::sym(cat_input)%in%categ_input_filter[[cat_input]])
      }
    }
    if(is.null(input$var_granularity)){
      if(is.null(aggreg_fun)) aggreg_fun <- sum

      tisefka_iheggan<- tisefka_iheggan%>%dplyr::select(date,!!input$variable_picker)%>%
        dplyr::group_by(date)%>%dplyr::summarise_all(aggreg_fun)
    }else{
      list_val_fn <- input$variable_picker%>%purrr::map(~aggreg_fun)%>%stats::setNames(input$variable_picker)
      tisefka_iheggan<- tisefka_iheggan %>%
        tidyr::pivot_wider(
          id_cols  = date,
          names_from  = input$var_granularity,
          values_from = input$variable_picker,
          values_fn = list_val_fn)
    }
    tisefka_iheggan <- tisefka_iheggan%>%dplyr::arrange(date)%>%
      dplyr::group_by(date)%>%dplyr::summarise_all(aggreg_fun,na.rm = TRUE)

    return(tisefka_iheggan)
  })



  target_variables <- reactive({
    req(tisefka_iheggan())
    target_variables <- colnames(tisefka_iheggan())
    target_variables <- target_variables[target_variables!="date"]
    return(target_variables)
  })
  tisefka_anomaly <- reactive({
    req(tisefka_iheggan())
    req(target_variables())
     SaldaeDataExplorer::anomaly_detection_nnegh(tisefka_iheggan(),target_ts = target_variables(),anomaly_mode = "anomalize")
  })

  output$variable_anomaly_view <- renderUI({
    req(target_variables())
    shinyWidgets::pickerInput(inputId = session$ns("variable_anomaly_view"),
                              label = "Select Variable:",
                              multiple = FALSE,
                              choices = target_variables(),
                              selected = target_variables()[1]
                              )%>%shinyhelper::helper(type = "markdown",buttonLabel="Got it",
                                                      icon= shiny::icon("fas fa-question-circle"),
                                                      colour = "orange",
                                                      fade   = FALSE,
                                                      size = "l",
                                                      content = "sald_anomaly")
    })
  #----------------main chart
  output$tisefka_table <- DT::renderDataTable({
    req(tisefka_anomaly())
    SaldaeDataExplorer::anomaly_to_DT_insight(tisefka_anomaly())
  })
  output$tisefka_plot <- dygraphs::renderDygraph({
    req(input$variable_anomaly_view)
    tisefka_anomaly()[[input$variable_anomaly_view]]%>%
      SaldaeDataExplorer::SA_anomaly_charter(target_variable = input$variable_anomaly_view)
  })
  #---------------
}
