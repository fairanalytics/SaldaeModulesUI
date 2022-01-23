#------------------------ multiple-select, multiple output
#' Saldae Dashboard Module UI (growth rate)
#' @description Saldae Dashboard module UI : growth rate calculator
#' @author Farid Azouaou
#' @param id  server module ID
#' @param div_width dimension information about the framework(html object)
#' @param mod_title module title (default NULL)
#' @return UI module
#' @export

SA_tisefka_gemmu_UI <- function(id,mod_title = NULL ,div_width = "col-xs-12 col-sm-6 col-md-8") {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("growth_board_box")),
    uiOutput(ns("graphs_ui"))
  )
}



#' Saldae Dashboard Module Server Growth Rate
#' @description Saldae Dashboard module SERVER : render and generate multiple output objects for growth rates
#' @author Farid Azouaou
#' @param input  input shinydashboard elements containing information to use for output generation
#' @param output output shinydashboard element
#' @param session shiny session
#' @param tisefka reactive object containing data
#' @param div_width dimension information about the framework(html object)
#' @return output objects to be displayed in corresponding UI module
#' @export

SA_tisefka_gemmu_mod <- function(input, output, session,tisefka,div_width = "col-xs-6 col-sm-12 col-md-6") {
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
  output$growth_board_box <- renderUI({
    bs4Dash::box(title = "Growth Rate Board",collapsible = TRUE,
                        status = "success",width = 12,
                        #-----HEADER CONTENT
                        fluidRow(
                          column(width = 3,uiOutput(session$ns("select_element")))    ,
                          column(width = 2,uiOutput(session$ns("var_granularity"))),
                          column(width = 3,uiOutput(session$ns("gemmu_rate_yellan"))),
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
                              selected = NULL
    )
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
      if(is.null(aggreg_fun))aggreg_fun <- sum
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
    tisefka_iheggan <- tisefka_iheggan%>%dplyr::arrange(date)
    return(tisefka_iheggan)
  })



  target_variables <- reactive({
    req(tisefka_iheggan())
    target_variables <- colnames(tisefka_iheggan())
    target_variables <- target_variables[target_variables!="date"]
    return(target_variables)
  })


  SA_div_width <- reactive({
    req(target_variables())
    if(length(target_variables())==1){
      div_width  <- c(12,12)
    }else if(length(target_variables())==2){
      div_width  <- c(6,12)
    }else{
      div_width  <- c(4,12)
    }
    return(div_width)
  })

  output$gemmu_rate_yellan <- renderUI({
    req(ts_time_units())
    gemmu_yellan <- SaldaeDataExplorer::gemmu_yellan_f(base_unit = ts_time_units()[1])
    shinyWidgets::radioGroupButtons(
      inputId = session$ns("gemmu_rate_yellan"),
      label = "Possible rates",
      choices = gemmu_yellan,
      justified = FALSE,
      status = "info",
      checkIcon = list(
        yes = shiny::icon("ok",
                          lib = "glyphicon"
        )
      )
    )%>%
      shinyhelper::helper(type = "markdown",buttonLabel="Got it",
                          icon= shiny::icon("fas fa-question-circle"),
                          colour = "orange",
                          fade   = FALSE,
                          size = "l",
                          content = "sald_growth_rate")
  })

  #----------------
  tisefka_gemmu <- reactive({
    req(target_variables)
    req(input$gemmu_rate_yellan)
    SaldaeDataExplorer::Saldae_rate_n_gemmu_f(tisefka = tisefka_iheggan(),target_ts = target_variables(),gemmu_iswi = input$gemmu_rate_yellan, base_unit = ts_time_units()[1])
  })

  tisefka_tables <- reactive({
    req(tisefka_gemmu())
    return(purrr::map(.x =tisefka_gemmu(),~DT::datatable(.x,extensions = c('Scroller','Buttons'), options = list(dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), deferRender = TRUE, scrollY = 200,scrollX= TRUE, scroller = TRUE)))%>%
             stats::setNames(names(tisefka_gemmu())))
  })

  tisefka_yiwen_plots <- reactive({
    req(tisefka_gemmu())
    purrr::map(.x = tisefka_gemmu(),~SaldaeDataExplorer::sekned_gemmu_f(gemmu_tisefka = .x))
  })
  #---------------------
  output$graphs_ui <- renderUI({
    req(tisefka_yiwen_plots())
    plots_list <- purrr::imap(tisefka_yiwen_plots(), ~{
      bs4Dash::tabBox(width = SA_div_width()[1], title = .y,
                      tabPanel(icon("fas fa-chart-bar"),
                               plotly::plotlyOutput(session$ns(paste0("tisefka_plot_",.y)), height = "300px")
                      ),
                      tabPanel(icon("table"),
                               DT::dataTableOutput(session$ns(paste0("tisefka_table_",.y)))
                      )
      )
    })
    fluidRow(plots_list)
  })
  observeEvent(input$submit, {
    req(tisefka_yiwen_plots())
    purrr::map(names(tisefka_yiwen_plots()), ~{
      output_name_plot <- paste0("tisefka_plot_", .x)
      output_name_table <- paste0("tisefka_table_", .x)
      output[[output_name_plot]] <- plotly::renderPlotly(tisefka_yiwen_plots()[[.x]])
      output[[output_name_table]] <- DT::renderDataTable(tisefka_tables()[[.x]])

    })
  })
}
