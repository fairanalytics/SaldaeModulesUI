key_value_calculator <- function(tisefka = NULL,key_value =NULL){
  tisefka <- na.omit(tisefka)
  my_value <- vector()
  if("Sum"%in%key_value){
    my_value["Sum"] <- sum(tisefka,na.rm = TRUE)
  }
  if("Average"%in%key_value){
    my_value["Average"] <- colMeans(tisefka,na.rm = TRUE)
  }
  if("Maximum"%in%key_value){
    my_value["Maximum"] <- max(tisefka,na.rm = TRUE)
  }
  if("Minimum"%in%key_value ){
    my_value["Minimum"] <- min(tisefka,na.rm = TRUE)
  }
  if("First Value"%in%key_value){
    my_value["First Value"] <- head(tisefka,1)
  }
  if( "Last Value"%in%key_value ){
    my_value["Last Value"]<- tail(tisefka,1)
  }
  return(round(my_value,3))
}

#------------------------ multiple-select, multiple output
#' Saldae Dashboard Module UI (analytics)
#' @description Saldae Dashboard module UI : forecasting
#' @author Farid Azouaou
#' @param id  server module ID
#' @param div_width dimension information about the framework(html object)
#' @param mod_title module title (default NULL)
#' @return UI module
#' @export

SA_tisefka_forecast_UI <- function(id,mod_title = NULL ,div_width = "col-xs-12 col-sm-6 col-md-8") {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(width = 12,
             uiOutput(ns("analytics_header_box"))
      )
    ),
    fluidRow(
      column(width = 6,
             uiOutput(ns("analytics_config_box"))
      ),
      column(width = 6,
             uiOutput(ns("analytics_advanced_box"))
      )
    ),
    uiOutput(ns("graphs_ui"))
  )
}



#' Saldae Dashboard Module Server Analytics
#' @description Saldae Dashboard module SERVER : render and generate multiple output objects for analytics
#' @author Farid Azouaou
#' @param input  input shinydashboard elements containing information to use for output generation
#' @param output output shinydashboard element
#' @param session shiny session
#' @param tisefka reactive object containing data
#' @param div_width dimension information about the framework(html object)
#' @return output objects to be displayed in corresponding UI module
#' @export

SA_tisefka_forecast_mod <- function(input, output, session,tisefka,div_width = "col-xs-6 col-sm-12 col-md-6") {
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


  output$analytics_header_box <- renderUI({
    bs4Dash::box(title = "Forecasting Board",collapsible = TRUE,
                        status = "success",width = 12,
                        #-----HEADER CONTENT
                        fluidRow(
                          column(width = 3,uiOutput(session$ns("select_element")))    ,
                          column(width = 2,uiOutput(session$ns("var_granularity"))),
                          column(width = 2,uiOutput(session$ns("aggregation_metric"))),
                          column(width = 4,uiOutput(session$ns("SA_outliers"))),
                          column(width = 3,uiOutput(session$ns("forecasting_submit")))
                        ),
                 uiOutput(session$ns("non_numeric_variables_inputs"))

    )
  })



  output$analytics_config_box <- renderUI({
    bs4Dash::box(title = "Settings",collapsible = TRUE,collapsed = FALSE,
                        status = "success",width = 12,
                        #-----HEADER CONTENT
                        fluidRow(
                          column(colourpicker::colourInput(session$ns("obs_col"), "Observations", "#00AFBB"),width = 3),
                          column(colourpicker::colourInput(session$ns("fcast_col"), "Predictions", "#E1AF00"),width = 3)
                        )
    )
  })
  output$analytics_advanced_box <- renderUI({
    bs4Dash::box(title = "Advanced",collapsible = TRUE,collapsed = FALSE,
                        status = "success",width = 12,
                        #-----HEADER CONTENT
                        fluidRow(
                          column(width = 6,uiOutput(session$ns("SA_key_figure_select")))
                        )
    )
  })
  output$SA_outliers <- renderUI({
    shinyWidgets::prettySwitch(
      inputId = session$ns("SA_outliers"),
      label = "Outliers detection",
      status = "info",
      fill = TRUE)
  })
  output$forecasting_submit <- renderUI({
    req(tisefka_choices())
    shinyWidgets::actionBttn(
      inputId = session$ns("forecasting_submit"),
      style = "material-flat",
      color = "success",
      label = "Start Predictions")%>%shinyhelper::helper(type = "markdown",buttonLabel="Got it",
                                                         icon= shiny::icon("fas fa-question-circle"),
                                                         colour = "orange",
                                                         # fade   = TRUE,
                                                         size = "l",
                                                         content = "sald_forecast")
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
      ml_app_UI <- purrr::map(non_numeric_variables(), ~{
        column(width = 2, uiOutput(session$ns(paste0("non_numeric_variables_",.x))))
      })
    )
    return(ml_app_UI)
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

  #----------------
  tisefka_forecast_aqerru <- eventReactive(input$forecasting_submit,{
    req(tisefka_iheggan())
    req(target_variables())

    tisefka_forecast_aqerru <- SaldaeForecasting::Saldae_Forecaster(tisefka = tisefka_iheggan(),target_variables = target_variables(), anomaly_detection = input$SA_outliers, Saldae_model = "saldae_prophet")
  })

  tisefka_forecast <- reactive({
    req(tisefka_forecast_aqerru())
    purrr::map(.x= tisefka_forecast_aqerru(),  ~SaldaeForecasting::sbed_forecast_aqerru(.x , asurif_arzdat = NULL))
  })


  tisefka_plots <- reactive({
    plot_settings <- list()
    plot_settings[["colors_inu"]] <- c(input$obs_col,"darkgreen",input$fcast_col,"#EBCC2A")
    purrr::map(.x =names(tisefka_forecast()),~SaldaeForecasting::sekned_forecast_aqeru(fcast_df =  tisefka_forecast()[[.x]],target_variable = .x ,plot_settings = plot_settings))%>%
      stats::setNames(names(tisefka_forecast()))
  })

  tisefka_tables <- reactive({
    req(tisefka_forecast())
    return(purrr::map(.x =tisefka_forecast(),~DT::datatable(.x,extensions = c('Scroller','Buttons'), options = list(dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), deferRender = TRUE, scrollY = 200,scrollX = TRUE, scroller = TRUE)) )%>%
             stats::setNames(names(tisefka_forecast())))
  })
  output$SA_key_figure_select <- renderUI({
    req(tisefka_forecast())
    key_figures_choices <- c("Average","Sum","Maximum","Minimum","First Value","Last Value")
    shinyWidgets::pickerInput(
      inputId = session$ns("SA_key_figure_select"),
      label = "Saldae Key Numbers:",
      choices = key_figures_choices,
      multiple = FALSE
    )
  })
  #---------------------

  observeEvent(eventExpr=tisefka_tables(),handlerExpr= {
    purrr::map(names(tisefka_plots()), ~{
      output_name_plot <- paste0("tisefka_plot_", .x)
      output_name_table <- paste0("tisefka_table_", .x)
      output_name_figures <- paste0("tisefka_key_figures_", .x)
      output[[output_name_table]] <- DT::renderDataTable(tisefka_tables()[[.x]])
      output[[output_name_plot]] <- plotly::renderPlotly(tisefka_plots()[[.x]])
      output[[output_name_figures]] <- bs4Dash::renderInfoBox({
        my_title <- paste(.x,":",input$SA_key_figure_select)
        bs4Dash::infoBox(title = my_title,
                         value =my_analytics_key_values()[[.x]],color = "maroon",
                         width = 6,
                         shiny::icon("fas fa-chart-bar")
        )
      })

      #
    })
  })
  # dynamic size of the panel
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
  # display the panels
  output$graphs_ui <- renderUI({
    req(tisefka_plots())
    plots_list <- purrr::imap(tisefka_plots(), ~{
      bs4Dash::tabBox(width = SA_div_width()[1], title = .y,
                      tabPanel(icon("fas fa-chart-bar"),
                               plotly::plotlyOutput(session$ns(paste0("tisefka_plot_",.y)), height = "300px")
                      ),tabPanel(icon("table"),
                                 DT::dataTableOutput(session$ns(paste0("tisefka_table_",.y)))
                      ),tabPanel(icon("align-left"),
                                 shiny::textAreaInput(inputId = session$ns(paste0("tisefka_awal_",.y)),label = "Comments",value = "insert your comments here",width = "100%",height = "50%")
                      ),tabPanel(icon("percentage"),
                                 fluidRow(
                                   bs4Dash::infoBoxOutput(session$ns(paste0("tisefka_key_figures_",.y)), width = 8)
                                 )# fluidRow
                      )# tabPanle: percentage

      )
    })
    fluidRow(plots_list)
  })
  my_analytics_key_values <- reactive({
    my_value <- purrr::map(names(tisefka_forecast()),~key_value_calculator(tisefka = tisefka_forecast()[[.x]][,"forecast"],key_value = input$SA_key_figure_select))%>%
              stats::setNames(names(tisefka_forecast()))
  })


  analytics_output <- reactive({
    output <- list()
    output$analytics_plots    <- tisefka_plots()
    output$analytics_tisefka  <- tisefka_forecast()
    output$analytics_awal <- purrr::map(names(tisefka_plots()),~ input[[paste0("tisefka_awal_",.x)]])%>%stats::setNames(names(tisefka_plots()))
    output$analytics_key_figures <- list(key_metric = input$SA_key_figure_select,
                                         key_figures =  my_analytics_key_values())
    output$tisefka            <- tisefka_iheggan()

    output$analytics_settings <- "ulac"
    return(output)
  })
}
