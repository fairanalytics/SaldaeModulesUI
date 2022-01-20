#' Saldae Aggregation functions
#' @description Plot single variable
#' @author Farid Azouaou
#' @param aggregation_metric aggregation metric :Average , Sum, Min , Max,

SA_aggregation_funs <- function(aggregation_metric = NULL){
  if(is.null(aggregation_metric))return(NULL)
  if(aggregation_metric == "Average")return(mean)
  if(aggregation_metric == "Sum")return(sum)
  if(aggregation_metric == "Max")return(max)
  if(aggregation_metric == "Min")return(min)
  if(aggregation_metric == "Median")return(median)
}

#' Saldae Dashboard Module plotly rawdata single
#' @description Plot single variable
#' @author Farid Azouaou
#' @param tisefka raw data
#' @param variable_inu variable to plot
#' @param graph_type  plot type  "bar" "scatter" "line" "hist",..
#' @return plotly interactive object
mod_sekned_yiwet_tisefka <- function(tisefka = NULL,variable_inu=NULL,graph_type="scatter",graph_col = "#228B22"){
  y <- list(title = variable_inu)
  x <- list(title =  "date")

  graph_type <- base::tolower(graph_type)

  graph_fill <- bar_type <- graph_mode <- NULL

  if (graph_type %in% c("markers")) {
    graph_type <- "scatter"
    graph_mode <- "markers"
    RAW_col <- NULL
  }
  if (graph_type %in% c("lines", "lines+markers")) {
    graph_mode <- graph_type
    graph_type <- "scatter"
  }
  if (graph_type %in% c("filled")) {
    graph_mode <- "lines"
    graph_type <- "scatter"
    graph_fill <- "tozeroy"
  }
  if (graph_type %in% c("bar1")) {
    graph_type <- "bar"
    bar_type <- NULL
  }
  if (graph_type %in% c("density")) {
    tisefka <- stats::density(tisefka[, variable_inu, drop = T], na.rm = TRUE)
    tisefka <- data.frame(date = tisefka$x, target_variable = tisefka$y)
    colnames(tisefka) <- c("date",variable_inu)
    graph_mode <- "lines"
    graph_type <- "scatter"
    graph_fill <- "tozeroy"
    y <- list(title = "probability")
    x <- list(title = variable_inu)
  }
  if(is.null(graph_col)==FALSE)graph_col <- I(graph_col)
  tisefka%>%plotly::plot_ly(x = ~date,y = ~base::get(variable_inu),name =variable_inu ,mode = graph_mode,fill = graph_fill,type = graph_type,color = graph_col) %>%
    plotly::layout(yaxis = y,xaxis = x)%>%plotly::config(displaylogo = F)
}

#' Saldae Dashboard Module plotly rawdata
#' @description multiple plots : data x-axis date and y-axis numerical variable
#' @author Farid Azouaou
#' @param tisefka raw data
#' @param target_variables variables to plot
#' @param graph_type plot type  "bar" "scatter" "line" "hist",..
#' @return plotly interactive object
#' @export

mod_sekned_tisefka_iceqfan <- function(tisefka = NULL,target_variables= NULL,graph_type = NULL){
  plotlist_inu <- target_variables%>%purrr::map(function(x)mod_sekned_yiwet_tisefka(tisefka =tisefka ,variable_inu = x,graph_type = graph_type,graph_col = NULL))
  sub_rows <- switch (length(plotlist_inu),
    "1" = 0,"2" = 1,"3" = 2,"4" = 2,"5" = 2,"6" = 2,"7" = 3, "8" = 3, "9" = 3,"10" = 4,"11" = 4, "12" = 4
  )
  if(sub_rows >0){
    plotlist_inu <- plotly::subplot(titleX = TRUE,titleY = TRUE,
      plotlist_inu,nrows = sub_rows,margin = 0.04
    )
  }else{
    plotlist_inu<- plotlist_inu[[1]]
  }
  plotlist_inu <- plotlist_inu%>%plotly::layout(legend = list(orientation = "h", x = 0.35, y = 100))%>%
    plotly::config(displaylogo = F)
  plotlist_inu<-plotlist_inu%>%plotly::partial_bundle()
  return(plotlist_inu)
}

#' Saldae Dashboard Module UI
#' @description Saldae Dashboard module UI : display data(chart/table)
#' @author Farid Azouaou
#' @param id  server module ID
#' @param div_width dimension information about the framework(html object)
#' @param mod_title module title (default NULL)
#' @return UI module
#' @export

SA_tisefka_UI <- function(id,mod_title = NULL ,div_width = "col-xs-12 col-sm-12 col-md-12") {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("tisefka_view_box")),
    div(class = div_width,
        bs4Dash::tabBox(width = 12, title = mod_title,status = "success",
                               tabPanel(icon("fas fa-chart-bar"),
                                        plotly::plotlyOutput(ns("tisefka_plot"))
                               ),
                               tabPanel(icon("table"),
                                        DT::dataTableOutput(ns("tisefka_table"))
                               )
        )
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

SA_tisefka_mod <- function(input, output, session,tisefka) {
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


  output$tisefka_view_box <- renderUI({
    bs4Dash::box(title = "Exploration Board",collapsible = TRUE,
                        status = "success",width = 12,
                        #-----HEADER CONTENT
                        fluidRow(
                          column(width = 3,uiOutput(session$ns("select_element"))),
                          column(width = 3,uiOutput(session$ns("var_granularity"))),
                          column(width = 3,uiOutput(session$ns("aggregation_metric"))),
                          column(width = 3,uiOutput(session$ns("graph_type")))
                        ),
                 uiOutput(session$ns("non_numeric_variables_inputs"))
    )
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
  output$aggregation_metric <- renderUI({
    req(input$variable_picker)
    aggregation_choices <- c("Average","Sum","Max","Min","Median")
    shinyWidgets::pickerInput(inputId = session$ns("aggregation_metric"),
                              label = "Aggregation:",
                              multiple = FALSE,
                              choices = aggregation_choices,
                              selected = aggregation_choices[1]
    )
  })



  #--------------- chart type
  output$graph_type <- renderUI({
    req(tisefka_tizegzawin())
    plot_choices <- c(
      `<i class='fa fa-line-chart'></i>` = "Lines", `<i class='fas fa-circle'></i>` = "Markers", `<i class='fa fa-line-chart'></i>` = "Lines+Markers",
      `<i class='fas fa-chart-area'></i>` = "Filled", `<i class='fa fa-bar-chart'></i>` = "Bar", `<i class='fas fa-bell'></i>` = "Density"
    )

    shinyWidgets::radioGroupButtons(
      inputId = session$ns("graph_type"),
      label = "Select Chart Type:",
      choices = plot_choices,
      status = "info",
      justified = TRUE,
      selected = plot_choices[1]
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
      tisefka_iheggan<- tisefka_iheggan%>%dplyr::select(date,!!input$variable_picker)
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
  #----------------main chart
  output$tisefka_table <- DT::renderDataTable({
    req(input$variable_picker)
    DT::datatable(tisefka_iheggan(),extensions = c('Scroller','Buttons'), options = list(deferRender = TRUE, scrollY = 200, scroller = TRUE,dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
    # return(reactable::reactable(tisefka_tizegzawin()%>%dplyr::select(!!input$variable_picker), pagination = FALSE, highlight = TRUE, height = 250))
  })
  output$tisefka_plot <- plotly::renderPlotly({
    req(tisefka_iheggan())
    target_variables <- colnames(tisefka_iheggan())
    target_variables <- target_variables[target_variables!="date"]
      return(mod_sekned_tisefka_iceqfan(tisefka = tisefka_iheggan(),target_variables = target_variables,graph_type = input$graph_type))
  })
  #---------------
}

#------------------------ multiple-select, multiple output
#' Saldae Dashboard Module UI
#' @description Saldae Dashboard module UI : display data(chart/table) multipleoutputs
#' @author Farid Azouaou
#' @param id  server module ID
#' @param div_width dimension information about the framework(html object)
#' @param mod_title module title (default NULL)
#' @return UI module
#' @export

SA_tisefka_multiple_UI <- function(id,mod_title = NULL ,div_width = "col-xs-12 col-sm-6 col-md-8") {
  ns <- NS(id)
  fluidPage(
  uiOutput(ns("multiple_view_box")),
  uiOutput(ns("graphs_ui"))
  )
}


