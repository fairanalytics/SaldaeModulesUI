#' Saldae Dashboard Module UI
#' @description Saldae Dashboard module UI : data upload
#' @author Farid Azouaou
#' @param id  server module ID
#' @export
ghred_tisefka_UI <- function(id){
  ns <- NS(id)
fluidPage(
  # tags$head(tags$style(".progress-bar{background-color:#A6761D;}")),
  fluidRow(
    column(width = 3,
           fileInput(inputId = ns("tisefka_file"), label = "Choose CSV File",
                     multiple = FALSE,
                     accept = c("csv")
           )),
    # column(width = 3,
    #        fileInput(inputId = ns("tisefka_mapping_file"), label = "Choose mapping File",
    #                  multiple = FALSE,
    #                  accept = c("csv")
    #        )),
    column(width= 3,
           shinyWidgets::radioGroupButtons(
             inputId = ns("tisefka_tala"),
             label = "Data Source",
             choices = c(
               `<i class="fas fa-table"></i>` = "table", `<i class="fas fa-database"></i>` = "database",
               `<i class="fas fa-globe"></i>` = "web_api",`<i class="fas fa-cloud-upload-alt"></i>` = "cloud",
               `<i class="fab fa-dropbox"></i>` = "dropbox"),
             status = "info",
             justified = TRUE,
             selected = "table"
           )
    ),
    column(width = 3, uiOutput(ns("excel_tawriqt")))
  ),
  #-----------date related settings
  fluidRow(column(width = 3,
                  uiOutput(ns("date_variable"))),
           column(width = 3,
                  uiOutput(ns("tisefka_target_variables")))
           ),

  #---- Data Overview 1(key figures)
  uiOutput(ns("SA_tisefka_overview")),
  #-------- Data overview 2
  # div(class = "col-xs-12 col-sm-12 col-md-12",
      bs4Dash::tabsetPanel(
                             tabPanel(title = "Overview",icon = icon("eye"),
                                      rhandsontable::rHandsontableOutput(ns("tisefka_view"))
                             ),
                             tabPanel(title = "Description",icon = icon("table"),
                                      rhandsontable::rHandsontableOutput(ns("tisefka_description"))
                             ),
                             tabPanel(title = "Outliers",icon = icon("table"),
                                      rhandsontable::rHandsontableOutput(ns("tisefka_outliers"))
                             )
      )
  # )
)

}

#' Saldae dashboard module: upload data
#' @description upload rwa data and prepare it to be used for exploration and analysis
#' @author Farid Azouaou
#' @param input  input shinydashboard elements containing information to use data upload
#' @param output output shinydashboard element
#' @param session shiny session
#' @return output objects to be displayed in corresponding UI module
#' @export

ghred_tisefka_mod <-function(input, output, session){

  file_tasetta <- reactive({
    req(input$tisefka_file)
    tools::file_ext(input$tisefka_file$name)
  })

  excel_tiwriqin <- reactive({
   req(file_tasetta())
   if(file_tasetta()=="csv")return(NULL)
   if(file_tasetta()=="xlsx"){
     readxl::excel_sheets(input$tisefka_file$datapath)
   }
  })

  output$excel_tawriqt <- renderUI({
    req(excel_tiwriqin())
    if(is.null(excel_tiwriqin()))return(NULL)
    shinyWidgets::pickerInput(
      inputId = session$ns("excel_tawriqt"),
      label = "Choose excel sheet:",
      choices = excel_tiwriqin(),
      options = list(
        style = "btn-primary"
      )
    )
  })


  tisefka <- reactive({
    req(file_tasetta())

    tisefka <- SaldaeDataExplorer::ghred_tisefka_aqerru(input_file = input$tisefka_file , tala = file_tasetta(), tawriqt = input$excel_tawriqt)
    shinyalert::shinyalert("Data Import", text = "Data Successfully uploaded", type = "success",
                           closeOnClickOutside  = TRUE,showConfirmButton = TRUE)
    # if(!is.null(input$tisefka_mapping_file)){
    #   tisefka_mapping <-readr::read_csv(input$tisefka_mapping_file$datapath)
    #   tisefka <- tisefka%>%dplyr::left_join(tisefka_mapping)
    # }
    return(tisefka)
  })



  #------- select date variable
  dates_yellan <- reactive({
    req(tisefka())
    SaldaeDataExplorer::detect_possible_date_var(tisefka())
  })

  output$date_variable <- renderUI({
    req(tisefka())
    req(dates_yellan())
    shinyWidgets::pickerInput(
      inputId = session$ns("date_variable"),
      label = "Choose Date variable:",
      choices = dates_yellan(),
      options = list(
        style = "btn-primary"
      )
    )
  })
  #  #---------- in case of duplicated dates (grouping is required)--------
  # duplicates_yellan <- reactive({
  #   req(input$date_variable)
  #   d_ukud <- SaldaeDataExplorer::IsDate(mydate = tisefka()[1,input$date_variable])
  #   if(d_ukud==FALSE)return(FALSE)
  #   TRUE%in%(tisefka()[,input$date_variable]%>%base::duplicated())
  # })
  # output$duplicated_date_warning <- renderUI({
  #   req(duplicates_yellan())
  #   if(duplicates_yellan()==FALSE)return(NULL)
  #   my_help_text <- h5("There are duplicated dates, please aggregate or spread your data", style = "color:orange")
  #   helpText(my_help_text)
  # })

# output$handle_duplicate <- renderUI({
#     req(duplicates_yellan())
#     handle_duplicate_choices <-c("Remove","Aggregate","Spread")
#     if(duplicates_yellan()==FALSE)return(NULL)
#     shinyWidgets::pickerInput(
#       inputId = session$ns("handle_duplicate"),
#       label = "Handle Duplicates",
#       choices = handle_duplicate_choices,
#       multiple = FALSE,
#       options = list(
#         style = "btn-primary"
#       )
#     )
# })

# output$aggregate_param1 <- renderUI({
#   req(input$handle_duplicate)
#   if(input$handle_duplicate=="Remove"){return(NULL)}
#   else if(input$handle_duplicate=="Aggregate"){
#     aggregation_metric <-c("Sum","Average","Maximum","Minimum","Median")
#     shinyWidgets::pickerInput(
#       inputId = session$ns("aggregate_duplicates"),
#       label = "Aggregation Metric",
#       choices = aggregation_metric,
#       multiple = FALSE,
#       options = list(
#         style = "btn-primary"
#       )
#     )
#   }else if(input$handle_duplicate=="Spread"){
#     req(spread_yellan())
#     if(is.null(spread_yellan()))return(NULL)
#     shinyWidgets::pickerInput(
#       inputId = session$ns("tisefka_spread"),
#       label = "Spread Data",
#       choices = spread_yellan(),
#       multiple = TRUE,
#       options = list(
#         style = "btn-primary"
#       )
#     )
#   }
#
# })


# #----------duplicates : spread
# spread_yellan <- reactive({
#   req(tisefka())
#     SaldaeDataExplorer::tisefka_spread_yella(tisefka = tisefka(), date_variable = input$date_variable)
# })

output$tisefka_target_variables <- renderUI({
    req(tisefka_tizegzawin())
    req(dates_yellan())
    req(input$date_variable)
    var_class <- dlookr::get_class(tisefka_tizegzawin())%>%dplyr::filter(class=="numeric")%>%dplyr::pull(variable)%>%paste()
    shinyWidgets::pickerInput(
      inputId = session$ns("tisefka_target_variables"),
      label = "Target variables",
      choices = var_class,
      selected = var_class,
      multiple = TRUE,
      options = list(
        style = "btn-primary"
      )
    )
  })


tisefka_tizegzawin <- reactive({
  req(tisefka())
  req(dates_yellan())
  req(input$date_variable)
  tisefka_tizegzawin <- SaldaeDataExplorer::sbed_tisefka(tisefka = tisefka(), date_variable = input$date_variable)
  return(tisefka_tizegzawin)
})

#--------- display raw data
data_summary <- reactive({
  req(tisefka_tizegzawin())
  req(dates_yellan())
  req(input$date_variable)
  diag_output <- SaldaeDataExplorer::data_diagnosis_f(tisefka = tisefka_tizegzawin(), categoricals_ukud = NULL)
  return(diag_output)
})

#------- display data Overview
output$SA_tisefka_overview <- renderUI({
  req(tisefka_tizegzawin())
  fluidRow(
    bs4Dash::valueBoxOutput(session$ns("SA_overview1")),
    bs4Dash::infoBoxOutput(session$ns("SA_overview2")),
    bs4Dash::infoBoxOutput(session$ns("SA_overview3"))

  )
})
output$SA_overview1 <- bs4Dash::renderInfoBox({
  req(ts_time_units())
  bs4Dash::infoBox(title = "Time frequency",
                          value = ts_time_units()[1],subtitle = paste("Date Variable:",input$date_variable),color = "info",fill = TRUE,
                          shiny::icon("hourglass")
  )
})
output$SA_overview2 <- bs4Dash::renderInfoBox({
  req(tisefka_tizegzawin())
  info_val <- paste(ncol(tisefka_tizegzawin()),"x",nrow(tisefka_tizegzawin()))
  bs4Dash::infoBox(title = "Data Info",
                          value = info_val,subtitle = "Variables x Elements",color = "navy",fill = TRUE,
                          shiny::icon("fas fa-chart-bar")
  )
})
output$SA_overview3 <- bs4Dash::renderInfoBox({
  req(data_summary())
  val_quality <- mean(data_summary()$diagnosis[,"missing_percent",drop=F])
  val_quality <- round((1- ifelse(is.na(val_quality),0,val_quality))*100,2)

  qual_col <- "olive"
  if(val_quality < 80 )qual_col <- "olive"
  if(val_quality < 50 )qual_col <- "orange"
  if(val_quality < 30 )qual_col <- "maroon"

  bs4Dash::infoBox(title = "Data Quality",
                          value = paste(val_quality,"%"),subtitle = "not missing values",color = qual_col,fill = TRUE,
                          shiny::icon("tasks")
  )
})


ts_time_units <- reactive({
  req(tisefka_tizegzawin())
  req(dates_yellan())
  req(input$date_variable)
  return(SaldaeDataExplorer::possible_units_for_summary(time_vect = tisefka_tizegzawin()$date))
})



tisefka_overview <- reactive({
  req(data_summary())
  SaldaeDataExplorer::Handson_exploration(tisefka = tisefka_tizegzawin(), tisefka_report = data_summary())
})

#-------------------------

output$tisefka_description <- rhandsontable::renderRHandsontable({
  req(data_summary())
  return(rhandsontable::rhandsontable(data_summary()$beschreibung, rowHeaders = NULL, width = 1000, height = 300))
})

output$tisefka_outliers <- rhandsontable::renderRHandsontable({
  req(data_summary())
  return(rhandsontable::rhandsontable(data_summary()$outliers, rowHeaders = NULL, width = 1000, height = 300))
})
output$tisefka_view <- rhandsontable::renderRHandsontable({
  req(tisefka_overview())
  return(tisefka_overview())
})

non_numeric_variables <- reactive({
  req(data_summary())
  req(input$tisefka_target_variables)
  non_numeric_variables <- data_summary()$categoricals
  temp_vect<-  sapply(non_numeric_variables,function(x)x%in%input$tisefka_target_variables)
  non_numeric_variables <- non_numeric_variables[!temp_vect]
  non_numeric_variables <- non_numeric_variables[non_numeric_variables!="date"]
  return(non_numeric_variables)
})

explore_output <- reactive({
  req(tisefka_overview())
  output <- list()
  output$tisefka_tizegzawin <- dplyr::tbl_df(tisefka_tizegzawin())

  output$tisefka_overview <- tisefka_overview()
  output$ts_time_units <- ts_time_units()

  output$numeric_variables     <- input$tisefka_target_variables
  output$non_numeric_variables <- non_numeric_variables()
  output$categoricals_unique_values <- data_summary()$categoricals_unique_values
  return(output)
})

}
