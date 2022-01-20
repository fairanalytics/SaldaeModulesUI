#' reporting_board_module UI Function
#'
#' @description A shiny Module.
#' @author Farid Azouaou
#' @param id,input,output,session Internal parameters for {shiny}.
#' @export
SA_reporting_UI <- function(id){
  ns <- NS(id)
  fluidPage(
    # report board
    uiOutput(ns("report_header_box")),
    #report additional content
    uiOutput(ns("report_body_box")),
    # generate Report
    uiOutput(ns("report_output_box"))
  )

}



#' reporting_board_module Server Function
#'
#' @description A shiny Module.
#' @author Farid Azouaou
#' @param id,input,output,session Internal parameters for {shiny}.
#' @export

SA_reporting_mod <- function(input, output, session,tisefka_list){


  output$report_header_box <- renderUI({
    bs4Dash::box(title = "Reporting Document Information",collapsible = TRUE,
                        status = "success",width = 12,
                        #-----HEADER CONTENT
                        fluidRow(
                          column(width = 3,
                                 uiOutput(session$ns("report_title"))),
                          column(width = 3,
                                 uiOutput(session$ns("report_author"))),
                          column(width = 3,
                                 uiOutput(session$ns("report_time"))),
                          column(width = 3,
                                 uiOutput(session$ns("report_type")))
                        )
                    )
  })


  output$report_title <- renderUI({
    textInput(session$ns("report_title"), label = "Document Title:", value = "Saldae Analytics Daily report")
  })
  output$report_author <- renderUI({
    textInput(session$ns("report_author"), label = "Author:", value = "Farid")
  })

  output$report_time <- renderUI({
    textInput(session$ns("report_time"), label = "Creation Time:", value = format(Sys.time(),"%Y-%m-%d %H:%M"))
  })


  output$report_type <- renderUI({
    selectInput(session$ns("report_type"), label = "Document Type:",
                choices = c("html","html_pretty","presentation","dashboard","powerpoint"))%>%
      shinyhelper::helper(type = "markdown",buttonLabel="Got it",
                          icon= shiny::icon("fas fa-question-circle"),
                          colour = "orange",
                          # fade   = TRUE,
                          size = "l",content = "sald_report")
  })




output$report_body_box <- renderUI({
  bs4Dash::box(title = "Reporting Document Content",collapsible = TRUE,
                      status = "success",width = 12,
                      #-----HEADER CONTENT
                      fluidRow(
                        column(width = 12,
                               textAreaInput(inputId =session$ns("report_asezwer"), label = "Introduction/Summary",
                                             value = "", width = "800px",height = "150px"))
                      )
  )
})


output$report_output_box <- renderUI({
  bs4Dash::box(title = "Report Generation",collapsible = TRUE,
                      status = "success",width = 12,
                      #-----HEADER CONTENT
                      fluidRow(
                        column(width = 3,uiOutput(session$ns("generate_SA_report"))),
                        column(width = 3,uiOutput(session$ns("export_SA_report"))),
                        column(width = 3,uiOutput(session$ns("share_SA_report")))

                      )
  )
})


saldae_report_header <- reactive({
   header_list <- list(
    title   = input$report_title,
    author  = input$report_author,
    date    = input$report_time,
    output  = list(paste0(input$report_type,"_document")),
    params = list(
      "sald_explor_chart"         = "NA",
      "sald_predict_chart"        = "NA",
      "sald_tisefka"              = "NA",
      "sald_predict_values"       = "NA",
      "sald_predict_comment"      = "NA",
      "sald_introduction"         = "NA",
      "sald_report_asezwer"       = "NA"
    )
   )
   return(header_list)
})

output$generate_SA_report <- renderUI({
  req(tisefka_list())
  shinyWidgets::actionBttn(inputId = session$ns("generate_SA_report"),
                           icon = icon("fas fa-globe"),
                           color = "success",
                           label = "Publish Report",style = "material-flat")
})
output$export_SA_report <- renderUI({
  req(tisefka_list())
  shinyWidgets::downloadBttn(outputId = session$ns("export_SA_report2"),
                             icon = icon("fas fa-file-download"),
                             color = "success",
                           label = "Download Report",style = "material-flat")
})
output$share_SA_report <- renderUI({
  req(tisefka_list())
  shinyWidgets::actionBttn(inputId = session$ns("share_SA_report"),
                             icon = icon("fas fa-share-alt"),
                             color = "success",
                             label = "Share Report",style = "material-flat")
})



SA_report_output <- reactive({
  rmd_files  <- c("favicon.ico","saldae_logo.png","Saldae_base_chunk_dashboard.Rmd","Saldae_base_chunk_html.Rmd",
                  "Saldae_base_chunk_html_pretty.Rmd","Saldae_base_chunk_presentation.Rmd",
                  "Saldae_base_chunk_powerpoint.Rmd","Saldae_variable_explorer_block.Rmd",
                  "Saldae_reporting_menu_dashboard.Rmd","Saldae_reporting_menu_html.Rmd",
                  "Saldae_reporting_menu_html_pretty.Rmd","Saldae_reporting_menu_powerpoint.Rmd",
                  "Saldae_reporting_menu_presentation.Rmd")
  rmd_files_package  <- system.file(rmd_files, package = "SaldaeReporting")
  dir.create("./reporting")
  file.copy(from = rmd_files_package,to = paste0("./reporting/",rmd_files),overwrite = TRUE)
})


output$export_SA_report2 <- downloadHandler(
  filename = function(){
    paste0("Saldae_Report_", format(Sys.time(), "%Y%m%d_%H%M"), ".html")
  },
  content = function(file) {
    SA_report_output()
    SaldaeReporting::SALDAE_reporting_engine(dash_aqerruy = saldae_report_header(),output_file = file ,dash_asezwer = input$report_asezwer,dash_ul = tisefka_list())
  }
)


observeEvent(input$generate_SA_report,{
  SA_report_output()
  SaldaeReporting::SALDAE_reporting_engine(dash_aqerruy = saldae_report_header(),output_file = input$report_title,dash_asezwer = input$report_asezwer,dash_ul = tisefka_list(), publish_report = TRUE)
  #---------report successfully generated
  shinyalert::shinyalert("Ready!", tags$a(href="https://fairanalytics.github.io/saldae_dashboard/", "Report Available here"), type = "success")
})


}
