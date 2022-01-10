#' Saldae Map based  Graph
#' @author Farid Azouaou
#' @param tisefka  data containing target region infrmation
#' @return leaflet map object
#' @export


Saldae_leaflet_amedya <- function(tisefka = NULL){
  tisefka <- tisefka %>%
    dplyr::mutate(mag.level = cut(mag,c(3,4,5,6),
                                  labels = c('>3 & <=4', '>4 & <=5', '>5 & <=6')))

  tisefka.df <- split(tisefka, tisefka$mag.level)

  l <- leaflet::leaflet() %>% leaflet::addTiles()

  names(tisefka.df) %>%
    purrr::walk( function(df) {
      l <<- l %>%
        leaflet::addMarkers(data=tisefka.df[[df]],
                            lng=~long, lat=~lat,
                            label=~as.character(mag),
                            popup=~as.character(mag),
                            group = df,
                            clusterOptions = leaflet::markerClusterOptions(removeOutsideVisibleBounds = F),
                            labelOptions = leaflet::labelOptions(noHide = F,
                                                                 direction = 'auto'))
    })

  l %>%
    leaflet::addLayersControl(
      overlayGroups = names(tisefka.df),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
return(l)
}


#' Saldae Dashboard Module UI (Map)
#' @description Saldae Dashboard module UI : Map  View
#' @author Farid Azouaou
#' @param id  server module ID
#' @param mod_title module title (default NULL)
#' @return UI module
#' @export

Saldae_amadal_UI <- function(id = NULL,mod_title = NULL){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      uiOutput(ns("amadal_box"))
    )
  )
}

#' Saldae Dashboard Module Server Map
#' @description Saldae Dashboard module SERVER : render and generate leaflet map based data
#' @author Farid Azouaou
#' @param input  input shinydashboard elements containing information to use for output generation
#' @param output output shinydashboard element
#' @param session shiny session
#' @param tisefka reactive object containing data
#' @param div_width dimension information about the framework(html object)
#' @return output objects to be displayed in corresponding UI module
#' @export
Saldae_amadal_mod <- function(input, output, session,tisefka,div_width = "col-xs-6 col-sm-12 col-md-6"){

  tisefka_tizegzawin <- reactive({
    # tisefka()$tisefka_tizegzawin
    quakes
  })

  output$amadal_box <- renderUI({
    bs4Dash::box(title = "Earth Quake Infrmation:",
                                width = 12,
                                collapsible = TRUE,

                                #-------content
                                leaflet::leafletOutput(session$ns("amadal"))
                                )
  })

  output$amadal <- leaflet::renderLeaflet({
    req(tisefka_tizegzawin())
    taftilt_plot_f <- Saldae_leaflet_amedya(tisefka = tisefka_tizegzawin())
  })
}





