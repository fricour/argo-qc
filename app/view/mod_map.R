box::use(
  shiny[moduleServer, NS, observeEvent, observe, req],
  leaflet[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  leafletOutput(ns("map"))
}

#' @export
server <- function(id, source_dir, metadata) {
  moduleServer(id, function(input, output, session){

    # "base" map
    output$map <- renderLeaflet({
      req(is.finite(metadata$lat()))
      leaflet() |>
        addTiles() |>
        addProviderTiles(providers$Esri.OceanBasemap, group = "Google Earth") |>
        addCircleMarkers(lng = metadata$lon(), lat = metadata$lat()) |>
        setView(lng = metadata$lon(), lat = metadata$lat(), zoom = 6)
    })
  })
}

