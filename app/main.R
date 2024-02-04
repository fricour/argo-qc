box::use(
  shiny[bootstrapPage, moduleServer, NS],
  shiny.semantic[...]
)

box::use(
  app/view/mod_plot_focus_view,
)

# directory where BGC-ARGO data are stored
source_dir <- './scripts/raw_data/'

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    mod_plot_focus_view$ui(ns("plotly_plot"), source_dir)
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    mod_plot_focus_view$server("plotly_plot", source_dir)
  })
}
