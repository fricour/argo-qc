box::use(
  shiny[moduleServer, NS],
  bslib[...]
)

box::use(
  app/view/mod_sidebar,
  app/view/mod_plot_focus_view,
)

# directory where BGC-ARGO data are stored
source_dir <- './scripts/raw_data/'

#' @export
ui <- function(id) {
  ns <- NS(id)

  # sidebar
  page_sidebar(
    title = "BGC-ARGO Quality Control",
    sidebar = sidebar(
      mod_sidebar$ui(ns("sidebar"), source_dir)
    ),
    # main plot
    card(
      card_header("Plot selected variable"),
      mod_plot_focus_view$ui(ns("plotly_plot")),
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    user_selection <- mod_sidebar$server("sidebar", source_dir)
    mod_plot_focus_view$server("plotly_plot", source_dir, user_selection)
  })
}
