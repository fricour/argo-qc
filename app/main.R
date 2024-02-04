box::use(
  shiny[moduleServer, NS],
  bslib[...]
)

box::use(
  app/view/mod_sidebar,
  app/view/mod_plot_focus_view,
  app/view/mod_map
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
    layout_columns(
      col_widths = c(9,3),
      row_heights = c(2,1),
      card(
        card_header("Plot selected variable"),
        mod_plot_focus_view$ui(ns("plotly_plot")),
      ),
      card(
        card_header("Profile position"),
        mod_map$ui(ns("map_profile"))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # sidebar module
    user_selection <- mod_sidebar$server("sidebar", source_dir)

    # single profile module
    metadata <- mod_plot_focus_view$server("plotly_plot", source_dir, user_selection)

    # map
    mod_map$server("map_profile", source_dir, metadata)
  })
}
