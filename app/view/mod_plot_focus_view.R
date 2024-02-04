box::use(
  plotly[plotlyOutput, renderPlotly],
  shiny[moduleServer, NS, reactive, validate, need],
  bslib[...],
)

box::use(
  app/logic/process_argo,
  app/logic/plot_argo,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  plotlyOutput(ns("var_plot"))
}

#' @export
server <- function(id, source_dir, user_selection) {
  moduleServer(id, function(input, output, session){

    #browser()
    print(user_selection)

    data <- reactive({
      ncfile <- paste0(source_dir, user_selection$dac(), '/', user_selection$wmo(), '/profiles/', user_selection$profile())
      process_argo$extract_one_parameter(ncfile = ncfile, user_selection$param())
    })

    output$var_plot <- renderPlotly({
      validate(need(nrow(data() > 0), message = "No data."))
      plot_argo$plotly_one_parameter(data(), user_selection$param())
    })
  })
}
