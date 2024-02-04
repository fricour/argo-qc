# app/view/table.R

box::use(
  plotly[plotlyOutput, renderPlotly],
  shiny[moduleServer, NS, selectInput, reactive, updateSelectInput, observe, validate, need, tags],
  bslib[...],
)

box::use(
  app/logic/process_argo,
  app/logic/plot_argo,
)

#' @export
ui <- function(id, source_dir) {
  ns <- NS(id)

  page_sidebar(
    title = "BGC-ARGO Quality Control",
    sidebar = sidebar(
      selectInput(inputId = ns("dac"), label = "DAC", choices = list.dirs(source_dir, recursive = F, full.names = F)),
      selectInput(inputId = ns("wmo"), label = "WMO", choices = ''),
      selectInput(inputId = ns("param"), label = "Parameter", choices = c('PSAL', 'DOXY', 'CHLA', 'BBP700',
                                                                          'CDOM', 'TRANSMITTANCE_PARTICLE_BEAM_ATTENUATION660', 'NITRATE',
                                                                          'PH_IN_SITU_FREE', 'DOWN_IRRADIANCE380', 'DOWN_IRRADIANCE412',
                                                                          'DOWN_IRRADIANCE490', 'DOWNWELLING_PAR')),
      selectInput(inputId = ns("profile"), label = "profile", choices = '')
    ),
    card(
      card_header("PLOT"),
      plotlyOutput(ns("var_plot"))
    )
  )
}

#' @export
server <- function(id, source_dir) {
  moduleServer(id, function(input, output, session){

    observe({
      # update WMO
      updateSelectInput(session, "wmo", choices = list.dirs(paste0(source_dir, input$dac), recursive = F, full.names = F))
      # update profile
      updateSelectInput(session, "profile", choices = list.files(paste0(source_dir, input$dac, '/', input$wmo, '/profiles'), recursive = F, full.names = F))
    })

    data <- reactive({
      ncfile <- paste0(source_dir,input$dac, '/', input$wmo, '/profiles/', input$profile)
      process_argo$extract_one_parameter(ncfile = ncfile, input$param)
    })

    output$var_plot <- renderPlotly({
      validate(need(nrow(data() > 0), message = "No data."))
      plot_argo$plotly_one_parameter(data(), input$param)
    })
  })
}
