box::use(
  shiny[moduleServer, NS, selectInput, reactive, tagList, updateSelectInput, observe, h4],
  shinyhelper[helper]
)

#' @export
ui <- function(id, source_dir) {
  ns <- NS(id)

  tagList(
    h4("User selection") |> helper(type = "markdown", icon = "circle-question", content = "help_sidebar"),
      selectInput(inputId = ns("dac"), label = "DAC", choices = list.dirs(source_dir, recursive = F, full.names = F)),
      selectInput(inputId = ns("wmo"), label = "WMO", choices = ''),
      selectInput(inputId = ns("param"), label = "Parameter", choices = c('TEMP', 'PSAL', 'DOXY', 'CHLA', 'BBP700',
                                                                          'CDOM', 'CP660', 'NITRATE',
                                                                          'PH_IN_SITU_FREE', 'DOWN_IRRADIANCE380', 'DOWN_IRRADIANCE412',
                                                                          'DOWN_IRRADIANCE490', 'DOWNWELLING_PAR')),
      selectInput(inputId = ns("profile"), label = "Profile", choices = '')
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

  # return selected DAC, WMO, parameter and profile number
  return(list(
    dac = reactive(input$dac),
    wmo = reactive(input$wmo),
    param = reactive(input$param),
    profile = reactive(input$profile)
  ))

  })
}
