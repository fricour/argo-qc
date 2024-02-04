box::use(
  dplyr[filter, group_by],
  #ggplot2[ggplot, geom_point],
  plotly[plot_ly, layout, toWebGL],
  stats[setNames],
  tibble[tibble]
)

#' @export
get_plot_attributes <- function(parameter){

  if(parameter == 'TEMP'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1f} °C", textdigits = "%{text:,.1f} °C", type = "-"))
  }else if(parameter == 'PSAL'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1f} PSU", textdigits = "%{text:,.1f} PSU", type = "-"))
  }else if(parameter == 'DOXY'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1f} &mu;mol/kg", textdigits = "%{text:,.1f} &mu;mol/kg", type = "-"))
  }else if(parameter == 'CHLA'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1f} mg/m<sup>3</sup>", textdigits = "%{text:,.1f} mg/m<sup>3</sup>", type = "-"))
  }else if(parameter == 'BBP700'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1e} m<sup>-1</sup>", textdigits = "%{text:,.1e}  m<sup>-1</sup>", type = "-"))
  }else if(parameter == 'TRANSMITTANCE_PARTICLE_BEAM_ATTENUATION660'){
    return(list(parameter_name = 'CP600', xdigits = "%{x:,.2f} m<sup>-1</sup>", textdigits = "%{text:,.2f}  m<sup>-1</sup>", type = "-"))
  }else if(parameter == 'PH_IN_SITU_FREE'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1f}", textdigits = "%{text:,.1f}", type = "-"))
  }else if(parameter == 'NITRATE'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1f} &mu;mol/kg", textdigits = "%{text:,.1f} &mu;mol/kg", type = "-"))
  }else if(parameter == 'CDOM'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1f} ppb", textdigits = "%{text:,.1f} ppb", type = "-"))
  }else if(parameter == 'DOWN_IRRADIANCE380'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1e} W m<sup>-2</sup> nm<sup>-1</sup>", textdigits = "%{text:,.1e} W m<sup>-2</sup> nm<sup>-1</sup>", type = "log"))
  }else if(parameter == 'DOWN_IRRADIANCE412'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1e} W m<sup>-2</sup> nm<sup>-1</sup>", textdigits = "%{text:,.1e} W m<sup>-2</sup> nm<sup>-1</sup>", type = "log"))
  }else if(parameter == 'DOWN_IRRADIANCE490'){
    return(list(parameter_name = parameter, xdigits = "%{x:,.1e} W m<sup>-2</sup> nm<sup>-1</sup>", textdigits = "%{text:,.1e} W m<sup>-2</sup> nm<sup>-1</sup>", type = "log"))
  }else{
    return(list(parameter_name = parameter, xdigits = "%{x:,.1f} &mu;molQuanta m<sup>-2</sup> sec <sup>-1</sup>", textdigits = "%{text:,.1f} &mu;molQuanta m<sup>-2</sup> sec <sup>-1</sup>", type = "log"))
  }
}

#' @export
plotly_one_parameter <- function(data, parameter){

  # filter data by parameter
  tmp <- data |> filter(parameter == parameter, !is.na(depth))

  # special case for the CP660 (at this time, values needs to be converted)
  # if(parameter_name == 'TRANSMITTANCE_PARTICLE_BEAM_ATTENUATION660'){
  #   CSCdark <- ArgoDownload::c_rover_calib[ArgoDownload::c_rover_calib$WMO == wmo,]$CSCdark
  #   CSCcal <- ArgoDownload::c_rover_calib[ArgoDownload::c_rover_calib$WMO == wmo,]$CSCcal
  #   x <- 0.25
  #   tmp$value <- -log((tmp$value - CSCdark)/(CSCcal-CSCdark))/x
  # }

  # convert QC to strings for discrete colours
  tmp$qc <- as.character(tmp$qc)

  # create discrete colour palette
  pal <- c("#000000ff", "#3EA344", "#5CC163", "#FF780A", "#F31634", "#9067C6", "#000000ff", "#000000ff", "#FF5CB6", "#000000ff")
  pal <- setNames(pal, c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))

  # get plot attributes based on parameter_name
  plot_attributes <- get_plot_attributes(parameter)

  # make plotly plot
  plot <- tmp |> group_by(parameter) |> plot_ly(x = ~value, y = ~depth, type = 'scatter', mode = 'markers',
                                                         text = ~qc,
                                                         color = ~qc,
                                                         colors = pal,
                                                         legendgroup = ~qc,
                                                         hovertemplate = paste(" DEPTH: %{y:,.0f} m<br>", paste0(plot_attributes[[1]],": ",plot_attributes[[2]],"<br>"), paste0('QC: %{text:, .0f}'))) |>
    layout(xaxis = list(title = plot_attributes[[1]], type = plot_attributes[[3]]),
                   yaxis = list(title = 'DEPTH', autorange = "reversed"),
                   showlegend = FALSE) #%>% plotly::toWebGL()

  return(plot)

}
