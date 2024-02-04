#' @example
#' extract_one_parameter(ncfile = 'scripts/raw_data/coriolis/6904240/profiles/BR6904240_001.nc', 'DOXY')
#
box::use(
  ncdf4[nc_open, ncvar_get, nc_close],
  tibble[tibble],
  stringr[str_split]
)

#' @export
extract_one_parameter <- function(ncfile, parameter){

  # probably need to add a check on the input file -> if B_ do that, if R_ do that and if S_ do this
  # ncfile <- 'scripts/raw_data/coriolis/6904240/profiles/R6904240_001.nc'
  # ncfile <- 'scripts/raw_data/coriolis/6904240/profiles/SR6904240_001.nc'
  # ncfile <- 'scripts/raw_data/coriolis/6904240/profiles/BR6904240_001.nc'
  #
  # # check file type
  # check_file <- unlist(str_split(ncfile, pattern = '.nc'))[1]
  # check_file <- tail(unlist(str_split(check_file, pattern = "/")), 1)
  # if(startsWith(check_file, "B")){
  #   filetype <- "bgc"
  # }else if(startsWith(check_file, "S")){
  #   filetype <- "synthetic"
  # }else{
  #   filetype <- "core"
  # }

  # open NetCDF file
  nc_data <- nc_open(ncfile, verbose = F)

  # extract metadata
  lat <- unique(ncvar_get(nc_data, 'LATITUDE'))
  lon <- unique(ncvar_get(nc_data, 'LONGITUDE'))
  juld <- unique(ncvar_get(nc_data, 'JULD'))

  # extract data
  value <- try(ncvar_get(nc = nc_data, varid = parameter))

  # check state of parameter
  if(class(value)[1] == 'try-error'){ # parameter does not exist, return empty tibble
    return(NULL)
  }

  # check for adjusted values for that parameter
  value_adjusted <- try(ncvar_get(nc_data, paste0(parameter,'_ADJUSTED')))
  if((class(value_adjusted)[1] == 'try-error') | (all(is.na(as.vector(value_adjusted))) == TRUE)){
      adjusted_exist <- FALSE
      depth <- as.vector(ncvar_get(nc_data, 'PRES')) # TODO : take the PRES ADJUSTED when possible?
      qc <- ncvar_get(nc_data, paste0(parameter,'_QC'))
      qc <- as.vector(as.numeric(unlist(strsplit(qc, split=""))))
      data <- tibble(depth = depth, value = as.vector(value), qc = qc, parameter = !! parameter, adjusted_exist = adjusted_exist, latitude = lat, longitude = lon, juld = juld)
      data <- data[!is.na(data$value),]
      return(data)
  }else{ # there are adjusted values
      adjusted_exist <- TRUE
      depth <- as.vector(ncvar_get(nc_data, 'PRES')) # TODO : take the PRES ADJUSTED when possible?
      qc <- ncvar_get(nc_data, paste0(parameter,'_ADJUSTED_QC'))
      qc <- as.vector(as.numeric(unlist(strsplit(qc, split=""))))
      data <- tibble(depth = depth, value = as.vector(value_adjusted), qc = qc, parameter = !! parameter, adjusted_exist = adjusted_exist, latitude = lat, longitude = lon, juld = juld)
      data <- data[!is.na(data$value),]
      return(data)
    }
}
