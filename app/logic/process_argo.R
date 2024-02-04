#' @example
#' extract_one_parameter(ncfile = 'scripts/raw_data/coriolis/6904240/profiles/BR6904240_001.nc', 'DOXY')
#
box::use(
  ncdf4[nc_open, ncvar_get, nc_close],
  tibble[tibble],
)

#' @export
extract_one_parameter <- function(ncfile, parameter){

  # open NetCDF file
  nc_data <- nc_open(ncfile, verbose = F)

  # extract parameter
  value <- try(ncdf4::ncvar_get(nc = nc_data, varid = parameter))

  # check state of parameter
  if(class(value)[1] == 'try-error'){ # parameter does not exist, return empty tibble
    return(NULL)
  }else if(is.na(ncol(value))){ # parameter exists in a single column
    depth <- ncvar_get(nc_data, 'PRES') # TODO : take the PRES ADJUSTED when possible?

  }else{ # [CASE 1] parameter exists in multiple columns but we take the 'longest' one (the small column is associated with very shallow depths an usually less than 5 values)
    # [CASE 2] there are just simply one column related to the selected parameter among other columns filled with NA
    # find the good column to extract the data from
    index_column <- which(!is.na(value)[1,]) # if CASE 2
    if(length(index_column) > 1){index_column <- 1} # if CASE 1, 1 is FIXED (longest column seems to always be the first one)

    # get value based on column index
    value <- value[, index_column]

    # check for adjusted values for that parameter
    value_adjusted <- try(ncvar_get(nc_data, paste0(parameter,'_ADJUSTED')))
    if(class(value_adjusted)[1] == 'try-error'){ # ADJUSTED field does not exist in NetCDF file for now
      depth <- ncvar_get(nc_data, 'PRES') # TODO : take the PRES ADJUSTED when possible?
      depth <- depth[,index_column]
      qc <- ncvar_get(nc_data, paste0(parameter,'_QC'))
      qc <- as.numeric(unlist(strsplit(qc[index_column],split="")))
      return(tibble(depth = depth, value = value, qc = qc, parameter = !! parameter))
    }else if(all(is.na(value_adjusted[1,])) == TRUE){ # if TRUE, there are no adjusted values
      depth <- ncvar_get(nc_data, 'PRES') # TODO : take the PRES ADJUSTED when possible?
      depth <- depth[,index_column]
      qc <- ncvar_get(nc_data, paste0(parameter,'_QC'))
      qc <- as.numeric(unlist(strsplit(qc[index_column],split="")))
      adjusted_exist <- FALSE
      return(tibble(depth = depth, value = value, qc = qc, parameter = !! parameter, adjusted_exist = adjusted_exist))
    }else{ # there are adjusted values
      depth <- ncvar_get(nc_data, 'PRES') # TODO : take the PRES ADJUSTED when possible?
      depth <- depth[,index_column]
      qc <- ncvar_get(nc_data, paste0(parameter,'_ADJUSTED_QC'))
      qc <- as.numeric(unlist(strsplit(qc[index_column],split="")))
      adjusted_exist <- TRUE
      return(tibble(depth = depth, value = value_adjusted[,index_column], qc = qc, parameter = !! parameter, adjusted_exist = adjusted_exist))
    }
  }
}
