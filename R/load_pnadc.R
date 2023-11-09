load_pnadc <- function(source = "download", year, quarter = 1:4, panel = "advanced", raw_data = FALSE) {
  
  #########
  # Setup #
  #########
  
  param <- list()
  param$source <- source
  param$year <- year
  param$panel <- panel
  param$raw_data <- raw_data
  
  ################
  # Loading data #
  ################
  
  # if download is chosen, download using download_pnadc
  
  if (any(param$source == "download")) {
    dat <- download_pnadc(
      year = param$year,
      quarter = param$quarter
    )
  }
  
  # otherwise, the user feeds in a previously downloaded list of dataframes
  
  else {
    dat <- param$source
  }
  
  ###################
  # Return Raw Data #
  ###################
  
  if (param$raw_data) {
    return(dat)
  }
  
  ####################
  # Data Engineering #
  ####################
  
  
  
  ####################
}