load_pnadc <- function(save_to = getwd(), year,
                       quarter = 1:4, panel = "advanced", raw_data = FALSE) {
  ###########
  ## Setup ##
  ###########

  param <- list()
  param$year <- year
  param$quarter <- quarter
  param$panel <- panel
  param$raw_data <- raw_data
  param$save_to <- save_to

  # parsing quarters as lists of quarters for each year

  if (!is.list(quarter)) {
    param$quarter <- rep(list(quarter), length(year))
  }

  n_quarters <- lapply(param$quarter, length)

  param$year <- purrr::map2(
    year, n_quarters,
    function(year, n) {
      rep(year, n)
    }
  )

  # two paralell vectors of years and quarter to loop over

  param$year <- unlist(param$year)
  param$quarter <- unlist(param$quarter)

  ##################
  ## Loading data ##
  ##################
  
  # store info on all panels and column names
  
  panel_list <- c()
  cnames <- NULL
  
  # download to the saving directory

  source_files <- purrr::map2(
    param$year, param$quarter,
    function(year, quarter) {
      
      base::message(
        paste0("Downloading PNADC ", year, " Q", quarter, "\n")
      )
      
      df <- PNADcIBGE::get_pnadc(
        year = year, quarter = quarter, labels = FALSE, design = FALSE
      )
      
      panel_list <<- c(panel_list, unique(df$V1014))
      cnames <<- names(df)
      
      # download each quarter to a separate file
      file_path <- file.path(
        param$save_to, paste0("pnadc", year, "_", quarter, ".rds")
      )
      readr::write_rds(df, file_path, compress = "gz")
      
      return(file_path)
    }
  )

  ## Return Raw Data

  if (param$raw_data) {
    return(paste("Raw Data saved to", param$save_to))
  }

  #################
  ## Panel Files ##
  #################

  ## Split data into panels
  
  panel_list <- unique(panel_list)

  # set up .csv file paths for each panel such as "pnadc_panel_2.csv"
  
  panel_files <- purrr::map(
    panel_list,
    function(panel) {
      file_path <- file.path(
        param$save_to, paste0("pnadc", "_panel_", panel, ".csv")
      )
      
      file_path
    }
  )
  
  # write an empty dataframe into each
  
  purrr::map(
    panel_files,
    function(path) {
      readr::write_csv(data.frame(), path, col_names = cnames)
    }
  )

  # read each of the source files, split into panels, and append
  # to their corresponding .csv files
  
  purrr::map(
    source_files,
    function(file) {
      dat <- readr::read_rds(file) %>%
        split(.$V1014)
      
      dat %>%
        purrr::imap(
          function(df, panel) {
            file_path <- file.path(
              param$save_to, paste0("pnadc", "_panel_", panel, ".csv")
            )
            readr::write_csv(df, file_path, append = TRUE)
          }
        )
    }
  )

  ##########################
  ## Panel Identification ##
  ##########################

  # read each file in panel_files and apply the identification algos
  
  purrr::map(
    panel_files,
    function(path) {
      df <- readr::read_csv(path, col_names = cnames) %>%
        build_pnadc_panel(panel = param$panel)
      
      readr::write_csv(df, path)
    }
  )
  
  ######################
  ## Data Engineering ##
  ######################
  
  ##############################
  ## Harmonize Variable Names ##
  ##############################

  ####################
  ## Returning Data ##
  ####################

  return(paste("Panel files saved to", param$save_to))
}
