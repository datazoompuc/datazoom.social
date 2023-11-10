load_pnadc <- function(source = "download", year, quarter = 1:4, panel = "advanced", raw_data = FALSE) {
  ###########
  ## Setup ##
  ###########

  param <- list()
  param$source <- source
  param$year <- year
  param$quarter <- quarter
  param$panel <- panel
  param$raw_data <- raw_data

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

  # if the user feeds in a list of dataframes, we read those

  if (is.list(source)) {
    dat <- param$source
  }

  # otherwise, download using download_pnadc

  else {
    dat <- purrr::map2(
      param$year, param$quarter,
      function(year, quarter) {
        PNADcIBGE::get_pnadc(year = year, quarter = quarter, labels = TRUE, design = FALSE)
      }
    )
  }

  ## Return Raw Data

  if (param$raw_data) {
    return(dat)
  }

  ######################
  ## Data Engineering ##
  ######################

  ## Sort dataframes into panels

  dat <- dat %>%
    purrr::map(~ split(., .$V1014)) # splitting each df by panel

  dat <- dat %>%
    purrr::transpose() %>% # clumps all equal panels together
    purrr::map(dplyr::bind_rows)

  ## Everything in clean_panel

  # add NA values

  dat <- dat %>%
    purrr::map(
      function(df) {
        df %>%
          dplyr::mutate(
            dplyr::across(
              c(V2008, V20081),
              ~ dplyr::case_match(
                ., "99" ~ NA,
                .default = .
              )
            ),
            dplyr::across(
              c(V20082),
              ~ dplyr::case_match(
                ., "9999" ~ NA,
                .default = .
              )
            )
          )
      }
    )

  ##########################
  ## Panel Identification ##
  ##########################
  
  if (panel != "none") {
    
    # creating numeric variables to build identifiers
    
    dat <- dat %>%
      purrr::map(
        function(df) {
          df %>%
            dplyr::mutate(
              dplyr::across(c(UPA, V1008, V1014, UF, V1023, id_dom, V20082, V20081, V2008, V2007),
                            ~ as.numeric(as.factor(.)),
                            .names = "n_{.col}")
            )
        }
      )
    
    dat <- dat %>%
      purrr::map(
        function(df) {
          df %>% dplyr::mutate(
            id_dom = paste0(n_UPA, n_V1008, n_V1014),
            id_ind = paste0(n_UF, n_V1023, n_id_dom, n_V20082, n_V20081, n_V2008, n_V2007)
          )
        }
      )
  }

  ## Calls panel functions

  ##############################
  ## Harmonize Variable Names ##
  ##############################

  ## Just rename, maybe relocate

  dat_mod <- dat

  ####################
  ## Returning Data ##
  ####################

  return(dat_mod)
}
