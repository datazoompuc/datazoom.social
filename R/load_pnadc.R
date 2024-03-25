#' Load PNADc Data
#'
#' This function downloads PNADc data for specified years and quarters, applies panel identification algorithms, and saves the data to .rds (the individual quarter files) and .csv (the individual panel files) files for each panel.
#'
#' @param save_to The directory in which the user desires to save the downloaded files.
#' @param year The years of the PNADc the user would like to download.
#' @param quarter The quarters within those years to be downloaded.
#' @param panel Which panel algorithm to apply to this data (none, basic, or advanced). Check the README for a detailed explanation.
#' @param raw_data A command to define if the user would like to download the raw or treated data.
#'
#' @return A message indicating the successful save of panel files.
#' @import PNADcIBGE
#' @import dplyr
#' @import purrr
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' load_pnadc(
#'   save_to = "Directory/You/Would/like/to/save/the/files",
#'   year = 2016,
#'   quarter = 1:4,
#'   panel = "basic",
#'   raw_data = FALSE
#' )
#' }
#' @export

load_pnadc <- function(save_to = getwd(), year,
                       quarter = 1:4, panel = "advanced",
                       raw_data = FALSE, language = "pt") {
  # Check if PNADcIBGE namespace is already attached
  if (!"PNADcIBGE" %in% loadedNamespaces()) {
    # If not attached, attach it
    attachNamespace("PNADcIBGE") # without this, an error appears
    # I believe this is a problem with the PNADcIBGE package
    # If you run PNADcIBGE::get_pnad(...) without library(PNADcIBGE)
    # you get the same error
  }

  ###########################
  ## Bind Global Variables ##
  ###########################

  . <- NULL

  #############################
  ## Define Basic Parameters ##
  #############################

  # The param list contains the various objects that will be used as parameters for this function
  param <- list()
  param$year <- year # the years the user would like to download
  param$quarter <- quarter # the quarters within those years to be downloaded
  param$panel <- panel # which panel algorithm (none, basic or advanced) should be applied to this data, check our READ-ME for greater explanation
  param$raw_data <- raw_data # A command to define if the user would like to download the raw data from the IBGE website directly
  param$save_to <- save_to # the directory in which the user desires to save the files downloaded
  param$language <- language
  
  # Check if quarter is a list; if not, wrap it in a list and repeat it for each year
  if (!is.list(quarter)) {
    param$quarter <- rep(list(quarter), length(year))
  }

  # Calculate the lengths of quarters for each year
  n_quarters <- lapply(param$quarter, length)

  # Map2: Repeat each year based on the corresponding lengths in n_quarters, so we can have two parallel vectors of years and quarters to loop over
  param$year <- purrr::map2(
    year, n_quarters,
    function(year, n) {
      rep(year, n)
    }
  )

  # generaring these two paralell vectors of years and quarter to loop over

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
    param$year, param$quarter, # looping over the two parallel vector of years and quarters (this was previoulsy done in a "for" structure, but qwe optimized it)
    function(year, quarter) {
      base::message(
        paste0("Downloading PNADC ", year, " Q", quarter, "\n") # just generating a message so the user knows which file is being downloaded now
      )
      df <- get_pnadc(
        year = year, quarter = quarter, labels = FALSE, design = FALSE # downloading the file, design= FALSE returns to us just the dataframe with all variables in the PNADc
      )

      # turns everything into numeric
      df <- df %>%
        mutate(across(everything(), as.numeric))
      
      panel_list <<- c(panel_list, unique(df$V1014)) # registering, for every quarter, the panel's which the quarter's observations are included (every OBS is just included in one panel, but there should be OBS inserted in 2 to 3 panels for every quarter, check our READ-ME or the IBGE's website about the rotation scheme for PNADc surveys)
      #<<- stabilishing a variable inside the function that continues to exist outside the function, it is not just local to the function's current context
      cnames <<- names(df)

      file_path <- file.path(
        param$save_to, paste0("pnadc_", year, "_", quarter, ".rds") # defining the file's names to a certain format: year= 2022, quarter=3, file -> pnadc_2022_3.rds
      )
      
      # runs data cleaning if desired
      if (!param$raw_data) {
        df <- treat_pnadc(df)
      }
      
      # download each quarter to a separate file

      readr::write_rds(df, file_path, compress = "gz") # saving the file into the user's computer

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

  if (param$panel != "none") {
    ## Split data into panels

    panel_list <- unique(panel_list) # listing all the panels included in the quarters downloaded

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

    # we use the .csv files because they have a appending propriety, meaning that they can receive new information without having the older one deleted
    # for the R users, you can simply think as literally doing a rbind() into those files, but in a much more efficient way

    purrr::map(
      source_files, # source_files= the .rds files with the data that were downloaded way before in this function before
      function(file) {
        dat <- readr::read_rds(file) %>%
          split(.$V1014)

        dat %>%
          purrr::imap(
            function(df, panel) {
              file_path <- file.path(
                param$save_to, paste0("pnadc", "_panel_", panel, ".csv")
              )
              readr::write_csv(df, file_path, append = TRUE) # append=TRUE allows us to add new info without deleting the older one, as comented above
            }
          )
      }
    )

    ##########################
    ## Panel Identification ##
    ##########################

    # read each file in panel_files and apply the identification algorithms defined in the build_pnadc_panel.R

    purrr::map(
      panel_files,
      function(path) {
        df <- readr::read_csv(path, col_names = cnames) %>%
          build_pnadc_panel(panel = param$panel)

        readr::write_csv(df, path)
      }
    )
  }
  
  ####################
  ## Returning Data ##
  ####################

  return(paste("Panel files saved to", param$save_to))
}

######################
## Data Engineering ##
######################

# define a data cleaning function which is run for each quarter separately

treat_pnadc <- function(df) {
  
  # regions
  
  df <- df %>%
    dplyr::mutate(
      regiao = substr(UF, 1, 1),
      regiao = dplyr::case_match(
        regiao,
        "1" ~ "Norte",
        "2" ~ "Nordeste",
        "3" ~ "Sudeste",
        "4" ~ "Sul",
        "5" ~ "Centro-Oeste"
      )
    )
  
  # states
  
  df <- df %>%
    dplyr::mutate(
      sigla_uf = dplyr::case_match(
        UF,
        11 ~ "RO",
        12 ~ "AC",
        13 ~ "AM",
        14 ~ "RO",
        15 ~ "PA",
        16 ~ "AP",
        17 ~ "TO",
        21 ~ "MA",
        22 ~ "PI",
        23 ~ "CE",
        24 ~ "RN",
        25 ~ "PB",
        26 ~ "PE",
        27 ~ "AL",
        28 ~ "SE",
        29 ~ "BA",
        31 ~ "MG",
        32 ~ "ES",
        33 ~ "RJ",
        35 ~ "SP",
        41 ~ "PR",
        42 ~ "SC",
        43 ~ "RS",
        50 ~ "MS",
        51 ~ "MT",
        52 ~ "GO",
        53 ~ "DF"
      )
    )
  
  # sex
  
  df <- df %>%
    dplyr::mutate(
      sexo = dplyr::case_match(
        V2007,
        1 ~ "Homem",
        2 ~ "Mulher"
      )
    )
  
  # age groups
  
  df <- df %>%
    dplyr::mutate(
      faixa_idade = dplyr::case_when(
        V2009 >= 14 & V2009 <= 17 ~ "Entre 14 e 17 anos",
        V2009 >= 18 & V2009 <= 24 ~ "Entre 18 e 24 anos",
        V2009 >= 25 & V2009 <= 29 ~ "Entre 25 e 29 anos",
        V2009 >= 30 & V2009 <= 39 ~ "Entre 30 e 39 anos",
        V2009 >= 40 & V2009 <= 49 ~ "Entre 40 e 49 anos",
        V2009 >= 50 & V2009 <= 59 ~ "Entre 50 e 59 anos",
        V2009 >= 60 ~ "60 anos ou mais"
      )
    )
  
  # education levels
  
  df <- df %>%
    dplyr::mutate(
      faixa_educ = dplyr::case_match(
        VD3004,
        1 ~ "Sem instru\u00e7\u00a3o",
        2 ~ "1 a 7 anos de estudo",
        3 ~ "8 a 11 anos de estudo",
        4:6 ~ "9 a 14 anos de estudo",
        7 ~ "15 ou mais anos de estudo"
      )
    )
  
  # Labor Market definitions taken from:
  # https://github.com/datazoompuc/datazoom_labour_amazon/blob/main/Labour_Market/code/_definicoes_pnadcontinua_trimestral.do
  
  # habitual income from all occupations
  
  df <- df %>%
    dplyr::mutate(rendimento_trabalho = VD4019)
  
  # occupied status
  
  df <- df %>%
    dplyr::mutate(
      ocupado = ifelse(VD4002 == 1, 1, 0),
      desocupado = ifelse(VD4002 == 2, 1, 0)
    )
  
  # formal vs. informal
  
  df <- df %>%
    dplyr::mutate(
      formal = dplyr::case_when(
        ocupado == 1 & VD4009 %in% c(1, 3, 5, 7) ~ 1,
        ocupado == 1 & VD4009 == 9 & VD4012 == 1 ~ 1,
        .default = 0
      ),
      informal = case_when(
        ocupado == 1 & VD4009 %in% c(2, 4, 6, 10) ~ 1,
        ocupado == 1 & VD4009 == 9 & VD4012 == 2 ~ 1,
        .default = 0
      )
    )
  
  # labor force
  
  df <- df %>%
    dplyr::mutate(
      fora_forca_trab = ifelse(VD4001 == 2, 1, 0),
      forca_trab = ifelse(VD4001 == 1, 1, 0)
    )
  
  # active population
  
  df <- df %>%
    dplyr::mutate(
      pia = ifelse(V2009 >= 14, 1, 0),
      idade_de_trabalhar = ifelse(V2009 >= 15 & V2009 <= 64, 1, 0),
      pea = ocupado + desocupado
    )
  
  # unemployed
  
  df <- df %>%
    dplyr::mutate(
      desempregado = forca_trab * desocupado,
      desalentado = ifelse(VD4005 == 1, 1, 0)
    )
  
  # neet
  
  df <- df %>%
    dplyr::mutate(
      nem_nem = dplyr::case_when(
        desocupado == 1 & forca_trab == 1 & V3002 == 2 & 
          (V4074 != 6 | is.na(V4074)) & (V4074A != 8 | is.na(V4074A)) ~ 1,
        fora_forca_trab == 1 & V3002 == 2 &
          (V4074 != 6 | is.na(V4074)) & (V4074A != 8 | is.na(V4074A)) ~ 1,
        .default = 0
      )
    )
  
  return(df)
}
