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
#' )}
#' @export

load_pnadc <- function(save_to = getwd(), year,
                       quarter = 1:4, panel = "advanced", raw_data = FALSE) {
  
  
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
  param$year <- year #the years the user would like to download
  param$quarter <- quarter #the quarters within those years to be downloaded
  param$panel <- panel # which panel algorithm (none, basic or advanced) should be applied to this data, check our READ-ME for greater explanation
  param$raw_data <- raw_data #A command to define if the user would like to download the raw data from the IBGE website directly
  param$save_to <- save_to #the directory in which the user desires to save the files downloaded

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
    param$year, param$quarter, #looping over the two parallel vector of years and quarters (this was previoulsy done in a "for" structure, but qwe optimized it)
    function(year, quarter) {
      
      base::message(
        paste0("Downloading PNADC ", year, " Q", quarter, "\n") #just generating a message so the user knows which fiule is being downloaded now
      )
      
      df <- get_pnadc(
        year = year, quarter = quarter, labels = FALSE, design = FALSE #downloading the file, design= FALSE returns to us just the dataframe with all variables in the PNADc
      )
      
      panel_list <<- c(panel_list, unique(df$V1014)) # registering, for every quarter, the panel's which the quarter's observations are included (every OBS is just included in one panel, but there should be OBS inserted in 2 to 3 panels for every quarter, check our READ-ME or the IBGE's website about the rotation scheme for PNADc surveys)
      #<<- stabilishing a variable inside the function that continues to exist outside the function, it is not just local to the function's current context
      cnames <<- names(df)
      
      file_path <- file.path(
        param$save_to, paste0("pnadc_", year, "_", quarter, ".rds") #defining the file's names to a certain format: year= 2022, quarter=3, file -> pnadc_2022_3.rds
      )
      
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

  ## Split data into panels
  
  panel_list <- unique(panel_list) #listing all the panels included in the quarters downloaded

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
            readr::write_csv(df, file_path, append = TRUE) #append=TRUE allows us to add new info without deleting the older one, as comented above
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

