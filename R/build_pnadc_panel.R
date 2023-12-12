#' Build PNADc Panel
#'
#' This function is designed to build a panel dataset from PNADC data based on the chosen panel algorithm. It performs basic and, if specified, advanced identification steps to create household and individual identifiers for panel construction.
#'
#' @param dat The current PNAD observations.
#' @param panel The type of panelling transformation you wish to apply to \code{dat}. Use "none" for no paneling, "basic" for basic paneling, and "advanced" for advanced paneling.
#' 
#' @return A modified dataset with added identifiers for household (\code{id_dom}) and individual (\code{id_ind}) based on the chosen panel algorithm.
#' 
#' 
#' @examples
#' \dontrun{
#' # Example usage:
#' data <- read.csv("path/to/PNADC_data.csv")
#' panel_data <- build_pnadc_panel(dat = data, panel = "basic")
#' }
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr cur_group_id
#' @importFrom dplyr case_when
#' @importFrom data.table rbindlist
#' @importFrom magrittr %>%
#' @references 
#' Ribas, Rafael Perez, and Sergei Suarez Dillon Soares. Sobre o painel da Pesquisa Mensal de Emprego (PME) do IBGE. No. 1348. Texto para discussão, 2008.
#' Data Zoom (2023). Data Zoom: Simplifying Access To Brazilian Microdata.
#'
#' @author Data Zoom
build_pnadc_panel <- function(dat, panel) {
  
  # Check if the panel type is 'none'; if so, return the original data
  if (panel == "none") {
    return(dat)
  }
  
  ##########################
  ## Basic Identification ##
  ##########################
  
  # If the panel type is not 'none', perform basic identification steps
  if (panel != "none") {
    
    # Household identifier combines UPA, V1008, and V1014, creating an unique number for every combination of those variables, all through the function cur_group_id
    dat <- dat %>%
      dplyr::mutate(
        id_dom = dplyr::cur_group_id(),
        .by = c(UPA, V1008, V1014)
      )
    
    # Individual id combines the household id with UF, V1023, V2007, and date of birth( V20082, V20081, V2008), creating an unique number for every combination of those variables, all through the function cur_group_id
    dat <- dat %>%
      dplyr::mutate(
        id_ind = dplyr::cur_group_id(),
        .by = c(id_dom, UF, V1023, V20082, V20081, V2008, V2007)
      )
  }
  
  #############################
  ## Advanced Identification ##
  #############################
  
  # Placeholder for advanced identification steps (currently empty)
  if (panel == "advanced") {
    # Additional steps for advanced panel identification can be added here
  }
  
  #################
  ## Return Data ##
  #################
  
  # Handle unidentifiable observations due to missing values
  dat <- dat %>% dplyr::mutate(
    id_ind = dplyr::case_when(
      V2008 != "99" | V20081 != "99" | V20082 != "9999" ~ NA,
      .default = id_ind
    )
  )
  
  # Return the modified dataset
  return(dat)
}
