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
  if (panel == "basic") {
    
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
    #Identifying the matched observations
    summary_data <- dat %>%
      group_by(id_ind) %>% #grouping by each individual
      summarize(appearances = list(V1016), #in this way, we can "paste" in a single line the interviews the person has appeared at
                disappearances = list(setdiff(1:5, unique(V1016)))) %>% #and then we can set the difference from the 1:5 vector, which will return the interviews not attended by the each one
      rowwise() %>% #perform the next commands line by line (it was doing the "unlist()" command to ALL observations of the "disappearances" column)
      mutate(missing_quarters = paste(as.character(unlist(disappearances)), collapse = " ")) #getting a column that tells us in which interviews was each person missing
    #rejoining this dataframe with our original database via left_join
    
    data_joined<- left_join(data_basic, summary_data, by= "id_ind")
    
    dat<- data_joined |> advanced_panel_1st_level()
    
    return(dat)
        

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
