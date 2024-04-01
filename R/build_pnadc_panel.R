#' Build PNADc Panel
#'
#' This function is designed to build a panel dataset from PNADC data based on the chosen panel algorithm. It performs basic and, if specified, advanced identification steps to create household and individual identifiers for panel construction.
#'
#' @param dat The current PNAD observations.
#' @param panel The type of panelling transformation you wish to apply to \code{dat}. Use "none" for no paneling, "basic" for basic paneling, and "advanced" for advanced paneling.
#' 
#' @return A modified dataset with added identifiers for household (\code{id_dom}) and individual (\code{id_ind} or \code{id_rs}) based on the chosen panel algorithm.
#' 
#' 
#' @examples
#' \dontrun{
#' # Example usage:
#' data <- read.csv("path/to/PNADC_data.csv")
#' panel_data <- build_pnadc_panel(dat = data, panel = "basic")
#' }
#' 
#' @references 
#' Ribas, Rafael Perez, and Sergei Suarez Dillon Soares. Sobre o painel da Pesquisa Mensal de Emprego (PME) do IBGE. No. 1348. Texto para discussão, 2008.
#' Data Zoom (2023). Data Zoom: Simplifying Access To Brazilian Microdata.
#'
#' @author Data Zoom
build_pnadc_panel <- function(dat, panel) {
  
  ###########################
  ## Bind Global Variables ##
  ###########################
  
  UPA <- V1008 <- V1014 <- id_dom <- UF <- V1023 <- V20082 <- V20081 <- NULL
  V2008 <- V2007 <- id_ind <- V2003 <- V1016 <- appearances <- V1016 <- NULL
  id_rs <- NULL
  
  #############################
  ## Define Basic Parameters ##
  #############################
  
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
        .by = c(UF, UPA, V1008, V1014)
      )
    
    # Individual id combines the household id with UF, V1023, V2007, and date of birth( V20082, V20081, V2008), creating an unique number for every combination of those variables, all through the function cur_group_id
    dat <- dat %>%
      dplyr::mutate(
        id_ind = dplyr::cur_group_id(),
        .by = c(id_dom, V1023, V20082, V20081, V2008, V2007)
      )
    
    # identifying matched observations
    
    dat <- dat %>%
      dplyr::mutate(
        num_quarters = dplyr::n(),
        .by = id_ind
      ) # counts number of times that each id appears
    
    dat <- dat %>%
      dplyr::mutate(
        matched_basic = dplyr::case_when(
          num_quarters == 5 ~ 1,
          .default = 0
        )
      )
  }
  
  #############################
  ## Advanced Identification ##
  #############################
  
  ## Stage 1:
  
  if (!(panel %in% c("none", "basic"))) {
    m<- max(dat$id_ind)
    
    # advanced identification is only run on previously unmatched individuals
    
    dat <- dat %>%
      dplyr::mutate(
        id_rs = dplyr::case_when(
          matched_basic == 1 ~ id_ind,
          as.numeric(V2005) %in% c(1, 2, 3) ~ dplyr::cur_group_id()+m,
          as.numeric(V2005) %in% c(4, 5) & as.numeric(V2009) >= 25 ~ dplyr::cur_group_id()+m,
          .default = id_ind
        ),
        .by = c(id_dom, V20081, V2008, V2003)
      )
    
    
    # identifying new matched observations
    
    dat <- dat %>%
      dplyr::mutate(
        num_quarters = dplyr::n(),
        .by = id_rs
      ) # counts number of times that each id appears
    
    dat <- dat %>%
      dplyr::mutate(
        matched_adv_1 = dplyr::case_when(
          num_quarters == 5 ~ 1,
          .default = 0
        )
      )
  }
  
  #################
  ## Return Data ##
  #################
  
  # Handle unidentifiable observations due to missing values
  dat <- dat %>% dplyr::mutate(
    id_ind = dplyr::case_when(
      V2008 == "99" | V20081 == "99" | V20082 == "9999" ~ NA,
      .default = id_ind
    ),
    id_rs = dplyr::case_when(
      V2008 == "99" | V20081 == "99" | V20082 == "9999" ~ NA,
      .default = id_rs
    ),
  )
  
  dat <- dat %>% dplyr::mutate(
    id_rs = dplyr::case_when(
      V2008 == "99" | V20081 == "99" | V20082 == "9999" ~ NA,
      .default = id_rs
    )
  )
  
  # Return the modified dataset
  return(dat)
}
