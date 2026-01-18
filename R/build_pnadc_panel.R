#' Build PNADc Panel
#'
#' This function builds a panel dataset from PNADC data, indentifying households and individuals
#'
#' @param dat Data frame with PNADC data, sorted into a single panel.
#' @param panel A \code{character} with the type of panel identification. Use "none" for no paneling, "basic" for basic paneling, and "advanced" for advanced paneling.
#'
#' @return A modified dataset with added identifiers for household (\code{id_dom}) and individual (\code{id_ind} or \code{id_rs}) based on the chosen panel algorithm.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' data <- fread("path/to/PNADC_data.csv")
#' panel_data <- build_pnadc_panel(dat = data, panel = "basic")
#' }
#'
#' @references
#' Ribas, Rafael Perez, and Sergei Suarez Dillon Soares. Sobre o painel da Pesquisa Mensal de Emprego (PME) do IBGE. No. 1348. Texto para discussão, 2008.
#' Data Zoom (2023). Data Zoom: Simplifying Access To Brazilian Microdata.
#'
#' @author Data Zoom
#' @export
build_pnadc_panel <- function(dat, panel) {
  ###########################
  ## Bind Global Variables ##
  ###########################

  UPA <- V1008 <- V1014 <- id_dom <- UF <- V20082 <- V20081 <- rs_valid <-  unmatched_basic <- NULL
  V2008 <- V2007 <- id_ind <- V2003 <- V1016 <- appearances <- V1016 <- id_rs <- unmatched_adv <- NULL

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

    # Individual id combines the household id with UF, V2007, and date of birth( V20082, V20081, V2008), creating an unique number for every combination of those variables, all through the function cur_group_id
    dat <- dat %>%
      dplyr::mutate(
        id_ind = dplyr::cur_group_id(),
        .by = c(id_dom, V20082, V20081, V2008, V2007)
      )
    
    # twin removal
    
    dat <- dat %>%
      dplyr::mutate(
        num_appearances = dplyr::n(),
        .by = c("id_ind", "Ano", "Trimestre")
      ) %>% # counts number of times that each id_ind appears
      dplyr::mutate(
        id_ind = dplyr::case_when(
          num_appearances != 1 ~ NA,
          .default = id_ind
        ))
    
    # missing values
    
    dat <- dat %>% dplyr::mutate(
      id_ind = dplyr::case_when(
        V2008 == "99" | V20081 == "99" | V20082 == "9999" ~ NA,
        .default = id_ind
      )
    )
  }

  #############################
  ## Advanced Identification ##
  #############################

  ## Stage 1:

  if (!(panel %in% c("none", "basic"))) {
    m <- max(dat$id_ind) # to avoid overlap between id numbers
    # id_rs are always higher numbers than id_ind

    # advanced identification is only run on previously unmatched individuals

    dat <- dat %>%
      dplyr::mutate(
        id_rs = dplyr::cur_group_id() + m,
        .by = c(id_dom, V20081, V2008, V2003)
      )
    
    # twin removal
    
    dat <- dat %>%
      dplyr::mutate(
        num_appearances = dplyr::n(),
        .by = c("id_rs", "Ano", "Trimestre")
      ) %>% # counts number of times that each id_ind appears
      dplyr::mutate(
        id_rs = dplyr::case_when(
          num_appearances != 1 ~ NA,
          .default = id_rs
        ))
    
    # missing values
    
    dat <- dat %>% dplyr::mutate(
      id_rs = dplyr::case_when(
        V2008 == "99" | V20081 == "99" ~ NA,
        .default = id_rs
      )
    )
    
    # identifying unmatched observations
    # those whose basic id has no matching pair anywhere
    # also see if advanced id could find a pair
    
    dat <- dat %>%
      dplyr::mutate(
        unmatched_basic = (dplyr::n() == 1),
        .by = c("id_ind")
      ) %>%
      dplyr::mutate(
        unmatched_adv = (dplyr::n() == 1),
        .by = c("id_rs")
      )
    
    # if basic couldn't match, but advanced could,
    # advanced takes over
    
    dat <- dat %>%
      dplyr::mutate(
        id_rs = ifelse(
          unmatched_basic & !unmatched_adv,
          id_rs,
          id_ind
        )
      )
  }
  
  ##########################
  ## Pasting panel number ##
  ##########################

  # to avoid overlap when binding more than one panel (all ids are just counts from 1, ..., N)

  # basic panel
  if (panel != "none") {
    
    dat$id_ind <- paste0(as.hexmode(dat$V1014), as.hexmode(dat$id_ind))

  }
  
  # advanced panel
  if (!(panel %in% c("none", "basic"))) {
    
    dat$id_rs <- paste0(as.hexmode(dat$V1014), as.hexmode(dat$id_rs))
    
  }

  #################
  ## Return Data ##
  #################

  # Return the modified dataset
  return(dat)
}
