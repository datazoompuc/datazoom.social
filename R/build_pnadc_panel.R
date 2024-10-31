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
build_pnadc_panel <- function(dat, panel) {
  ###########################
  ## Bind Global Variables ##
  ###########################

  UPA <- V1008 <- V1014 <- id_dom <- UF <- V20082 <- V20081 <- rs_valid <- NULL
  V2008 <- V2007 <- id_ind <- V2003 <- V1016 <- appearances <- V1016 <- id_rs <- NULL

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

    # Individual id combines the household id with UF, V2003, V2007, and date of birth( V20082, V20081, V2008), creating an unique number for every combination of those variables, all through the function cur_group_id
    dat <- dat %>%
      dplyr::mutate(
        id_ind = dplyr::cur_group_id(),
        .by = c(id_dom, V2003, V20082, V20081, V2008, V2007)
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
    m <- max(dat$id_ind) # to avoid overlap between id numbers
    # id_rs are always higher numbers than id_ind

    # advanced identification is only run on previously unmatched individuals

    dat <- dat %>%
      dplyr::mutate(
        rs_valid = dplyr::case_when(
          matched_basic != 1 & as.numeric(V2005) %in% c(1, 2, 3) ~ 1,
          matched_basic != 1 & as.numeric(V2005) %in% c(4, 5) & as.numeric(V2009) >= 25 ~ 1,
          TRUE ~ NA
        )
      )
    dat <- dat %>%
      dplyr::mutate(
        id_rs = dplyr::cur_group_id() + m,
        .by = c(id_dom, V20081, V2008, V2003, rs_valid)
      )
    dat <- dat %>%
      dplyr::mutate(
        id_rs = ifelse(is.na(rs_valid), id_ind, id_rs)
      )
    
    # identifying new matched observations

    dat <- dat %>%
      dplyr::mutate(
        num_quarters = dplyr::n(),
        .by = id_rs
      ) # counts number of times that each id appears

    dat <- dat %>%
      dplyr::mutate(
        matched_adv = dplyr::case_when(
          num_quarters == 5 ~ 1,
          .default = 0
        )
      )
  }

  ##################
  ## Twin Removal ##
  ##################
  
  # basic panel
  if (panel != "none") {
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
  }
  
  # advanced panel
  if (!(panel %in% c("none", "basic"))) {
  dat <- dat %>%
    dplyr::mutate(
      num_appearances = dplyr::n(),
      .by = c("id_ind", "Ano", "Trimestre")
    ) %>% # counts number of times that each id_ind appears
    dplyr::mutate(
      num_appearances_adv = dplyr::n(),
      .by = c("id_rs", "Ano", "Trimestre")
    ) %>% # counts number of times that each id_rs appears
    dplyr::mutate(
      id_ind = dplyr::case_when(
        num_appearances != 1 ~ NA,
        .default = id_ind
      )) %>%
  dplyr::mutate(id_rs = dplyr::case_when(
        num_appearances_adv != 1 ~ NA,
        .default = id_rs
      )) # sets id to NA when it appears more than once per trimester/year
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

  # Handle unidentifiable observations due to missing values
  if (panel != "none") {
  dat <- dat %>% dplyr::mutate(
    id_ind = dplyr::case_when(
      V2008 == "99" | V20081 == "99" | V20082 == "9999" ~ NA,
      .default = id_ind
    )
  )}

  # Check whether the panel param is advanced so the function does not iterate over a non-existing variable
  if (!(panel %in% c("none", "basic"))) {
    dat <- dat %>% dplyr::mutate(
      id_rs = dplyr::case_when(
        V2008 == "99" | V20081 == "99" | V20082 == "9999" ~ NA,
        .default = id_rs
      )
    )
  }

  # Return the modified dataset
  return(dat)
}
