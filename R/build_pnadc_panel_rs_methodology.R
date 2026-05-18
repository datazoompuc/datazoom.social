#' Build PNADc Panel
#'
#' This function builds a panel dataset from PNADC data, identifying households and individuals
#'
#' @param dat Data frame with PNADC data, sorted into a single panel.
#' @param panel A \code{character} with the type of panel identification. Use "none" for no paneling, "basic" for basic paneling, and "advanced" for advanced paneling.
#'
#' @return A modified dataset with added identifiers for household (\code{id_dom}) and individual (\code{id_ind} or \code{id_rs}) based on the chosen panel algorithm.
#'
#' @examplesIf interactive()
#' # Example usage:
#' 
#' panel_data <- build_pnadc_panel(dat = pnad_sample, panel = "basic")
#'
#' @export
build_pnadc_panel <- function(dat, panel) {
  
  ###########################
  ## Bind Global Variables ##
  ###########################
  
  UPA <- V1008 <- V1014 <- id_dom <- V2009 <- V20082 <- V20081 <- V2008 <- V2007 <- NULL
  Ano <- Trimestre <- id_ind <- num_appearances <- V2003 <- id_rs <- NULL
  num_appearances_rs <- q_count_ind <- q_count_rs <- NULL
  
  #############################
  ## Define Basic Parameters ## 
  #############################
  
  if (panel == "none") {
    return(dat)
  }
  
  ##########################
  ## Basic Identification ##
  ##########################
  
  if (panel != "none") {
    # Household identifier
    dat <- dat %>%
      dplyr::mutate(
        id_dom = dplyr::cur_group_id(),
        .by = c(UPA, V1008, V1014)
      )
    
    # Individual id (Strict match: Sex, Day, Month, Year of Birth)
    dat <- dat %>%
      dplyr::mutate(
        id_ind = dplyr::cur_group_id(),
        .by = c(id_dom, V20082, V20081, V2008, V2007)
      )
    
    # Twin removal for basic ID
    dat <- dat %>%
      dplyr::add_count(id_ind, Ano, Trimestre, name = "num_appearances") %>%
      dplyr::mutate(
        id_ind = dplyr::case_when(
          num_appearances != 1 ~ NA_real_,
          .default = id_ind
        )) %>%
      dplyr::select(-num_appearances)
    
    # Missing values exclusion for basic ID
    dat <- dat %>% dplyr::mutate(
      id_ind = dplyr::case_when(
        V2008 == "99" | V20081 == "99" | V20082 == "9999" ~ NA_real_,
        .default = id_ind
      )
    )
  }
  
  #############################
  ## Advanced Identification ##
  #############################
  # Based on Ribas & Soares (2008) hierarchical criteria.
  
  if (!(panel %in% c("none", "basic"))) {
    
    # Start the advanced ID with the basic ID
    dat <- dat %>% dplyr::mutate(id_rs = id_ind)
    m <- max(dat$id_ind, na.rm = TRUE)
    
    # --- Criterion 1: Relaxing Day of Birth ---
    # Matches on: Household, Sex, Month, Year
    dat <- dat %>%
      dplyr::mutate(grp_relax_d = dplyr::cur_group_id(), .by = c(id_dom, V2007, V20081, V20082)) %>%
      dplyr::group_by(grp_relax_d) %>%
      dplyr::mutate(
        id_rs = dplyr::case_when(
          is.na(id_rs) & length(stats::na.omit(unique(id_rs))) == 1 ~ unique(stats::na.omit(id_rs))[1],
          TRUE ~ id_rs
        )
      ) %>% dplyr::ungroup()
    
    # --- Criterion 2: Relaxing Month of Birth ---
    # Matches on: Household, Sex, Day, Year
    dat <- dat %>%
      dplyr::mutate(grp_relax_m = dplyr::cur_group_id(), .by = c(id_dom, V2007, V2008, V20082)) %>%
      dplyr::group_by(grp_relax_m) %>%
      dplyr::mutate(
        id_rs = dplyr::case_when(
          is.na(id_rs) & length(stats::na.omit(unique(id_rs))) == 1 ~ unique(stats::na.omit(id_rs))[1],
          TRUE ~ id_rs
        )
      ) %>% dplyr::ungroup()
    
    # --- Criterion 3: Relaxing Year of Birth ---
    # Matches on: Household, Sex, Day, Month
    dat <- dat %>%
      dplyr::mutate(grp_relax_y = dplyr::cur_group_id(), .by = c(id_dom, V2007, V2008, V20081)) %>%
      dplyr::group_by(grp_relax_y) %>%
      dplyr::mutate(
        id_rs = dplyr::case_when(
          is.na(id_rs) & length(stats::na.omit(unique(id_rs))) == 1 ~ unique(stats::na.omit(id_rs))[1],
          TRUE ~ id_rs
        )
      ) %>% dplyr::ungroup()
    
    # --- Criterion 4: Order Number (V2003) & Sex ---
    # When dates fail completely, fallback to household order and sex.
    dat <- dat %>%
      dplyr::mutate(grp_relax_order = dplyr::cur_group_id(), .by = c(id_dom, V2007, V2003)) %>%
      dplyr::group_by(grp_relax_order) %>%
      dplyr::mutate(
        id_rs = dplyr::case_when(
          is.na(id_rs) & length(stats::na.omit(unique(id_rs))) == 1 ~ unique(stats::na.omit(id_rs))[1],
          TRUE ~ id_rs
        )
      ) %>% dplyr::ungroup()
    
    # Clean temporary grouping variables
    dat <- dat %>% dplyr::select(-grp_relax_d, -grp_relax_m, -grp_relax_y, -grp_relax_order)
    
    # For individuals STILL without an ID (they are entirely new to the tracking criteria),
    # assign them a new unique ID continuing from 'm'
    dat <- dat %>%
      dplyr::mutate(
        temp_new_id = dplyr::cur_group_id() + m, .by = c(id_dom, V20082, V20081, V2008, V2007, V2003)
      ) %>%
      dplyr::mutate(
        id_rs = dplyr::coalesce(id_rs, temp_new_id)
      ) %>%
      dplyr::select(-temp_new_id)
    
    # Twin removal for advanced ID (Safeguard)
    dat <- dat %>%
      dplyr::add_count(id_rs, Ano, Trimestre, name = "num_appearances_rs") %>%
      dplyr::mutate(
        id_rs = dplyr::case_when(
          num_appearances_rs != 1 ~ NA_real_,
          .default = id_rs
        )) %>% dplyr::select(-num_appearances_rs)
  }
  
  ##########################
  ## Pasting panel number ##
  ##########################
  
  if (panel != "none") {
    dat$id_ind <- ifelse(
      is.na(dat$id_ind),
      NA_character_,
      paste0(as.hexmode(dat$V1014), as.hexmode(dat$id_ind))
    )
  }
  
  if (!(panel %in% c("none", "basic"))) {
    dat$id_rs <- ifelse(
      is.na(dat$id_rs),
      NA_character_,
      paste0(as.hexmode(dat$V1014), as.hexmode(dat$id_rs))
    )
  }
  
  #################
  ## Return Data ##
  #################
  
  return(dat)
}