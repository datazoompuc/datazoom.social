#' Build PNADc Panel
#'
#' This function builds a panel dataset from PNADC data, indentifying households and individuals
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
  
  UPA <- V1008 <- V1014 <- id_dom <- V20082 <- V20081 <- V2008 <- V2007 <- NULL
  Ano <- Trimestre <- id_ind <- num_appearances <- V2003 <- id_rs <- NULL
  num_appearances_rs <- q_count_ind <- q_count_rs <- NULL
  
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
        .by = c(UPA, V1008, V1014)
      )
    
    # Individual id combines the household id, sex (V2007), and date of birth (V20082, V20081, V2008), creating an unique number for every combination of those variables, all through the function cur_group_id
    dat <- dat %>%
      dplyr::mutate(
        id_ind = dplyr::cur_group_id(),
        .by = c(id_dom, V20082, V20081, V2008, V2007)
      )
    
    # twin removal
    
    dat <- dat %>%
      dplyr::add_count(id_ind, Ano, Trimestre, name = "num_appearances") %>% # counts number of times that each id_ind appears
      dplyr::mutate(
        id_ind = dplyr::case_when(
          num_appearances != 1 ~ NA_real_,
          .default = id_ind
        ))
    
    # missing values
    
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
  
  ## Stage 1:
  
  if (!(panel %in% c("none", "basic"))) {
    m <- max(dat$id_ind, na.rm = TRUE) # to avoid overlap between id numbers
    # id_rs are always higher numbers than id_ind
    # remove NAs otherwise m is NA and all id_rs are NAs
    
    # advanced identification is only run on previously unmatched individuals
    
    dat <- dat %>%
      dplyr::mutate(
        id_rs = dplyr::cur_group_id() + m,
        .by = c(id_dom, V20081, V2008, V2003)
      )
    
    # twin removal
    ## kept here so we remember this step
    ## in practice it is useless here
    ## mechanically there are no repeated V2003 in a household/trimestre/year
    
    dat <- dat %>%
      dplyr::add_count(id_rs, Ano, Trimestre, name = "num_appearances_rs") %>% # counts number of times that each id_ind appears
      dplyr::mutate(
        id_rs = dplyr::case_when(
          num_appearances_rs != 1 ~ NA_real_,
          .default = id_rs
        ))
    
    # missing values
    
    dat <- dat %>% dplyr::mutate(
      id_rs = dplyr::case_when(
        V2008 == "99" | V20081 == "99" ~ NA_real_,
        .default = id_rs
      )
    )
    
    dat <- dat %>%
      # calculate distinct quarter counts for each ID type
      # how many times (in diff quarters/years) each ID appears
      dplyr::mutate(
        q_count_ind = dplyr::n_distinct(interaction(Ano, Trimestre)), 
        .by = id_ind
      ) %>%
      dplyr::mutate(
        q_count_rs = dplyr::n_distinct(interaction(Ano, Trimestre)), 
        .by = id_rs
      ) %>%
      dplyr::mutate(
        id_rs = dplyr::case_when(
          # perfect tracking with basic identification
          q_count_ind == 5 ~ id_ind,
          
          # id_rs takes over if it finds more quarters than id_ind does
          # no more than 5 appearances (or else errors are introduced)
          q_count_rs > q_count_ind & q_count_rs <= 5 ~ id_rs,
          
          # fallback: prefer id_ind if available, otherwise keep id_rs
          TRUE ~ dplyr::coalesce(id_ind, id_rs)
        )
      )
    
  }
  
  ##########################
  ## Pasting panel number ##
  ##########################
  
  # to avoid overlap when binding more than one panel (all ids are just counts from 1, ..., N)
  # ifelse guards against as.hexmode(NA) which returns the string "NA" instead of a true NA
  
  # basic panel
  if (panel != "none") {
    
    dat$id_ind <- ifelse(
      is.na(dat$id_ind),
      NA_character_,
      paste0(as.hexmode(dat$V1014), as.hexmode(dat$id_ind))
    )
    
  }
  
  # advanced panel
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
  
  # Return the modified dataset
  return(dat)
}