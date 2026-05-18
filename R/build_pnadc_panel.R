#' Build PNADc Panel
#'
#' This function builds a panel dataset from PNADC data, identifying households and individuals.
#'
#' @param dat Data frame with PNADC data, sorted into a single panel.
#' @param panel A \code{character} with the type of panel identification. Use "none" for no paneling, "basic" for basic paneling, "advanced_1" for advanced stage 1 paneling, and "advanced_2" for advanced stage 2 paneling.
#'
#' @return A modified dataset with added identifiers for household (\code{id_dom}) and individual (\code{id_ind}, and progressively \code{id_rs1} or \code{id_rs2}) based on the chosen panel algorithm.
#'
#' @examplesIf interactive()
#' # Example usage:
#' 
#' panel_data <- build_pnadc_panel(dat = pnad_sample, panel = "advanced_2")
#'
#' @export
build_pnadc_panel <- function(dat, panel) {
  ###########################
  ## Bind Global Variables ##
  ###########################
  
  UPA <- V1008 <- V1014 <- id_dom <- V20082 <- V20081 <- V2008 <- V2007 <- NULL
  Ano <- Trimestre <- id_ind <- num_appearances <- V2003 <- NULL
  q_count_ind <- NULL
  birth_day <- birth_month <- birth_year <- NULL
  id_rs1 <- id_rs2 <- num_appearances_rs1 <- num_appearances_rs2 <- NULL
  q_count_rs1 <- q_count_rs2 <- NULL
  
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
  
  if (panel %in% c("advanced_1", "advanced_2")) {
    
    # Call the internal donation function to populate birth_day, birth_month, and birth_year
    dat <- donate_birth_dates(dat)
    
    ## Stage 1:
    m <- max(dat$id_ind, na.rm = TRUE) # to avoid overlap between id numbers
    
    dat <- dat %>%
      dplyr::mutate(
        id_rs1 = dplyr::cur_group_id() + m,
        .by = c(id_dom, birth_year, birth_month, birth_day, V2007)
      ) %>%
      # twin removal for stage 1
      dplyr::add_count(id_rs1, Ano, Trimestre, name = "num_appearances_rs1") %>%
      dplyr::mutate(
        id_rs1 = dplyr::case_when(
          num_appearances_rs1 != 1 ~ NA_real_,
          is.na(birth_year) | is.na(birth_month) | is.na(birth_day) ~ NA_real_,
          .default = id_rs1
        )
      )
    
    # Stage 1 evaluation and fallback
    dat <- dat %>%
      dplyr::mutate(
        q_count_ind = dplyr::n_distinct(interaction(Ano, Trimestre)), 
        .by = id_ind
      ) %>%
      dplyr::mutate(
        q_count_rs1 = dplyr::n_distinct(interaction(Ano, Trimestre)), 
        .by = id_rs1
      ) %>%
      dplyr::mutate(
        # id_rs1 falls back to id_ind if ind performed better or perfectly
        id_rs1 = dplyr::case_when(
          q_count_ind == 5 ~ id_ind,
          q_count_rs1 > q_count_ind & q_count_rs1 <= 5 ~ id_rs1,
          TRUE ~ dplyr::coalesce(id_ind, id_rs1)
        ),
        # Update q_count_rs1 to reflect the merged reality for the potential next stage
        q_count_rs1 = dplyr::case_when(
          q_count_ind == 5 ~ q_count_ind,
          q_count_rs1 > q_count_ind & q_count_rs1 <= 5 ~ q_count_rs1,
          TRUE ~ dplyr::coalesce(q_count_ind, q_count_rs1)
        )
      )
    
    ## Stage 2:
    if (panel == "advanced_2") {
      m2 <- max(dat$id_rs1, na.rm = TRUE) # avoid overlap with Stage 1
      
      dat <- dat %>%
        dplyr::mutate(
          id_rs2 = dplyr::cur_group_id() + m2,
          .by = c(id_dom, birth_month, birth_day, V2003)
        ) %>%
        # twin removal for stage 2
        dplyr::add_count(id_rs2, Ano, Trimestre, name = "num_appearances_rs2") %>%
        dplyr::mutate(
          id_rs2 = dplyr::case_when(
            num_appearances_rs2 != 1 ~ NA_real_,
            is.na(birth_month) | is.na(birth_day) ~ NA_real_,
            .default = id_rs2
          )
        )
      
      # Stage 2 evaluation and fallback
      dat <- dat %>%
        dplyr::mutate(
          q_count_rs2 = dplyr::n_distinct(interaction(Ano, Trimestre)), 
          .by = id_rs2
        ) %>%
        dplyr::mutate(
          # id_rs2 falls back to the already-optimized id_rs1
          id_rs2 = dplyr::case_when(
            q_count_rs1 == 5 ~ id_rs1, 
            q_count_rs2 > q_count_rs1 & q_count_rs2 <= 5 ~ id_rs2,
            TRUE ~ dplyr::coalesce(id_rs1, id_rs2)
          ),
          q_count_rs2 = dplyr::case_when(
            q_count_rs1 == 5 ~ q_count_rs1,
            q_count_rs2 > q_count_rs1 & q_count_rs2 <= 5 ~ q_count_rs2,
            TRUE ~ dplyr::coalesce(q_count_rs1, q_count_rs2)
          )
        )
    }
    
    # Cleanup auxiliary variables mapped during the advanced stages (KEEPING id_rs1 & id_rs2)
    cols_to_remove <- c("num_appearances_rs1", "q_count_rs1", "q_count_ind")
    if (panel == "advanced_2") {
      cols_to_remove <- c(cols_to_remove, "num_appearances_rs2", "q_count_rs2")
    }
    dat <- dat %>% dplyr::select(-dplyr::any_of(cols_to_remove))
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
  
  # advanced panel 1
  if (panel %in% c("advanced_1", "advanced_2")) {
    dat$id_rs1 <- ifelse(
      is.na(dat$id_rs1),
      NA_character_,
      paste0(as.hexmode(dat$V1014), as.hexmode(dat$id_rs1))
    )
  }
  
  # advanced panel 2
  if (panel == "advanced_2") {
    dat$id_rs2 <- ifelse(
      is.na(dat$id_rs2),
      NA_character_,
      paste0(as.hexmode(dat$V1014), as.hexmode(dat$id_rs2))
    )
  }
  
  #################
  ## Return Data ##
  #################
  
  # Return the modified dataset
  return(dat)
}