#' Build PNADc Panel
#'
#' This function builds a panel dataset from PNADC data, identifying households and individuals.
#'
#' @param dat Data frame with PNADC data, sorted into a single panel.
#' @param panel A \code{character} with the type of panel identification. Use "none" for no paneling, "basic" for basic paneling, "advanced_1" for advanced stage 1 paneling, "advanced_2" for advanced stage 2 paneling, and "advanced_3" for the fuzzy-matching stage 3 paneling.
#'
#' @return A modified dataset with added identifiers for household (\code{id_dom}) and individual (\code{id_ind}, and progressively \code{id_rs1}, \code{id_rs2}, or \code{id_rs3}) based on the chosen panel algorithm.
#'
#' @examplesIf interactive()
#' # Example usage:
#' 
#' panel_data <- build_pnadc_panel(dat = pnad_sample, panel = "advanced_3")
#'
#' @export
build_pnadc_panel <- function(dat, panel) {
  
  func_start_time <- Sys.time()
  message(sprintf("[%s] Starting build_pnadc_panel with panel type: '%s'", func_start_time, panel))
  
  ###########################
  ## Bind Global Variables ##
  ###########################
  
  UPA <- V1008 <- V1014 <- id_dom <- V20082 <- V20081 <- V2008 <- V2007 <- NULL
  Ano <- Trimestre <- id_ind <- num_appearances <- V2003 <- V2009 <- NULL
  q_count_ind <- NULL
  birth_day <- birth_month <- birth_year <- NULL
  id_rs1 <- id_rs2 <- id_rs3 <- num_appearances_rs1 <- num_appearances_rs2 <- NULL
  q_count_rs1 <- q_count_rs2 <- q_count_rs3 <- NULL
  is_candidate <- row_id <- row_id.A <- row_id.B <- NULL
  Ano.A <- Ano.B <- Trimestre.A <- Trimestre.B <- NULL
  V2007.A <- V2007.B <- birth_day.A <- birth_day.B <- NULL
  birth_month.A <- birth_month.B <- V2009.A <- V2009.B <- NULL
  id_rs3_fuzzy <- cluster_id <- NULL
  
  #############################
  ## Define Basic Parameters ## 
  #############################
  
  # Check if the panel type is 'none'; if so, return the original data
  if (panel == "none") {
    message(sprintf("[%s] Panel is 'none'. Returning original data unmodified.", Sys.time()))
    return(dat)
  }
  
  ##########################
  ## Basic Identification ##
  ##########################
  
  # If the panel type is not 'none', perform basic identification steps
  if (panel != "none") {
    t_start_basic <- Sys.time()
    message(sprintf("[%s] --> Starting Basic Identification...", t_start_basic))
    
    # Household identifier combines UPA, V1008, and V1014
    dat <- dat %>%
      dplyr::mutate(
        id_dom = dplyr::cur_group_id(),
        .by = c("UPA", "V1008", "V1014")
      )
    
    # Individual id combines the household id, sex (V2007), and date of birth
    dat <- dat %>%
      dplyr::mutate(
        id_ind = dplyr::cur_group_id(),
        .by = c("id_dom", "V20082", "V20081", "V2008", "V2007")
      )
    
    # twin removal
    dat <- dat %>%
      dplyr::add_count(id_ind, Ano, Trimestre, name = "num_appearances") %>%
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
    
    t_end_basic <- Sys.time()
    message(sprintf("[%s] <-- Finished Basic Identification. Time elapsed: %s", t_end_basic, format(round(difftime(t_end_basic, t_start_basic), 2))))
  }
  
  #############################
  ## Advanced Identification ##
  #############################
  
  if (panel %in% c("advanced_1", "advanced_2", "advanced_3")) {
    
    t_start_donate <- Sys.time()
    message(sprintf("[%s] --> Populating birth dates (donate_birth_dates)...", t_start_donate))
    # Call the internal donation function to populate birth_day, birth_month, and birth_year
    dat <- donate_birth_dates(dat)
    t_end_donate <- Sys.time()
    message(sprintf("[%s] <-- Finished populating birth dates. Time elapsed: %s", t_end_donate, format(round(difftime(t_end_donate, t_start_donate), 2))))
    
    ## Stage 1:
    t_start_stg1 <- Sys.time()
    message(sprintf("[%s] --> Starting Advanced Identification: Stage 1...", t_start_stg1))
    
    m <- max(dat$id_ind, na.rm = TRUE) # to avoid overlap between id numbers
    
    dat <- dat %>%
      dplyr::mutate(
        id_rs1 = dplyr::cur_group_id() + m,
        .by = c("id_dom", "birth_year", "birth_month", "birth_day", "V2007")
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
        .by = "id_ind"
      ) %>%
      dplyr::mutate(
        q_count_rs1 = dplyr::n_distinct(interaction(Ano, Trimestre)), 
        .by = "id_rs1"
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
    
    t_end_stg1 <- Sys.time()
    message(sprintf("[%s] <-- Finished Stage 1. Time elapsed: %s", t_end_stg1, format(round(difftime(t_end_stg1, t_start_stg1), 2))))
    
    ## Stage 2:
    if (panel %in% c("advanced_2", "advanced_3")) {
      t_start_stg2 <- Sys.time()
      message(sprintf("[%s] --> Starting Advanced Identification: Stage 2...", t_start_stg2))
      
      m2 <- max(dat$id_rs1, na.rm = TRUE) # avoid overlap with Stage 1
      
      dat <- dat %>%
        dplyr::mutate(
          id_rs2 = dplyr::cur_group_id() + m2,
          .by = c("id_dom", "birth_month", "birth_day", "V2003")
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
          .by = "id_rs2"
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
      
      t_end_stg2 <- Sys.time()
      message(sprintf("[%s] <-- Finished Stage 2. Time elapsed: %s", t_end_stg2, format(round(difftime(t_end_stg2, t_start_stg2), 2))))
    }
    
    ## Stage 3 (Fuzzy Matching):
    if (panel == "advanced_3") {
      t_start_stg3 <- Sys.time()
      message(sprintf("[%s] --> Starting Advanced Identification: Stage 3 (Fuzzy Matching)...", t_start_stg3))
      
      if (!requireNamespace("igraph", quietly = TRUE)) {
        stop("The 'igraph' package is required for the 'advanced_3' panel algorithm. Please install it using install.packages('igraph').")
      }
      
      # 1. Target Candidates (Less than 5 matches in id_rs2)
      dat <- dat %>%
        dplyr::mutate(
          is_candidate = dplyr::coalesce(q_count_rs2 < 5, TRUE),
          row_id = dplyr::row_number()
        )
      
      candidates <- dat %>% dplyr::filter(is_candidate)
      
      # 2. Build the Nest (Self-join within household)
      nest <- candidates %>%
        dplyr::select("row_id", "id_dom", "V2007", "birth_day", "birth_month", "V2009", "Ano", "Trimestre") %>%
        dplyr::inner_join(
          candidates %>% dplyr::select("row_id", "id_dom", "V2007", "birth_day", "birth_month", "V2009", "Ano", "Trimestre"),
          by = "id_dom",
          suffix = c(".A", ".B"),
          relationship = "many-to-many"
        ) %>%
        # Apply strict fuzzy evaluation constraints inside the nest
        dplyr::filter(
          row_id.A != row_id.B,
          interaction(Ano.A, Trimestre.A) != interaction(Ano.B, Trimestre.B),
          V2007.A == V2007.B,
          abs(birth_day.A - birth_day.B) <= 4,
          abs(birth_month.A - birth_month.B) <= 2,
          abs(V2009.A - V2009.B) <= dplyr::if_else(V2009.A < 25, 2, exp(V2009.A / 30))
        )
      
      # 3. Apply Uniqueness Tie-Breaker
      valid_matches <- nest %>%
        dplyr::group_by(row_id.A, Ano.B, Trimestre.B) %>%
        dplyr::filter(dplyr::n() == 1) %>% 
        dplyr::ungroup()
      
      # 4. Generate Graph & Component IDs
      if (nrow(valid_matches) > 0) {
        # Use cbind to ensure a strict 2-column character matrix
        edges <- cbind(
          as.character(valid_matches$row_id.A), 
          as.character(valid_matches$row_id.B)
        )
        
        # Pass the matrix directly; do NOT wrap 'edges' in as.character() here
        g <- igraph::graph_from_edgelist(edges, directed = FALSE)
        comps <- igraph::components(g)$membership
        
        cluster_map <- data.frame(
          row_id = as.integer(names(comps)),
          cluster_id = as.integer(comps)
        )
        
        m3 <- max(dat$id_rs2, na.rm = TRUE)
        cluster_map$id_rs3_fuzzy <- cluster_map$cluster_id + m3
        
        dat <- dat %>%
          dplyr::left_join(cluster_map, by = "row_id") %>%
          dplyr::mutate(
            id_rs3 = dplyr::case_when(
              !is_candidate ~ id_rs2,
              !is.na(id_rs3_fuzzy) ~ id_rs3_fuzzy,
              TRUE ~ id_rs2
            )
          )
      } else {
        dat <- dat %>% dplyr::mutate(id_rs3 = id_rs2)
      }
      
      # 5. Evaluate and Fallback
      dat <- dat %>%
        # Count the ID appearances in the same Year/Quarter (exactly like Stages 1 and 2)
        dplyr::add_count(id_rs3, Ano, Trimestre, name = "num_appearances_rs3") %>%
        dplyr::mutate(
          q_count_rs3 = dplyr::n_distinct(interaction(Ano, Trimestre)), 
          .by = "id_rs3"
        ) %>%
        dplyr::mutate(
          # id_rs3 falls back to id_rs2 if it generated twins in the quarter OR if rs2 performed better
          id_rs3 = dplyr::case_when(
            num_appearances_rs3 > 1 ~ id_rs2, # <- EXPLICIT GUARDRAIL: fallback if multiple rows appear in the same quarter
            q_count_rs2 == 5 ~ id_rs2,
            q_count_rs3 > q_count_rs2 & q_count_rs3 <= 5 ~ id_rs3,
            TRUE ~ dplyr::coalesce(id_rs2, id_rs3)
          ),
          q_count_rs3 = dplyr::case_when(
            num_appearances_rs3 > 1 ~ q_count_rs2, # Adjust the count to reflect the fallback
            q_count_rs2 == 5 ~ q_count_rs2,
            q_count_rs3 > q_count_rs2 & q_count_rs3 <= 5 ~ q_count_rs3,
            TRUE ~ dplyr::coalesce(q_count_rs2, q_count_rs3)
          )
        )
      
      # Discard the nest and related tracking vars from the environment
      rm(candidates, nest, valid_matches)
      
      t_end_stg3 <- Sys.time()
      message(sprintf("[%s] <-- Finished Stage 3 (Fuzzy Matching). Time elapsed: %s", t_end_stg3, format(round(difftime(t_end_stg3, t_start_stg3), 2))))
    }
    
    # Cleanup auxiliary variables mapped during the advanced stages (KEEPING id_rs1 & id_rs2 & id_rs3)
    cols_to_remove <- c("num_appearances_rs1", "q_count_rs1", "q_count_ind")
    if (panel %in% c("advanced_2", "advanced_3")) {
      cols_to_remove <- c(cols_to_remove, "num_appearances_rs2", "q_count_rs2")
    }
    if (panel == "advanced_3") {
      cols_to_remove <- c(cols_to_remove, "is_candidate", "row_id", "id_rs3_fuzzy", "q_count_rs3")
    }
    dat <- dat %>% dplyr::select(-dplyr::any_of(cols_to_remove))
  }
  
  ##########################
  ## Pasting panel number ##
  ##########################
  
  t_start_paste <- Sys.time()
  message(sprintf("[%s] --> Pasting panel numbers...", t_start_paste))
  
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
  if (panel %in% c("advanced_1", "advanced_2", "advanced_3")) {
    dat$id_rs1 <- ifelse(
      is.na(dat$id_rs1),
      NA_character_,
      paste0(as.hexmode(dat$V1014), as.hexmode(dat$id_rs1))
    )
  }
  
  # advanced panel 2
  if (panel %in% c("advanced_2", "advanced_3")) {
    dat$id_rs2 <- ifelse(
      is.na(dat$id_rs2),
      NA_character_,
      paste0(as.hexmode(dat$V1014), as.hexmode(dat$id_rs2))
    )
  }
  
  # advanced panel 3
  if (panel == "advanced_3") {
    dat$id_rs3 <- ifelse(
      is.na(dat$id_rs3),
      NA_character_,
      paste0(as.hexmode(dat$V1014), as.hexmode(dat$id_rs3))
    )
  }
  
  t_end_paste <- Sys.time()
  message(sprintf("[%s] <-- Finished pasting panel numbers. Time elapsed: %s", t_end_paste, format(round(difftime(t_end_paste, t_start_paste), 2))))
  
  #################
  ## Return Data ##
  #################
  
  func_end_time <- Sys.time()
  message(sprintf("[%s] build_pnadc_panel finished successfully. Total Execution Time: %s", func_end_time, format(round(difftime(func_end_time, func_start_time), 2))))
  
  # Return the modified dataset
  return(dat)
}