#' Create a 5-column attrition table for a PNADc panel
#'
#' @param data Input data frame (PNADc panel).
#' @param panel ID strategy: "basic" (id_ind), "advanced_1" (id_rs1), "advanced_2" (id_rs2), "advanced_3" (id_rs3), or "households" (id_dom).
#'
#' @return A tibble with 5 columns: wave, and four recovery rates.
calculate_attrition_pnadc <- function(data, panel) {
  
  # 1. Standardize ID column based on panel type
  id_col <- switch(panel,
                   "basic"      = "id_ind",
                   "advanced_1" = "id_rs1",
                   "advanced_2" = "id_rs2",
                   "advanced_3" = "id_rs3",
                   "households" = "id_dom",
                   stop("Invalid panel type."))
  
  message(paste0(stringr::str_to_title(panel), " panel attrition calculated using ", id_col, "."))
  
  # 2. Denominators: Total Rows and Unique Valid IDs in Wave 1
  total_obs_w1 <- data %>%
    dplyr::filter(as.integer(V1016) == 1) %>%
    nrow()
  
  valid_ids_w1 <- data %>%
    dplyr::filter(as.integer(V1016) == 1, !is.na(!!rlang::sym(id_col))) %>%
    dplyr::distinct(!!rlang::sym(id_col)) %>%
    dplyr::pull(!!rlang::sym(id_col))
  
  cohort_size_w1 <- length(valid_ids_w1)
  
  # 3. Initialize storage vectors
  uncond_counts <- numeric(5)
  cond_counts <- numeric(5)
  
  # The conditional surviving pool starts as the full valid W1 cohort
  current_cond_ids <- valid_ids_w1
  
  # 4. Iterate through waves using highly efficient filtering/intersections
  for (w in 1:5) {
    
    # Extract unique, valid IDs present in the current wave that belong to the W1 cohort
    curr_wave_cohort_ids <- data %>%
      dplyr::filter(as.integer(V1016) == w, 
                    !is.na(!!rlang::sym(id_col)),
                    !!rlang::sym(id_col) %in% valid_ids_w1) %>%
      dplyr::distinct(!!rlang::sym(id_col)) %>%
      dplyr::pull(!!rlang::sym(id_col))
    
    # Unconditional Count: Simply the number of W1 cohort members found in Wave W
    uncond_counts[w] <- length(curr_wave_cohort_ids)
    
    # Conditional Count: Intersect current wave IDs with the surviving pool from Wave W-1
    current_cond_ids <- base::intersect(current_cond_ids, curr_wave_cohort_ids)
    cond_counts[w] <- length(current_cond_ids)
  }
  
  # 5. Assemble the final tibble
  results <- tibble::tibble(
    wave = paste("Wave", 1:5),
    uncond_rate_row_base = (uncond_counts / total_obs_w1) * 100,
    uncond_rate_id_base  = (uncond_counts / cohort_size_w1) * 100,
    cond_rate_row_base   = (cond_counts / total_obs_w1) * 100,
    cond_rate_id_base    = (cond_counts / cohort_size_w1) * 100
  )
  
  return(results)
}