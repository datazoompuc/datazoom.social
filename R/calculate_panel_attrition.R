#' Create an attrition table for a PNADc panel file
#'
#' @param data Input data frame (PNADc panel).
#' @param panel ID strategy: "basic" (id_ind), "advanced" (id_rs), or "households" (id_dom).
#'
#' @return A data frame with 8 columns detailing unconditional and conditional attrition.
calculate_panel_attrition <- function(data, panel) {
  
  # 1. Standardize ID column based on panel type
  id_col <- switch(panel,
                   "basic"      = "id_ind",
                   "advanced"   = "id_rs",
                   "households" = "id_dom",
                   stop("Invalid panel type. Use 'basic', 'advanced', or 'households'."))
  
  message(paste0(stringr::str_to_title(panel), " panel attrition calculated."))
  
  # 2. Preparation: Filter for those present in Wave 1 and rename ID
  df_prepared <- data %>%
    dplyr::mutate(V1016 = as.integer(V1016)) %>%
    dplyr::rename(individual_id = !!id_col)
  
  ids_w1 <- df_prepared %>%
    dplyr::filter(V1016 == 1) %>%
    dplyr::pull(individual_id) %>%
    unique()
  
  n_w1 <- length(ids_w1)
  
  # 3. Create a wide presence matrix (1 if present, 0 if absent)
  # This makes logical comparisons across waves much faster
  presence_matrix <- df_prepared %>%
    dplyr::filter(individual_id %in% ids_w1) %>%
    dplyr::select(individual_id, V1016) %>%
    dplyr::mutate(present = 1) %>%
    tidyr::pivot_wider(names_from = V1016, 
                       values_from = present, 
                       names_prefix = "w", 
                       values_fill = 0)
  
  # 4. Initialize the results storage
  results <- data.frame(
    wave_step               = character(),
    n_missing_uncond        = numeric(),
    n_found_uncond          = numeric(),
    pct_found_uncond        = numeric(),
    n_missing_cond          = numeric(),
    n_found_cond            = numeric(),
    pct_found_cond_w1       = numeric(),
    pct_found_cond_prev     = numeric(),
    stringsAsFactors        = FALSE
  )
  
  # 5. Calculate metrics for each transition (1->2, 2->3, 3->4, 4->5)
  # For conditional, we track the 'survivors' who were in ALL previous waves
  current_cond_ids <- ids_w1
  
  for (i in 1:4) {
    prev_w <- paste0("w", i)
    next_w <- paste0("w", i + 1)
    
    # --- Unconditional Logic ---
    # Who was in Wave i?
    ids_in_prev <- presence_matrix$individual_id[presence_matrix[[prev_w]] == 1]
    # Out of those, who is in Wave i + 1?
    found_uncond <- presence_matrix %>%
      dplyr::filter(!!rlang::sym(prev_w) == 1, !!rlang::sym(next_w) == 1) %>%
      nrow()
    
    # --- Conditional Logic ---
    # Who was in ALL waves from 1 to i? (stored in current_cond_ids)
    # Out of those, who is also in Wave i + 1?
    found_cond_df <- presence_matrix %>%
      dplyr::filter(individual_id %in% current_cond_ids, !!rlang::sym(next_w) == 1)
    
    n_found_cond <- nrow(found_cond_df)
    n_missing_cond <- length(current_cond_ids) - n_found_cond
    
    # Update denominator for next conditional step
    prev_cond_total <- length(current_cond_ids)
    current_cond_ids <- found_cond_df$individual_id
    
    # --- Assemble Row ---
    results[i, ] <- list(
      wave_step           = paste("Wave", i, "to", i + 1),
      n_missing_uncond    = length(ids_in_prev) - found_uncond,
      n_found_uncond      = found_uncond,
      pct_found_uncond    = (found_uncond / n_w1) * 100,
      n_missing_cond      = n_missing_cond,
      n_found_cond        = n_found_cond,
      pct_found_cond_w1   = (n_found_cond / n_w1) * 100,
      pct_found_cond_prev = (n_found_cond / prev_cond_total) * 100
    )
  }
  
  return(results)
}