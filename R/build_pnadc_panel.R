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
  period_key <- period_key.A <- period_key.B <- already_occupied <- NULL
  V2007.A <- V2007.B <- birth_day.A <- birth_day.B <- NULL
  birth_month.A <- birth_month.B <- V2009.A <- V2009.B <- NULL
  id_rs2.A <- id_rs2.B <- NULL
  id_rs3_fuzzy <- cluster_root <- NULL

  ###################
  ## Cleaning Data ##
  ###################

  dat <- dat %>%
    # Convert dates and ages to numeric
    dplyr::mutate(
      V2008  = as.numeric(V2008),
      V20081 = as.numeric(V20081),
      V20082 = as.numeric(V20082),
      V2009  = as.numeric(V2009),
      Ano    = as.numeric(Ano)
    ) %>%
    # Identify the error codes (99/9999) and replace them with NA
    dplyr::mutate(
      V2008  = dplyr::if_else(V2008 == 99, NA_real_, V2008),
      V20081 = dplyr::if_else(V20081 == 99, NA_real_, V20081),
      V20082 = dplyr::if_else(V20082 == 9999, NA_real_, V20082)
    )

  #############################
  ## Define Basic Parameters ##
  #############################

  # Check if the panel type is 'none'; if so, return the original raw data
  if (panel == "none") {
    return(dat)
  }

  ##########################
  ## Basic Identification ##
  ##########################

  # If the panel type is not 'none', perform the basic identification steps
  if (panel != "none") {
    # Household identifier combines UPA, V1008, and V1014, creating a unique number for every combination of those variables using cur_group_id
    dat <- dat %>%
      dplyr::mutate(
        id_dom = dplyr::cur_group_id(),
        .by = c("UPA", "V1008", "V1014")
      )

    # Individual identifier combines the household ID, sex (V2007), and date of birth (V20082, V20081, V2008), creating a unique number for every combination
    dat <- dat %>%
      dplyr::mutate(
        id_ind = dplyr::cur_group_id(),
        .by = c("id_dom", "V20082", "V20081", "V2008", "V2007")
      )

    # Twin removal
    dat <- dat %>%
      dplyr::add_count(id_ind, Ano, Trimestre, name = "num_appearances") %>% # Counts the number of times that each id_ind appears in the same quarter
      dplyr::mutate(
        id_ind = dplyr::case_when(
          num_appearances != 1 ~ NA_real_,
          .default = id_ind
        ))

    # Treat missing values
    dat <- dat %>% dplyr::mutate(
      id_ind = dplyr::case_when(
        is.na(V2008) | is.na(V20081) | is.na(V20082) ~ NA_real_,
        .default = id_ind
      )
    )
  }

  #############################
  ## Advanced Identification ##
  #############################

  if (panel %in% c("advanced_1", "advanced_2", "advanced_3")) {

    # Call the internal donation function to populate birth_day, birth_month, and birth_year
    dat <- donate_birth_dates(dat)

    ## Stage 1:
    m <- max(dat$id_ind, na.rm = TRUE) # Avoid overlap between ID numbers

    dat <- dat %>%
      dplyr::mutate(
        id_rs1 = dplyr::cur_group_id() + m,
        .by = c("id_dom", "birth_year", "birth_month", "birth_day", "V2007")
      ) %>%
      # Twin removal for Stage 1
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
        q_count_ind = dplyr::if_else(
          is.na(id_ind),
          NA_integer_,
          dplyr::n_distinct(interaction(Ano, Trimestre))
        ),
        .by = "id_ind"
      ) %>%
      dplyr::mutate(
        q_count_rs1 = dplyr::if_else(
          is.na(id_rs1),
          NA_integer_,
          dplyr::n_distinct(interaction(Ano, Trimestre))
        ),
        .by = "id_rs1"
      ) %>%
      dplyr::mutate(
        # id_rs1 falls back to id_ind if the basic method performed better or perfectly
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
    if (panel %in% c("advanced_2", "advanced_3")) {
      m2 <- max(dat$id_rs1, na.rm = TRUE) # Avoid overlap with Stage 1 IDs

      dat <- dat %>%
        dplyr::mutate(
          id_rs2 = dplyr::cur_group_id() + m2,
          .by = c("id_dom", "birth_month", "birth_day", "V2003")
        ) %>%
        # Twin removal for Stage 2
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
          q_count_rs2 = dplyr::if_else(
            is.na(id_rs2),
            NA_integer_,
            dplyr::n_distinct(interaction(Ano, Trimestre))
          ),
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
    }

    ## Stage 3 (Fuzzy Matching):
    if (panel == "advanced_3") {

      # 1. Target Candidates (Less than 5 successful matches in id_rs2)
      dat <- dat %>%
        dplyr::mutate(
          is_candidate = dplyr::coalesce(q_count_rs2 < 5, TRUE),
          row_id = dplyr::row_number()
        )

      candidates <- dat %>% dplyr::filter(is_candidate)

      # Quarters already assigned to each Stage 2 trajectory. Fuzzy matching
      # must only search for observations in quarters that are still missing.
      occupied_periods <- candidates %>%
        dplyr::filter(!is.na(id_rs2)) %>%
        dplyr::transmute(
          id_rs2.A = id_rs2,
          period_key.B = paste(Ano, Trimestre, sep = "-"),
          already_occupied = TRUE
        ) %>%
        dplyr::distinct()

      # 2. Build the Nest (Self-join within household)
      nest <- candidates %>%
        dplyr::mutate(period_key = paste(Ano, Trimestre, sep = "-")) %>%
        dplyr::select(row_id, id_dom, id_rs2, V2007, birth_day, birth_month, V2009, Ano, Trimestre, period_key) %>%
        dplyr::inner_join(
          candidates %>%
            dplyr::mutate(period_key = paste(Ano, Trimestre, sep = "-")) %>%
            dplyr::select(row_id, id_dom, id_rs2, V2007, birth_day, birth_month, V2009, Ano, Trimestre, period_key),
          by = "id_dom",
          suffix = c(".A", ".B"),
          relationship = "many-to-many"
        ) %>%
        dplyr::left_join(
          occupied_periods,
          by = c("id_rs2.A", "period_key.B")
        ) %>%
        # Apply strict fuzzy evaluation constraints inside the nest
        dplyr::filter(
          is.na(already_occupied),
          row_id.A != row_id.B,
          interaction(Ano.A, Trimestre.A) != interaction(Ano.B, Trimestre.B),
          V2007.A == V2007.B,
          abs(birth_day.A - birth_day.B) <= 4,
          abs(birth_month.A - birth_month.B) <= 2,
          abs(V2009.A - V2009.B) <= dplyr::if_else(V2009.A < 25, 2, exp(V2009.A / 30))
        ) %>%
        dplyr::group_by(row_id.A, Ano.B, Trimestre.B) %>%
        dplyr::filter(
          if (any(id_rs2.A == id_rs2.B, na.rm = TRUE)) {
            id_rs2.A == id_rs2.B
          } else {
            TRUE
          }
        ) %>%
        dplyr::ungroup()

      # 3. Apply Uniqueness Tie-Breaker
      valid_matches <- nest %>%
        dplyr::group_by(row_id.A, Ano.B, Trimestre.B) %>%
        dplyr::filter(dplyr::n() == 1) %>%
        dplyr::ungroup() %>%
        # Confidence score (lower = closer pair/more plausible) : used
        # by the union-find below to resolve conflicts betweens edges,
        # prioritizing the best matches.
        dplyr::mutate(
          match_score = abs(birth_day.A - birth_day.B) +
            abs(birth_month.A - birth_month.B) * 30 +
            abs(V2009.A - V2009.B) * 365
        )

      # Preserve the links already established by id_rs2 when constructing the
      # graph. This lets a fuzzy match extend an existing trajectory as a whole.
      rs2_edges <- candidates %>%
        dplyr::filter(!is.na(id_rs2)) %>%
        dplyr::arrange(id_rs2, Ano, Trimestre, row_id) %>%
        dplyr::mutate(row_id.B = dplyr::lead(row_id), .by = "id_rs2") %>%
        dplyr::filter(!is.na(row_id.B)) %>%
        dplyr::transmute(row_id.A = row_id, row_id.B)

      # 4. Cluster IDs using capacity-constrained union-find
      # (replaces igraph::components(), which computed the transitive closure
      # of the edges without ever checking whether a resulting cluster contained
      # two rows from the same quarter. See build_id_rs3_capacity_constrained()
      # (its own file, build_id_rs3_capacity_constrained.R): rs2_edges are
      # merged first, followed by fuzzy edges from the closest matches to the
      # most uncertain ones. Any merge that would create a temporal collision
      # is rejected (only that specific edge, not the entire cluster).
      cluster_map_raw <- build_id_rs3_capacity_constrained(candidates, valid_matches, rs2_edges)

      m3 <- max(dat$id_rs2, na.rm = TRUE)
      cluster_map <- cluster_map_raw %>%
        dplyr::mutate(id_rs3_fuzzy = cluster_root + m3) %>%
        dplyr::select(row_id, id_rs3_fuzzy)

      dat <- dat %>%
        dplyr::left_join(cluster_map, by = "row_id") %>%
        dplyr::mutate(
          id_rs3 = dplyr::case_when(
            !is_candidate ~ id_rs2,
            !is.na(id_rs3_fuzzy) ~ id_rs3_fuzzy,
            TRUE ~ id_rs2
          )
        )

      # 5. Evaluate and Fallback
      dat <- dat %>%
        dplyr::mutate(
          q_count_rs3 = dplyr::if_else(
            is.na(id_rs3),
            NA_integer_,
            dplyr::n_distinct(interaction(Ano, Trimestre))
          ),
          .by = "id_rs3"
        ) %>%
        dplyr::mutate(
          # id_rs3 falls back to id_rs2 if rs2 performed better
          id_rs3 = dplyr::case_when(
            q_count_rs2 == 5 ~ id_rs2,
            q_count_rs3 > q_count_rs2 & q_count_rs3 <= 5 ~ id_rs3,
            TRUE ~ dplyr::coalesce(id_rs2, id_rs3)
          ),
          q_count_rs3 = dplyr::case_when(
            q_count_rs2 == 5 ~ q_count_rs2,
            q_count_rs3 > q_count_rs2 & q_count_rs3 <= 5 ~ q_count_rs3,
            TRUE ~ dplyr::coalesce(q_count_rs2, q_count_rs3)
          )
        )

      # Discard the nest and related tracking variables from the environment
      rm(candidates, occupied_periods, nest, valid_matches, rs2_edges, cluster_map_raw, cluster_map)
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
  ## Pasting Panel Number ##
  ##########################

  # To avoid overlap when binding more than one panel (all IDs are just counts from 1, ..., N)
  # The ifelse function guards against as.hexmode(NA) which returns the string "NA" instead of a true NA

  # Basic panel
  if (panel != "none") {
    dat$id_ind <- ifelse(
      is.na(dat$id_ind),
      NA_character_,
      paste0(as.hexmode(dat$V1014), as.hexmode(dat$id_ind))
    )
  }

  # Advanced panel 1
  if (panel %in% c("advanced_1", "advanced_2", "advanced_3")) {
    dat$id_rs1 <- ifelse(
      is.na(dat$id_rs1),
      NA_character_,
      paste0(as.hexmode(dat$V1014), as.hexmode(dat$id_rs1))
    )
  }

  # Advanced panel 2
  if (panel %in% c("advanced_2", "advanced_3")) {
    dat$id_rs2 <- ifelse(
      is.na(dat$id_rs2),
      NA_character_,
      paste0(as.hexmode(dat$V1014), as.hexmode(dat$id_rs2))
    )
  }

  # Advanced panel 3
  if (panel == "advanced_3") {
    dat$id_rs3 <- ifelse(
      is.na(dat$id_rs3),
      NA_character_,
      paste0(as.hexmode(dat$V1014), as.hexmode(dat$id_rs3))
    )
  }

  #################
  ## Return Data ##
  #################

  # Return the modified dataset
  return(dat)
}
