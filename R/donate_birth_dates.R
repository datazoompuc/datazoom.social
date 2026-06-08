# Donate Birth Dates for PNADC
#
# This internal function reproduces Rafael Osorio's birth date donation method. 
# It estimates and imputes missing birth dates (day, month, and year) by matching 
# individuals with donors from different interviews within the same household based 
# on sex, acceptable household condition changes, and estimated age.
#
# @param dat A data frame with PNADC data to be processed.
#
# @return A modified dataset with donated birth dates replacing missing ones. 
# The final dataset includes updated \code{birth_day}, \code{birth_month}, and 
# \code{birth_year} variables.
# 
# @keywords internal
# @noRd
donate_birth_dates <- function(dat) {
  
  ###########################
  ## Bind Global Variables ##
  ###########################
  
  UPA <- V1008 <- V1014 <- id_dom <- V2008 <- V20081 <- V20082 <- NULL
  Ano <- V2009 <- V2005 <- V2007 <- V1016 <- V1016.x <- V1016.y <- NULL
  year_missing <- year_estimated <- summarised_condition <- diff <- NULL
  donor_day <- donor_month <- donor_year <- NULL
  
  ##########################################
  ## Pre-processing & Auxiliary Variables ##
  ##########################################
  
  prep <- dat %>%
    dplyr::mutate(
      # Create a unique household identifier
      id_dom = dplyr::cur_group_id(),
      .by = c(UPA, V1008, V1014)
    ) %>%
    dplyr::mutate(
      
      # Identify observations that are missing the birth year (those needing donations)
      year_missing = is.na(V20082),
      
      # Estimate birth year to compare with potential donors' years and choose the best candidate
      year_estimated = Ano - V2009,
      
      # Map acceptable household condition interchanges based on the methodology
      summarised_condition = dplyr::case_when(
        V2005 %in% c(1, 2, 3) ~ 1,
        V2005 %in% c(4, 5, 6) ~ 4,
        V2005 %in% c(8, 9)    ~ 8,
        TRUE ~ as.numeric(V2005)
      )
    )
  
  ###########################
  ## Define the Donor Pool ##
  ###########################
  
  donors <- prep %>%
    # Potential donors must have a known (non-missing) birth year
    dplyr::filter(!year_missing) %>%
    dplyr::select(id_dom, V1016, V2007, summarised_condition, 
                  donor_day = V2008, donor_month = V20081, donor_year = V20082)
  
  #########################################
  ## Vectorized Search for Best Donors   ##
  #########################################
  
  imputed_matches <- prep %>%
    dplyr::filter(year_missing) %>%
    # We join candidates and donors by household, sex, and condition group
    dplyr::left_join(donors, by = c("id_dom", "V2007", "summarised_condition"), 
                     relationship = "many-to-many") %>%
    # Apply Step 2.a criteria: the donor must be from a different interview, 
    # and the donor's year must be within a 3-year window of the estimated year
    dplyr::filter(
      V1016.x != V1016.y, 
      abs(donor_year - year_estimated) <= 3
    ) %>%
    # Step 2.b: Calculate the absolute difference for sorting purposes
    dplyr::mutate(diff = abs(donor_year - year_estimated)) %>%
    # Optimized selection: Arrange candidates by the closest match, then pick the first one
    dplyr::arrange(id_dom, V1016.x, V2007, summarised_condition, V2009, diff) %>%
    dplyr::distinct(id_dom, V1016.x, V2007, summarised_condition, V2009, .keep_all = TRUE)
  
  ######################
  ## Final Merge Back ##
  ######################
  
  # Combine the matches back with the original data, prioritizing original values where they exist
  final_data <- prep %>%
    dplyr::left_join(
      imputed_matches %>% 
        dplyr::select(id_dom, V1016.x, V2007, summarised_condition, V2009, 
                      donor_day, donor_month, donor_year),
      by = c("id_dom", "V1016" = "V1016.x", "V2007", "summarised_condition", "V2009")
    ) %>%
    # Step 2.c: Fallback logic using coalesce (if the original is NA, use the donor's value)
    dplyr::mutate(
      birth_day   = dplyr::coalesce(V2008, donor_day),
      birth_month = dplyr::coalesce(V20081, donor_month),
      birth_year  = dplyr::coalesce(V20082, donor_year)
    ) %>%
    # Cleanup auxiliary tracking columns
    dplyr::select(-donor_day, -donor_month, -donor_year, -year_missing, 
                  -year_estimated, -summarised_condition)
  
  #################
  ## Return Data ##
  #################
  
  # Return the modified dataset with donated birth dates
  return(final_data)
}