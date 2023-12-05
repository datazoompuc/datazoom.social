# Function to build a panel dataset from PNADC data based on the chosen panel algorithm
build_pnadc_panel <- function(dat, panel) {
  
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
    
    # Individual id combines the household id with UF, V1023, V2007, and date of birth( V20082, V20081, V2008), creating an unique number for every combination of those variables, all through the function cur_group_id
    dat <- dat %>%
      dplyr::mutate(
        id_ind = dplyr::cur_group_id(),
        .by = c(id_dom, UF, V1023, V20082, V20081, V2008, V2007)
      )
  }
  
  #############################
  ## Advanced Identification ##
  #############################
  
  # Placeholder for advanced identification steps (currently empty)
  if (panel == "advanced") {
    # Additional steps for advanced panel identification can be added here
  }
  
  #################
  ## Return Data ##
  #################
  
  # Handle unidentifiable observations due to missing values
  dat <- dat %>% dplyr::mutate(
    id_ind = dplyr::case_when(
      V2008 != "99" | V20081 != "99" | V20082 != "9999" ~ NA,
      .default = id_ind
    )
  )
  
  # Return the modified dataset
  return(dat)
}
