build_pnadc_panel <- function(dat, panel) {
  
  if (panel == "none") {
    return(dat)
  }
  
  ##########################
  ## Basic Identification ##
  ##########################
  
  if (panel != "none") {
    
    # household identifier combines UPA, selection number, and panel
    
    dat <- dat %>%
      dplyr::mutate(
        id_dom = dplyr::cur_group_id(),
        .by = c(UPA, V1008, V1014)
      )
    
    # individual id combines the household id with state, type of area, sex, and date of birth
    
    dat <- dat %>%
      dplyr::mutate(
        id_ind = dplyr::cur_group_id(),
        .by = c(id_dom, UF, V1023, V20082, V20081, V2008, V2007)
      )
  }
  
  #############################
  ## Advanced Identification ##
  #############################
  
  if (panel == "advanced") {
    
  }
  
  #################
  ## Return Data ##
  #################
  
  ## unidentifiable observations due to missing values
  
  dat <- dat %>% dplyr::mutate(
          id_ind = dplyr::case_when(
            V2008 != "99" | V20081 != "99" | V20082 != "9999" ~ NA,
            .default = id_ind
          )
        )
  
  return(dat)
}