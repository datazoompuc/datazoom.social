#creating the basic panel 
library(tidyverse)

cleans_dat = function(incoming_dat){
  raw_dat = incoming_dat %>%
    as.data.frame()
  
  #filtering the data base received by the user to not have year, month and day =99 or =9999
  #filter V2007 != 99?
  filtered_dat = raw_dat %>% dplyr::filter(V2008 != "99" | V20081!= "99" | V20082!= "9999")
  
  #consider mutate_at, could be faster
  #or simply create the identifier w/ paste0()
  character_dat = filtered_dat %>%
    dplyr::mutate(V2007 = as.character(V2007)) %>%
    dplyr::mutate(V2008 = as.character(V2008)) %>%
    dplyr::mutate(V20081 = as.character(V20081)) %>%
    dplyr::mutate(V20082 = as.character(V20082))

  return(character_dat)
  
}


builds_identifiers = function(character_dat) {
  w_id_dom = character_dat %>%
    dplyr::bind_cols(
      # creates household identifier
      id_dom = dplyr::group_indices(as.data.frame(character_dat), character_dat$UPA, character_dat$V1008, character_dat$V1014))
  
  #creates individual identifier
  #id_ind concatenates household identifier (id_dom), birthday in Ymd format and gender
  basic_panel = w_id_dom %>%
    dplyr::mutate(id_ind = paste(id_dom, V20082, V20081, V2008, V2007))
  
  #returns the final product
  return(basic_panel)
}

