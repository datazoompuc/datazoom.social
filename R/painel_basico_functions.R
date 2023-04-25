#creating the basic panel 
library(tidyverse)

filtra_dados = function(dados_recebidos){
  dados_raw = dados_recebidos %>%
    as.data.frame()
  
  #filtering the data base received by the user to not have year, month and day =99 or =9999
  #necessario filtrar para V2007 != 99?
  dados_filtrados = dados_raw %>% dplyr::filter(V2008 != "99" | V20081!= "99" | V20082!= "9999")
  
  #considerar fazer com mutate_at, pode ser mais rapido
  #outra opcao e pular essa etapa e criar os identificadores com paste0()
  dados_character = dados_filtrados %>%
    dplyr::mutate(V2007 = as.character(V2007)) %>%
    dplyr::mutate(V2008 = as.character(V2008)) %>%
    dplyr::mutate(V20081 = as.character(V20081)) %>%
    dplyr::mutate(V20082 = as.character(V20082))
  
  com_id_dom = dados_character %>%
    dplyr::bind_cols(
      # adiciona identificadores tanto para domicilio
      id_dom = dplyr::group_indices(as.data.frame(dados_character),dados_character$UPA, dados_character$V1008, dados_character$V1014))
  
  #cria identificador basico
  #id_ind concatena identificador de domicilio, data de nascimento no formado Ymd e sexo
  basic_panel = com_id_dom %>%
    dplyr::mutate(id_ind = paste(id_dom, V20082, V20081, V2008, V2007))
  
  #retorna produto final
  return(basic_panel)
}

test_run = filtra_dados(pnad_tri1)


