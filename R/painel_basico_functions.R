#creating the basic panel 
library(tidyverse)
#filtering the data base received by the user to not have year, month and day =99 or =9999

pnad_tri1<- pnadjunta21_1_22_1 %>% filter(Ano == 2021, Trimestre== 1)
pnad_tri2<- pnadjunta21_1_22_1 %>% filter(Ano == 2021, Trimestre== 2)
pnad_tri3<- pnadjunta21_1_22_1 %>% filter(Ano == 2021, Trimestre== 3)
pnad_tri4<- pnadjunta21_1_22_1 %>% filter(Ano == 2021, Trimestre== 4)
pnad_tri5<- pnadjunta21_1_22_1 %>% filter(Ano == 2022, Trimestre== 1)

filtra_dados= function(dados_recebidos){
  dados_recebidos<-pnad_tri1
  dados_recebidos = as.data.frame(dados_recebidos)
  
  dados_filtrados = dados_recebidos %>% dplyr::filter(V2008 != "99" | V20081!= "99" | V20082!= "9999")
  
  dados_character = dados_filtrados %>%
    dplyr::mutate(V2007 = as.character(V2007)) %>%
    dplyr::mutate(V2008 = as.character(V2008)) %>%
    dplyr::mutate(V20081 = as.character(V20081)) %>%
    dplyr::mutate(V20082 = as.character(V20082))
  com_id_dom = dados_character %>%
    dplyr::bind_cols(
      ### Adiciona identificadores tanto para municipios quanto chefe
      
      id_dom = dplyr::group_indices(as.data.frame(dados_character),dados_character$UPA, dados_character$V1008, dados_character$V1014))
  return(com_id_dom)
}
i<-filtra_dados(pnad_tri1)


