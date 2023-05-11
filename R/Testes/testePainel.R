library(tidyverse)
library(readr)
library(readxl)
library(PNADcIBGE)
library(magrittr)
library(purrr)
#tirando a notação científica
options(scipen = 999)
#testando outro método

pnad_2021_1 <- get_pnadc(year = 2021, quarter = 1,  labels = TRUE)
vars2021_1 <- pnad_2021_1[["variables"]]%>% select(Ano, Trimestre, UF, UPA,V1008, V1014, V2003,V2005, V2007, V2008,V20081, V20082, V1023) %>% as.data.frame()

pnad_2021_2 <- get_pnadc(year = 2021, quarter = 2,  labels = TRUE)
vars2021_2<- pnad_2021_2[["variables"]]%>% select(Ano, Trimestre, UF, UPA,V1008, V1014, V2003,V2005, V2007, V2008,V20081, V20082, V1023) %>% as.data.frame()

pnad_2021_3 <- get_pnadc(year = 2021, quarter = 3,  labels = TRUE)
vars2021_3<- pnad_2021_3[["variables"]]%>% select(Ano, Trimestre, UF, UPA,V1008, V1014, V2003,V2005, V2007, V2008,V20081, V20082, V1023) %>% as.data.frame()

pnad_2021_4 <- get_pnadc(year = 2021, quarter = 4,  labels = TRUE)
vars2021_4<- pnad_2021_4[["variables"]]%>% select(Ano, Trimestre, UF, UPA,V1008, V1014, V2003,V2005, V2007, V2008,V20081, V20082, V1023) %>% as.data.frame()

pnad_2022_1 <- get_pnadc(year = 2022, quarter = 1,  labels = TRUE)
vars2022_1<- pnad_2022_1[["variables"]]%>% select(Ano, Trimestre, UF, UPA,V1008, V1014, V2003,V2005, V2007, V2008,V20081, V20082, V1023) %>% as.data.frame()

junta.pnad<- rbind(vars2021_1,vars2021_2,vars2021_3,vars2021_4, vars2022_1)

write.csv(junta.pnad, "junta_pnad.csv", row.names = FALSE)
View(junta.pnad)

############## corringindo e testando as funcções do painel
#creating the basic panel 
library(tidyverse)

cleans_dat = function(incoming_dat){
  raw_dat = incoming_dat %>%
    as.data.frame()
  
  #filtering the data base received by the user to not have year, month and day =99 or =9999
  #filter V2007 != 99? according to dictionary, there should be no 99
  filtered_dat = raw_dat %>% dplyr::filter(V2008 != "99" | V20081!= "99" | V20082!= "9999")
  
  #consider mutate_at, could be faster
  #or simply create the identifier w/ paste0()
  character_dat = filtered_dat %>%
    dplyr::mutate(V2007 = as.character(V2007)) %>%
    #factorizes gender according to code establhised in the dictionary. 1 == man; 2 == woman
    dplyr::mutate(V2007 = ifelse(V2007 == "Homem", "1", "2")) %>%
    dplyr::mutate(V2008 = as.character(V2008)) %>%
    dplyr::mutate(V20081 = as.character(V20081)) %>%
    dplyr::mutate(V20082 = as.character(V20082)) %>%
    dplyr::mutate(UF= as.character(UF)) %>% 
    dplyr::mutate(V1023= as.character(V1023))
  
  return(character_dat)
  
}


builds_identifiers = function(character_dat) {
  w_id_dom = character_dat %>%
    dplyr::bind_cols(
      # creates household identifier
      id_dom = dplyr::group_indices(as.data.frame(character_dat), character_dat$UPA, character_dat$V1008, character_dat$V1014))
  
  #creates individual identifier
  #id_ind concatenates household identifier (id_dom), birthday in Ymd format and gender
  #We also added the variables UF and V1023, which configurate a good municipality identifer in the context
  basic_panel = w_id_dom %>%
    dplyr::mutate(id_ind = paste(UF,V1023,
                                 id_dom, V20082, V20081, V2008, V2007))
  
  #returns the final product
  return(basic_panel)
}

############
#Rodando as duas funções na pnad toda junta de 2021.1 até 2022.1 (incluso)

Y<-junta_pnad %>% cleans_dat() %>% builds_identifiers()
View(Y)

# Ideia do painel- Pegar e fazer um loop, trimestre por trimestre, comparando Cada indivíduo até que ele seja matcheado, 
# podemos fazer uma estrutura de stop ou de while, em um for/if, para compararmos o id de cada individuo
# Se colocarmos como base de comparacao só a variavel id_ind, já que só conseguiremos parear por municipio mesmo, 
# devemos ter uma identificacao sem problemas.

# minha ideia é: Criar duas variaveis que assumem 1 se o match já aconteceu,
# uma para trimestres futuros ("prox"- meaning proximo), outra para tri passados ("ant"- meaning anterior)

# Depois, fazemos um comando para verificar se um certo id_ind existe no trimestre seguinte, se existir, a variavel "prox" adquire 1
# O mesmo é feito para a variavel ant


unique(junta_pnad)

