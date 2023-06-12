painel_6 = rbind(readRDS(".\\pnad2017_1_6"), readRDS(".\\pnad2017_2_6"), readRDS(".\\pnad2017_3_6"), readRDS(".\\pnad2017_4_6"), readRDS(".\\pnad2018_1_6"), readRDS(".\\pnad2018_2_6"), readRDS(".\\pnad2018_3_6"), readRDS(".\\pnad2018_4_6"), readRDS(".\\pnad2019_1_6")) %>%
  mutate(id_ind = as.character(id_ind))
#atrito via individuo- exemplo usando o painel 6
contagem_ind = table(painel_6$id_ind) %>% as.data.frame() %>%
  rename("id_ind" = "Var1", "contagem" = "Freq")

com_contagem = left_join(painel_6, contagem_ind) %>%
  dplyr::filter(V1016 == 1) #filtrando para só contarmos as pessoas que fizeram a 1a entrevista
#assim, medimos o real atrito, que é a quantidade de pessoas que fomos capazes de acompanhar ao longo das 5 entrevistas

atrito = table(com_contagem$contagem) %>% as.data.frame() %>%
  rename("quantidades" = "Var1", "ocorrencias" = "Freq") %>%
  mutate(total = sum(ocorrencias)) %>%
  mutate(porcentagem = (ocorrencias/total)*100)

#atrito via domicilio

contagem_dom = table(painel_6$id_dom) %>% as.data.frame() %>%
  rename("id_dom" = "Var1", "contagem" = "Freq")

com_contagem_dom = left_join(painel_6, contagem_dom) %>%
  dplyr::filter(V1016 == 1)

atrito_dom = table(com_contagem_dom$contagem) %>% as.data.frame() %>%
  rename("quantidades" = "Var1", "ocorrencias" = "Freq") %>%
  mutate(total = sum(ocorrencias)) %>%
  mutate(porcentagem = (ocorrencias/total)*100)
