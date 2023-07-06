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

#primeiro, vamos splitar o data frame de atrito em cada par (ano-trimestre) usando a função divide_trimestres, criada no doc "testes_basico_met1.R"
painel_dividido<- divide_trimestres(painel_6)
#agora criamos uma nova lista, com os valores únicos de id_dom de cada data frame da lista criada acima
lista_unique<- list()
for (i in 1:length(painel_dividido)) {
  lista_unique[[i]] <-painel_dividido[[i]] %>% dplyr::pull(id_dom)%>% unique()%>% data.frame()
}

#criando uma coluna com todos ids únicos de cada trimestre do painel
unique_id_dom<- unlist(lista_unique)%>% data.frame()
unique_id_dom<-unique_id_dom |> rename("id_dom"=".")

#obtendo as estatísticas para calcular o atrito
contagem_dom = table(unique_id_dom$id_dom) %>% as.data.frame() %>%
  rename("quantidades" = "Var1", "ocorrencias" = "Freq")%>%
  mutate(total = sum(ocorrencias)) %>%
  mutate(porcentagem = (ocorrencias/total)*100)

#agrupando as observações pelo número de ocorrências, contamos cada uma tem e dividimos pelo total de domicílios únicos
atrito_dom= contagem_dom |> group_by(ocorrencias) |> summarise(quantidade= n()/nrow(contagem_dom))

#fazendo um loop para termos, em uma lista, um data frame com o atrito individual para cada painel
painel_desejado<-data.frame()
atritos_juntos_painel<- list()

for (i in 2:9) { #não existe, no painel 1, nenhum indivíduo que está na 1a entrevista, penso que por ser uma fase de transição entre PNAD e PNADc, ou algo do tipo

painel_desejado<- panel_data_list[[i]]
#atrito via individuo- exemplo usando o painel 6
contagem_ind = painel_desejado %>% dplyr::pull(id_ind) %>% table() %>% as.data.frame() %>%
  rename_with(~ "id_ind", 1) |> rename("contagem" = "Freq")

com_contagem = left_join(painel_desejado, contagem_ind) %>%
  dplyr::filter(V1016 == 1) #filtrando para só contarmos as pessoas que fizeram a 1a entrevista
#assim, medimos o real atrito, que é a quantidade de pessoas que fomos capazes de acompanhar ao longo das 5 entrevistas
atrito = table(com_contagem$contagem) %>% as.data.frame()%>%
  rename_with(~ "quantidades", 1) |> rename("ocorrencias"= "Freq") %>% #rename_with= função para mudar uma coluna específica, só sabendo a posição dela no data frame
  mutate(total = sum(ocorrencias)) %>%
  mutate(porcentagem = (ocorrencias/total)*100)
atritos_juntos_painel[[i-1]]<- atrito #deve-se comentar que o 1o objeto nessa lista é referente ao SEGUNDO painel, por conta do problema citado na linha 33
}
