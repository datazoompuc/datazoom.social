library(tidyverse)
library(readr)
library(readxl)
library(PNADcIBGE)
library(magrittr)
library(purrr)
#tirando a notação científica
options(scipen = 999)

#### loop para criar identificadores por painel por trimestre e salvar
#fazendo um loop pra baixar cada trimestre da PNAD desde 2012
pnad_list <- list() # create an empty list to store the data frames
vars_list <- list() # create an empty list to store the data frames
panel_list <- list() # create an empty list to store the data frames

for (i in 2019:2023) {
  for(j in 1:4) {
      pnad_list[[paste0("pnad", i, "_", j)]] = get_pnadc(year = i, quarter = j, labels = TRUE)
      vars_list[[paste0("vars", i, "_", j)]] = pnad_list[[paste0("pnad", i, "_", j)]]$variables %>%
        select(Ano, Trimestre, UF, UPA,V1008, V1014, V2003,V2005, V2007, V2008,V20081, V20082, V1023, V1016) %>%
        as.data.frame()
      for(k in 1:9) {
      panel_list[[paste0("pnad", i, "_", j, "_", k)]] = vars_list[[paste0("vars", i, "_", j)]] %>%
        as.data.frame() %>%
        dplyr::filter(as.integer(V1014) == k) %>%
        cleans_dat() %>%
        builds_identifiers()
      panel.intermediary<-panel_list[[paste0("pnad", i, "_", j, "_", k)]] %>% as.data.frame()
      if(nrow(panel.intermediary)>5000){
        saveRDS(panel.intermediary, file = paste0(".\\pnad", i, "_", j, "_", k))
      } else {
        rm(panel.intermediary)}
    }
  }
}

# Create an empty list to store the data frames for each panel
panel_data_list <- list()

# Loop through panels 1 to 9
for (panel in 1:9) {
  # Create a regular expression pattern to match the files for the current panel
  pattern <- paste0("_", panel,"$")

  # Get the list of files in the directory that match the pattern
  file_list <- list.files(directory, pattern = pattern, full.names = TRUE)

  # Create an empty data frame to store the combined data for the current panel
  panel_data <- data.frame()

  # Read and combine the RDS files that match the pattern for the current panel
  for (file in file_list) {
    panel_file_data <- readRDS(file)  # Load the RDS file
    panel_data <- rbind(panel_data, panel_file_data)  # Combine with existing data
  }

  # Store the combined data frame for the current panel in the list
  panel_data_list[[panel]] <- panel_data
}
################################################################################


for (i in 1:length(panel_data_list)) {

  coluna_desejada<- panel_data_list[[2]] %>% dplyr::pull(id_ind) %>% data.frame()
  x <- sort(table(coluna_desejada), decreasing = TRUE) %>% data.frame()
tabela.matches<-x |> group_by(Freq) %>% summarise(contagem= n()) |> mutate(porcentagem= round(100*(contagem/sum(contagem)),2))

  plot <- ggplot(data = tabela.matches, mapping = aes(x = Freq, y = contagem)) +
    geom_col() +
    geom_text(aes(label = paste0(porcentagem,"%")), position = position_stack(vjust = 1.15), color = "red", size = 3)

  # Save the plot to a file
  ggsave(paste0("grafico_painel_", i, ".png"), plot = plot)
}
#### rodando metodo 1 para o painel 6
# arthur baixou e enviou csv para laura por email, por isso aqui nao temos a parte do download
# painel 6 vai 2017.3 até 2019.3

painel_6 = read.csv(file = ".\\painel_6.csv")

x<- sort(table(painel_6$id_ind), decreasing= T)
x<- data.frame(x)
frequencia<- c()
frequencia[1]<-filter(data.frame(x), Freq== 1)%>% nrow()
frequencia[2]<-filter(data.frame(x), Freq== 2)%>% nrow()
frequencia[3]<-filter(data.frame(x), Freq== 3)%>% nrow()
frequencia[4]<-filter(data.frame(x), Freq== 4)%>% nrow()
frequencia[5]<-filter(data.frame(x), Freq== 5)%>% nrow()
frequencia[6]<-filter(data.frame(x), Freq== 6)%>% nrow()
frequencia[7]<-filter(data.frame(x), Freq== 7)%>% nrow()
frequencia[8]<-filter(data.frame(x), Freq== 8)%>% nrow()
frequencia[9]<-filter(data.frame(x), Freq== 9)%>% nrow()
frequencia[10]<-filter(data.frame(x), Freq== 10)%>% nrow()
frequencia[11]<-filter(data.frame(x), Freq== 11)%>% nrow()
frequencia[12]<-filter(data.frame(x), Freq== 12)%>% nrow()

matriz<- data.frame(frequencia, contagem= seq(1:12))

ggplot(data= matriz, mapping = aes(x= contagem, y= frequencia))+
  geom_col()+geom_text(aes(label= frequencia), position= position_stack(vjust= 1.20), color= "red", size=4)


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

#fazendo um loop pra baixar cada trimestre da PNAD desde 2012
pnad_list <- list() # create an empty list to store the data frames
vars_list <- list() # create an empty list to store the data frames

for (i in 2012:2023) {
  for(j in 1:4){
    pnad_list[[paste0("pnad", i, "_", j)]] <- get_pnadc(year = i, quarter = j, labels = TRUE)
    vars_list[[paste0("vars", i, "_", j)]] <- pnad_list[[paste0("pnad", i, "_", j)]]$variables %>%
      select(Ano, Trimestre, UF, UPA,V1008, V1014, V2003,V2005, V2007, V2008,V20081, V20082, V1023) %>%
      as.data.frame()
  }
}
#in this case, the variables we'll want will be in the "vars_list" object
# For example, pnad_list[["pnad2012_1"]] should give you the data frame for the first quarter of 2012.

#creating the basic panel
library(tidyverse)


#criando uma função que lê o data frame e divide ele em trimestres, para assim podermos criar o id_dom corretamente para cada domicílio
#(se fizermos para vários trimestres, ele cria id_dom diferentes, mesmo se for o mesmo domicílio, repetido ao longo do tempo)
divide_trimestres<- function(vars_list_compiled){ #recebe como objeto um dataframe com vários trimestres da PNAD
  junta_pnad_1<- vars_list_compiled %>% mutate(Tri_ano = paste0(Ano,Trimestre))
  junta_pnad_2<- list()
  junta_pnad_2 <- junta_pnad_1 %>% split(.$Tri_ano)

}
#testando a função
#pnads_divididas<-divide_trimestres(junta_pnad)
####################################################################################################################################################
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

####################################################################################################################################################
#building the identifiers for household (id_dom) and individuals (id_ind)
builds_identifiers = function(character_dat) {
  w_id_dom = character_dat %>%
    dplyr::bind_cols(
      # creates household identifier
      id_dom = paste0(character_dat$UPA, character_dat$V1008, character_dat$V1014))

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
#primeiro, vamos criar um dataframe que tem o primeiro objeto da lista, depois, vamos aplicar cada uma das duas funções ao objeto subsequente da lista e dar um r_bind juntando o objeto anterior à ele

trata_e_junta_trimestres<-function(x){
  objeto_inicial<- data.frame(x[[1]]) %>% cleans_dat() %>% builds_identifiers()
  for (i in 2:length(x)) {
    objeto_intermediario<-data.frame(x[[i]]) %>% cleans_dat() %>% builds_identifiers()
  objeto_inicial<-rbind(objeto_inicial,objeto_intermediario)
  }
  return(objeto_inicial)
}
# Ideia do painel- Pegar e fazer um loop, trimestre por trimestre, comparando Cada indivíduo até que ele seja matcheado,
# podemos fazer uma estrutura de stop ou de while, em um for/if, para compararmos o id de cada individuo
# Se colocarmos como base de comparacao só a variavel id_ind, já que só conseguiremos parear por municipio mesmo,
# devemos ter uma identificacao sem problemas.

# minha ideia é: Criar duas variaveis que assumem 1 se o match já aconteceu,
# uma para trimestres futuros ("prox"- meaning proximo), outra para tri passados ("ant"- meaning anterior)

# Depois, fazemos um comando para verificar se um certo id_ind existe no trimestre seguinte, se existir, a variavel "prox" adquire 1
# O mesmo é feito para a variavel ant
pnad_certa<-trata_e_junta_trimestres(pnads_divididas)

x<- sort(table(pnad_certa$id_ind), decreasing= T)
x<- data.frame(x)
frequencia<- c()
frequencia[1]<-filter(data.frame(x), Freq== 1)%>% nrow()
frequencia[2]<-filter(data.frame(x), Freq== 2)%>% nrow()
frequencia[3]<-filter(data.frame(x), Freq== 3)%>% nrow()
frequencia[4]<-filter(data.frame(x), Freq== 4)%>% nrow()
frequencia[5]<-filter(data.frame(x), Freq== 5)%>% nrow()
frequencia[6]<-filter(data.frame(x), Freq== 6)%>% nrow()
frequencia[7]<-filter(data.frame(x), Freq== 7)%>% nrow()
frequencia[8]<-filter(data.frame(x), Freq== 8)%>% nrow()
frequencia[9]<-filter(data.frame(x), Freq== 9)%>% nrow()
frequencia[10]<-filter(data.frame(x), Freq== 10)%>% nrow()
frequencia[11]<-filter(data.frame(x), Freq== 11)%>% nrow()
frequencia[12]<-filter(data.frame(x), Freq== 12)%>% nrow()

matriz<- data.frame(frequencia, contagem= seq(1:12))

ggplot(data= matriz, mapping = aes(x= contagem, y= frequencia))+
  geom_col()
