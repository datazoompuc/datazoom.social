#novo método de cálculo do atrito
library(tidyverse)
library(readxl)
directory<- "C:/Users/tuca1/OneDrive/Documentos/Datazoom/Painel_PNAD/Paineis"
paineis_juntos<-bundle_panel(directory)
lista_atrito<-list()

# Create the data frame
cria_df_de_atrito<-function(data){
data$V1016<- as.integer(data$V1016) #transformando a variável 1016 em integer para facilitar o filter abaixo
presentes_na_1a_entrevista= data |> filter(V1016== 1) |> pull(id_ind) |> as.vector() #criando um vetor com todas as id's dos indivíduos que estavam na 1a entrevista
#filtrando para a base só ter as pessoas que participaram da 1a entrevista
data<- data |> filter(id_ind %in% presentes_na_1a_entrevista)
########################################################################################################

summary_data <- data %>%
  group_by(id_ind) %>% #grouping by each individual
  summarize(appearances = list(V1016), #in this way, we can "paste" in a single line the interviews the person has appeared at
            disappearances = list(setdiff(1:5, unique(V1016)))) %>% #and then we can set the difference from the 1:5 vector, which will return the interviews not attended by the each one
  rowwise() %>% #perform the next commands line by line (it was doing the "unlist()" command to ALL observations of the "disappearances" column)
  mutate(missing_quarters = paste(as.character(unlist(disappearances)), collapse = " "),
         first_interview= ifelse("1" %in% unlist(disappearances), 1, 0),
        second_interview= ifelse("2" %in% unlist(disappearances), 1, 0),#the logic of this line applies to the others, if the observation contains the string "2" in its disappearances columns, this means this person was not present in the 2nd interview, therefore, this column gets the value of 1.
         third_interview= ifelse("3" %in% unlist(disappearances), 1, 0),
         fourt_interview= ifelse("4" %in% unlist(disappearances), 1, 0),
         fifth_interview= ifelse("5" %in% unlist(disappearances), 1, 0))

soma_quinta_int<-sum(as.integer(summary_data$fifth_interview))
# comparando a soma dessa coluna da quinta entrevista com a diferença de pessoas entre a 1a e 5a entrevista:
# vemos que a soma da coluna(116848) é bem semelhante à diferença (calculada abaixo)

# CALCULANDO O ATRITO PARA O PAINEL 6

atrito_definite= data.frame(Entrevista=seq(1,5),"Contagem de faltantes"= c(0,0,0,0,0))
for (i in 5:ncol(summary_data)) {
  atrito_definite[i-4,2]<- sum(summary_data[,i])# tiramos 4 pois queremos adicionar às 5 primeiras colunas de atrito_definite os dados das colunas 5:8 de summary_data
}

atrito_definite$Percentage_found= 100*(round(1-(atrito_definite$Contagem.de.faltantes/nrow(summary_data)),5))# nrow(summary_data)= Número de presentes na 1a entrevista
# atrito_definite$Contagem.de.faltantes= Contagem de faltantes por entrevista.
return(atrito_definite)
}

lista_atrito<- lapply(paineis_juntos, cria_df_de_atrito) #aplicando, para cada dataframe da lista que contém os paineis de 2 a 7, a função que cria o dataframe de atrito.
df_atrito<- unlist(lista_atrito) |> data.frame() #dando unlist nesses dados e gerando um dataframe (bem desorganizado, tive que mexer bastante na mão no Excel para formar a planilha)
#adicionando cada data frame de lista_atrito à uma planilha de excel

writexl::write_xlsx(df_atrito, "output.xlsx")

