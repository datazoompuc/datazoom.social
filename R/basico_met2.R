#compartilhado por romero
#desenvolvido por matheus leal
rm(list = ls())


library(dplyr) 
library(bit64)
library(data.table)
library(descr)
library(readr)
library(survey)
library(checkmate)
library(lme4)
library(ggplot2)
library(gganimate)
library(gifski)
library(png)
library(transformr)
library(zoo)
library(foreach)
library(readxl)
library(reshape2)
library(tidyr)
library(rdrobust)
library(plm)
library(pglm)
library(PNADcIBGE)
library(survey)

#Seleciona o diretório de trabalho
setwd("C:/Users/matheus.leal/Documents/PNADs Cont/Trimestral")



#Lê as PNADs

PNADC2018_1 <- read_pnadc(microdata = "PNADC_022017_educacao.txt", input_txt="Input_PNADC_trimestral_educacao_20190619.txt", vars = c("VD4002", "VD4019", "VD3004", "V3018", "V3017","V3002", "V3003", "V3003A", "V3002A", "Ano","Trimestre","V2008","V20081","V20082","UPA","V1008","V1014","V1016","V1022","V4040","V40401","V40402","V40403","V2009","V2007","V2010","UF","VD4031", "VD3005", "VD4009", "VD4011"))
PNADC2018_2 <- read_pnadc(microdata = "PNADC_032017_20190516.txt", input_txt="Input_PNADC_trimestral.txt", vars= c("UPA", "UF", "Trimestre", "Ano", "V1008", "V1014", "V1016", "V2007", "V2010", "V1022", "V20082", "VD4002",  "V2008", "V20081", "VD4031", "VD3004", "V2009", "VD4019", "V4040", "V40401", "V40402", "V40403", "VD3005", "VD4009", "VD4011"))
PNADC2018_4 <- read_pnadc(microdata = "PNADC_042017_20190516.txt", input_txt="Input_PNADC_trimestral.txt", vars= c("UPA", "UF", "Trimestre", "Ano", "V1008", "V1014", "V1016", "V2007", "V2010", "V1022", "V20082", "VD4002",  "V2008", "V20081", "VD4031", "VD3004", "V2009", "VD4019", "V4040", "V40401", "V40402", "V40403", "VD3005", "VD4009", "VD4011"))
PNADC2019_1 <- read_pnadc(microdata = "PNADC_012018_20190516.txt", input_txt="Input_PNADC_trimestral.txt", vars= c("UPA", "UF", "Trimestre", "Ano", "V1008", "V1014", "V1016", "V2007", "V2010", "V1022", "V20082", "VD4002",  "V2008", "V20081", "VD4031", "VD3004", "V2009", "VD4019", "V4040", "V40401", "V40402", "V40403", "VD3005", "VD4009", "VD4011"))
PNADC2019_2 <- read_pnadc(microdata = "PNADC_022018_educacao.txt", input_txt="Input_PNADC_trimestral_educacao_20190619.txt", vars = c("VD4002", "VD4019", "VD3004", "V3018", "V3017","V3002", "V3003", "V3003A", "V3002A", "Ano","Trimestre","V2008","V20081","V20082","UPA","V1008","V1014","V1016","V1022","V4040","V40401","V40402","V40403","V2009","V2007","V2010","UF","VD4031", "VD3005", "VD4009", "VD4011"))
PNADC2019_3 <- read_pnadc(microdata = "PNADC_012018_20190516.txt", input_txt="Input_PNADC_trimestral.txt", vars= c("UPA", "UF", "Trimestre", "Ano", "V1008", "V1014", "V1016", "V2007", "V2010", "V1022", "V20082", "VD4002",  "V2008", "V20081", "VD4031", "VD3004", "V2009", "VD4019", "V4040", "V40401", "V40402", "V40403", "VD3005", "VD4009", "VD4011"))
PNADC2018_4 <- read_pnadc(microdata = "PNADC_022018_educacao.txt", input_txt="Input_PNADC_trimestral_educacao_20190619.txt", vars = c("VD4002", "VD4019", "VD3004", "V3018", "V3017","V3002", "V3003", "V3003A", "V3002A", "Ano","Trimestre","V2008","V20081","V20082","UPA","V1008","V1014","V1016","V1022","V4040","V40401","V40402","V40403","V2009","V2007","V2010","UF","VD4031", "VD3005", "VD4009", "VD4011"))


#Alguns tratamentos nas variáveis, transformando as numéricas em numéricas
base1 <- PNADC2018_1 %>%
  mutate(V4040 = as.numeric(V4040),
         V40401 = as.numeric(V40401),
         V40402 = as.numeric(V40402),
         V40403 = as.numeric(V40403),
         VD4002 = as.numeric(VD4002),
         VD4019 = as.numeric(VD4019),
         VD3004 = as.numeric(VD3004),
         V3018 = as.numeric(V3018),
         V3017 = as.numeric(V3017),
         V3003A = as.numeric(V3003A),
         V3002A = as.numeric(V3002A),
         V2008 = as.numeric(V2008),
         V20081 = as.numeric(V20081),
         V20082 = as.numeric(V20082),
         UPA = as.numeric(UPA),
         V1008 = as.numeric(V1008),
         V1014 = as.numeric(V1014),
         Trimestre = as.numeric(Trimestre),
         V1016 = as.numeric(V1016),
         V1022 = as.numeric(V1022),
         V2009 = as.numeric(V2009),
         V2007 = as.numeric(V2007),
         V2010 = as.numeric(V2010),
         UF = as.numeric(UF),
         VD4031 = as.numeric(VD4031),
         V3002 = as.numeric(V3002),
         V3003 = as.numeric(V3003),
         VD3005 = as.numeric(VD3005),
         VD4009 = as.numeric(VD4009),
         VD4011 = as.numeric(VD4011)) %>% 
  filter(V1016 == 1)   #Aqui eu seleciono somente pessoas na primeira entrevista


base2 <- PNADC2018_2 %>%
  mutate(V4040 = as.numeric(V4040),
         V40401 = as.numeric(V40401),
         V40402 = as.numeric(V40402),
         V40403 = as.numeric(V40403),
         VD4002 = as.numeric(VD4002),
         VD4019 = as.numeric(VD4019),
         VD3004 = as.numeric(VD3004),
         V3018 = as.numeric(V3018),
         V3017 = as.numeric(V3017),
         V3003A = as.numeric(V3003A),
         V3002A = as.numeric(V3002A),
         V2008 = as.numeric(V2008),
         V20081 = as.numeric(V20081),
         V20082 = as.numeric(V20082),
         UPA = as.numeric(UPA),
         V1008 = as.numeric(V1008),
         V1014 = as.numeric(V1014),
         Trimestre = as.numeric(Trimestre),
         V1016 = as.numeric(V1016),
         V1022 = as.numeric(V1022),
         V2009 = as.numeric(V2009),
         V2007 = as.numeric(V2007),
         V2010 = as.numeric(V2010),
         UF = as.numeric(UF),
         VD4031 = as.numeric(VD4031),
         V3002 = as.numeric(V3002),
         V3003 = as.numeric(V3003),
         VD3005 = as.numeric(VD3005),
         VD4009 = as.numeric(VD4009),
         VD4011 = as.numeric(VD4011)) %>% 
  filter(V1016 == 2)   #Aqui eu seleciono somente pessoas na segunda entrevista



base3 <- PNADC2018_3 %>%
  mutate(V4040 = as.numeric(V4040),
         V40401 = as.numeric(V40401),
         V40402 = as.numeric(V40402),
         V40403 = as.numeric(V40403),
         VD4002 = as.numeric(VD4002),
         VD4019 = as.numeric(VD4019),
         VD3004 = as.numeric(VD3004),
         V3018 = as.numeric(V3018),
         V3017 = as.numeric(V3017),
         V3003A = as.numeric(V3003A),
         V3002A = as.numeric(V3002A),
         V2008 = as.numeric(V2008),
         V20081 = as.numeric(V20081),
         V20082 = as.numeric(V20082),
         UPA = as.numeric(UPA),
         V1008 = as.numeric(V1008),
         V1014 = as.numeric(V1014),
         Trimestre = as.numeric(Trimestre),
         V1016 = as.numeric(V1016),
         V1022 = as.numeric(V1022),
         V2009 = as.numeric(V2009),
         V2007 = as.numeric(V2007),
         V2010 = as.numeric(V2010),
         UF = as.numeric(UF),
         VD4031 = as.numeric(VD4031),
         V3002 = as.numeric(V3002),
         V3003 = as.numeric(V3003),
         VD3005 = as.numeric(VD3005),
         VD4009 = as.numeric(VD4009),
         VD4011 = as.numeric(VD4011)) %>% 
  filter(V1016 == 3)   #Aqui eu seleciono somente pessoas na terceira entrevista



base4 <- PNADC2018_4 %>%
  mutate(V4040 = as.numeric(V4040),
         V40401 = as.numeric(V40401),
         V40402 = as.numeric(V40402),
         V40403 = as.numeric(V40403),
         VD4002 = as.numeric(VD4002),
         VD4019 = as.numeric(VD4019),
         VD3004 = as.numeric(VD3004),
         V3018 = as.numeric(V3018),
         V3017 = as.numeric(V3017),
         V3003A = as.numeric(V3003A),
         V3002A = as.numeric(V3002A),
         V2008 = as.numeric(V2008),
         V20081 = as.numeric(V20081),
         V20082 = as.numeric(V20082),
         UPA = as.numeric(UPA),
         V1008 = as.numeric(V1008),
         V1014 = as.numeric(V1014),
         Trimestre = as.numeric(Trimestre),
         V1016 = as.numeric(V1016),
         V1022 = as.numeric(V1022),
         V2009 = as.numeric(V2009),
         V2007 = as.numeric(V2007),
         V2010 = as.numeric(V2010),
         UF = as.numeric(UF),
         VD4031 = as.numeric(VD4031),
         V3002 = as.numeric(V3002),
         V3003 = as.numeric(V3003),
         VD3005 = as.numeric(VD3005),
         VD4009 = as.numeric(VD4009),
         VD4011 = as.numeric(VD4011)) %>% 
  filter(V1016 == 4)   #Aqui eu seleciono somente pessoas na quarta entrevista




base5 <- PNADC2019_1 %>%
  mutate(V4040 = as.numeric(V4040),
         V40401 = as.numeric(V40401),
         V40402 = as.numeric(V40402),
         V40403 = as.numeric(V40403),
         VD4002 = as.numeric(VD4002),
         VD4019 = as.numeric(VD4019),
         VD3004 = as.numeric(VD3004),
         V3018 = as.numeric(V3018),
         V3017 = as.numeric(V3017),
         V3003A = as.numeric(V3003A),
         V3002A = as.numeric(V3002A),
         V2008 = as.numeric(V2008),
         V20081 = as.numeric(V20081),
         V20082 = as.numeric(V20082),
         UPA = as.numeric(UPA),
         V1008 = as.numeric(V1008),
         V1014 = as.numeric(V1014),
         Trimestre = as.numeric(Trimestre),
         V1016 = as.numeric(V1016),
         V1022 = as.numeric(V1022),
         V2009 = as.numeric(V2009),
         V2007 = as.numeric(V2007),
         V2010 = as.numeric(V2010),
         UF = as.numeric(UF),
         VD4031 = as.numeric(VD4031),
         V3002 = as.numeric(V3002),
         V3003 = as.numeric(V3003),
         VD3005 = as.numeric(VD3005),
         VD4009 = as.numeric(VD4009),
         VD4011 = as.numeric(VD4011)) %>% 
  filter(V1016 == 5)   #Aqui eu seleciono somente pessoas na quinta entrevista



#Removo quem tem missing nas variáveis de data de nascimento
base1 <- base1 %>% filter(V2008 != 99 & V20081 != 99)
base2 <- base2 %>% filter(V2008 != 99 & V20081 != 99)
base3 <- base3 %>% filter(V2008 != 99 & V20081 != 99)
base4 <- base4 %>% filter(V2008 != 99 & V20081 != 99)
base5 <- base5 %>% filter(V2008 != 99 & V20081 != 99)


#Crio as variáveis de identificador do domicílio e de data de nascimento
base1 <- base1 %>% mutate(id_dom = as.numeric(paste(UPA,V1008,V1014, sep = "")),
                          datanasc=as.numeric(paste(V2008,V20081,V20082, sep = "")))

#Rankear os moradores do mesmo domicílio a partir da data de nascimento
PDC2018_1 <- base1 %>% group_by(id_dom) %>% mutate(rankdata=rank(datanasc))

#Corrige problemas no rankeamento
base1 <- base1 %>% filter(rankdata==round(rankdata, digits = 0))


#Crio as variáveis de identificador do domicílio e de data de nascimento
base2 <- base2 %>% mutate(id_dom = as.numeric(paste(UPA,V1008,V1014, sep = "")),
                          datanasc=as.numeric(paste(V2008,V20081,V20082, sep = "")))

#Rankear os moradores do mesmo domicílio a partir da data de nascimento
base2 <- base2 %>% group_by(id_dom) %>% mutate(rankdata=rank(datanasc))

#Corrige problemas no rankeamento
base2 <- base2 %>% filter(rankdata==round(rankdata, digits = 0))



#Crio as variáveis de identificador do domicílio e de data de nascimento
base3 <- base3 %>% mutate(id_dom = as.numeric(paste(UPA,V1008,V1014, sep = "")),
                          datanasc=as.numeric(paste(V2008,V20081,V20082, sep = "")))

#Rankear os moradores do mesmo domicílio a partir da data de nascimento
base3 <- base3 %>% group_by(id_dom) %>% mutate(rankdata=rank(datanasc))

#Corrige problemas no rankeamento
base3 <- base3 %>% filter(rankdata==round(rankdata, digits = 0))



#Crio as variáveis de identificador do domicílio e de data de nascimento
base4 <- base4 %>% mutate(id_dom = as.numeric(paste(UPA,V1008,V1014, sep = "")),
                          datanasc=as.numeric(paste(V2008,V20081,V20082, sep = "")))

#Rankear os moradores do mesmo domicílio a partir da data de nascimento
base4 <- base4 %>% group_by(id_dom) %>% mutate(rankdata=rank(datanasc))

#Corrige problemas no rankeamento
base4 <- base4 %>% filter(rankdata==round(rankdata, digits = 0))




#Crio as variáveis de identificador do domicílio e de data de nascimento
base5 <- base5 %>% mutate(id_dom = as.numeric(paste(UPA,V1008,V1014, sep = "")),
                          datanasc=as.numeric(paste(V2008,V20081,V20082, sep = "")))

#Rankear os moradores do mesmo domicílio a partir da data de nascimento
base5 <- base5 %>% group_by(id_dom) %>% mutate(rankdata=rank(datanasc))

#Corrige problemas no rankeamento
base5 <- base5 %>% filter(rankdata==round(rankdata, digits = 0))


#Finalmente, o identificador de indivíduo
base1 <- base1 %>% mutate(id_pes=paste(id_dom,datanasc, sep = ""))
base2 <- base2 %>% mutate(id_pes=paste(id_dom,datanasc, sep = ""))
base3 <- base3 %>% mutate(id_pes=paste(id_dom,datanasc, sep = ""))
base4 <- base4 %>% mutate(id_pes=paste(id_dom,datanasc, sep = ""))
base5 <- base5 %>% mutate(id_pes=paste(id_dom,datanasc, sep = ""))


#Empilha as bases em painel
painel1 <- rbind(base1, base2, base3, base4, base5)


#Remove quem está com missing na variável de identificação de indivíduo
painel1 <- painel1 %>% filter(!is.na(id_pes))


#Mantém somente quem aparece nas 5 visitas
painel1 <- painel1 %>% group_by(id_pes) %>% filter(n()==5)


#Variável de data (ano-tri) somente para se guiar melhor
painel1 <- painel1 %>%  mutate(AnoTrim=paste(Ano,Trimestre,sep="-"))


###############################################################################################
# Versão DZ para esse método- transformando tudo feito em uma função, para facilitar a execução
###############################################################################################


# cada funcao é aplicada para um trimestre, entao podemos usar a funcao divide_trismestres, criada no arquivo basico_met1, para dividir qualquer base de dados por trimestres
divide_trimestres<- function(vars_list_compiled){ #recebe como objeto um dataframe com vários trimestres da PNAD
  junta_pnad_1<- vars_list_compiled %>% mutate(Tri_ano = paste0(Ano,Trimestre))
  junta_pnad_2<- list()
  junta_pnad_2 <- junta_pnad_1 %>% split(.$Tri_ano)
  return(junta_pnad_2)
}

cria_painel_met2<- function(x){
  x_1 <- x %>% filter(V2008 != 99 & V20081 != 99)
  x_2 <- x_1 %>% mutate(id_dom = as.numeric(paste(UPA,V1008,V1014, sep = "")),
                        datanasc=as.numeric(paste(V2008,V20081,V20082, sep = "")))
  
  #Rankear os moradores do mesmo domicílio a partir da data de nascimento
  PDC2018_1 <- x_2 %>% group_by(id_dom) %>% mutate(rankdata=rank(datanasc))
  
  #Corrige problemas no rankeamento
  x_2 <- PDC2018_1 %>% filter(rankdata==round(rankdata, digits = 0)) %>% mutate(id_pes=paste(id_dom,datanasc, sep = ""))
  return(x_2)
}

#aplicando para o painel 6
#dividindo o data frame em todos as combinacoes Ano-trimestre que ele contem
painel_6_trimestres_divididos<-divide_trimestres(painel_6)
#aplicando o metodo para cada trimestre, depois juntando-o (depois, percebi que isso não seria necessário, mas o método funciona, então não quero mudar)
painel_met2<- list()
for (i in 1:length(painel_6_trimestres_divididos)) {
  painel_met2[[i]]<-painel_6_trimestres_divididos[[i]] %>% as.data.frame() |> cria_painel_met2()
}
#juntando tudo em um só dataframe
resultado_painel6_met2<- data.frame()
for (i in 1:length(painel_met2)) {
  resultado_painel6_met2<- dplyr::bind_rows(resultado_painel6_met2, painel_met2[[i]])
}
#primeiro comentário- tamanho do painel_6 com met1 = 1.898.413
#tamanho do painel 6 com met2= 1,886,671- eu imagino que seja devido ao comando na linha 335, que filtra para os ind sem problemas no rankeamento

#Remove quem está com missing na variável de identificação de indivíduo
painel6_met2 <- resultado_painel6_met2 |> filter(!is.na(id_pes))


#Mantém somente quem aparece nas 5 visitas
painel6_met2 <- painel1 %>% group_by(id_pes) %>% filter(n()==5) |> select(Ano, Trimestre, id_pes, everything())

###########################################################
#testando os resultados
###########################################################

tabela_resultados<- sort(table(painel1$id_pes), decreasing= T) %>% as.data.frame()
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

