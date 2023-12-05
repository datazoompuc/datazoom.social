##########################
###               Build advanced PNADC panel
### This file contains PNADC advanced panel related functions
##########################
setwd("C:/Users/tuca1/OneDrive/Documentos/GitHub/Datazoom-Social_R/datazoom.social/R/PNADC")

library(haven)
library(tidyverse)
library(ggplot2)
library(data.table)
source("basic panel functions.R")
# We will create the advanced panel for Panel 6 first, as an example, and then generalize it with functions and lists in order to apply the advanced panel to all panels.

# Firstly, we'll filter the observations that have already been identified using our basic panel

#We'll do so by using our code for friction calculations in our basic panel,
#this chunk of code gets an already-identified panel (all 5 quarters of it) and creates a column that indicates to us which interview the observation was identified by us

#in this example, data= the 6h panel, to replicate our example, just run the code below

###################################################
#
# setwd("desired_directory")
#
# load_pnadc_panel(panel= 6)
#
# list_painel6<- bundle_panel(directory= getwd(), desired_panel= 6)
#
# data<- list_painel6[["Panel  6"]]
#
####################################################

####################################################

#the steps below are unnecessary if you ran the code above
#
#importing the Panel 6
PNAD_painel_6_basic <- read_dta("Datazoom/Painel_PNAD/paineis_novos_advanced/PNAD_painel_6_basic.dta")
#applying the
data<- PNAD_painel_6_basic |> select(-c(id_dom,idind)) |> select(Ano, Trimestre,UF, Capital, UPA, Estrato, V1008,V1014,V1016,V1022,V1023,
                                                                 V2001,V2003,V2005,V2007, V2008,V20081,V20082,V2009,V2010,V3001,V3002,
                                                                 V3002A,V3003,V3003A,V3004,V3005,V3005A,V3006,V3006A,V3007,V3008,V3009,
                                                                 V3009A,V3010,V3011,V3011A, V3012,V3013,V3013A,V3013B,V3014)

data_basic<- data |> basic_panel()
#Now, we'll figure out a way to combine the persons that, if matched with other persons, would be seen in all the 5 interviews, through a slight modification of their id_ind

# First, we'll have to line up the different changes available for each person
# For every change, we'll firstly investigate the number of matches per person, if they only have 1 match, we'll consider it done, and mark this person as matched, we'll have a column also illutrating at what "level" of strictness (from the defined below) this observation was matched.
# PS: the strictness levels will be based on the Ribas e Soares (2008) article, in which they build a similar panel, but for another survey, as cited on our READ.ME


###################################
#
# The first 5 levels of strictness are applied ONLY to heads of the household, their spouses or sons (who are 25 years or older)
#
###################################

#1st level of strictness: if the observations match the specified criteria for this panel, they get the new id, which consists in:paste(V20081,V2008,V2003)
#criteria= either head of the house or their spouse
#criteria 2= childs of the head of the house who are over 25 years old
##########################################


summary_data <- data_basic %>%
  group_by(id_ind) %>% #grouping by each individual
  summarize(appearances = list(V1016), #in this way, we can "paste" in a single line the interviews the person has appeared at
            disappearances = list(setdiff(1:5, unique(V1016)))) %>% #and then we can set the difference from the 1:5 vector, which will return the interviews not attended by the each one
  rowwise() %>% #perform the next commands line by line (it was doing the "unlist()" command to ALL observations of the "disappearances" column)
  mutate(missing_quarters = paste(as.character(unlist(disappearances)), collapse = " ")) #getting a column that tells us in which interviews was each person missing
#rejoining this dataframe with our original database via left_join

data_joined<- left_join(data_basic, summary_data, by= "id_ind")

# creating the column for the 1st stage id, which is created only to those who were not fully matched and match the criteria below
dataframe_1st_stage <- data_joined %>%
  mutate(id_1st_stage = ifelse(V2005 %in% c("1", "2", "3") & missing_quarters!= "" | (V2005 %in% c("4", "5") & as.numeric(V2009) >= 25 & missing_quarters!= ""),
                               paste(V20081, V2008, V2003),
                               id_ind))
#now, we can calculate the friction
#pulling the people who appeared in the 1st quarter
first_quarter_appearance<- dataframe_1st_stage |> filter(V1016== "1") |> pull(id_1st_stage) |> as.vector()
#first, let's calculate the number of answers by quarter

summary_appearances<- dataframe_1st_stage |> filter(id_1st_stage %in% first_quarter_appearance) |>
  group_by(id_1st_stage) |> summarize(appearances = list(V1016), #in this way, we can "paste" in a single line the interviews the person has appeared at
                                      disappearances = list(setdiff(1:5, unique(V1016)))) %>% #and then we can set the difference from the 1:5 vector, which will return the interviews not attended by the each one
  rowwise() %>% #perform the next commands line by line (it was doing the "unlist()" command to ALL observations of the "disappearances" column)
  mutate(missing_quarters= paste(as.character(unlist(disappearances)), collapse = " "),
         first_interview= ifelse("1" %in% unlist(disappearances), 1, 0),
         second_interview= ifelse("2" %in% unlist(disappearances), 1,0),
         third_interview= ifelse("3" %in% unlist(disappearances), 1, 0),
         fourt_interview= ifelse("4" %in% unlist(disappearances), 1, 0),
         fifth_interview= ifelse("5" %in% unlist(disappearances), 1, 0))

#then, using this dataframe, we'll generate one that calculate the friction
atrito_definite= data.frame(Entrevista=seq(1,5),"Contagem de faltantes"= c(0,0,0,0,0))
for (i in 4:ncol(summary_appearances)) {
  atrito_definite[i-3,2]<- sum(summary_appearances[,i])# tiramos 4 pois queremos adicionar às 5 primeiras colunas de atrito_definite os dados das colunas 5:8 de summary_data
}

atrito_definite$Percentage_found= 100*(round(1-(atrito_definite$Contagem.de.faltantes/nrow(summary_appearances)),5))# nrow(summary_appearances)= Number of responses
# atrito_definite$Contagem.de.faltantes= Contagem de faltantes por entrevista.

#calculating the basic panel's friction rate
oioi_atrito_painel_6<-cria_df_de_atrito(data_basic)

#generating the comparison between the basic and the advanced panels for panel 6

comparison_df<- cbind(atrito_definite, oioi_atrito_painel_6)
# Assuming your dataframe is named "df"
colnames(comparison_df)[1:3] <- paste0("advanced_panel_level_1_", colnames(comparison_df)[1:3])
colnames(comparison_df)[4:6] <- paste0("basic_panel_", colnames(comparison_df)[4:6])

writexl::write_xlsx(comparison_df, "C:/Users/tuca1/OneDrive/Documentos/Datazoom/Painel_PNAD/paineis_novos_advanced/atrito/comparacao_atrito_painel_6.xlsx")

###########################################################
#turning this into a function

panel_advanced_1st_level<- function(data){
  data_initial<- data |> select(-c(id_dom,idind)) |> select(Ano, Trimestre,UF, Capital, UPA, Estrato, V1008,V1014,V1016,V1022,V1023,
                                                                   V2001,V2003,V2005,V2007, V2008,V20081,V20082,V2009,V2010,V3001,V3002,
                                                                   V3002A,V3003,V3003A,V3004,V3005,V3005A,V3006,V3006A,V3007,V3008,V3009,
                                                                   V3009A,V3010,V3011,V3011A, V3012,V3013,V3013A,V3013B,V3014)
  data_basic_panel<- data_initial |> basic_panel()
  summary_data <- data_basic_panel %>%
    group_by(id_ind) %>% #grouping by each individual
    summarize(appearances = list(V1016), #in this way, we can "paste" in a single line the interviews the person has appeared at
              disappearances = list(setdiff(1:5, unique(V1016)))) %>% #and then we can set the difference from the 1:5 vector, which will return the interviews not attended by the each one
    rowwise() %>% #perform the next commands line by line (it was doing the "unlist()" command to ALL observations of the "disappearances" column)
    mutate(missing_quarters = paste(as.character(unlist(disappearances)), collapse = " ")) #getting a column that tells us in which interviews was each person missing
  #rejoining this dataframe with our original database via left_join

  data_joined<- left_join(data_basic_panel, summary_data, by= "id_ind")

  # creating the column for the 1st stage id, which is created only to those who were not fully matched and match the criteria below
  dataframe_1st_stage <- data_joined %>%
    mutate(id_1st_stage = ifelse(V2005 %in% c("1", "2", "3") & missing_quarters!= "" | (V2005 %in% c("4", "5") & as.numeric(V2009) >= 25 & missing_quarters!= ""),
                                 paste(V20081, V2008, V2003),
                                 id_ind))
  #now, we can calculate the freaking friction, thank goodness
  #pulling the people who appeared in the 1st quarter
  first_quarter_appearance<- dataframe_1st_stage |> filter(V1016== "1") |> pull(id_1st_stage) |> as.vector()
  #first, let's calculate the number of answers by quarter

  summary_appearances<- dataframe_1st_stage |> filter(id_1st_stage %in% first_quarter_appearance) |>
    group_by(id_1st_stage) |> summarize(appearances = list(V1016), #in this way, we can "paste" in a single line the interviews the person has appeared at
                                        disappearances = list(setdiff(1:5, unique(V1016)))) %>% #and then we can set the difference from the 1:5 vector, which will return the interviews not attended by the each one
    rowwise() %>% #perform the next commands line by line (it was doing the "unlist()" command to ALL observations of the "disappearances" column)
    mutate(first_interview= ifelse("1" %in% unlist(disappearances), 1, 0),
           second_interview= ifelse("2" %in% unlist(disappearances), 1,0),
           third_interview= ifelse("3" %in% unlist(disappearances), 1, 0),
           fourt_interview= ifelse("4" %in% unlist(disappearances), 1, 0),
           fifth_interview= ifelse("5" %in% unlist(disappearances), 1, 0))

  #then, using this dataframe, we'll generate one that calculate the friction
  atrito_definite= data.frame(Entrevista=seq(1,5),"Contagem de faltantes"= c(0,0,0,0,0))
  for (i in 4:ncol(summary_appearances)) {
    atrito_definite[i-3,2]<- sum(summary_appearances[,i])# tiramos 4 pois queremos adicionar às 5 primeiras colunas de atrito_definite os dados das colunas 5:8 de summary_data
  }

  atrito_definite$Percentage_found= 100*(round(1-(atrito_definite$Contagem.de.faltantes/nrow(summary_appearances)),5))# nrow(summary_appearances)= Number of responses
  # atrito_definite$Contagem.de.faltantes= Contagem de faltantes por entrevista.
return(atrito_definite)
}

#calculating the basic panel's friction rate
oioi_atrito_painel_6<-cria_df_de_atrito(data_basic)
#calculating the advanced panel's friction rate
atrito_avancado<- panel_advanced_1st_level(data_basic)
#generating the comparison between the basic and the advanced panels for panel 6

comparison_df<- cbind(atrito_definite, oioi_atrito_painel_6)
# Assuming your dataframe is named "df"
colnames(comparison_df)[1:3] <- paste0("advanced_panel_level_1_", colnames(comparison_df)[1:3])
colnames(comparison_df)[4:6] <- paste0("basic_panel_", colnames(comparison_df)[4:6])

##############################################################
#2nd level of strictness (column 3 at the Ribas and Soares article:if the observations match the specified criteria for this panel, they get the new id, which consists in:paste(V2007,V20081+-2,V2008+-4,V2003)
#criteria= either head of the house or their spouse
#criteria 2= childs of the head of the house who are over 25 years old
##########################################

#first, we'll filter the dataframe to get only the people who have not been matched fully yet, considering both basic and advanced panels (the past criterias, of course)

appearances_2nd_phase_ids<- summary_appearances |> filter(missing_quarters!= "") |> pull(id_1st_stage)
#here we have 60327 individuals not identified fully yet

#now filtering the dataframe to have only these people who have not been fully identified
df_2nd_phase<- dataframe_1st_stage |> mutate(id_2nd_stage= paste(V2003,V2007,V2008,V20081)) |> filter(id_1st_stage %in% appearances_2nd_phase_ids)
#now, creating a function that matches people inside every household

#first, dividing the df into a list of dataframes, one per household, to facilitate the function we'll do next
list_doms_2nd_phase<- split(df_2nd_phase,df_2nd_phase$id_dom)

# # group_similar <- function(df) {
#   df_summarised<- df |> group_by(id_2nd_stage) |> summarise(Mes_nascimento= as.numeric(V20081), dia_nascimento=as.numeric(V2008),mini_id= paste(V2003,V2007))
#   for (i in 1:nrow(df_summarised)) {
#     for (j in 1:nrow(df_summarised)) {
#       if (i != j) {
#         if (abs(df_summarised$Mes_nascimento[i] - df_summarised$Mes_nascimento[j]) <= 2 &&
#             abs(df_summarised$dia_nascimento[i] - df_summarised$dia_nascimento[j]) <= 4 &&
#             df_summarised$mini_id[i]== df_summarised$mini_id[j]) {
#           df_summarised$id_2nd_stage[i] <- df_summarised$id_2nd_stage[j]
#         }
#       }
#     }
#   }
#   return(df_summarised)
# }
group_similar_2 <- function(df) {
  df_summarised<- df |> group_by(id_2nd_stage) |> summarise(mes_nascimento= as.numeric(V20081), dia_nascimento=as.numeric(V2008))
  for (i in 1:nrow(df)) {
    similar_indices <- which(
      abs(df_summarised$Mes_nascimento - df_summarised$Mes_nascimento[i]) <= 2 &
        abs(df_summarised$dia_nascimento - df_summarised$dia_nascimento[i]) <= 4
    )
    if (length(similar_indices) > 1) {
      df_summarised$id_2nd_stage[similar_indices] <- df_summarised$id_2nd_stage[i]
    }
  }
  return(df_summarised)
}


# Apply the grouping function to each household
matched_households <- lapply(list_doms_2nd_phase, group_similar_2)

# Combine the grouped households back into a single dataframe
df_grouped <-rbindlist(matched_households)

saveRDS(df_grouped, file = "df_2nd_phase_panel_calculated_panel_6.RDS")

# substiuting the previous id_2nd_phase with the new, edited one

df_2nd_phase$id_2nd_stage<- df_grouped$id_2nd_stage

#calculating the friction regarding this stage

# From this spot on, you can ignore my code, it is just me going insane trying to create perfect matches in a way that will make sense in the future, just not now
############### IGNORE THIS IGNORE THIS, IGNORE THIS ##################################
#So, we'll restrict the dataframe above to only the people who match the requirements above

#filtering the data to only have observations that we'd like
heads_spouses<- data_basic|>
  filter(V2005== "1" | V2005== "2"|V2005== "3")
sons_25_older<-data_basic|>
  filter(V2005== "4" |V2005== "5") |> filter(V2009>= "25")
#combining both vectors creates above, in order for us to have all the heads of the household, their spouses and their sons who are more than 25 years old

wanted_observations_1<- rbind(heads_spouses, sons_25_older)

#getting the transformed dataframe (the one that has the missing interviews for every person) and filtering so we'd only have the observations we'll deal with at this stage
df_1_to_5_stages<-inner_join(wanted_observations_1, summary_data, by= "id_ind")

saveRDS(df_1_to_5_stages, file = "C:/Users/tuca1/OneDrive/Documentos/Datazoom/Painel_PNAD/paineis_novos_advanced/temp_files/data_filters1_to_5.RDS")

#Now, I'll generate a list that gathers, for every possible combination of the column "missing quarters", using the split() function
#factorizing the missing_quarters column
df_1_to_5_stages_factorized<- df_1_to_5_stages |> mutate(missing_quarters_factor= as.factor(missing_quarters)) |> filter(missing_quarters!= "")
saveRDS(df_1_to_5_stages_factorized, file = "C:/Users/tuca1/OneDrive/Documentos/Datazoom/Painel_PNAD/paineis_novos_advanced/temp_files/data_filters1_to_5_factorized.RDS")
# generating the list, in which every object gathers the observatios
data_splited<-list()
data_splited<-split(df_1_to_5_stages_factorized,f= df_1_to_5_stages_factorized$missing_quarters_factor)

# Generating a new id (a more relaxed one to all the observations), which is composed only by the day and month of birth, and the control number

gen_1st_stage_id<- function(data){
  data_transformed<- data |> mutate(id_1st_stage= paste(V20081,V2008,V2003)) |>
    mutate(appearances_character = paste(as.character(unlist(appearances)), collapse = " ")) }
list_1st_stage<-lapply(data_splited, gen_1st_stage_id)

#Now I'll generate a code to gather the people with matching interviews (aka, the one's that have appearances= 1 2 3 4 5)

#following the logic, for 2 observations to be a "perfect match", the missing_quarters column of one should be equal to the appearances of another
# therefore, if I created a specific id for every one of them consisting of a character equal to a paste of both (missing_quarter and appearances), one's id would be the exact opposite of others

lista_matching_obs <- list()
nomes_lista <- c()

# Loop to combine data frames
for (i in 2:length(list_1st_stage)) {
  # Importing the 1st dataframe
  first_df_merge <- list_1st_stage[[i]]
  appearances_indicator <- names(list_1st_stage)[i]
  # Split the appearances_indicator string into a vector
  vector1 <- as.numeric(unlist(strsplit(appearances_indicator, " ")))
  # Define the perfect match string
  string2 <- "1 2 3 4 5"
  # Split the string2 into a vector
  vector2 <- as.numeric(unlist(strsplit(string2, split = " ")))
  # Find the elements that are in vector2 but not in vector1
  result <- setdiff(vector2, vector1)

  # Convert the result back to a character string
  result_string <- paste(result, collapse = " ")

  # Importing the 2nd dataframe's perfect match
  second_df_merge <- list_1st_stage[[result_string]]

  # Naming the new object
  nome_novo_objeto <- paste(appearances_indicator, result_string, sep = "_")
  nomes_lista <- c(nomes_lista, nome_novo_objeto)

  # Binding the 1st and 2nd dataframes
  binded_df <- rbind(first_df_merge, second_df_merge)

  # Adding this dataframe to a list
  lista_matching_obs[[i - 1]] <- binded_df
}
names(lista_matching_obs)= nomes_lista
############### IGNORE THIS IGNORE THIS, IGNORE THIS ############################################################################################################################
#Now, for the 1st stage, we'll calculate the friction with for the basic id_ind and the new, advanced, individual id, for every group, as to evaluate the impact of this relaxation




