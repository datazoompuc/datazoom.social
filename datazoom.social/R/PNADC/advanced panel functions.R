##########################
###               Build advanced PNADC panel
### This file contains PNADC advanced panel related functions
##########################
setwd("C:/Users/tuca1/OneDrive/Documentos/GitHub/Datazoom-Social_R/datazoom.social/R/PNADC")

library(haven)
library(tidyverse)
library(ggplot2)
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
#
####################################################
#REMEMBER TO GATHER THIS INTO A FUNCTION LATER
#Now we'll obtain, for every observation of this panel, the interviews the individual has been identified at by our basic panel
summary_data <- data_basic %>%
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
#Now, we'll filter all the people who were already spotted at all 5 interviews (missing_quarters column == "")

data_filtered_from_complete_obs<- summary_data |> filter(missing_quarters!= "")


#Now, we'll figure out a way to combine the persons that, if matched with other persons, would be seen in all the 5 interviews, through a slight modification of their id_ind

# First, we'll have to line up the different changes available for each person
# For every change, we'll firstly investigate the number of matches per person, if they only have 1 match, we'll consider it done, and mark this person as matched, we'll have a column also illutrating at what "level" of strictness (from the defined below) this observation was matched.
# PS: the strictness levels will be based on the Ribas e Soares (2008) article, in which they build a similar panel, but for another survey, as cited on our READ.ME


###################################
#
# The first 5 levels of strictness are applied ONLY to heads of the household, their spouses or sons (who are 25 years or older)
#
###################################

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

##############################
# 1st level of strictness: variables considered: Same day and month of birth and order number (our id_ind will only be that)
##############################

# Generating a new id (a more relaxed one to all the observations), which is composed only by the day and month of birth, and the control number

gen_1st_stage_id<- function(data){
  data_transformed<- data |> mutate(id_1st_stage= paste(V20081,V20082,V2003)) |>
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

################################################################################################
#testing if the objects in the list are correctly defined:

#oioi<- lista_matching_obs[[3]] |> select(missing_quarters) |> as.data.frame() #selecting a random dataframe from the list

#checking which values of the variable "missing_quarters" there are in this df- correct values should be: "1 2" and "3 4 5"

#unique(oioi$missing_quarters)

#therefore, by checking the result of the code above, we can infer that the transformation was successful

###########################################################################################################################################

#Now, for the 1st stage, we'll calculate the friction with for the basic id_ind and the new, advanced, individual id, for every group, as to evaluate the impact of this relaxation

###########################################################################################################################################


# Define the updated function
calculates_friction <- function(data) {
  dataframe_atrito <- data.frame()
  data_gp <- data %>%
    group_by(id_1st_stage) %>%
    summarise(number_of_appearances = nchar(gsub(" ", "", appearances_character)))

  dataframe_atrito[, 1] <- paste0("Atrito", names(data))
  dataframe_atrito[, 2] <- mean(data_gp$number_of_appearances)

  return(dataframe_atrito)
}

# Apply the updated function to the list of data frames
result_list <- lapply(lista_matching_obs, calculates_friction)


# group by painel 6 by the new id_ind (the one used in the 1st method of the advanced panel) and recalculate the friction

grouped_df_friction_1stmethod<- PNAD_painel_6_basic |> mutate()
