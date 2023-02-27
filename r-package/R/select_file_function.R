library(RCurl)
library(tidyverse) #contains the stringr
library(curl)

select_quarter<-function(year,quarter){

  #The chunk of code that gets the files from the FTP directory, while also transforming them to a format that is recognizable to the functions used after it

  directory.provisory <- ("https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/")

  #adding the year we have been provided to the URL
  data.provisory<- paste0(directory.provisory, year, "/")
  
  datayear <- data.provisory %>% 
  # Download directory listing from FTP server (gotten from the URL)
  RCurl::getURL(dirlistonly = TRUE) %>% 
  #subbing the windows style to Unix-style endings
  str_replace_all("\r\n", "\n", .) %>% 
  #split listing every character that has this ending into different lines lines
  strsplit("\n") %>% 
  #unlisting them
  unlist() %>% 
  #split listing the characters again, but using another method, the "<a href=" pattern"
  strsplit("<a href=[[:punct:]]") %>% 
  unlist() %>% 
  #split listing the characters again, but using another method, the ".zip" pattern"
  strsplit(".zip") %>% 
  #unlisting the object one last time, now we just have to source from this object the desired names of the archives, filtering the choice by quarter and year
  unlist()

#selecting the names of the archives we wish to download from the object created above, selecting ONLY the one with the specific quarter and year
dataname <- datayear[which(startsWith(datayear, paste0("PNADC_0", quarter, year)))]
}

#exemplifying this function, it returns to you the exact name of the archive that contains the PNAD file for the selected year and quarter.
x<-select_quarter(2018,02)
#note that this function only returns an OBJECT OF CLASS CHARACTER, not the actual file, this file will be downloaded on another function

################################################################################################################################################

# Below, there is a number of tests that I performed, with help of ChatGPT, to transform the function above (select_quarter) in one that delivered the same results, but with different functions, how?
# I was trying to use packages that were NOT discontinued, like the RCurl package (tried to use the curl package) and the stringi package (tryed to replace the chunks of code to stringr functions)

################################################################################################################################################
#Newer version, without discontinued packages
# library(stringr)
# library(purrr)
# 
# url <- ("https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/")
# 
# # Define a function to download the directory listing from the FTP server
# download_dir_listing <- function(url) {
#   con <- url(url, "r")
#   dir_listing <- readLines(con)
#   close(con)
#   return(dir_listing)
# }
# 
# # Define a function to process the directory listing and extract the desired file names
# process_dir_listing <- function(dir_listing, quarter, year) {
#   file_names <- dir_listing %>%
#     str_replace_all("\r\n", "\n") %>%
#     str_split("\n") %>%
#     unlist() %>%
#     str_split("<a href=[[:punct:]]") %>%
#     flatten_chr() %>%
#     str_split(".zip") %>%
#     flatten_chr() %>%
#     str_trim() %>%
#     keep(str_starts, paste0("PNADC_0", quarter, year))
#   return(file_names)
# }
# 
# # Set the URL of the FTP server
# ftpdata <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/2019"
# 
# # Set the quarter and year
# quarter1 <- "01"
# year1 <- "2015"
# 
# # Download the directory listing from the url and generate the files
# example<-download_dir_listing(url= ftpdata)
# 
# #process the directory listing of this example and extract the desired file names
# process_dir_listing(dir_listing = example, quarter1, year1)
# 
# ########################################################################################
# 
# year<-2019
# quarter<-02
# library(curl)
# 
# select_quarter<- function(year, quarter){
# directory.provisory <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/"
# data.provisory <- paste0(directory.provisory, year, "/")
# 
# datayear <- data.provisory %>% 
#   # Download directory listing from FTP server
#   curl_download() %>% 
#   #subbing the windows style to Unix-style endings
#   str_replace_all("\r\n", "\n") %>% 
#   #split listing every character that has this ending into different lines lines
#   str_split("\n") %>% 
#   #unlisting them
#   unlist() %>% 
#   #split listing the characters again, but using another method, the "<a href=" pattern"
#   str_split("<a href=[[:punct:]]") %>% 
#   unlist() %>% 
#   #split listing the characters again, but using another method, the ".zip" pattern"
#   str_split(".zip") %>% 
#   #unlistingt the object one last time, now we just have to source from this object the desired names of the archives, filtering the choice by quarter and year
#   unlist()
# 
# #selecting the names of the archives we wish to download from the object created above, selecting ONLY the one with the specific quarter and year
# dataname <- datayear[str_detect(datayear, regex(paste0("^PNADC_0", quarter, year)))]
# }
# select_quarter(2019,02)
# ?regex
