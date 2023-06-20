# DataZoom Social R package - functions

### cleans_dat cleans incoming PNADC data, preparing it to create identifiers
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
    dplyr::mutate(V20082 = as.character(V20082))

  return(character_dat)

}

### builds_identifiers takes clean PNADC data and builds household (dom) and individual (ind) identifiers
# the identifiers are simply the concatenation of variables that characterize household/the individual
builds_identifiers = function(character_dat) {
  w_id_dom = character_dat %>%
    dplyr::bind_cols(
      # creates household identifier
      id_dom = paste0(character_dat$UPA, character_dat$V1008, character_dat$V1014))

  #creates individual identifier
  #id_ind concatenates household identifier (id_dom), birthday in Ymd format and gender
  #We also added the variables UF and V1023, which configurate a good municipality identifier in the context
  b_panel = w_id_dom %>%
    dplyr::mutate(id_ind = paste(UF,V1023,id_dom, V20082, V20081, V2008, V2007))

  #returns the final product
  return(b_panel)
}


### basic_panel function runs both clean_dat and builds_identifiers
basic_panel = function(incoming_dat) {

  cleans_dat() %>%
    builds_identifiers()

}

### download_panel function downloads PNADC files from source, separates them by panel and then puts the panel files together
# attention! data comes out non-identified. to identify data, either basic_panel or cleans_dat + builds_identifiers must be used
# attention! the function takes a "years" vector - not panel- as parameter.
# therefore, the user should know which years contain the panel of interest

download_panel = function(years){
pnad_list <- list() # create an empty list to store the data frames
vars_list <- list() # create an empty list to store the data frames
panel_list <- list() # create an empty list to store the data frames

for (i in years) {
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
}

### download_quarter function replaces the get_pnadc function. in dev, we only use get_pnadc. download_quarter is mostly a backup
download_quarter <- function(quarter, year, directory = getwd()) {
  if (!dir.exists(directory)) {
    stop("Provided directory doesn't exist")
  }
  if (!is_period_valid(quarter, year)) {
    stop("Provided period is not valid")
  }


  if (year >= 2019 & quarter >= 2 | year >= 2020) {
    quarter <- stringr::str_pad(quarter,
                                width = 2,
                                side = "left",
                                pad = 0
    )
    file_name <- paste0("PNADC_", quarter, year, ".zip")
  } else {
    quarter <- stringr::str_pad(quarter,
                                width = 2,
                                side = "left",
                                pad = 0
    )

    #### Ficar de olho para ver se o IBGE muda o seu padrão de nomes nos códigos
    file_name <- paste0("PNADC_", quarter, year, "_20190729.zip")
  }

  url_path <- file.path(
    "http://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados",
    year,
    file_name
  )

  utils::download.file(
    url = url_path,
    destfile = file.path(directory, file_name),
    mode = "wb"
  )

  utils::unzip(
    zipfile = file.path(directory, file_name),
    exdir = file.path(directory, "PNADC_microdata")
  )

  return(file.path(directory, "PNADC_microdata"))
}

### is_period_valid function checks whether there is PNADC data for a given moment in time.
is_period_valid <- function(quarter, year) {
  quarter >= 1 && quarter <= 4 && year >= 2012 && year <= timeDate::getRmetricsOptions("currentYear")
}
