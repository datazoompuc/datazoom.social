##########################
###                    Download PNADC
### This file contains PNADC download-related functions
##########################


### is_period_valid function checks whether there is PNADC data for a given moment in time.
is_period_valid <- function(quarter, year) {
  quarter >= 1 && quarter <= 4 && year >= 2012 && year <= timeDate::getRmetricsOptions("currentYear")
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

is_period_valid <- function(quarter, year) {
  quarter >= 1 && quarter <= 4 && year >= 2012 && year <= timeDate::getRmetricsOptions("currentYear")
}

### download_years function downloads PNADC files from source, separates them by panel and then puts the panel files together
# attention! data comes out non-identified. to identify data, either basic_panel or cleans_dat + builds_identifiers must be used
# attention! the function takes a "years" vector - not panel- as parameter.
# therefore, the user should know which years contain the panel of interest

download_years = function(years){
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
}

### new edition that takes "panel" as parameter
############

download_panel = function(panel){
  pnad_list <- list() # create an empty list to store the data frames
  vars_list <- list() # create an empty list to store the data frames
  panel_list <- list() # create an empty list to store the data frames

  if (panel == 1) {
    x = c("2012.1", "2012.2", "2012.3", "2012.4")
  } else if (panel == 2) {
    x = c("2012.1", "2012.2", "2012.3", "2012.4", "2013.1", "2013.2", "2013.3", "2013.4", "2014.1")
  } else if (panel == 3) {
    x = c("2013.2", "2013.3", "2013.4", "2014.1", "2014.2", "2014.3", "2014.4", "2015.1", "2015.2")
  } else if (panel == 4) {
    x = c("2014.3", "2014.4", "2015.1", "2015.2", "2015.3", "2015.4", "2016.1", "2016.2", "2016.3")
  } else if (panel == 5) {
    x = c("2015.4", "2016.1", "2016.2", "2016.3", "2016.4", "2017.1", "2017.2", "2017.3", "2017.4")
  } else if (panel == 6) {
    x = c("2017.1", "2017.2", "2017.3", "2017.4", "2018.1", "2018.2", "2018.3", "2018.4", "2019.1")
  } else if (panel == 7) {
    x = c("2018.2", "2018.3", "2018.4", "2019.1", "2019.2", "2019.3", "2019.4", "2020.1", "2020.2")
  } else if (panel == 8) {
    x = c("2019.3", "2019.4", "2020.1", "2020.2", "2020.3", "2020.4", "2021.1", "2021.2", "2021.3")
  } else { #(panel == 9)
    x = c("2020.4", "2021.1", "2021.2", "2021.3", "2021.4", "2022.1", "2022.2", "2022.3", "2022.4")
  }


  for (i in x) {
    s_plit = stringi::stri_split_fixed(i, pattern = ".", simplify = FALSE)
    j = s_plit[[1]][1]
    k = s_plit[[1]][2]

    pnad_list[[paste0("pnad", j, "_", k)]] = get_pnadc(year = j, quarter = k, labels = TRUE)
    vars_list[[paste0("vars", j, "_", k)]] = pnad_list[[paste0("pnad", j, "_", k)]]$variables %>%
      select(Ano, Trimestre, UF, UPA,V1008, V1014, V2003,V2005, V2007, V2008,V20081, V20082, V1023, V1016) %>%
      as.data.frame()

    panel_list[[paste0("pnad", j, "_", k, "_", panel)]] = vars_list[[paste0("vars", j, "_", k)]] %>%
      as.data.frame() %>%
      dplyr::filter(as.integer(V1014) == panel) %>%
      cleans_dat() %>%
      builds_identifiers()
    panel.intermediary<-panel_list[[paste0("pnad", j, "_", k, "_", panel)]] %>% as.data.frame()

    if(nrow(panel.intermediary)>5000){
      saveRDS(panel.intermediary, file = paste0(".\\pnad", j, "_", k, "_", panel))
    } else {
      rm(panel.intermediary)}
  }
}

############
