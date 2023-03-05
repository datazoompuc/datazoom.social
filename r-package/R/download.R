source("~/GitHub/PNAD_Continua/r-package/R/select_file_function.R")
download_quarter <- function(quarter, year, directory = "~") {

  if (!dir.exists(directory)) {
    stop("Provided directory doesn't exist")
  }
  if (!is_period_valid(quarter, year)) {
    stop("Provided period is not valid")
  }

#pasting the file's name
  file_name.intermediary <- paste(select_quarter(year,quarter))
  x<- paste0(file_name.intermediary,".zip")
#adding the year before
  file_name<-paste0(year,"/",x)

  url_path <- file.path(
    "http://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/",
    file_name
  )

  # Check if the "PNADC_microdata" directory exists and create it if it doesn't
  pnadc_dir <- file.path(directory, "PNADC_microdata")
  if (!dir.exists(pnadc_dir)) {
    dir.create(pnadc_dir, recursive = TRUE)
  }

filename<-basename(url_path)
dest_path <- file.path(directory, filename)

utils::download.file(
    url = url_path,
    destfile = file.path(pnadc_dir,file_name),
    method = "auto"
  )

  return(file.path(directory, "PNADC_microdata"))
}

is_period_valid <- function(quarter, year) {
  quarter >= 1 && quarter <= 4 && year >= 2012 && year <= timeDate::getRmetricsOptions("currentYear")
}

#Even newer version of ChatGPT because he wronged the last one

download_quarter <- function(quarter, year, directory = "~") {

  if (!dir.exists(directory)) {
    stop("Provided directory doesn't exist")
  }
  if (!is_period_valid(quarter, year)) {
    stop("Provided period is not valid")
  }

  # Pasting the file's name
  file_name.intermediary <- paste0(select_quarter(year, quarter))
  x <- paste0(file_name.intermediary, ".zip")

  # Adding the year before
  file_name <- file.path(year, x)

  url_path <- file.path(
    "http://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados",
    file_name
  )

  # Check if the "PNADC_microdata" directory exists and create it if it doesn't
  pnadc_dir <- file.path(directory, "PNADC_microdata")
  if (!dir.exists(pnadc_dir)) {
    dir.create(pnadc_dir, recursive = TRUE)
  }

  filename <- basename(url_path)
  dest_path <- file.path(pnadc_dir, filename)

  utils::download.file(
    url = url_path,
    destfile = dest_path,
    method = "auto"
  )

  return(pnadc_dir)
}
