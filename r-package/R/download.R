#' Download microdata from a selected year and quarter
#'
#' @param quarter Number between 1 and 4 indicating the quarter
#' @param year Number between 2012 and present year
#' @param dir Directory where data should be saved
#' @return filename of the downloaded file
#' @example download_quarter(1, 2019)
#' @export
download_quarter <- function(quarter, year, dir = tempdir()) {
  if (!dir.exists(dir))
    stop("Provided directory doesn't exist")
  if (!is_period_valid(quarter, year))
    stop("Provided period is not valid")

  host_path <- "ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/"
  path_with_year <- paste0(host_path, year, "/")

  # IBGE has an irregular naming scheme for files, but they will always begin with the identifier
  identifier <- paste0("PNADC_0", quarter, year)
  list_of_files <- unlist(strsplit(RCurl::getURL(path_with_year, dirlistonly = TRUE), "\n"))
  file <- list_of_files[startsWith(list_of_files, identifier)]
  if (length(file) != 1) {
    stop("Retrieval for selected quarter and year failed.")
  }

  # Files are compressed when downloaded
  zip_location <- file.path(dir, file)
  utils::download.file(url = paste0(path_with_year, file), destfile = zip_location, mode = "wb")
  utils::unzip(zipfile = zip_location, exdir = dir)

  # Again, need find function because of irregular names
  file <- dir(dir, pattern = paste0("^PNADC_0", quarter, year, ".*\\.txt$"))
  file.path(dir, file)
}

is_period_valid <- function(quarter, year) {
  quarter >= 1 && quarter <= 4 && year >= 2012 && year <= timeDate::getRmetricsOptions("currentYear")
}
