#' Download microdata from multiple quarters
#'
#' @param periods List of pairs of the form (t, y) where t is the trimester (1-4) and y is year (2012-present)
#' @param dir dir where data should be saved
#' @return List of filenames of the downloaded files
#' @example download_quarters(list(c(1, 2019), c(2, 2019)))
#' @export
download_quarters <- function(periods, dir = tempdir()) {
  files = c()
  for (p in periods) {
    files <- c(files, download_quarter(p, dir = dir))
  }
  files
}

#' Download microdata from a single quarter
#'
#' @param period Pair of the form (t, y) where t is the trimester (1-4) and y is year (2012-present)
#' @param dir dir where data should be saved
#' @return filename of the downloaded file
#' @example download_quarter(c(1, 2019))
#' @export
download_quarter <- function(period, dir = tempdir()) {
  if (!dir.exists(dir))
    stop("Provided directory doesn't exist")
  if (!is_period_valid(period))
    stop("Provided period is not valid")

  host_path <- "ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/"
  quarter <- period[1]
  year <- period[2]

  path_with_year <- paste0(host_path, year, "/")

  # IBGE has an irregular naming scheme for files, but they will always begin with the identifier
  identifier <- paste0("PNADC_0", quarter, year)
  list_of_files <- unlist(strsplit(RCurl::getURL(path_with_year, dirlistonly = TRUE), "\n"))
  file <- list_of_files[startsWith(list_of_files, identifier)]
  if (length(file) != 1) {
    stop("Retrieval for selected quarter and year failed.")
  }

  # Files are compressed when downloaded
  zip_location <- paste0(dir, "/", file)
  utils::download.file(url = paste0(path_with_year, file), destfile = zip_location, mode = "wb")
  utils::unzip(zipfile = zip_location, exdir = dir)

  # Again, need find function because of irregular names
  file <- dir(dir, pattern = paste0("^PNADC_0", quarter, year, ".*\\.txt$"))
  paste0(dir, "/", file)
}

is_period_valid <- function(period) {
  length(period) == 2 && period[1] >= 1 && period[1] <= 4 && period[2] >= 2012 && period[2] <= timeDate::getRmetricsOptions("currentYear")
}
