#' @importFrom dplyr %>%
#' @importFrom rlang .data

NULL

#' Loads and cleans PNAD Continua microdata

#'
#' @encoding UTF-8
#'
#' @param sources A number of different sources are supported:
#'
#' Passing a list of dates will download the corresponding data from IBGE's website.
#'
#' Passing a string with a directory's path will read data from all files named
#'   "\code{path}/PNADC_XXXX.txt".
#'
#' Alternatively, \code{sources} may be a list of full file paths
#'
#' @param language Choose whether the data should come in Portuguese or in English. Default is
#' \code{language = 'eng'}. For portuguese, write instead \code{language = 'pt'}.
#'
#' @param download_directory In case \code{sources} is such that data is downloaded
#' from IBGE, where should it be stored? Default is the working directory
#'
#'
#' @return A list of dataframes with the microdata from each
#' required period. Downloaded raw data are stored in a folder called "PNADC_microdata"
#' on the directory specified in argument \code{download_directory}
#'
#' @examples
#'
#' \dontrun{
#' To download data, set \code{sources} as a list of vectors
#' of time periods
#'
#' dates <- list(c(1, 2012), c(2, 2012))
#'
#' microdata <- load_pnadcontinua(language = 'eng',
#'                         sources = dates,
#'                         download_directory = './Desktop')
#'
#' To load the data from a folder:
#'
#' microdata <- load_pnadcontinua(language = 'eng',
#'                         sources = './Desktop/folder_name')
#'
#' To load an individual .txt file corresponding to a given period of the survey:
#'
#'   microdata <- load_pnadcontinua(sources = './PNADC_012020.txt')
#'}
#'


#' @export
load_pnadcontinua <- function(language = 'eng',
                       sources, download_directory = getwd()) {

dataset <- load_and_tidy_data_pnadcontinua(files = sources,
                              download_location = download_directory)


#### splitting by year/quarter and naming each dataset

   dataset <- dplyr::bind_rows(dataset) %>%
     dplyr::group_by(.data$ANO, .data$TRIMESTRE) %>%
     dplyr::group_split() %>%
     purrr::map(~ .x %>% dplyr::ungroup(.))

   dataset_names <- purrr::map(dataset, ~ paste0('pnadc_',
                                          stringr::str_pad(unique(.x$TRIMESTRE),
                                                 width = 2, pad = "0"),
                                          unique(.x$ANO)))
   names(dataset) <- dataset_names


dataset <- purrr::map(dataset, ~ .x %>% convert_types(survey = 'continua'))

dataset <- purrr::map(dataset, ~ translation_and_labels(df = .,
                                                 language = language,
                                                 survey = 'continua'))

dataset <- purrr::map(dataset,
               ~ .x %>%
                 dplyr::mutate(
                   hous_id = paste0(.data$UPA, .data$V1008, V1014),
                   ind_id = paste0(.data$UPA, .data$V1008, V1014, .data$V2003)
                   ) %>%
                 dplyr::relocate(hous_id, ind_id)
               )



}


#### Separates columns according to dictionary, changes column types.
#### Label addition is left to later, after building panel


load_and_tidy_data_pnadcontinua <- function(files, download_location = getwd()){

  #### In case user wants to download from IBGE

  if (purrr::map_lgl(files, is.numeric) %>% all()) {

    quarters <- purrr::map(files, ~ .[[1]])
    years <- purrr::map(files, ~ .[[2]])

    download_path <- purrr::map2(quarters, years, ~ download_quarter_pnadc(quarter = .x, year = .y,
                                                              directory = download_location))

    files <- list.files(download_path[[1]], full.names = TRUE)

    #### In case the user provides a directory with the data

  } else if(is.character(files) && length(files) == 1 && dir.exists(files)){

    files <-
      list.files(path = files, full.names = TRUE,
                 pattern = "PNADC_.*.txt$")

  }

  raw_data <- purrr::map(files, ~ readr::read_tsv(. , col_names = 'a', n_max = 1000))

  tidy_data <- purrr::map(raw_data, ~ spread_columns(dataset = ., original_column = a))

  return(tidy_data)


}

spread_columns <- function(dataset, original_column) {
  dataset %>%
    tidyr::separate({{original_column}},
                    into = data_labels_pnadcontinua$variable_pt.br, sep = data_labels_pnadcontinua$position[-1] - 1) %>%
    dplyr::mutate(dplyr::across(c(.data$V2009, .data$VD3004, .data$V2005,
                                  .data$V2008, .data$V20081, .data$V20082),
                  as.numeric))
}



