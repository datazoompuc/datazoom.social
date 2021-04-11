#### THESE ARE AUXILIARY FUNCTIONS USED EITHER FOR PNAD CONTINUA OR FOR PNAD COVID


convert_types <- function(df, survey) {
  if (survey != "covid") {
    data_labels <- data_labels_pnadcontinua

    df %>%
      dplyr::mutate(
        dplyr::across(
          tidyselect::vars_select_helpers$where(is.factor),
          ~ ifelse(.data == ".", "", .) %>%
            as.character(.data)
        ),
        dplyr::across(tidyselect::everything(), ~ ifelse(stringr::str_trim(.) == "", NA, .) %>%
          as.factor(.)),
        dplyr::across(
          data_labels$variable_pt.br[data_labels$var_type == "factor"],
          as.factor
        ),
        dplyr::across(
          data_labels$variable_pt.br[data_labels$var_type == "double"],
          as.double
        )
      )
  } else {
    data_labels <- data_labels_pnadcovid

    df %>%
      dplyr::mutate(
        dplyr::across(
          tidyselect::any_of(data_labels$variable_pt.br[data_labels$var_type == "factor"]),
          as.factor
        ),
        dplyr::across(
          tidyselect::any_of(data_labels$variable_pt.br[data_labels$var_type == "double"]),
          as.double
        )
      )
  }
}


translation_and_labels <- function(df, survey, language) {
  if (survey == "covid") {
    data_labels <- data_labels_pnadcovid %>%
      dplyr::filter(.data$variable_pt.br %in% colnames(df) | .data$variable_eng %in% colnames(df))
  } else {
    data_labels <- data_labels_pnadcontinua
  }

  if (language == "eng") {
    labels_key <- as.list(data_labels$label_eng) %>%
      stats::setNames(data_labels$variable_eng)
  } else {
    labels_key <- as.list(data_labels$label_pt.br) %>%
      stats::setNames(data_labels$variable_pt.br)
  }


  if (language == "eng" & survey == "continua") {
    df <- df %>%
      dplyr::rename(
        Year = .data$ANO,
        Quarter = .data$TRIMESTRE,
        Capital = .data$CAPITAL,
        Stratum = .data$ESTRATO
      )
  }
  if (language == "eng" & survey == "covid") {
    df <- df %>%
      dplyr::rename(
        Year = .data$Ano,
        Stratum = .data$Estrato
      )
  }

  df %>%
    labelled::set_variable_labels(.labels = labels_key)

}
