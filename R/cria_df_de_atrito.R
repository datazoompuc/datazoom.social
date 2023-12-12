#' Create a friction dataframe for a panel file
#'
#' This function takes a dataset and generates a summary data frame indicating the count of missing interviews for each individual and the percentage of interviews attendeded, allowing for the friction calculation, a panel-efficiency measuring method.
#'
#' @param data The input dataset, preferably a PNADc panel file with all 5 interviews of the individuals of that panel, with a certain panel method already applied.
#'
#' @return A data frame summarizing missing interviews and the percentage of interviews attended for the individuals forming that panel.
#'
#' @examples
#' \dontrun{
#' data <- read.csv("path/to/your/painel_6_PNADc.csv")
#' atrito_df_panel_6_PNADc <- cria_df_de_atrito(data)
#' }
cria_df_de_atrito <- function(data) {
  data$V1016 <- as.integer(data$V1016)
  
  # Create a vector with the IDs of individuals present in the 1st interview
  presentes_na_1a_entrevista <- data %>%
    filter(V1016 == 1) %>%
    pull(id_ind) %>%
    as.vector()
  
  # Filter the data to include only individuals who participated in the 1st interview
  data <- data %>%
    filter(id_ind %in% presentes_na_1a_entrevista)
  
  # Generate a summary data frame
  summary_data <- data %>%
    group_by(id_ind) %>%
    summarize(
      appearances = list(V1016),
      disappearances = list(setdiff(1:5, unique(V1016)))
    ) %>%
    rowwise() %>%
    mutate(
      missing_quarters = paste(as.character(unlist(disappearances)), collapse = " "),
      first_interview = ifelse("1" %in% unlist(disappearances), 1, 0),
      second_interview = ifelse("2" %in% unlist(disappearances), 1, 0),
      third_interview = ifelse("3" %in% unlist(disappearances), 1, 0),
      fourt_interview = ifelse("4" %in% unlist(disappearances), 1, 0),
      fifth_interview = ifelse("5" %in% unlist(disappearances), 1, 0)
    )
  
  # Create a data frame for definite attrition
  atrito_definite <- data.frame(Entrevista = seq(1, 5), "Contagem de faltantes" = c(0, 0, 0, 0, 0))
  
  # Calculate the count of missing interviews for each column
  for (i in 5:ncol(summary_data)) {
    atrito_definite[i - 4, 2] <- sum(summary_data[, i])
  }
  
  # Calculate the percentage of interviews attended
  atrito_definite$Percentage_found <- 100 * (round(1 - (atrito_definite$Contagem.de.faltantes / nrow(summary_data)), 5))
  
  return(atrito_definite)
}
