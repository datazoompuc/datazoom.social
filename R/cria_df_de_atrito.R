#' Create an attrition table for a panel file
#'
#' This function generates a summary dataframe indicating the count of missing
#' interviews for each individual and the percentage of interviews attended,
#' calculating the attrition for a PNADc panel.
#'
#' @param data The input data frame, preferably a PNADc panel file with all 5
#'   interviews of the individuals of that panel, with a certain panel method
#'   already applied.
#' @param panel The identification strategy whose friction will be identified. Can either be "basic" or "advanced"
#'
#' @return A data frame summarizing missing interviews and the percentage of
#'   interviews attended for the individuals forming that panel.
#'
#' @examples
#' \dontrun{
#' data <- read.csv("path/to/your/database/pnadc_panel_6.csv")
#' attrition_panel_6_PNADc <- cria_df_de_atrito(data, panel = "advanced")
#' }
#' 
cria_df_de_atrito <- function(data, panel) {
  
  # binding globals
  V1016 <- individual_identifier <- disappearances <- NULL
  
  data$V1016 <- as.integer(data$V1016)
  
  # Identify whether basic or advanced panel attrition will be calculated
  
  if (panel == "basic") {
    data <- data %>% rename("individual_identifier" = "id_ind")
    print("Basic panel attrition calculated.")
  } else if(panel == "advanced") {
    data <- data %>% rename("individual_identifier" = "id_rs")
    print("Advanced panel attrition calculated.")
  }
  
  # Create a vector with the IDs of individuals present in the 1st interview
  presentes_na_1a_entrevista <- data %>%
    filter(V1016 == 1) %>%
    pull(individual_identifier) %>%
    as.vector()
  
  # Filter the data to include only individuals who participated in the 1st interview
  data <- data %>%
    filter(individual_identifier %in% presentes_na_1a_entrevista)
  
  # Generate a summary data frame
  summary_data <- data %>%
    group_by(individual_identifier) %>%
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
  
  # Create a data frame for definite friction
  atrito_definite <- data.frame(Entrevista = seq(1, 5), "Contagem de faltantes" = c(0, 0, 0, 0, 0))
  
  # Calculate the count of missing interviews for each column
  for (i in 5:ncol(summary_data)) {
    atrito_definite[i - 4, 2] <- sum(summary_data[, i])
  }
  
  # Calculate the percentage of interviews attended
  atrito_definite$Percentage_found <- 100 * (round(1 - (atrito_definite$Contagem.de.faltantes / nrow(summary_data)), 5))
  
  return(atrito_definite)
}
