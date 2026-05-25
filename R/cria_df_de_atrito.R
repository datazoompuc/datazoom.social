#' Create an attrition table for a panel file
#'
#' This function generates a summary dataframe indicating the count of missing
#' interviews for each individual and the unconditional tracking rates.
#'
#' @param data The input data frame, preferably a PNADc panel file.
#' @param panel The identification strategy: "basic", "advanced_1", "advanced_2", "advanced_3" or "households".
#'
#' @return A data frame summarizing missing interviews and the tracking rates.
cria_df_de_atrito <- function(data, panel) {
  # Binding globals
  V1016 <- individual_identifier <- disappearances <- NULL
  
  data$V1016 <- as.integer(data$V1016)
  
  # Identify which panel strategy is being calculated
  if (panel == "basic") {
    data <- data %>% dplyr::rename("individual_identifier" = "id_ind")
    print("Basic panel attrition calculated.")
  } else if (panel == "advanced_1") {
    data <- data %>% dplyr::rename("individual_identifier" = "id_rs1")
    print("Advanced Stage 1 panel attrition calculated.")
  } else if (panel == "advanced_2") {
    data <- data %>% dplyr::rename("individual_identifier" = "id_rs2")
    print("Advanced Stage 2 panel attrition calculated.")
  } else if (panel == "advanced_3") {
    data <- data %>% dplyr::rename("individual_identifier" = "id_rs3")
    print("Advanced Stage 3 (Fuzzy) panel attrition calculated.")
  } else if (panel == "households") {
    data <- data %>% dplyr::rename("individual_identifier" = "id_dom")
    print("Household panel attrition calculated.")
  }
  
  # 1. Fundamental Definitions (The Baseline - Wave 1)
  
  # N_linhas: The absolute total number of raw observations recorded in Wave 1
  N_linhas <- sum(data$V1016 == 1, na.rm = TRUE)
  
  # Identify the valid IDs present in Wave 1
  presentes_na_1a_entrevista <- data %>%
    dplyr::filter(V1016 == 1 & !is.na(individual_identifier)) %>%
    dplyr::pull(individual_identifier) %>%
    unique()
  
  # N_ids: The total number of unique and valid identifiers successfully built in Wave 1
  N_ids <- length(presentes_na_1a_entrevista)
  
  # Filter the data only for the cohort we are actively tracking
  data <- data %>%
    dplyr::filter(individual_identifier %in% presentes_na_1a_entrevista)
  
  # --- ORIGINAL STRUCTURE MAINTAINED ---
  # Generate a summary data frame mapping appearances and disappearances
  summary_data <- data %>%
    dplyr::group_by(individual_identifier) %>%
    dplyr::summarize(
      appearances = list(V1016),
      disappearances = list(setdiff(1:5, unique(V1016)))
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      missing_quarters = paste(as.character(unlist(disappearances)), collapse = " "),
      first_interview = ifelse("1" %in% unlist(disappearances), 1, 0),
      second_interview = ifelse("2" %in% unlist(disappearances), 1, 0),
      third_interview = ifelse("3" %in% unlist(disappearances), 1, 0),
      fourth_interview = ifelse("4" %in% unlist(disappearances), 1, 0),
      fifth_interview = ifelse("5" %in% unlist(disappearances), 1, 0)
    )
  
  # Create a data frame for definite attrition tracking
  atrito_definite <- data.frame(Onda = seq(1, 5), "Contagem_de_faltantes" = c(0, 0, 0, 0, 0))
  
  # Calculate the total count of missing interviews for each wave
  for (i in 5:ncol(summary_data)) {
    atrito_definite[i - 4, 2] <- sum(summary_data[, i])
  }
  
  # --- APPLICATION OF THE DATA ZOOM GUIDE FORMULAS ---
  
  # S_w: Survivors Found in Wave w
  atrito_definite$Sobreviventes_S_w <- N_ids - atrito_definite$Contagem_de_faltantes
  
  # Metric A: Unconditional Rate (ID-Based) -> (S_w / N_ids) * 100
  atrito_definite$Taxa_Incondicional_Base_ID <- (atrito_definite$Sobreviventes_S_w / N_ids) * 100
  
  # Metric B: Unconditional Rate (Line-Based) -> (S_w / N_linhas) * 100
  atrito_definite$Taxa_Incondicional_Base_Linha <- (atrito_definite$Sobreviventes_S_w / N_linhas) * 100
  
  # Export the quantity of observations to be used as weights in the final mean calculations
  atrito_definite$Peso_N_ids <- N_ids
  atrito_definite$Peso_N_linhas <- N_linhas
  
  return(atrito_definite)
}