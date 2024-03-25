pnadc_dictionary <- function() {
    tibble::tribble(
        ~ var_code, ~ name_pt, ~ name_eng, ~ level, ~ label_pt, ~ label_eng,
        "UF", "uf", "state", "11", "Rond\u00f4nia", "Rond\u00f4nia",
        "UF", "uf", "state", "12", "Acre", "Acre",
        "UF", "uf", "state", "13", "Amazonas", "Amazonas",
        "UF", "uf", "state", "default", NA, NA
    )
}