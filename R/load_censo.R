#' Load Census Data
#'
#' This function downloads and opens Censo data using censobr
#'
#' @param save_to A \code{character} with the directory in which to save the downloaded cache files.
#' @param dataset A \code{character} indicating which dataset to open
#' @param year A \code{numeric} indicating for which year the data will be loaded, in the format YYYY. Can be 1960, 1970, 1980, 1991, 2000, 2010 or 2022
#' @param columns A \code{character} vector indicating which variables to load
#' 
#' @return A message indicating sucess.
#' @import remotes
#' @import censobr
#' @import arrow
#' @importFrom magrittr %>%
#' @import tidyverse
#'
#' @examples
#' \dontrun{
#' load_censo(
#'   save_to = "Directory/You/Would/like/to/save/the/files",
#'   dataset = "households"
#'   year = 2010,
#'   raw_data = FALSE
#' )
#' }
#' @export



load_censo <- function(save_to = getwd(), dataset,
                       year, columns = everything()) {
  
  # Check if censobr namespace already attached
  if (!"censobr" %in% .packages()) {
    # If not attached, attach it
    remotes::install_github("ipeaGIT/censobr", ref="dev")
    attachNamespace("censobr")
  }

options(arrow.unsafe_metadata = TRUE) # Stop throwing safety warnings

censobr::set_censobr_cache_dir(path = save_to)

# Open data
# Works regardless of whether data is being downloaded for the first time or not

if (dataset == "population") {
  dat <- censobr::read_population(year = year) 
} else if (dataset ==  "households") {
  dat <- censobr::read_households(year = year)
} else if (dataset == "families") {
  dat <- censobr::read_families(year = year)
} else if (dataset == "emigration") {
  dat <- censobr::read_emigration(year = year)
} else if (dataset == "mortality") {
  dat <- censobr::read_mortality(year = year)
} else if (dataset == "tracts") {
  dat <- censobr::read_tracts(year = year)
}

dat <- dat |>
  dplyr::select(all_of(columns)) |>
  collect()

assign("dat", dat, envir = .GlobalEnv)

return(paste0("Sucessfully opened ", dataset, " data from year ", year, ".")
) }