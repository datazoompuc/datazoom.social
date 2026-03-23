#' Simulated PNAD sample dataset
#'
#' A small simulated dataset inspired by microdata from the Brazilian
#' Continuous National Household Sample Survey (PNAD Contínua), included for
#' examples, tests, and documentation in the `datazoom.social` package.
#'
#' This dataset does not contain real PNAD observations. It was created only
#' for demonstration purposes and includes a small subset of variables from the
#' original files distributed by IBGE. Its reduced size makes package examples
#' faster and lighter, while preserving a structure similar to that of the
#' original survey data.
#'
#' @format A `data.table` and `data.frame` with 31 rows and 23 variables:
#' \describe{
#'   \item{V1}{Record identifier.}
#'   \item{Ano}{Survey year.}
#'   \item{Trimestre}{Survey quarter.}
#'   \item{UF}{Federative unit code.}
#'   \item{UPA}{Primary sampling unit identifier.}
#'   \item{V1008}{Household serial identifier.}
#'   \item{V1014}{Number of household members.}
#'   \item{V1016}{Household interview status or type code.}
#'   \item{V20082}{Year of birth.}
#'   \item{V20081}{Month of birth.}
#'   \item{V2008}{Day of birth or age-related auxiliary code, as provided in the simulated data.}
#'   \item{V2007}{Sex code.}
#'   \item{V2009}{Age in years.}
#'   \item{VD3004}{Educational attainment code.}
#'   \item{VD4001}{Labor force status code.}
#'   \item{VD4002}{Employment status code.}
#'   \item{VD4005}{Employment position or job category code.}
#'   \item{VD4009}{Usual hours worked category or related labor variable code.}
#'   \item{VD4019}{Monthly labor income.}
#'   \item{V4010}{Main job identifier or occupation-related code.}
#'   \item{V4012}{Economic activity or occupation grouping code.}
#'   \item{V4013}{Time in job or age-related auxiliary labor code.}
#'   \item{V4022}{Household or person weight, as represented in the simulated data.}
#' }
#'
#' @details
#' The purpose of `pnad_sample` is to provide a lightweight object that mimics
#' part of the structure of PNAD Contínua microdata, allowing users to run
#' examples without downloading or processing the full original files.
#'
#' Variable names follow the naming convention used in the original survey
#' microdata, but the values in this dataset are simulated and should not be
#' used for substantive empirical analysis.
#'
#' @source Inspired by the structure of PNAD Contínua microdata produced by the
#' Brazilian Institute of Geography and Statistics (IBGE).
#' \url{https://www.ibge.gov.br}
"pnad_sample"