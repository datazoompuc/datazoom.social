## R CMD check results

0 errors | 0 warnings | 0 notes

## This is a resubmission

This is an update to the existing CRAN package datazoom.social (0.1.0 -> 0.1.1).

Changes in this version:
* Added igraph to Imports (previously used but not declared)
* Excluded .github directory from build
* Fixed missing @param/@return documentation for cria_df_de_atrito()
* Updated NEWS.md

## Test environments

* local macOS install (x86_64-apple-darwin20), R 4.5.1 -- 0 errors | 0 warnings | 0 notes
* win-builder (R-devel) -- pending (awaiting results)

## Vignettes

Vignette code chunks are set to `eval = FALSE` because the examples require downloading large microdata files (~100MB+ per quarter) from IBGEs servers, which would make vignette build times impractical for CRAN checks. The code is correct and functional. Users can run it interactively after installing the package.
