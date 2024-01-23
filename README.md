
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datazoom.social - R version

The datazoom.social package facilitates access to official Brazilian
social data. The package provides functions that download, pre-process
and edit (in research-oriented ways) selected datasets, easing
comprehension and research aimed at better understanding brazillian
microdata.

This package is in development stage - more datasets will be released
soon.

# Installation

You can install the released version of `datazoom.social` from
[CRAN](https://CRAN.R-project.org/package=datazoom.social) with:

``` r
install.packages("datazoom.social")
```

And the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("datazoompuc/datazoom.social")
```

# Functions

## LOAD_PNADC

## load_pnadc()

**Description**

We created this function in order to integrate the
[*`get_pnadc`*](https://www.rdocumentation.org/packages/PNADcIBGE/versions/0.7.0/topics/get_pnadc)
function with the logic process of applying a panel structure to PNADc
datasets.

The function not only download the PNADc files for the given quarters of
a given year. It processes and generates panel data from the PNAD
(National Household Sample Survey) dataset for specified years and
quarters. The function also reads the data and divides all the
observations by which panel they belong (variable `V1014` tells us
that). The goal is to organize and store the data frames in a structured
manner for further analysis.

The function performs by the following steps:

1.  Create two parallel vector of years and quarters for futher loop.
2.  Loop over the two vectors with the get_pnadc to download the data
    for each quarter of the year and save in the directory.
3.  Register, for every quarter, the panel’s which the quarter’s
    observations are included using variable `V1014`.
4.  Spliting data into panels.
5.  Read each file and apply the identification algorithms defined in
    the `build_pnadc_panel`.

------------------------------------------------------------------------

**Options:**

1.  **save_to**: The directory in which the user desires to save the
    downloaded files.

2.  **year**: The years of the PNADc the user would like to download.

3.  **quarter**: The quarters within those years to be downloaded.

4.  **panel**: Which panel algorithm to apply to this data. There are
    three options:

    - `none`:
    - `basic`:
    - `advanced`:

5.  **raw_data**: A command to define if the user would like to download
    the raw or treated data. There are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

------------------------------------------------------------------------

**Examples:**

``` r
# Download treated basic panel from 2016 year
data <- load_pnadc(save_to = "Directory/You/Would/like/to/save/the/files",
                   year = 2016,
                   quarter = 1:4,
                   panel = "basic",
                   raw_data = FALSE)
```

## BUILD_PNADC

# Credits

DataZoom is developed by a team at Pontifícia Universidade Católica do
Rio de Janeiro (PUC-Rio), Department of Economics. Our official website
is at: <https://www.econ.puc-rio.br/datazoom/>.

To cite package `datazoom.social` in publications use:

> Data Zoom (2023). Data Zoom: Simplifying Access To Brazilian
> Microdata.  
> <https://www.econ.puc-rio.br/datazoom/english/index.html>

A BibTeX entry for LaTeX users is:

    @Unpublished{DataZoom2024,
        author = {Data Zoom},
        title = {Data Zoom: Simplifying Access To Brazilian Microdata},
        url = {https://www.econ.puc-rio.br/datazoom/english/index.html},
        year = {2024},
    }
