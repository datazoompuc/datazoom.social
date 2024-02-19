
<a href="https://github.com/datazoompuc/datazoom_social_Stata"><img src="https://raw.githubusercontent.com/datazoompuc/datazoom_social_stata/master/logo.jpg" align="left" width="100" hspace="10" vspace="6"></a>

<!-- README.md is generated from README.Rmd. Please edit that file -->

# datazoom.social

<!-- badges: start -->

![Languages](https://img.shields.io/github/languages/count/datazoompuc/datazoom_social_Stata?style=flat)
![Commits](https://img.shields.io/github/commit-activity/y/datazoompuc/datazoom_social_Stata?style=flat)
![Open
Issues](https://img.shields.io/github/issues-raw/datazoompuc/datazoom_social_Stata?style=flat)
![Closed
Issues](https://img.shields.io/github/issues-closed-raw/datazoompuc/datazoom_social_Stata?style=flat)
![Files](https://img.shields.io/github/directory-file-count/datazoompuc/datazoom_social_Stata?style=flat)
![Followers](https://img.shields.io/github/followers/datazoompuc?style=flat)
<!-- badges: end -->

The datazoom.social package facilitates access to official Brazilian
social data.

This package is in development stage - more datasets will be released
soon.

In this first version of the package, the focus is only on the
[Continuous
PNAD](https://www.ibge.gov.br/estatisticas/sociais/populacao/9173-pesquisa-nacional-por-amostra-de-domicilios-continua-trimestral.html).
We allow for many quarters to be easily downloaded and read, as well as
identifying individuals across time, forming a panel.

# Installation

<!-- You can install the released version of `datazoom.social` from -->
<!-- [CRAN](https://CRAN.R-project.org/package=datazoom.social) with: -->
<!-- ``` {r, eval=FALSE} -->
<!-- install.packages("datazoom.social") -->
<!-- ``` -->

You can install the development version of `datazoom.social` from GitHub
with:

``` r
install.packages("devtools")
devtools::install_github("datazoompuc/datazoom.social")
```

## Data

<table>
<tr>
<td>

|                                                        |                                         |
|--------------------------------------------------------|-----------------------------------------|
| **[Continuous PNAD](#continuous-pnad)**                | *Download PNADC of a range of quarters* |
| **[Panel Identification](#pnad-panel-identification)** | *Build a Panel of PNADC individuals*    |

</td>
</tr>
</table>

## Continuous PNAD

This function is a wrapper for
[*`get_pnadc`*](https://www.rdocumentation.org/packages/PNADcIBGE/versions/0.7.0/topics/get_pnadc)
from the package `PNADcIBGE`, with added identification algorithms to
build a Panel.

------------------------------------------------------------------------

**Options:**

1.  **save_to**: The directory in which the user desires to save the
    downloaded files.

2.  **year**: The years of the PNADc the user would like to download.

3.  **quarter**: The quarters within those years to be downloaded. Can
    be either a vector such as `1:4` for consistent quarters across
    years, or a list of vectors, if quarters are different for each
    year.

4.  **panel**: Which panel algorithm to apply to this data. There are
    three options:

    - `none`: No panel build. Returns the original data. If
      `raw_data = TRUE`, data is still split into panel files.
    - `basic`: Performs basic identification steps for creating
      households and individual identifiers for panel construction
    - `advanced`: Performs advanced identification steps for creating
      households and individual identifiers for panel construction.

5.  **raw_data**: A command to define if the user would like to download
    the raw or treated data. There are two options:

    - `TRUE`: if you want the data as it is originally.
    - `FALSE`: if you want the treated version of the data.

------------------------------------------------------------------------

**Details:**

The function performs by the following steps:

1.  Loop over years and quarters using `PNADcIBGE::get_pnadc` the two
    vectors with the get_pnadc to download the data and save in the
    directory, in files named `pnadc_year_quarter.rds`.

2.  Split the data into panels, by reading each `.rds` file and
    filtering by the quarter variable `V1014`. Data from each panel `x`
    is saved to `pnad_panel_x.csv`. The use of `.csv` allows for data
    from each quarter to be appended on top of the previous, making the
    process much faster.

3.  Read each panel file and apply the identification algorithms defined
    in the `build_pnadc_panel`.

- The identification algorithms in `build_pnadc_panel` are drawn from
  Ribas, Rafael Perez, and Sergei Suarez Dillon Soares (2008): “Sobre o
  painel da Pesquisa Mensal de Emprego (PME) do IBGE”.

------------------------------------------------------------------------

**Examples:**

``` r
# Download treated basic panel from 2016 year
load_pnadc(save_to = "Directory/You/Would/like/to/save/the/files",
                   year = 2016,
                   quarter = 1:4,
                   panel = "basic",
                   raw_data = FALSE)
```

------------------------------------------------------------------------

## PNAD Panel Identification

**Description**

The main goal of the function build_pnadc_panel is to create a Panel of
PNAD where it’s possible to identify each household or individual all
over the interviews by a specific serial number (id) based on some
variables of PNAD Continua.

The method used for the identification is based on the paper of Ribas,
Rafael Perez, and Sergei Suarez Dillon Soares(2008): “Sobre o painel da
Pesquisa Mensal de Emprego (PME) do IBGE”.

------------------------------------------------------------------------

## Basic Identification

For the household identifier, it combines the variables:

- `UPA` - Primary Sampling Unit - PSU;

- `V1008` - Household ;

- `V1014` - Panel Number;

In order to create a unique number for every combination of those
variables.

------------------------------------------------------------------------

For the Individual id identifier, it combines the `household id` with:

- `UF` - Federal Unit;

- `V1023` - Type of Area;

- `V2007` - Gender;

- Date of Birth - \[`V20082` (year), `V20081` (month), `V2008` (day)\];

In order to create an unique number for every combination of those
variables.

For identifying matched observations, the function counts the number of
time that each id appears.

------------------------------------------------------------------------

## Advanced Identification

It’s only run on previously unmatched individuals for identifying new
matched observations.

Advanced identification is divided in two stages:

In `Stage 1` the function combines the variables:

- `V2005` - Housing Condition;

- `V2009` - Age of Resident;

- Date of Birth - \[`V20082`, `V20081`, `V2008`\].

In `Stage 2` it checks if there’s any missing quarters and if there’s no
intersection between their appearences. In that case the function
returns the id.

At last, it handles with unidentifiable observations due to missing
values.

------------------------------------------------------------------------

**Options:**

1.  **dat**: The current PNAD observations.

2.  **panel**: The type of panelling transformation you wish to apply to
    dat:

    - `none`: No panel build. Returns the original data.

    - `basic`: Performs basic identification steps for creating
      households and individual identifiers for panel construction.

    - `advanced`: Performs advanced identification steps for creating
      households and individual identifiers for panel construction.

------------------------------------------------------------------------

**Examples:**

``` r
# Build basic panel 
data <- read.csv("path/to/PNADC_data.csv")
 panel_data <- build_pnadc_panel(dat = data, panel = "basic")
```

------------------------------------------------------------------------

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
        year = {2024}}
