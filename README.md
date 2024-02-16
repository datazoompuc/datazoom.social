
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Datazoom.social - R version

> <a href="http://www.econ.puc-rio.br/datazoom/">**DataZoom:**</a>
> Desenvolvido pelo Departamento de Economia da Pontifícia Universidade
> Católica do Rio de Janeiro (PUC-Rio)

<a href="https://github.com/datazoompuc/datazoom_social_Stata"><img
src="https://raw.githubusercontent.com/datazoompuc/datazoom_social_stata/master/logo.jpg" align="" width="100" hspace="10" vspace="6"></a>

This package is in development stage - more datasets will be released
soon.

# Descripton

The datazoom.social package facilitates access to official Brazilian
social data.

The package provides functions that download, pre-process and edit (in
research-oriented ways) data released from IBGE (Brazilian Institute of
Geography and Statistics) about the most important surveys of Brasil in
terms of individual information.

In this first version of the package (1.0), the focus is only on the
Continuous PNAD (Continuous National Household Sample Survey), easing
comprehension and research aimed at better understanding brazillian
microdata. The Continuous PNAD is conducted through a sample of
households with a rotation scheme. Thus, the same household is
interviewed five times and, therefore, the same individuals are likely
to be interviewed more than once. However, the identification of them is
not provided. The survey is produced periodically every quarter. It
releases information such as the working-age population, average income,
employed population, among other important indicators for socioeconomic
research.

You can find more specific information about PNADC in:
<https://www.ibge.gov.br/estatisticas/sociais/populacao/9173-pesquisa-nacional-por-amostra-de-domicilios-continua-trimestral.html>

# Installation

You can install the released version of `datazoom.social` from
[CRAN](https://CRAN.R-project.org/package=datazoom.social) with:

``` r
install.packages("datazoom.social")
```

And the development version from GitHub with:

``` r
install.packages("devtools")
devtools::install_github("datazoompuc/datazoom.social")
```

------------------------------------------------------------------------

## Data

<table>
<tr>
<td>

|                                 |                                      |
|---------------------------------|--------------------------------------|
| **[LOAD_PNADC](#load_pnadc)**   | *Download PNAD of a specific period* |
| **[BUILD_PNADC](#build_pnadc)** | *Build a Panel of PNAD*              |

------------------------------------------------------------------------

# Functions

## LOAD_PNADC

**Description**

We created this function in order to integrate the
[*`get_pnadc`*](https://www.rdocumentation.org/packages/PNADcIBGE/versions/0.7.0/topics/get_pnadc)
function with the logic process of applying a panel structure to PNADc
datasets. This function is from the package `PNADcIBGE`.

The function not only download the PNADc files for the given quarters of
a given year. It processes and generates panel data from the PNADC
(Continuous National Household Sample Survey) dataset for specified
years and quarters. The function also reads the data and divides all the
observations by which panel they belong (variable `V1014` shows that).
The goal is to organize and store the data frames in a structured manner
for further analysis.

The function performs by the following steps:

*1.* Create two parallel vector of years and quarters for futher loop.

*2.* Loop over the two vectors with the get_pnadc to download the data
for each quarter of the year and save in the directory.

*3.* Register, for every quarter, the panel’s which the quarter’s
observations are included using variable `V1014`.

*4.* Download each quarter to a separate file type `.rds`.

*5.* Spliting data into panels and saving in files csv:
`pnad_panel_x.csv`.

*6.* Read each file and apply the identification algorithms defined in
the `build_pnadc_panel`.

- This algorithms used in the function `build_pnadc_panel` for the
  identification are the same ones used in the paper: Ribas, Rafael
  Perez, and Sergei Suarez Dillon Soares(2008): “Sobre o painel da
  Pesquisa Mensal de Emprego (PME) do IBGE”.

------------------------------------------------------------------------

**Options:**

1.  **save_to**: The directory in which the user desires to save the
    downloaded files.

2.  **year**: The years of the PNADc the user would like to download.

3.  **quarter**: The quarters within those years to be downloaded.

4.  **panel**: Which panel algorithm to apply to this data. There are
    three options:

    - `none`: No panel build. Returns the original data.
    - `basic`: Performs basic identification steps for creating
      households and individual identifiers for panel construction
    - `advanced`: Performs advanced identification steps for creating
      households and individual identifiers for panel construction.

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

------------------------------------------------------------------------

## BUILD_PNADC

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
