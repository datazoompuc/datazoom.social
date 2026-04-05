
<a href="https://github.com/datazoompuc/datazoom.social"><img src="https://raw.githubusercontent.com/datazoompuc/datazoom.social/master/logo_hex.png" align="left" width="100" hspace="10" vspace="6"></a>

<!-- README.md is generated from README.Rmd. Please edit that file -->

# datazoom.social

<!-- badges: start -->

![Languages](https://img.shields.io/github/languages/count/datazoompuc/datazoom.social?style=flat)
![Commits](https://img.shields.io/github/commit-activity/y/datazoompuc/datazoom.social?style=flat)
![Open
Issues](https://img.shields.io/github/issues-raw/datazoompuc/datazoom.social?style=flat)
![Closed
Issues](https://img.shields.io/github/issues-closed-raw/datazoompuc/datazoom.social?style=flat)
![Files](https://img.shields.io/github/directory-file-count/datazoompuc/datazoom.social?style=flat)
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

The `load_pnadc` function is a wrapper for
[*`get_pnadc`*](https://www.rdocumentation.org/packages/PNADcIBGE/versions/0.7.0/topics/get_pnadc)
from the package `PNADcIBGE`, with added identification algorithms to
build a Panel. For any additional questions not related to the
identification variables, please check with them.

------------------------------------------------------------------------

**Panel Structure:**

The table below shows the first and last quarter (`ANOtrimestre`, e.g.
`20121` = 2012 Q1) covered by each PNADC rotating panel:

| Panel | Start |   End |
|------:|------:|------:|
|     1 | 20121 | 20124 |
|     2 | 20121 | 20141 |
|     3 | 20132 | 20152 |
|     4 | 20143 | 20163 |
|     5 | 20154 | 20174 |
|     6 | 20171 | 20191 |
|     7 | 20182 | 20202 |
|     8 | 20193 | 20213 |
|     9 | 20204 | 20224 |
|    10 | 20221 | 20241 |
|    11 | 20232 | 20252 |
|    12 | 20243 | 20263 |
|    13 | 20254 | 20274 |
|    14 | 20271 | 20291 |

------------------------------------------------------------------------

**Panel Structure:**

The table below shows the first and last quarter (`ANOtrimestre`, e.g.
`20121` = 2012 Q1) covered by each PNADC rotating panel:

| Panel | Start |   End |
|------:|------:|------:|
|     1 | 20121 | 20124 |
|     2 | 20121 | 20141 |
|     3 | 20132 | 20152 |
|     4 | 20143 | 20163 |
|     5 | 20154 | 20174 |
|     6 | 20171 | 20191 |
|     7 | 20182 | 20202 |
|     8 | 20193 | 20213 |
|     9 | 20204 | 20224 |
|    10 | 20221 | 20241 |
|    11 | 20232 | 20252 |
|    12 | 20243 | 20263 |
|    13 | 20254 | 20274 |
|    14 | 20271 | 20291 |

------------------------------------------------------------------------

**Usage:**

Default

``` r

load_pnadc(
  save_to = getwd(),
  years,
  quarters = 1:4,
  panel = "advanced",
  raw_data = FALSE,
  save_options = c(TRUE, TRUE)
)
```

To download PNADC data for all quarters of 2022 and 2023, with advanced
identification, simply run

``` r
load_pnadc(
  save_to = "Directory/You/Would/like/to/save/the/files",
  years = 2022:2023
)
```

To download PNADC data for all of 2022, but only the first quarter of
2023, run

``` r
load_pnadc(
  save_to = "Directory/You/Would/like/to/save/the/files",
  years = 2022:2023,
  quarters = list(1:4, 1)
)
```

To download PNADC data without any variables treatment or identification
(e.g., for all quarters of 2021), run

``` r
load_pnadc(
  save_to = "Directory/You/Would/like/to/save/the/files",
  years = 2021,
  panel = "none",
  raw_data = TRUE
)
```

To download PNADC data, keep the quarters parquet on disk, and save
panels as Parquet, run

``` r
load_pnadc(
  save_to = "Directory/You/Would/like/to/save/the/files",
  years = 2022,
  save_options = c(TRUE, FALSE)
)
```

To download PNADC data and save panels as CSV but discard the
intermediate quarters parquet, run

``` r
load_pnadc(
  save_to = "Directory/You/Would/like/to/save/the/files",
  years = 2022,
  save_options = c(FALSE, TRUE)
)
```

------------------------------------------------------------------------

**Options:**

1.  **save_to**: The directory in which the user desires to save the
    downloaded files.

2.  **years**: picks the years for which the data will be downloaded

3.  **quarters**: The quarters within those years to be downloaded. Can
    be either a vector such as `1:4` for consistent quarters across
    years, or a list of vectors, if quarters are different for each
    year.

4.  **panel**: Which panel algorithm to apply to this data. There are
    three options:

    - `none`: No panel is built. If `raw_data = TRUE`, returns the
      original data. Otherwise, creates some extra treated variables.
      The intermediate quarters parquet is always kept when
      `panel = "none"`.
    - `basic`: Performs basic identification steps for creating
      households and individual identifiers for panel construction
    - `advanced`: Performs advanced identification steps for creating
      households and individual identifiers for panel construction.

5.  **raw_data**: A command to define if the user would like to download
    the raw or treated data. There are two options:

    - `TRUE`: if you want the PNADC variables as they come.
    - `FALSE`: if you want the treated version of the PNADC variables.

6.  **save_options**: A logical vector of length 2 controlling file
    saving behaviour:

    - `c(TRUE, TRUE)` (default): keeps the intermediate quarters parquet
      after panel is built; saves panel files as `.csv`.
    - `c(FALSE, TRUE)`: deletes the quarters parquet after use; saves
      panel files as `.csv`.
    - `c(TRUE, FALSE)`: keeps the quarters parquet; saves panel files as
      a `.parquet` dataset.
    - `c(FALSE, FALSE)`: deletes the quarters parquet after use; saves
      panel files as a `.parquet` dataset.

------------------------------------------------------------------------

**Details:**

The function performs the following steps:

1.  Loop over years and quarters using `PNADcIBGE::get_pnadc` to
    download the data. All quarters are collected in memory and saved
    together into a single `pnadc_quarters.parquet` file in `save_to`.
    If the `raw_data` option is `FALSE`, some PNADC variables are
    treated at this stage.

2.  Split the data into panels by the panel variable `V1014`. Data from
    each panel is saved depending on `save_options`.

3.  Read each panel file and apply the identification algorithms defined
    in `build_pnadc_panel`.

4.  If `save_options[1] = FALSE`, the intermediate quarters parquet is
    deleted after the panels are built.

- The identification algorithms in `build_pnadc_panel` are drawn from
  Ribas, Rafael Perez, and Sergei Suarez Dillon Soares (2008): “Sobre o
  painel da Pesquisa Mensal de Emprego (PME) do IBGE”.

------------------------------------------------------------------------

------------------------------------------------------------------------

## PNAD Panel Identification

------------------------------------------------------------------------

**Usage:**

Basic Panel

``` r

panel_data <- build_pnadc_panel(dat = pnad_sample, panel = "basic")
```

Advanced Panel

``` r

panel_data <- build_pnadc_panel(dat = pnad_sample, panel = "advanced")
```

------------------------------------------------------------------------

**Description**

Our `load_pnadc` function uses the internal function `build_pnadc_panel`
to identify households and individuals across quarters. The method used
for the identification is based on the paper of Ribas, Rafael Perez, and
Sergei Suarez Dillon Soares (2008): “Sobre o painel da Pesquisa Mensal
de Emprego (PME) do IBGE”.

------------------------------------------------------------------------

## Basic Identification

The household identifier – stored as `id_dom` – combines the variables:

- `UPA` – Primary Sampling Unit - PSU;

- `V1008` – Household;

- `V1014` – Panel Number;

In order to create a unique number for every combination of those
variables.

------------------------------------------------------------------------

The basic individual identifier – stored as `id_ind` – combines the
household id with:

- `V2003` – Order number: individual’s unique number within their
  household;

- `V2007` – Sex;

- Date of Birth – \[`V20082` (year), `V20081` (month), `V2008` (day)\];

In order to create an unique number for every combination of those
variables.

------------------------------------------------------------------------

## Advanced Identification

The advanced identifier is saved as `id_rs`. On individuals who were not
matched on all interviews, we relax some assumptions to increase
matching power. Under the assumption that the date of birth is often
misreported, we take individuals who are either:

1.  Head of the household or their partner

2.  Child of the head of the household, 25 or older

For these observations, we run the basic identification again, but
allowing the year of birth to be wrong. We also include the order
number.

## Attrition

The tables below show the levels of attrition obtained using the basic
and advanced identification algorithms, and compares them to the
attrition levels obtained in the Stata `datazoom_social` package.

| Interview | Percentage found (R) | Percentage found (Stata) |
|----------:|---------------------:|-------------------------:|
|         1 |                100.0 |                    100.0 |
|         2 |                 86.2 |                     85.7 |
|         3 |                 78.5 |                     77.5 |
|         4 |                 73.2 |                     71.6 |
|         5 |                 69.1 |                     66.8 |

Attrition for Panel 2

Each cell is the percentage of PNADC observations that are identified by
the advanced algorithm in each interview.

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
