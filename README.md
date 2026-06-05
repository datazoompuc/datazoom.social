
<a href="https://github.com/datazoompuc/datazoom.social"><img src="https://raw.githubusercontent.com/datazoompuc/datazoom.social/master/logo_hex.png" align="left" width="100" hspace="10" vspace="6"></a>

<!-- README.md is generated from README.Rmd. Please edit .Rmd file -->

# datazoom.social

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/datazoom.social)](https://CRAN.R-project.org/package=datazoom.social)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/datazoom.social)](https://CRAN.R-project.org/package=datazoom.social)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/datazoom.social)](https://CRAN.R-project.org/package=datazoom.social)
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

This package is in development: more datasets will be added soon.

In the first version of the package, the focus is on the [Continuous
PNAD](https://www.ibge.gov.br/estatisticas/sociais/populacao/9173-pesquisa-nacional-por-amostra-de-domicilios-continua-trimestral.html)
panel. We allow for many quarters to be easily downloaded and read, and
provide identification of individuals accross time forming a panel.

# Installation

You can install the released version of `datazoom.social` from
[CRAN](https://CRAN.R-project.org/package=datazoom.social) with:

``` r
install.packages("datazoom.social")
```

You can install the development version of `datazoom.social` from GitHub
with:

``` r
install.packages("devtools")
devtools::install_github("datazoompuc/datazoom.social")
```

## Functions

<table>

<tr>

<td>

|  |  |
|----|----|
| **[`load_pnadc`](#continuous-pnad)** | *Download PNADC of a range of quarters* |
| **[`build_pnadc_panel`](#pnad-panel-identification)** | *Identify individuals throughout time* |

</td>

</tr>

</table>

## Continuous PNAD

The `load_pnadc` function is a wrapper for
*[`get_pnadc`](https://www.rdocumentation.org/packages/PNADcIBGE/versions/0.7.0/topics/get_pnadc)*
from the package `PNADcIBGE`, with added identification algorithms for
panel construction. For details on the identification algorithms, see
`vignette("BUILD_PNADC_PANEL")`.

------------------------------------------------------------------------

**Panel Structure:**

The table below shows the first and last quarter (`ANOtrimestre`,
e.g. `20121` = 2012 Q1) covered by each PNADC rotating panel:

| Panel | Start | End   |
|------:|-------|-------|
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

**Options:**

1.  **save_to**: The directory in which the user desires to save the
    downloaded files.

2.  **years**: picks the years for which the data will be downloaded

3.  **quarters**: The quarters within those years to be downloaded. Can
    be either a vector such as `1:4` for consistent quarters across
    years, or a list of vectors, if quarters are different for each year
    (e.g. `list(1:4, 1:2)` for four quarters in the first year and two
    in the second).

4.  **panel**: Which panel algorithm to apply to this data. There are
    five options:

    - `none`: No panel is built. If `raw_data = TRUE`, returns the
      original data. Otherwise, creates some extra treated variables.
      Quarterly files are saved depending on `save_options`.
    - `basic`: Performs basic identification steps using household IDs,
      sex, and exact dates of birth.
    - `advanced_1`: Performs Stage 1 advanced identification, imputing
      missing birth dates using within-household donors.
    - `advanced_2`: Performs Stage 2 advanced identification, relaxing
      the year of birth constraint.
    - `advanced_3` (Recommended): Performs Stage 3 advanced
      identification, utilizing Graph Theory for fuzzy matching of
      fragmented interviews to account for typographical errors.

5.  **raw_data**: A command to define if the user would like to download
    the raw or treated data. There are two options:

    - `TRUE`: if you want the PNADC variables as they come.
    - `FALSE`: if you want the treated version of the PNADC variables.

6.  **save_options**: A logical vector of length 2 controlling file
    saving behaviour:

    - `c(TRUE, TRUE)` (default): saves quarterly and panel files as
      `.rds`.
    - `c(FALSE, TRUE)`: does not save quarterly files; saves panel files
      as `.rds`.
    - `c(TRUE, FALSE)`: saves quarterly and panel files as `.parquet`
      datasets.
    - `c(FALSE, FALSE)`: does not save quarterly files; saves panel
      files as a `.parquet` dataset.

7.  **vars**: A character vector of additional variable names to
    download, following the same convention as `vars` in
    `PNADcIBGE::get_pnadc()`. Use `NULL` (the default) to download all
    available microdata columns. See the note above regarding the ~210
    structural columns that are always returned by
    `PNADcIBGE::get_pnadc()` regardless of this argument

------------------------------------------------------------------------

**Details:**

The function performs the following steps:

1.  Loop over years and quarters using `PNADcIBGE::get_pnadc` to
    download the data. All quarters are collected in memory and saved
    depending on `save_options`.
2.  Split the data into panels by the panel variable `V1014`.
3.  Apply the identification algorithms defined in `build_pnadc_panel`.
4.  Save the panel files as `.rds` or `.parquet`, depending on
    `save_options`.

- The base identification logic in `build_pnadc_panel` was originally
  drawn from Ribas, Rafael Perez, and Sergei Suarez Dillon Soares
  (2008): “Sobre o painel da Pesquisa Mensal de Emprego (PME) do IBGE”,
  with extensive modernizations, missing-data imputation, and
  graph-based fuzzy matching introduced by the Data Zoom team.

------------------------------------------------------------------------

**Usage:**

Default:

``` r

load_pnadc(
  save_to = getwd(),
  years,
  quarters = 1:4,
  panel = "advanced_3",
  raw_data = FALSE,
  save_options = c(TRUE, TRUE),
  vars = NULL
)
```

To download PNADC data for all quarters of 2022 and 2023, with advanced
fuzzy identification (Stage 3), simply run:

``` r
load_pnadc(
  save_to = "Directory/You/Would/like/to/save/the/files",
  years = 2022:2023,
  panel = "advanced_3"
)
```

To download PNADC data for all of 2022, but only the first quarter of
2023, run:

``` r
load_pnadc(
  save_to = "Directory/You/Would/like/to/save/the/files",
  years = 2022:2023,
  quarters = list(1:4, 1)
)
```

To download PNADC data without any variables treatment or identification
(e.g., for all quarters of 2021), run:

``` r
load_pnadc(
  save_to = "Directory/You/Would/like/to/save/the/files",
  years = 2021,
  panel = "none",
  raw_data = TRUE
)
```

To download PNADC data, save quarters on disk, and save panels as
Parquet, run:

``` r
load_pnadc(
  save_to = "Directory/You/Would/like/to/save/the/files",
  years = 2022,
  save_options = c(TRUE, FALSE)
)
```

To download PNADC data and save panels as RDS but discard the quarterly
files, run:

``` r
load_pnadc(
  save_to = "Directory/You/Would/like/to/save/the/files",
  years = 2022,
  save_options = c(FALSE, TRUE)
)
```

To download only a specific subset of variables — for example, age
(`V2009`) and habitual income (`VD4019`) — alongside the structural
columns that `PNADcIBGE` always returns, run:

``` r
load_pnadc(
  save_to = "Directory/You/Would/like/to/save/the/files",
  years = 2022,
  vars = c("V2009", "VD4019")
)
```

> **Note:** `PNADcIBGE::get_pnadc()` always downloads a set of ~210
> structural columns regardless of the `vars` argument. These include
> survey design weights (`V1027`, `V1028`, `V1028001`–`V1028200`,
> `posest`, `posest_sxi`), deflator variables (`Habitual`, `Efetivo`),
> and identifiers such as `UF`, `Estrato`, `V1029`, `V1033`, and
> `ID_DOMICILIO`. The `vars` argument adds columns **on top of** those;
> it does not restrict them. Use `vars = NULL` (the default) to download
> all available microdata columns.

If you specify `vars` and also request panel identification, any columns
required by the identification algorithm that are absent from `vars`
will be added automatically and a warning will tell you which ones were
added. For example, when using `panel = "advanced_3"`, the columns
`V2007`, `V20082`, `V20081`, `V2008`, and `V2003` must be present. If
you omit them from `vars`, the function adds them for you:

``` r
# Only V2009 requested, but panel = "advanced_3" needs
# V2007, V20082, V20081, V2008 and V2003 — these are added automatically
# with a warning.
load_pnadc(
  save_to = "Directory/You/Would/like/to/save/the/files",
  years = 2022,
  panel = "advanced_3",
  vars = c("V2009", "VD4019")
)
```

------------------------------------------------------------------------

## PNADC Panel Identification

------------------------------------------------------------------------

**Description**

Our `load_pnadc` function uses the internal function `build_pnadc_panel`
to identify households and individuals across quarters. The base method
used for the identification draws from the paper of Rafael Perez Ribas
and Sergei Suarez Dillon Soares (2008): “Sobre o painel da Pesquisa
Mensal de Emprego (PME) do IBGE”, with modernizations implemented by the
Data Zoom team to handle missing data and typographical errors.

------------------------------------------------------------------------

**Usage:**

Basic Panel:

``` r
panel_data <- build_pnadc_panel(dat = pnad_sample, panel = "basic")
```

Advanced Panel (Stages 1, 2, or 3):

``` r
# Stage 1: Exact matching with donated birth dates
panel_data <- build_pnadc_panel(dat = pnad_sample, panel = "advanced_1")

# Stage 2: Relaxed matching constraints
panel_data <- build_pnadc_panel(dat = pnad_sample, panel = "advanced_2")

# Stage 3: Fuzzy matching using Graph Theory (Recommended)
panel_data <- build_pnadc_panel(dat = pnad_sample, panel = "advanced_3")
```

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

- `V2007` – Sex;
- Date of Birth – \[`V20082` (year), `V20081` (month), `V2008` (day)\];

In order to create a unique number for every combination of those
variables.

------------------------------------------------------------------------

## Advanced Identification

On individuals who were not matched across all interviews using the
basic method, we apply a progressive multi-stage algorithm to increase
matching power without compromising uniqueness.

First, we reproduce the birth date donation method based on the
methodology described in the IPEA technical note (Osório, 2019). It
estimates and imputes missing birth dates (day, month, and year) by
matching individuals with donors from different interviews within the
same household based on sex, acceptable household condition changes, and
estimated age. This process is executed by our internal function
`donate_birth_dates`.

- **Stage 1 (`advanced_1`):** Repeats the basic identification logic,
  but utilizing the donated dates. The identifier – stored as `id_rs1` –
  combines:
  - `id_dom` – Household ID
  - `V2007` – Sex
  - `birth_day` – Donated day of birth
  - `birth_month` – Donated month of birth
  - `birth_year` – Donated year of birth
- **Stage 2 (`advanced_2`):** For individuals not completely matched in
  Stage 1, we relax the year of birth constraint (assuming it is often
  misreported). The identifier – stored as `id_rs2` – combines:
  - `id_dom` – Household ID
  - `birth_month` – Donated month of birth
  - `birth_day` – Donated day of birth
  - `V2003` – Order number in the household
- **Stage 3 (`advanced_3`):** Targets candidates with fragmented
  interviews (less than 5 matches in the previous stages). It considers
  a match successful if there is a unique individual in the same
  household, in a different quarter, that satisfies the acceptable
  difference criteria established by Ribas and Soares (up to 4 days
  difference in the day of birth, 2 months in the month of birth, and a
  dynamically adjusted year-of-birth difference based on the
  individual’s reported age). The final identifier is stored as
  `id_rs3`.

------------------------------------------------------------------------

## Attrition and Identification Rates

### 1. Household Attrition Rate

The table below shows the unconditional attrition rate for households.
This represents the percentage of household units observed in Wave 1
that were successfully re-interviewed and tracked in subsequent waves.

| Interview (Wave) | Household Attrition Rate (%) |
|:----------------:|:----------------------------:|
|        1         |          100.00000           |
|        2         |           93.75667           |
|        3         |           91.84360           |
|        4         |           90.59256           |
|        5         |           89.60861           |

### 2. Initial Identification Rate (Line-Based)

This table reports the percentage of raw PNADC individual observations
(lines) **in Wave 1** for which we successfully built a valid
identifier. Data is lost in this stage exclusively due to the inability
to construct the identifier (e.g., missing essential data) or household
grouping constraints.

| Interview (Wave) | Basic Rate (%) | Adv 1 Rate (%) | Adv 2 Rate (%) | Adv 3 Rate (%) |
|:----------------:|:--------------:|:--------------:|:--------------:|:--------------:|
|        1         |    93.82378    |    95.82954    |    96.40170    |    96.39606    |

### 3. Individual Unconditional Tracking Rate (ID-Based)

This table demonstrates the cumulative retention of tracked individuals
over time. It uses the total number of uniquely identified individuals
from Wave 1 as the universal denominator (starting at 100%), showing how
much tracking power is gained by using the advanced algorithms.

| Interview (Wave) | Basic Rate (%) | Adv 1 Rate (%) | Adv 2 Rate (%) | Adv 3 Rate (%) | Difference (Adv 3 - Basic) |
|:--:|:--:|:--:|:--:|:--:|:--:|
| 1 | 100.00000 | 100.00000 | 100.00000 | 100.00000 | 0.00000 p.p. |
| 2 | 87.01360 | 88.20828 | 88.50570 | 88.83374 | \+ 1.82014 p.p. |
| 3 | 80.55773 | 82.33729 | 82.85546 | 83.38268 | \+ 2.82495 p.p. |
| 4 | 75.81465 | 77.91677 | 78.57830 | 79.25447 | \+ 3.43982 p.p. |
| 5 | 72.01655 | 74.27815 | 75.01486 | 75.79868 | \+ 3.78213 p.p. |

------------------------------------------------------------------------

# Credits

DataZoom is developed by a team at Pontifícia Universidade Católica do
Rio de Janeiro (PUC-Rio), Department of Economics. Our official website
is at: <https://datazoom.com.br/en/>.

To cite package `datazoom.social` in publications use:

> Data Zoom (2023). Data Zoom: Simplifying Access To Brazilian
> Microdata.  
> <https://datazoom.com.br/en/>

A BibTeX entry for LaTeX users is:

    @Unpublished{DataZoom2024,
        author = {Data Zoom},
        title = {Data Zoom: Simplifying Access To Brazilian Microdata},
        url = {https://datazoom.com.br/en/},
        year = {2024}}
