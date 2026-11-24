# Load Continuous PNAD Data

This function downloads PNADC data and applies panel identification
algorithms

## Usage

``` r
load_pnadc(
  save_to,
  years,
  quarters = 1:4,
  panel = "advanced",
  raw_data = FALSE,
  deflator = TRUE,
  defyear = NULL,
  defperiod = NULL,
  save_options = c(TRUE, TRUE),
  vars = NULL
)
```

## Arguments

- save_to:

  A `character` with the directory in which to save the downloaded
  files.

- years:

  A `numeric` indicating for which years the data will be loaded, in the
  format YYYY. Can be any vector of numbers, such as 2010:2012.

- quarters:

  The quarters within those years to be downloaded. Can be a numeric
  vector or a list of vectors, for different quarters per year.

- panel:

  A `character` choosing the panel algorithm to apply ("none", "basic",
  or "advanced"). For details, check
  [`vignette("BUILD_PNADC_PANEL")`](https://datazoom.com.br/datazoom.social/articles/BUILD_PNADC_PANEL.md)

- raw_data:

  A `logical` setting the return of raw (`TRUE`) or processed (`FALSE`)
  variables.

- deflator:

  A logical passed to
  [`get_pnadc`](https://rdrr.io/pkg/PNADcIBGE/man/get_pnadc.html). If
  `TRUE` (the default), deflator variables supplied by `PNADcIBGE` are
  included in the downloaded microdata.

- defyear:

  The deflator year passed to
  [`get_pnadc`](https://rdrr.io/pkg/PNADcIBGE/man/get_pnadc.html) for
  annual microdata. This argument is ignored for quarterly downloads and
  is used only when `deflator = TRUE`. See
  [`get_pnadc`](https://rdrr.io/pkg/PNADcIBGE/man/get_pnadc.html) for
  details.

- defperiod:

  The deflator period passed to
  [`get_pnadc`](https://rdrr.io/pkg/PNADcIBGE/man/get_pnadc.html) for
  annual per-topic microdata. This argument is ignored for quarterly
  downloads and is used only when `deflator = TRUE`. See
  [`get_pnadc`](https://rdrr.io/pkg/PNADcIBGE/man/get_pnadc.html) for
  details.

- save_options:

  A `logical` vector of length 2. Controls whether quarterly files are
  saved and in which format all files are saved. Panel files are always
  saved. There are four possible combinations:

  - `c(TRUE, TRUE)`: saves quarterly and panel files in `.rds` format.
    This is the default.

  - `c(TRUE, FALSE)`: saves quarterly and panel files in `.parquet`
    format.

  - `c(FALSE, TRUE)`: does not save quarterly files; panel files are
    saved in `.rds` format.

  - `c(FALSE, FALSE)`: does not save quarterly files; panel files are
    saved in `.parquet` format.

- vars:

  A `character` vector of additional variable names to be downloaded,
  following the same convention as the `vars` parameter in
  [`get_pnadc`](https://rdrr.io/pkg/PNADcIBGE/man/get_pnadc.html). Each
  name must match a column in the PNADC microdata exactly as published
  by IBGE (e.g. `"VD4019"`, `"V2009"`).

  Note that
  [`get_pnadc`](https://rdrr.io/pkg/PNADcIBGE/man/get_pnadc.html) always
  returns a set of structural columns regardless of this argument, these
  include survey design weights (`V1027`, `V1028`, `V1028001`,
  `V1028200`, `posest`, `posest_sxi`) and identifiers such as `UF`,
  `Estrato`, `V1029`, `V1033`, `ID_DOMICILIO`, totalling around 210
  columns. When `deflator = TRUE`, deflator variables (`Habitual`,
  `Efetivo`) are also included. The `vars` argument adds *on top of*
  those columns; it does not restrict them. Use `NULL` (the default) to
  download all available microdata columns.

  Deflation support in this wrapper is provided by `PNADcIBGE`. For the
  deflator methodology and the deflator files themselves, see
  [`pnadc_deflator`](https://rdrr.io/pkg/PNADcIBGE/man/pnadc_deflator.html)
  in that package.

  If `panel` is not `"none"`, any columns required by the panel
  identification algorithm that are missing from `vars` will be added
  automatically and a [`warning`](https://rdrr.io/r/base/warning.html)
  will list the columns that were added. The required columns per
  algorithm are:

  - `"basic"`: `UPA`, `V1008`, `V1014`, `V2007`, `V20082`, `V20081`,
    `V2008`.

  - `"advanced"`: all of the above, plus `V2003`.

  Note that several of these (`UPA`, `V1008`, `V1014`) are part of the
  structural columns always returned by
  [`get_pnadc`](https://rdrr.io/pkg/PNADcIBGE/man/get_pnadc.html), so in
  practice only `V2007`, `V20082`, `V20081`, `V2008` (and `V2003` for
  `"advanced"`) are likely to be auto-added.

## Value

A message indicating the successful save of panel files.

## Examples

``` r
if (FALSE) { # interactive()
### DO NOT RUN ###
load_pnadc(
  save_to = tempdir(),
  years = 2016,
  quarters = 1:4,
  panel = "advanced",
  raw_data = FALSE,
  deflator = TRUE,
  save_options = c(FALSE, FALSE)
)
}
```
