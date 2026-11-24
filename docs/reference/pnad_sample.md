# Simulated PNAD sample dataset

A small simulated dataset inspired by microdata from the Brazilian
Continuous National Household Sample Survey (PNAD Contínua), included
for examples, tests, and documentation in the `datazoom.social` package.

## Usage

``` r
pnad_sample
```

## Format

A `data.table` and `data.frame` with 31 rows and 23 variables:

- V1:

  Record identifier.

- Ano:

  Survey year.

- Trimestre:

  Survey quarter.

- UF:

  Federative unit code.

- UPA:

  Primary sampling unit identifier.

- V1008:

  Household serial identifier.

- V1014:

  Number of household members.

- V1016:

  Household interview status or type code.

- V20082:

  Year of birth.

- V20081:

  Month of birth.

- V2008:

  Day of birth or age-related auxiliary code, as provided in the
  simulated data.

- V2007:

  Sex code.

- V2009:

  Age in years.

- VD3004:

  Educational attainment code.

- VD4001:

  Labor force status code.

- VD4002:

  Employment status code.

- VD4005:

  Employment position or job category code.

- VD4009:

  Usual hours worked category or related labor variable code.

- VD4019:

  Monthly labor income.

- V4010:

  Main job identifier or occupation-related code.

- V4012:

  Economic activity or occupation grouping code.

- V4013:

  Time in job or age-related auxiliary labor code.

- V4022:

  Household or person weight, as represented in the simulated data.

## Source

Inspired by the structure of PNAD Contínua microdata produced by the
Brazilian Institute of Geography and Statistics (IBGE).
<https://www.ibge.gov.br>

## Details

This dataset does not contain real PNAD observations. It was created
only for demonstration purposes and includes a small subset of variables
from the original files distributed by IBGE. Its reduced size makes
package examples faster and lighter, while preserving a structure
similar to that of the original survey data.

The purpose of `pnad_sample` is to provide a lightweight object that
mimics part of the structure of PNAD Contínua microdata, allowing users
to run examples without downloading or processing the full original
files.

Variable names follow the naming convention used in the original survey
microdata, but the values in this dataset are simulated and should not
be used for substantive empirical analysis.
