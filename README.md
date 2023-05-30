# datazoom.social - R version

The datazoom.social package facilitates access to official 
Brazilian social data. The package provides functions that 
download and pre-process selected datasets.

This package is in development stage - more datasets will
be released soon.

# Installation

You can install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("datazoompuc/datazoom.social")
```

# PNAD Contínua

The following functions are available. They allow data 
cleansing and building the basic panel.

<table>
<tr>
<td>

|                               |                                                         |
|-------------------------------|---------------------------------------------------------|
| [cleans_dat]                  | *Prepares data to build identifiers*                    |
| [builds_identifiers]          | *Creates household and individual identifiers*          |
| [basic_panel]                 | *Runs the other two automatically*                      |

</td>
</tr>
</table>
