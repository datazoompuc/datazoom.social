# datazoom.pnadcontinua

datazoom.pnadcontinua is an R package that facilitates access to official data regarding the Continuous PNAD. The package provides functions that download and pre-process selected datasets. 

## Installation
The package can be installed using `devtools` like so:

```
if(!require(devtools)) install.packages("devtools")
devtools::install_github("datazoompuc/datazoompuc/PNAD_Continua/tree/master/r-package")
```

## Painel of Individuals

The Continuous PNAD is a panel survey, in which each household is interviewed for five consecutive quarters. Despite correctly identifying the same household in all five interviews, the Pnad Continuous does not assign the same identification number to each member of the household at every interview. 
In case the user wishes to work with an individuals panel,
it is necessary to construct a variable to identify each individual throughout different surveys.
For this reason we use algorithms suggested by Ribas and Soares (2008).
The authors elaborated a basic system identification and an advanced one. They differ by the number of 
variables used to identify individuals in different surveys.
The idea is to verify inconsistencies in the set of variables. Either way the program may take a resonable 
amount of time to perform the identification process, depending on the computational
capacity.


## Usage

```
library(datazoom.pnadcontinua)

# Downloads data
data <- download(c(2018, 2019))

# Loads painel data
painel_data <- funcoes_painel(2018)
```

## Credits
DataZoom is developed by a team at Pontifícia Universidade Católica do Rio de Janeiro (PUC-Rio), Department of Economics. Our official website is at: http://www.econ.puc-rio.br/datazoom/index.html.
