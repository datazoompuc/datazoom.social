# datazoom.pnadcontinua

datazoom.pnadcontinua is an R package that facilitates access to official data regarding the Continuous PNAD. The package provides functions that download and pre-process selected datasets. 

## Installation
The package can be installed using `devtools`:

```
if(!require(devtools)) install.packages("devtools")
devtools::install_github("datazoompuc/PNAD_Continua/r-package")
```

## Panel of Individuals

The Continuous PNAD is a panel survey from IBGE, in which each household is interviewed for five consecutive quarters. 

## Usage

```
library(datazoom.pnadcontinua)
```
Use ```load_pnadc``` to load and clean microdata from a specified directory.

To download data, set ```sources``` as a list of vectors
of time periods
```

dates <- list(c(1, 2012), c(2, 2012))

microdata <- load_pnadc(lang = 'english',
                        sources = dates,
                        download_directory = './Desktop')
```

To load the data from a folder:
```
microdata <- load_pnadc(lang = 'english',
                        sources = './Desktop/folder_name')
```

To load an individual .txt file corresponding to a given period of the survey:

```
 microdata <- load_pnadc(sources = './PNADC_012020.txt')
```
### Dictionary

The package also comes with dictionaries in English and Portuguese,
```dictionary_eng```, ```dictionary_pt.br```, with a description of all variables in the
survey, as well as of the variables assumed by them.

## Credits
DataZoom is developed by a team at Pontifícia Universidade Católica do Rio de Janeiro (PUC-Rio), Department of Economics. Our official website is at: http://www.econ.puc-rio.br/datazoom/index.html.
