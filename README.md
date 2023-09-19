# datazoom.social - R version

The datazoom.social package facilitates access to official 
Brazilian social data. The package provides functions that 
download, pre-process and edit (in research-oriented ways) 
selected datasets, easing comprehension and research aimed
at better understanding brazillian microdata.

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
| **[cleans_dat](#cleans_dat)**                | *Prepares data to build identifiers*                    |
| **[builds_identifiers](#builds_identifiers)**      | *Creates household and individual identifiers*          |
| **[basic_panel](#basic_panel)**                | *Applies both functions above to the data provided* |
| **[load_pnad](#load_pnad)** | *Facilitates the retrieval (from IBGE) of PNAD Contínua data for user-specified years, while also applying the functions above to the data.*  |

</td>
</tr>
</table>

# Functions' Details

## cleans_dat

**Arguments** 

1.  *Data received*: `"incoming_dat"`

**Actions**
1. Filters for missing values (`"99"` or `"9999"`) in variables that refer to the individual's reported birthday ( `V2008`- Day , `V20081`- Month ,  `V20082`- Year ).
2. Turns 4 variables (the 3 cited above + `V2007`) into characters, in order to facilitate the identifier's creation later.
3. Returns the variable `character_dat`, which is the treated data frame.

------------------------------------------------------------------------
**Example**:

```{r}
# data_untreated= data downloaded from an external source, a PNADC dataframe for a specific period.
treated_data <- incoming_dat(data_untreated)
```

## builds_identifiers ()

**Arguments** 

1.  *Data received*: `"character_dat"`
This variable is created in the **[cleans_dat](#cleans_dat)** function described above.

**Actions**
1. Creates each household's unique identifier ( `id_dom` ) by binding each individual's `UPA` , `V1008` and  `V1014` variables.

    PS: All of those variables should be the same for every member of the same household, but differ between every household in the time sample, hence the described action above. This identifier should be the same for the household across time.

2. Creates each individual's unique identifer by pasting together the variables: `UF` , `V1023` , `id_dom` , `V20082` , `V20081` , `V2008` , `V2007`.

3. Returns the variable `b_panel`, which represents the original, treated data frame with the 2 columns (representing the 2 identifiers) added to it.
------------------------------------------------------------------------
**Example**:

```{r}
# treated_data= A treated dataset (using our cleans_dat function)
identified_data <- builds_identifiers(treated_data)
```
## basic_panel()

**Arguments** 

1.  *Data received*: `"identied_data"`
This variable is created in the **[builds_identifiers](#builds_identifiers)** function described above.

**Actions**
1. Applies both functions ( *[cleans_dat](#cleans_dat)* and *[builds_identifiers](#builds_identifiers)*) to the received dataset.

## load_pnad()

**Parameters default**
```{r}
load_pnad(year, quarters= c(1:4))
#the year should always be specified, the quarters default is c(1:4)

```

**Description**

We created this function in order to integrate the [*`get_pnadc`*](https://www.rdocumentation.org/packages/PNADcIBGE/versions/0.7.0/topics/get_pnadc) function with the logic process of applying a panel structure to PNADc datasets.
______________________________________________________________________________________________________

In conclusion:
What our function do is not only download the PNADc files for the given quarters of a given year. It processes and generates panel data from the PNAD (National Household Sample Survey) dataset for specified years and quarters. The function also reads the data and divides all the observations by which panel they belong (variable  `V1014` allows us to do that with a simple `filter`). The goal is to organize and store the data frames in a structured manner for further analysis. 

The function performs the following steps:

1. Initializes empty lists: `pnad_list`, `vars_list`, and `panel_list` to store different data frames.
2. Loops through the input `years`:
   
   - For each year, it further loops through quarters designated by the user (if the user does not specify, it loops through 1 to 4).
     
   - Fetches PNADC (Continuous National Household Sample Survey) data for the given year and quarter using the `get_pnadc` function. The resulting data is stored in the `pnad_list` with a specific naming convention (which we'll discuss below).
       
   - Subsets the data based on specific conditions (integer value of `V1014`) and applies data cleaning and identifier-building functions (`cleans_dat()` and `builds_identifiers()`) to create intermediary panel data frames for each condition.
     
   - If the number of rows in the intermediary panel data exceeds 5000 (this is an arbitrary number we defined based on the observation of existent data frames), it is saved as an RDS file in the current directory with a specific naming convention (the naming is based on the correspondent number of year, quarter and panel of the file). Otherwise, the intermediary data is removed from memory.
     
     Example of RDS file naming:
 `“2021_1_2”` (this file corresponds to the second panel of the first quarter of the year 2021).

3. The process is repeated for each quarter of each year specified in the input.
------------------------------------------------------------------------
**Example**: Downloading the files from the year 2014, specifically from the first through the third quarters.

```{r}
# setwd("desired directory where you'd like the files to be downloaded")

load_pnad(year= 2014, panel= c(1:3))

```

PS: the year should be typed as an integer, always, as well as the quarters.
## bundle_panel()
**Description**

This function aims to combine and organize data from different PNADC panel files. The PNADC survey's data is collected in different quarters, and the individuals are divided into panels. You can use these panels (specified by the variable V1016 in PNADc datasets) to try and "follow" individuals over time.

For more information about this process, please check out the IBGE's website section about the Continuous PNAD: [available here](https://www.ibge.gov.br/estatisticas/sociais/trabalho/2511-np-pnad-continua/30980-pnadc-divulgacao-pnadc4.html?=&t=o-que-e).

 The bundle_panel function is used to combine data from different panels into a single dataset. Essentially, this function helps organize and combine data from different quarters and years of the PNADC into a format that is easier to analyze. It's particlarly useful when paired with the functions from this package, since the files will already be named in a specific way (as specified at the end of the *[download_years](#download_years)* section of this READ.ME) that facilitates the recognition of which file contains data from which panel.
____________________________________________________________________________________________________

**Requirements**

In a designed file in your PC, download every single file that composes a designed PNADc Panel, you can easily do that using our *[download_years](#download_years)* function (we are working on a function that downloads specifically the data for a designated PNADc panel, which will be much more efficient in these types of cases).

Here's a detailed explanation of how the function works:

1) It takes a directory parameter, which should be the directory where the panel data files are located.

2) The function iterates through different panels (from 1 to 9).

3) For each panel, it creates a regular expression pattern to find files related to that panel in the directory. For example, for panel 1, the pattern will be _1$.

4) It then lists all files in the directory that match the current panel's pattern.

5) For each found file, it reads the data (which should be in RDS format, a serialization format for R data) and combines it into a single dataset for the current panel.

6) It stores the combined dataset for the current panel in a list.

7) If there are fewer than 9 files for a specific panel, the function issues a warning, indicating that some files may be missing for that panel.

8) Finally, the function returns a list of combined datasets for each of the 9 panels.

**Example**: This example was designed assuming you have downloaded all files that contain data for the sixth and seventh panel of the PNADc.

For context, the quarters contain observations that belong to this panel range from 2017.1 to 2020.2 (all included)

```{r}
# to download the desired files for these 2 panels, run the commented code below
# setwd("desired directory where you'd like the files to be downloaded")
# load_pnad(c(2016,2017))

# then, to get a list that has 2 dataframes (one for panel 6, another for panel 7), run the code below

panels_6_and_7<-bundle_panel("desired directory where you'd like the files to be downloaded")

```