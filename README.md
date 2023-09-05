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
| **[download_years](#download_years)** | *Facilitates the retrieval (from IBGE) of PNAD Contínua data for user-specified years, while also applying the functions above to the data.*  |

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
Example:

```{r}
# data_untreated= data downloaded from an external source, a PNADC dataframe for a specific period.
data <- incoming_dat(data_untreated)
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

## basic_panel()

**Arguments** 

1.  *Data received*: `"incoming_dat"`
This variable is created in the **[builds_identifiers](#builds_identifiers)** function described above.

**Actions**
1. Applies both functions ( *[cleans_dat](#cleans_dat)* and *[builds_identifiers](#builds_identifiers)*) to the received dataset.

## download_years()
**Description**

We created this function in order to integrate the [*`get_pnadc`*](https://www.rdocumentation.org/packages/PNADcIBGE/versions/0.7.0/topics/get_pnadc) function with the logic process of applying a panel structure to PNADc datasets.
______________________________________________________________________________________________________

First, we must state that, to the current state of this package, you can only download selected years worth of PNADc datasets, so, if you wish to do a panel regression analysis of the PNADc data from the 2nd and 3rd quarters of 2015, we suggest that you use the *`get_pnadc`* function (documentation above) and apply our *[basic_panel](#basic_panel)* function after that.
Our function could do that as well, but you'd have downloaded also the data from the 1st and 4th quarters of 2015, therefore wasting valuable resources like computer capacity, internet and, most of all, time.
______________________________________________________________________________________________________
What our function does do is not only download the PNADc files for all the quarters of a given year. It processes and generates panel data from the PNAD (National Household Sample Survey) dataset for specified years and quarters. The function also reads the data and divides all the observations by which panel they belong (variable  `V1014` allows us to do that with a simple `filter`). The goal is to organize and store the data frames in a structured manner for further analysis. 

The function performs the following steps:

1. Initializes empty lists: `pnad_list`, `vars_list`, and `panel_list` to store different data frames.
2. Loops through the input `years`:
   
   - For each year, it further loops through quarters 1 to 4.
     
   - Fetches PNADC (Continuous National Household Sample Survey) data for the given year and quarter using the `get_pnadc` function. The resulting data is stored in the `pnad_list` with a specific naming convention.
     
   - Selects specific variables of interest from the PNAD data: `year, quarter, UF (Federal Unit), UPA (Analytical Planning Unit), V1008, V1014, V2003, V2005, V2007, V2008, V20081, V20082 and V1023`. These selected variables are stored in the `vars_list` with appropriate naming.
     
   - Subsets the data based on specific conditions (integer value of `V1014`) and applies data cleaning and identifier-building functions (`cleans_dat()` and `builds_identifiers()`) to create intermediary panel data frames for each condition.
     
   - If the number of rows in the intermediary panel data exceeds 5000 (this is an arbitrary number we defined based on the observation of existent data frames), it is saved as an RDS file in the current directory with a specific naming convention (the naming is based on the correspondent number of year, quarter and panel of the file). Otherwise, the intermediary data is removed from memory.
     
     Example of RDS file naming:
 `“2021_1_2”` (this file corresponds to the second panel of the first quarter of the year 2021).

3. The process is repeated for each quarter of each year specified in the input.


PS: We are developing a function that gets all those files and turns them into a list with one dataframe per panel (combining the quarters).



**Arguments** 

1.  *Data received*: `"incoming_dat"`
This variable is created in the **[builds_identifiers](#builds_identifiers)** function described above.

**Actions**
1. Applies both functions ( *[cleans_dat](#cleans_dat)* and *[builds_identifiers](#builds_identifiers)*) to the received dataset.
