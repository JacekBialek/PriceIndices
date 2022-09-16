# Package update 0.0.9 -> 0.1.1

## New function
* One new function (*expenditures*) has been added (faster version of *sales*)

## Changes
* System of weights in the TPD index has been changed. Now it corresponds to the system of weights used in "HICP Guide for Multilateral Methods".

## Improvements
Improved the speed of operation of all functions for price indices.

# Package update 0.0.8 -> 0.0.9

## New functions
* 4 new functions for calculating elasticity of substitution have been added: *elasticity*, *elasticity2*, *elasticity_fig*, *elasticity2_fig*.

# Package update 0.0.7 -> 0.0.8

## New fucntion
* a new internal function has been added (*conversion*). This function provides integer representation of the given text string.

## Improvements
* the following functions have been improved by changing a method for dealing with categorical variables: *model_classification*, *data_classifying*.
* An existing artificial data set (dataCOICOP) has been change into the real scanner data set
* Some new references have been added (e.g. for *GEKS-L*, *GEKS-GL*, *GEKS-AQI*, *GEKS-AQU* functions).
* A README file has been improved by adding some new example with using ML methods for product classifiction.


# Package update 0.0.6 -> 0.0.7

## New function
* a new function which allows to compare indices with a target price index was added (*compare_to_target*)

## Improvements
* some function descriptions have been changed or improved
* set of files.R has been reorganized
* README file and vignette have been extended


# Package update 0.0.5 -> 0.0.6

## New function
* 37 new functions (concerning the **WGEKS**, **GEKSL**, **WGEKSL**, **GEKS-AQU**, **WGEKS-AQU**, **GEKS-AQI**, **WGEKS-AQI**, **GEKS-GAQI**, **WGEKS-GAQI** indices and its extensions, a new function **compare_distances**)

## Bug fixes
* 6 small bugs fixed (they concern **chlowe**, **chgeolowe**, **chyoung**, **chgeoyoung**, **chhybrid** and **chgeohybrid** functions)
* data set on **milk** was corrected and modified

## Data set modification
* A data set on milk has beed modified (one new product has beed added) *

## Function modification
* A function **matched_fig** has been extended by adding a new parameter

# Package update 0.0.4 -> 0.0.5

## New function

* 1 new function (**data_aggregating**) has beed added. This function aggregates the user's data frame over time and/or over outlets (retIDs)

## New data set

* 1 new artificial data set has been added (*dataAGGR*) to demonstrate the utility of the **data_aggregation** function.

## Bug fixes

* 2 bugs fixed (they concern **final_index** and **final_index2** functions)


# Package update 0.0.3 -> 0.0.4

## New function

* 1 new function (**data_check**) has beed added. This function checks if the user's data frame is suitable for further price index calculation.

## Bug fixes

* 2 bugs fixed (they concern **data_norm** and **unit** functions)
* corrected descriptions (e.g. **model_classification** function)


# Package update 0.0.2 -> 0.0.3

## Bug fixes

* Function **data_norm** (which is crucial for product unit standarization) has beed corrected 
(the previous version of that function duplicated rows in data frame)


# Package update 0.0.1 -> 0.0.2

## Improvements

* Function **data_matching()** has been modified and now it is much more faster,

* Examples concerning **chain indices** have been modified and now they are less time-consuming while checking,

* New data set is added (**dataU**)

* New functions for scanner data processing are added (**data_unit** and **data_norm**)

## Bug fixes

* Access to HASP and WISP methods has been added in the **price_index()** function and now **price_indices()** works correctly,

* The default value and the meaning of the **sensitivity** parameter in **data_preparing()** and **model_classification()** functions have been corrected.
