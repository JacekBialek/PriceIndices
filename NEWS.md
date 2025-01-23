# Package update 0.2.2 -> 0.2.3

## New functions: 
* New function for multiplicative decomposing the GEKS-type indices (*m_decomposition*)

## Improvements:
* The **data_selecting** function is now much faster.
* The **gk** function is now much faster.

# Package update 0.2.1 -> 0.2.2

## Bag fixed
* Splicing methods on published indices (e.g. HASP and WISP) have been corrected for the case when **interval=TRUE**

## Improvements:
* The references concerning indicators have been updated.

# Package update 0.2.0 -> 0.2.1

## Improvements: 
* All splice indices (e.g. geks_splice(), gk_splice(), ccdi_splice() or tpd_splice()) are now much faster when the **interval** parameter is FALSE.

## Bag fixed
* A parameter **prec** in the **shrinkflation** function now works fine.

# Package update 0.1.9 -> 0.2.0

## New functions:
* There are two new index functions: **dikhanov** (bilateral formula) and **chdikhanov** (chain index formula).

## New dataset:
* The **data_DOWN_UP_SIZED** (artficial) scanner dataset on the sale of coffee products (the corresponding old one, i.e. **dataDOWNSIZED**, has been deleted);

## Improvements:
* The **shrinkflation** function has been extended (new parameters, new types of downsizing/upsizing);

## Bag fixed
* The *compare_to_target** function has been fixed and now it allows for comparison of only two indices.

# Package update 0.1.7 -> 0.1.8

## Improvements: 
* New references to some of multilateral methods have been added (e.g. GEKS-AGI, GEKS-AQU methods)
* The **final index** function has been improved. Now it skips data frames with zero rows.

# Package update 0.1.6 -> 0.1.7

## New functions: 
* New functions for the calculation of the Montgomery indicators, i.e. **montgomery** and **mmontgomery** for the bilateral and multilateral variant, respectively).

## Improvements: 
* The **compare_indices_jk** function obtained new functionality (there are new elements in the resulting list for comparing pseudovalues from jackknife method).


# Package update 0.1.5 -> 0.1.6

## Improvements: 
* The **elasticity** and thus **elasticity_fig** are much faster now.

## Bug fixes: 
* An example in **elasticity_fig** has been corrected (there is no error now). The *price_indices* function has been corrected due to the *lloyd-moulton* sub-function.

# Package update 0.1.4 -> 0.1.5

## New functions: 
* New functions for price imputations (*data_imputing*) and for detecting new and disappearing products (*products* and *products_fig*).

## Improvements: 
* The **data_preparing** function has now two additional parameters which allow to consider zero prices and zero quantities.
* New methods for **elasticity** and **elasticity_fig** functions have been added.

## Bug fixes: 
* a new parameter has beed added to the function 'price_indices' (*names*). It allows to compare e.g. the same splice multilateral index but for many different splicing methods (their names can be distinguished).
* The *geksj* function has been corrected for the case when we observe large number of decreasing prices.

# Package update 0.1.3 -> 0.1.4

## New functions
* New functions for bennet price and quantity indicators have been added (*bennet* and *mbennet*).
* New function for comparison of price indices by using the jackknife method has been added (*compare_indices_jk*).

## Bug fixes
* A new parameter has beed added to the function 'price_indices' (*names*). It allows to compare e.g. the same splice multilateral index but for many different splicing methods (their names can be distinguished).
* The *geksj* function has been corrected for the case when we observe large number of decreasing prices.
* Problems with examples have been deleted ('donttest{}' formula is used)

# Package update 0.1.2 -> 0.1.3

## Bug fixes

* Graphical result of the **compare_indices_list** function has been corrected 
(a label concerning the X-axis is now named 'date')
* References have been updated in the case of the *generate* function.
* A wrong example concerning the *geksgaqi_splice* function has been deleted.

# Package update 0.1.1 -> 0.1.2

## New functions:

* New functions added: a) bilateral methods - *value_index*, *unit_value_index*, *QMp*, *IQMp*, *QMq*; b) chain indices: *chQMp*, *chIQMp*, *chQMq*; c) multilateral methods: *gekslm*, *geksqm*,*geksiqm*, *utpd* + their extensions (splice, FMW, FBEW). New function for generating artficial scanner datasets (*generate_CES*). Moreover: new (faster) function *price_indices*, new functions for price comparisons: *compare_indices_df* and *compare_indices_list* (old functions: *compare_indices* and *compare_final_indices* were removed). Rebuilt function *final_index* (old function *final_index2* was removed).
Rebuilt functions: *elasticity* and *elasticity_fig* (old functions *elasticity2* and *elasticity_fig2* were removed).

## Improvements
* Improved speed of **data_filtering** and **data_matching** functions (**data_matching** is now based on the *reclin2* package instead of *reclin*).
* Improved speed of multilateral index functions: **geks**, **wgeks**, **geksj**, **geksw**, **geksl**, **geksgl**, **wgeksl** and **wgeksgl**.

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
