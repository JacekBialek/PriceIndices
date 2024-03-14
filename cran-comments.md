## Package update 

## New functions:
* The **data_reducing** function for eliminating product groups with small number of matched products;
* The **shrinkflation** function for detecting downsized products;

## New dataset:
* The **dataDOWNSIZED** (artficial) scanner dataset on the sale of coffee products;

## Improvements: 
* The **data_norm** function is much faster;
* The **data_unit** has been corrected and now it works better. Its parameter **units** has now a new form (see the package documentation);
* The functions **data_classifying** and **model_classification** have been changed with respect to the returned columns. In the current version, the user may indicate the column with classes for training of the model. As a consequence, after product classification we obtain a new column named "class_predicted";
* A meaning of the parameter 'prec' in functions for indicators (**bennnet**, **montgomery**, **mbennet** and **mmontgomery**) has been improved. 

## Test environments
* local OS (Windows 10 Home) install, R 3.6.3
* Ubuntu Linux 16.04 LTS, R-release, GCC
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* Fedora Linux, R-devel, clang, gfortra

## R CMD check results

0 errors √ | 0 warnings √ | 0 notes √ 

## Downstream dependencies

I have also run R CMD check on downstream dependencies of httr 
(https://github.com/wch/checkresults/blob/master/httr/r-release). 
All packages that I could install passed.

## I run the following instruction successfully:

* devtools::check() -> 0 errors √ | 0 warnings √ | 0 notes √

* check_win_devel() -> -> 0 errors √ | 0 warnings √ | 0 notes √

* devtools::spell_check() -> Ok, done.

## Thank you very much! 


