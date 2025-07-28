# Package update 

## New functions: 
* New function for data stratification via the MARS method (*MARS*)

## New data sets:
* New artificial scanner data set for testing the MARS method (*dataMARS*)

## Improvements:
* New parameter (description) has been added to the *data_aggregating* function
* New imputation methods (overall mean, class mean) have been added to the *data_imputing* function.
* New index methods (GEKS-AQI, GEKS-GAQI) have been added to the *m_decomposition* function


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


