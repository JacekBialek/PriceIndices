## Package update 

* New function:
* a new internal function has been added (*conversion*). This function provides integer representation of the given text string.

* Improvements:
* the following functions have been improved by changing a method for dealing with categorical variables: *model_classification*, *data_classifying*.
* An existing artificial data set (dataCOICOP) has been change into the real scanner data set
* Some new references have been added (e.g. for *GEKS-L*, *GEKS-GL*, *GEKS-AQI*, *GEKS-AQU* functions).
* A README file has been improved by adding some new example with using ML methods for product classifiction.

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


