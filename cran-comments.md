## Package update 

## New functions:

## New functions:
* There are two new index functions: **dikhanov** (bilateral formula) and **chdikhanov** (chain index formula).

## New dataset:
* The **data_DOWN_UP_SIZED** (artficial) scanner dataset on the sale of coffee products (the corresponding old one, i.e. **dataDOWNSIZED**, has been deleted);

## Improvements:
* The **shrinkflation** function has been extended (new parameters, new types of downsizing/upsizing); 

## Bag fixed
* The *compare_to_target** function has been fixed and now it allows for comparison of only two indices.

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


