# Package update 

## New functions:
* The following new functions for bilateral price index calculations have been added: *ybmd* (the Young-Balk-Mehrhoff-Dikhanov index), 
*geowalsh* (the geometric Walsh index), *theil1* (the Theil I index), *theil2* (the Theil II index), *walsh_vartia* (the Walsh-Vartia index),
*hlc* (the harmonic log-change index) + their chain versions, i.e., *chybmd*, *chgeowalsh*, *chtheil1*, *chtheil2*, *chwalsh_vartia*, and *chhlc*.

## Improvements
* Functions *price_indices* and *final_index* have been extended to take into consideration all these new index formulas.. 

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


