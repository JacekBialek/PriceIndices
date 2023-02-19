## Package update 

* New functions for price imputations (*data_imputing*) and for detecting new and disappearing products (*products* and *products_figure*).
* Improvement: the *data_preparing* function has now two additional parameters which allow to consider zero prices and zero quantities.
* New methods for **elasticity** and **elasticity_fig** functions have beelibraryn added.

* Bug fixes: a new parameter has beed added to the function 'price_indices' (*names*). It allows to compare e.g. the same splice multilateral index but for many different splicing methods (their names can be distinguished).
* The *geksj* function has been corrected for the case when we observe large number of decreasing prices.
* Problems with examples have been deleted ('donttest{}' formula is used)

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


