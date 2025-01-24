# Package update 

## Resubmission
This is a resubmission. Sending the package update yesterday, I received 1 note that the examples in the functions **data_norm** and **shrinkflation** are too time-consuming (I had no such comments before, on my computer also 'check' did not give such a message). Anyway, I corrected it using 'donttest' in the current package version. Thank you for this notification!

## New functions: 
* New function for multiplicative decomposing the GEKS-type indices (*m_decomposition*)

## Improvements:
* The **data_selecting** function is now much faster.
* The **gk** function is now much faster.

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


