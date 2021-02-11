## Package update 
* 2 Improvements
* 2 Bug fixes

## Test environments
* local OS (Windows 10 Home) install, R 3.6.3
* Ubuntu Linux 16.04 LTS, R-release, GCC
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* Fedora Linux, R-devel, clang, gfortra

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  
   New submission
   Maintainer: 'Jacek Bialek <jacek.bialek@uni.lodz.pl>'
  
   Possibly mis-spelled words in DESCRIPTION:
   
   Diewert (10:282)
   Haan (10:228)
   Krsinich (10:237)
   de (10:225)
    
0 errors √ | 0 warnings √ | 1 note x

* This is a new release. The misspelled words in DESCRIPTION are actually names, which I have checked. 

## Downstream dependencies

I have also run R CMD check on downstream dependencies of httr 
(https://github.com/wch/checkresults/blob/master/httr/r-release). 
All packages that I could install passed.

## I run the following instruction successfully:

* devtools::check() -> 0 errors √ | 0 warnings √ | 0 notes √

* check_win_devel()

Your package PriceIndices_0.0.2.tar.gz has been built (if working) and checked for Windows.
Please check the log files and (if working) the binary package at:
https://win-builder.r-project.org/0PL6nmTTa16c
The files will be removed after roughly 72 hours.
Installation time in seconds: 15
Check time in seconds: 137
Status: 1 NOTE


* devtools::spell_check() -> All notes were verified.

## Thank you very much! 


