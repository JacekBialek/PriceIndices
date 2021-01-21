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

0 errors √ | 0 warnings √ | 1 note x

* This is a new release.

## Downstream dependencies

I have also run R CMD check on downstream dependencies of httr 
(https://github.com/wch/checkresults/blob/master/httr/r-release). 
All packages that I could install passed.

## I run the following instruction successfully:

* devtools::check() -> 0 errors √ | 0 warnings √ | 0 notes √

* check_win_devel()

Your package PriceIndices_0.0.0.9000.tar.gz has been built (if working) and checked for Windows.
Please check the log files and (if working) the binary package at:
https://win-builder.r-project.org/n58tBx2h3KvO
The files will be removed after roughly 72 hours.
Installation time in seconds: 17
Check time in seconds: 113
Status: 1 NOTE ("New submission")


* devtools::spell_check() -> All notes were verified.

## Thank you very much! 


