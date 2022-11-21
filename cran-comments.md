## Package update 

* New functions added: a) bilateral methods - *value_index*, *unit_value_index*, *QMp*, *IQMp*, *QMq*; b) chain indices: *chQMp*, *chIQMp*, *chQMq*; c) multilateral methods: *gekslm*, *geksqm*, *geksiqm*, *utpd* + their extensions (splice, FMW, FBEW). Moreover: new (faster) function *price_indices*, new functions for price comparisons: *compare_indices_df* and *compare_indices_list* (old functions: *compare_indices* and *compare_final_indices* were removed). Rebuilt function *final_index* (old function *final_index2* was removed).
* Improved speed of **data_filtering** and **data_matching** functions (**data_matching** is now based on the *reclin2* package instead of *reclin*).
* Improved speed of multilateral index functions: **geks**, **wgeks**, **geksj**, **geksw**, **geksl**, **geksgl**, **wgeksl** and **wgeksgl**.

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


