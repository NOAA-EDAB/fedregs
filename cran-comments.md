## Test environments
* local Windows 7 x64 build 7601 SP1, R 3.5.0
* ubuntu 14.04.05 (on travis-ci), R 3.5.0

## R CMD check results
This is the first submission of 'fedregs'.
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There are no downstream dependencies.

## Corrections/Comments:

1) pls omit the redundant " A Package to Facilitate". *DONE*
2) Is there some reference about the method you can add in the Description field in the form Authors (year) <doi:.....>? *NO*
3) We see all your examples are wrapped in \dontrun{}, hence nothing get tested. But then you always need to download data, so this part is probably to be accepted as is. *This version of the package will need to download the data for each step, so the \dontrun{} is to keep the speed within CRAN checks.*
