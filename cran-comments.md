## Test environments (fedregs version 1.0.0)
* local Windows 7 x64 build 7601 SP1, R version 3.6.1 (2019-07-05)
* r_hub() Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* r_hub()Ubuntu Linux 16.04 LTS, R-release, GCC
* r_hub()Fedora Linux, R-devel, clang, gfortran
* ubuntu 16.04.6 (on travis-ci), R 3.6.1
* win-builder (devel)

## R CMD check results
The previous version failed because of a breaking change with tidyr::nest and tidyr::unnest (tidyr >= 1.0.0). The code was updated to function as expected with these new changes.
There were no ERRORs, WARNINGS, or NOTEs in R 3.6.1 or R-devel on the first 5 systems.

On win-builder (devel) there was 1 ERROR
Package required and available but unsuitable version: 'tidyr'
This update to fedregs is in response to the update to tidyr, so this should be expected, and I am depending on the improved version.

## Corrections/Comments:

## Test environments (fedregs version 0.1.1)
* local Windows 7 x64 build 7601 SP1, R 3.5.0
* ubuntu 14.04.05 (on travis-ci), R 3.5.0

## R CMD check results
The previous version failed because of a breaking change in the Code of Federal Regulations API. The US Federal Government was shutdown and the maintainer was not able to update code before the package was removed from CRAN. The current CFR API should be stable for the foreseeable future.
There were no ERRORs, WARNINGS, or NOTEs.

## Corrections/Comments:



## Test environments (fedregs version 0.1.0)
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
