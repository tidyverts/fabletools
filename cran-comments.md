This submission resolves issues on the CRAN check page.

## Test environments
* local kubuntu 20.04 install, R 4.0.2
* ubuntu 16.04 (on GitHub actions), R-devel, R 4.0.0, R 3.6.3, R 3.5.3
* macOS (on GitHub actions), R 4.0.0
* windows (on GitHub actions), R 4.0.0
* win-builder, R-devel, R-release, R-oldrelease

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdep checks

All revdeps have been checked. No changes to worse were found.

## Resubmission

Fixed undetected reverse dependency issues with the {fable} package. Fixed
subsequent reverse dependency issues introduced to {fable.prophet} when fixing
earlier reverse dependency issue. Reverse dependencies have been checked again
and no changes to worse have been found.