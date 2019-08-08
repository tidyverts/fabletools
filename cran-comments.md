## Test environments
* local ubuntu 18.04 install, R 3.5.3, R 3.6.0
* ubuntu 14.04 (on travis-ci), R-devel, R 3.6.0, R 3.5.3
* macOS 10.13 (on travis-ci), R 3.6.1
* windows server 2012 R2 (on AppVeyor), R 3.6.1

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Comments

This is a joint submission with fable and feasts. These packages are
interdependent in their examples and functionality, so all packages will need
to be installed prior to checking the packages.

fabletools should be installed first, and then fable and feasts.

Unfortunately due to this dependency, the packages could not be tested using 
win-builder. Instead these packages have been checked on Windows using AppVeyor.

Apologies for the added complexity in reviewing this submission.

## Re-submission

* Fixed URL in description.
* Removed VignetteBuilder field from DESCRIPTION.
* Conditionally require suggested packages in examples and tests.
* Fix registration of guide_train.level_guide to ggplot2::guide_train.
* Sped up examples to be <10s on win-builder.
* Added Additional_repositories for suggested packages not yet on CRAN.
* Fixed package title's title case.

> If there are references describing the (theoretical background of)
methods in your package, please add these in the Description field of
your DESCRIPTION file in the form

There are no published references for the package's methods yet.