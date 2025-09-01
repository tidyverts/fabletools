This release ensures compatibility with the upcoming ggplot2 v4.0.0 release.

## Test environments
* local ubuntu 24.04 install, R 4.4.1
* ubuntu-latest (on GitHub actions), R 4.4.1, R 4.3.3, R 4.2.3
* macOS-latest (on GitHub actions), R-devel, R 4.4.1
* windows-latest (on GitHub actions), R 4.4.1
* win-builder, R-devel, R-release, R-oldrelease

## R CMD check results

0 errors | 0 warnings | 0 notes

## Revdep checks

All reverse dependencies have been checked, none have changed to worse.

Please note that the check process is likely to fail for the fpp3 package, since it has a circular dependency on fabletools via its dependency on fable. This causes fabletools to be unable to be cleanly unloaded during the revdep checks. The fpp3 package has been separately checked in isolated test environments, and there have been no changes to worse. I have requested that the maintainer of fpp3 remove the fabletools import to remove this circular dependency problem.