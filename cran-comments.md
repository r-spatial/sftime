This release should fix CRAN check notes required to be fixed before 2024-10-04.

To this end, we removed the old `tests` directory which has no purpose any more 
and caused the check note.

In addition, this submission adds some new functions (`tidyr::drop_na()` method, `dplyr::dplyr_reconstruct()` method, methods to convert objects of other classes to `sftime` objects) and fixes a minor bug in `st_time<-.sftime`.


## R CMD check results

0 errors | 0 warnings | 0 notes


## revdepcheck results

We checked 6 reverse dependencies (5 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
