
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://travis-ci.org/r-spatial/sftime.png?branch=master)](https://travis-ci.org/r-spatial/sftime)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/r-spatial/sftime?branch=master&svg=true)](https://ci.appveyor.com/project/edzerpebesma/sftime)
[![codecov](https://codecov.io/gh/r-spatial/sftime/branch/master/graph/badge.svg)](https://codecov.io/gh/r-spatial/sftime)
[![CRAN](https://www.r-pkg.org/badges/version/sftime)](https://cran.r-project.org/package=sftime)
[![cran
checks](https://cranchecks.info/badges/worst/sftime)](https://cran.r-project.org/web/checks/check_results_sftime.html)

# sftime

`sftime` provides time extension for simple features in R. `sftime` is
an extension to the [`sf`](https://github.com/r-spatial/sf) package. It
allows to store spatial features which are accompanied by time
information, similar to the
[`stars`](https://github.com/r-spatial/stars/) package.

`sftime` is a complement to the `stars` package: Whereas `stars` is
dedicated to handle regular spatiotemporal data, where space and time
represent array dimensions of data cubes, `sftime` provides a generic
data format which can also handle irregular spatiotemporal data.

Examples for such data include earthquakes, accidents, disease or death
cases, lightning strikes, but also movement data which have further
constraints.

## Installation

You can install the CRAN version of the package with:

``` r
install.packages("sftime")
```

You can install the development version of `sftime` from
[GitHub](https://github.com/) with:

``` r
library(remotes)
install_github("r-spatial/sftime")
```

## Contributing

  - Contributions of all sorts are most welcome, issues and pull
    requests are the preferred ways of sharing them.
  - Please note that the sftime project is released with a [Contributor
    Code of
    Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
    By contributing to this project, you agree to abide by its terms.

## Acknowledgment

This project gratefully acknowledges financial
[support](https://www.r-consortium.org/projects) from the

<a href="https://www.r-consortium.org/projects/awarded-projects">
<img src="https://www.r-consortium.org/wp-content/uploads/sites/13/2016/09/RConsortium_Horizontal_Pantone.png" width="300">
</a>
