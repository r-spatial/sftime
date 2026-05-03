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

- Contributions of all sorts are most welcome, issues and pull requests
  are the preferred ways of sharing them.
- Please note that the sftime project is released with a [Contributor
  Code of
  Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
  By contributing to this project, you agree to abide by its terms.

## Acknowledgment

This project gratefully acknowledges financial
[support](https://r-consortium.org/all-projects/2020-group-1.html#spatiotemporal-data-and-analytics)
from the

[![](https://r-consortium.org/images/RConsortium_Horizontal_Pantone.webp)](https://r-consortium.org/all-projects/2020-group-1.html#spatiotemporal-data-and-analytics)
