# Changelog

## sftime (development version)

- Switch from the magrittr pipe (`%>%`) to the R-native pipe (`|>`) in
  examples and vignettes
  ([\#15](https://github.com/r-spatial/sftime/issues/15)).

## sftime 0.3.1

CRAN release: 2025-08-19

- Correct argument `versionCheck` the `requireNamespace` for the
  `cubble` package in
  [`st_as_sftime.cubble_df()`](https://r-spatial.github.io/sftime/reference/st_as_sftime.md).

## sftime 0.3.0

CRAN release: 2024-09-11

- Add a dedicated
  [`tidyr::drop_na()`](https://tidyr.tidyverse.org/reference/drop_na.html)
  method for `sftime` objects. (See the same recent addition for `sf`
  objects [\#1975](https://github.com/r-spatial/sf/pull/1975/)).

- Add a dedicated
  [`dplyr::dplyr_reconstruct()`](https://dplyr.tidyverse.org/reference/dplyr_extending.html)
  method for `sftime` objects. Relying on the method for `sf` objects
  caused erroneously column binding when the second object was a data
  frame without conflicting column names for the `sf` and time columns.
  In this case, a `sf` objects was returned, even though an `sftime`
  object should be returned. See also
  <https://github.com/r-spatial/sf/issues/1958#issuecomment-1181982244>.

- Add methods to convert `sftime` objects from:

  - Objects from the `spatstat` package classes (`ppp`, `psp`, `lpp`)
  - `sftrack` and `sftraj` objects from the `sftrack` package.
  - `cubble_df` objects from the `cubble` package.

- Bug fix in `st_time<-.sftime`:

  - Still contained references to the old `tc`class.
  - Did not allow to give the active time column a character vector as
    value.

## version 0.2-0

CRAN release: 2022-03-17

- initial CRAN submission
