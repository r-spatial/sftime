# 'tidyverse' methods for `sftime` objects

'tidyverse' methods for `sftime` objects. Geometries are sticky, use
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html) to let
`dplyr`'s own methods drop them. Use these methods without the `.sftime`
suffix and after loading the 'tidyverse' package with the generic (or
after loading package 'tidyverse').

## Usage

``` r
inner_join.sftime(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

left_join.sftime(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

right_join.sftime(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

full_join.sftime(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

semi_join.sftime(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

anti_join.sftime(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

filter.sftime(.data, ..., .dots)

arrange.sftime(.data, ..., .dots)

group_by.sftime(.data, ..., add = FALSE)

ungroup.sftime(.data, ...)

rowwise.sftime(.data, ...)

mutate.sftime(.data, ..., .dots)

transmute.sftime(.data, ..., .dots)

select.sftime(.data, ...)

rename.sftime(.data, ...)

slice.sftime(.data, ..., .dots)

summarise.sftime(.data, ..., .dots, do_union = TRUE, is_coverage = FALSE)

summarize.sftime(.data, ..., .dots, do_union = TRUE, is_coverage = FALSE)

distinct.sftime(.data, ..., .keep_all = FALSE)

gather.sftime(
  data,
  key,
  value,
  ...,
  na.rm = FALSE,
  convert = FALSE,
  factor_key = FALSE
)

pivot_longer.sftime(
  data,
  cols,
  names_to = "name",
  names_prefix = NULL,
  names_sep = NULL,
  names_pattern = NULL,
  names_ptypes = NULL,
  names_transform = NULL,
  names_repair = "check_unique",
  values_to = "value",
  values_drop_na = FALSE,
  values_ptypes = NULL,
  values_transform = NULL,
  ...
)

spread.sftime(
  data,
  key,
  value,
  fill = NA,
  convert = FALSE,
  drop = TRUE,
  sep = NULL
)

sample_n.sftime(
  tbl,
  size,
  replace = FALSE,
  weight = NULL,
  .env = parent.frame()
)

sample_frac.sftime(
  tbl,
  size = 1,
  replace = FALSE,
  weight = NULL,
  .env = parent.frame()
)

nest.sftime(.data, ...)

unnest.sftime(data, ..., .preserve = NULL)

separate.sftime(
  data,
  col,
  into,
  sep = "[^[:alnum:]]+",
  remove = TRUE,
  convert = FALSE,
  extra = "warn",
  fill = "warn",
  ...
)

unite.sftime(data, col, ..., sep = "_", remove = TRUE)

separate_rows.sftime(data, ..., sep = "[^[:alnum:]]+", convert = FALSE)

drop_na.sftime(data, ...)
```

## Arguments

- x:

  An object of class `sftime`.

- y:

  See
  [`` dplyr::`mutate-joins` ``](https://dplyr.tidyverse.org/reference/mutate-joins.html).

- by:

  A join specification created with
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html), or
  a character vector of variables to join by.

  If `NULL`, the default, `*_join()` will perform a natural join, using
  all variables in common across `x` and `y`. A message lists the
  variables so that you can check they're correct; suppress the message
  by supplying `by` explicitly.

  To join on different variables between `x` and `y`, use a
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
  specification. For example, `join_by(a == b)` will match `x$a` to
  `y$b`.

  To join by multiple variables, use a
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
  specification with multiple expressions. For example,
  `join_by(a == b, c == d)` will match `x$a` to `y$b` and `x$c` to
  `y$d`. If the column names are the same between `x` and `y`, you can
  shorten this by listing only the variable names, like `join_by(a, c)`.

  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html) can
  also be used to perform inequality, rolling, and overlap joins. See
  the documentation at
  [?join_by](https://dplyr.tidyverse.org/reference/join_by.html) for
  details on these types of joins.

  For simple equality joins, you can alternatively specify a character
  vector of variable names to join by. For example, `by = c("a", "b")`
  joins `x$a` to `y$a` and `x$b` to `y$b`. If variable names differ
  between `x` and `y`, use a named character vector like
  `by = c("x_a" = "y_a", "x_b" = "y_b")`.

  To perform a cross-join, generating all combinations of `x` and `y`,
  see
  [`cross_join()`](https://dplyr.tidyverse.org/reference/cross_join.html).

- copy:

  If `x` and `y` are not from the same data source, and `copy` is
  `TRUE`, then `y` will be copied into the same src as `x`. This allows
  you to join tables across srcs, but it is a potentially expensive
  operation so you must opt into it.

- suffix:

  If there are non-joined duplicate variables in `x` and `y`, these
  suffixes will be added to the output to disambiguate them. Should be a
  character vector of length 2.

- ...:

  other arguments

- .data:

  An object of class `stime`.

- .dots:

  see corresponding function in package `dplyr`

- add:

  see corresponding function in dplyr

- do_union:

  logical; in case `summary` does not create a geometry column, should
  geometries be created by unioning using
  [st_union](https://r-spatial.github.io/sf/reference/geos_combine.html),
  or simply by combining using
  [st_combine](https://r-spatial.github.io/sf/reference/geos_combine.html)?
  Using
  [st_union](https://r-spatial.github.io/sf/reference/geos_combine.html)
  resolves internal boundaries, but in case of unioning points, this
  will likely change the order of the points; see Details.

- is_coverage:

  logical; if `do_union` is `TRUE`, use an optimized algorithm for
  features that form a polygonal coverage (have no overlaps)

- .keep_all:

  see corresponding function in dplyr

- data:

  data object of class
  [sf](https://r-spatial.github.io/sf/reference/sf.html)

- key:

  see original function docs

- value:

  see original function docs

- na.rm:

  see original function docs

- convert:

  see
  [separate_rows](https://tidyr.tidyverse.org/reference/separate_rows.html)

- factor_key:

  see original function docs

- cols:

  see original function docs

- names_to, names_pattern, names_ptypes, names_transform:

  see
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)

- names_prefix, names_sep, names_repair:

  see original function docs.

- values_to, values_drop_na, values_ptypes, values_transform:

  See
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)

- fill:

  see original function docs

- drop:

  see original function docs

- sep:

  see
  [separate_rows](https://tidyr.tidyverse.org/reference/separate_rows.html)

- tbl:

  see original function docs

- size:

  see original function docs

- replace:

  see original function docs

- weight:

  see original function docs

- .env:

  see original function docs

- .preserve:

  see [unnest](https://tidyr.tidyverse.org/reference/nest.html)

- col:

  see [separate](https://tidyr.tidyverse.org/reference/separate.html)

- into:

  see [separate](https://tidyr.tidyverse.org/reference/separate.html)

- remove:

  see [separate](https://tidyr.tidyverse.org/reference/separate.html)

- extra:

  see [separate](https://tidyr.tidyverse.org/reference/separate.html)

## Value

- For `_join` methods: An object of class `sftime` representing the
  joining result of `x` and `y`. See
  [`mutate-joins`](https://dplyr.tidyverse.org/reference/mutate-joins.html).

- For `filter`: See
  [`filter`](https://dplyr.tidyverse.org/reference/filter.html).

- For `arrange`: See
  [`arrange`](https://dplyr.tidyverse.org/reference/arrange.html).

- For `group_by` and `ungroup`: A grouped `sftime` object. See
  [`arrange`](https://dplyr.tidyverse.org/reference/arrange.html).

- For `rowwise`: An `sftime` object. See
  [`rowwise`](https://dplyr.tidyverse.org/reference/rowwise.html).

- For `mutate` and `transmute`: See
  [`mutate`](https://dplyr.tidyverse.org/reference/mutate.html).

- For `select`: See
  [`select`](https://dplyr.tidyverse.org/reference/select.html). If the
  active time column is not explicitly selected, a `sf` object is
  returned.

- For `rename`: See
  [`rename`](https://dplyr.tidyverse.org/reference/rename.html).

- For `slice`: See
  [`slice`](https://dplyr.tidyverse.org/reference/slice.html).

- For `summarize` and `summarise`: See
  [`summarise`](https://dplyr.tidyverse.org/reference/summarise.html).

- For `distinct`: See
  [`distinct`](https://dplyr.tidyverse.org/reference/distinct.html).

- For `gather`: See
  [`gather`](https://tidyr.tidyverse.org/reference/gather.html).

## Examples

``` r
g1 <- st_sfc(st_point(1:2), st_point(c(5, 8)), st_point(c(2, 9)))
x1 <- st_sftime(a = 1:3, geometry = g1, time = Sys.time())

g2 <- st_sfc(st_point(c(4, 6)), st_point(c(4, 6)), st_point(c(4, 6)))
x2 <- st_sftime(a = 2:4, geometry = g2, time = Sys.time())

library(dplyr)

## inner_join
inner_join(x1, as.data.frame(x2), by = "a") # note: the active time column is
#> Spatiotemporal feature collection with 2 features and 2 fields
#> Active geometry column: geometry.x
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 2 ymin: 8 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#>   a              time.y  geometry.x  geometry.y              time.x
#> 1 2 2026-05-03 09:57:55 POINT (5 8) POINT (4 6) 2026-05-03 09:57:55
#> 2 3 2026-05-03 09:57:55 POINT (2 9) POINT (4 6) 2026-05-03 09:57:55
# time.x and the active geometry column geometry.x

inner_join(x2, as.data.frame(x1), by = "a")
#> Spatiotemporal feature collection with 2 features and 2 fields
#> Active geometry column: geometry.x
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 4 ymin: 6 xmax: 4 ymax: 6
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.607153 to 2026-05-03 09:57:55.607153.
#>   a              time.y  geometry.x  geometry.y              time.x
#> 1 2 2026-05-03 09:57:55 POINT (4 6) POINT (5 8) 2026-05-03 09:57:55
#> 2 3 2026-05-03 09:57:55 POINT (4 6) POINT (2 9) 2026-05-03 09:57:55

## left_join
left_join(x1, as.data.frame(x2), by = "a")
#> Spatiotemporal feature collection with 3 features and 2 fields
#> Active geometry column: geometry.x
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#>   a              time.y  geometry.x  geometry.y              time.x
#> 1 1                <NA> POINT (1 2) POINT EMPTY 2026-05-03 09:57:55
#> 2 2 2026-05-03 09:57:55 POINT (5 8) POINT (4 6) 2026-05-03 09:57:55
#> 3 3 2026-05-03 09:57:55 POINT (2 9) POINT (4 6) 2026-05-03 09:57:55

left_join(x2, as.data.frame(x1), by = "a")
#> Spatiotemporal feature collection with 3 features and 2 fields
#> Active geometry column: geometry.x
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 4 ymin: 6 xmax: 4 ymax: 6
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.607153 to 2026-05-03 09:57:55.607153.
#>   a              time.y  geometry.x  geometry.y              time.x
#> 1 2 2026-05-03 09:57:55 POINT (4 6) POINT (5 8) 2026-05-03 09:57:55
#> 2 3 2026-05-03 09:57:55 POINT (4 6) POINT (2 9) 2026-05-03 09:57:55
#> 3 4                <NA> POINT (4 6) POINT EMPTY 2026-05-03 09:57:55

## right_join
right_join(x1, as.data.frame(x2), by = "a")
#> Spatiotemporal feature collection with 3 features and 2 fields
#> Active geometry column: geometry.x (with 1 geometry empty)
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 2 ymin: 8 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#>   a              time.y  geometry.x  geometry.y              time.x
#> 1 2 2026-05-03 09:57:55 POINT (5 8) POINT (4 6) 2026-05-03 09:57:55
#> 2 3 2026-05-03 09:57:55 POINT (2 9) POINT (4 6) 2026-05-03 09:57:55
#> 3 4 2026-05-03 09:57:55 POINT EMPTY POINT (4 6)                <NA>

right_join(x2, as.data.frame(x1), by = "a")
#> Spatiotemporal feature collection with 3 features and 2 fields
#> Active geometry column: geometry.x (with 1 geometry empty)
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 4 ymin: 6 xmax: 4 ymax: 6
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.607153 to 2026-05-03 09:57:55.607153.
#>   a              time.y  geometry.x  geometry.y              time.x
#> 1 2 2026-05-03 09:57:55 POINT (4 6) POINT (5 8) 2026-05-03 09:57:55
#> 2 3 2026-05-03 09:57:55 POINT (4 6) POINT (2 9) 2026-05-03 09:57:55
#> 3 1 2026-05-03 09:57:55 POINT EMPTY POINT (1 2)                <NA>

## full_join
full_join(x1, as.data.frame(x2), by = "a")
#> Spatiotemporal feature collection with 4 features and 2 fields
#> Active geometry column: geometry.x (with 1 geometry empty)
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#>   a              time.y  geometry.x  geometry.y              time.x
#> 1 1                <NA> POINT (1 2) POINT EMPTY 2026-05-03 09:57:55
#> 2 2 2026-05-03 09:57:55 POINT (5 8) POINT (4 6) 2026-05-03 09:57:55
#> 3 3 2026-05-03 09:57:55 POINT (2 9) POINT (4 6) 2026-05-03 09:57:55
#> 4 4 2026-05-03 09:57:55 POINT EMPTY POINT (4 6)                <NA>

full_join(x2, as.data.frame(x1), by = "a")
#> Spatiotemporal feature collection with 4 features and 2 fields
#> Active geometry column: geometry.x (with 1 geometry empty)
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 4 ymin: 6 xmax: 4 ymax: 6
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.607153 to 2026-05-03 09:57:55.607153.
#>   a              time.y  geometry.x  geometry.y              time.x
#> 1 2 2026-05-03 09:57:55 POINT (4 6) POINT (5 8) 2026-05-03 09:57:55
#> 2 3 2026-05-03 09:57:55 POINT (4 6) POINT (2 9) 2026-05-03 09:57:55
#> 3 4                <NA> POINT (4 6) POINT EMPTY 2026-05-03 09:57:55
#> 4 1 2026-05-03 09:57:55 POINT EMPTY POINT (1 2)                <NA>

## semi_join
semi_join(x1, as.data.frame(x2), by = "a")
#> Spatiotemporal feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 2 ymin: 8 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#>   a    geometry                time
#> 1 2 POINT (5 8) 2026-05-03 09:57:55
#> 2 3 POINT (2 9) 2026-05-03 09:57:55

semi_join(x2, as.data.frame(x1), by = "a")
#> Spatiotemporal feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 4 ymin: 6 xmax: 4 ymax: 6
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.607153 to 2026-05-03 09:57:55.607153.
#>   a    geometry                time
#> 1 2 POINT (4 6) 2026-05-03 09:57:55
#> 2 3 POINT (4 6) 2026-05-03 09:57:55

## anti_join
anti_join(x1, as.data.frame(x2), by = "a")
#> Spatiotemporal feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Representing 2026-05-03 09:57:55.604221.
#>   a    geometry                time
#> 1 1 POINT (1 2) 2026-05-03 09:57:55

anti_join(x2, as.data.frame(x1), by = "a")
#> Spatiotemporal feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 4 ymin: 6 xmax: 4 ymax: 6
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Representing 2026-05-03 09:57:55.607153.
#>   a    geometry                time
#> 1 4 POINT (4 6) 2026-05-03 09:57:55

## filter
filter(x1, a <= 2)
#> Spatiotemporal feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 8
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#>   a    geometry                time
#> 1 1 POINT (1 2) 2026-05-03 09:57:55
#> 2 2 POINT (5 8) 2026-05-03 09:57:55

## arrange
arrange(x1, dplyr::desc(a))
#> Spatiotemporal feature collection with 3 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#>   a                time    geometry
#> 1 3 2026-05-03 09:57:55 POINT (2 9)
#> 2 2 2026-05-03 09:57:55 POINT (5 8)
#> 3 1 2026-05-03 09:57:55 POINT (1 2)

## group_by
group_by(x1, time)
#> Spatiotemporal feature collection with 3 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#> # A tibble: 3 × 3
#> # Groups:   time [1]
#>       a geometry time               
#> * <int>  <POINT> <dttm>             
#> 1     1    (1 2) 2026-05-03 09:57:55
#> 2     2    (5 8) 2026-05-03 09:57:55
#> 3     3    (2 9) 2026-05-03 09:57:55

## ungroup
ungroup(group_by(x1, time))
#> Spatiotemporal feature collection with 3 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#> # A tibble: 3 × 3
#>       a geometry time               
#> * <int>  <POINT> <dttm>             
#> 1     1    (1 2) 2026-05-03 09:57:55
#> 2     2    (5 8) 2026-05-03 09:57:55
#> 3     3    (2 9) 2026-05-03 09:57:55

## rowwise
x1 |>
  mutate(a1 = 5:7) |>
  rowwise() |>
  mutate(a2 = mean(a, a1))
#> Spatiotemporal feature collection with 3 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#> # A tibble: 3 × 5
#> # Rowwise: 
#>       a geometry time                   a1    a2
#> * <int>  <POINT> <dttm>              <int> <int>
#> 1     1    (1 2) 2026-05-03 09:57:55     5     1
#> 2     2    (5 8) 2026-05-03 09:57:55     6     2
#> 3     3    (2 9) 2026-05-03 09:57:55     7     3

## mutate
x1 |>
  mutate(a1 = 5:7)
#> Spatiotemporal feature collection with 3 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#>   a    geometry                time a1
#> 1 1 POINT (1 2) 2026-05-03 09:57:55  5
#> 2 2 POINT (5 8) 2026-05-03 09:57:55  6
#> 3 3 POINT (2 9) 2026-05-03 09:57:55  7

## transmute
x1 |>
  transmute(a1 = 5:7)
#> Simple feature collection with 3 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#>   a1    geometry
#> 1  5 POINT (1 2)
#> 2  6 POINT (5 8)
#> 3  7 POINT (2 9)

## select
x1 |>
  select(-time) |>
  select(geometry)
#> Simple feature collection with 3 features and 0 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#>      geometry
#> 1 POINT (1 2)
#> 2 POINT (5 8)
#> 3 POINT (2 9)

## rename
x1 |>
  rename(a1 = a)
#> Spatiotemporal feature collection with 3 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#>   a1                time    geometry
#> 1  1 2026-05-03 09:57:55 POINT (1 2)
#> 2  2 2026-05-03 09:57:55 POINT (5 8)
#> 3  3 2026-05-03 09:57:55 POINT (2 9)

## slice
x1 |>
  slice(1:2)
#> Spatiotemporal feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 8
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#>   a                time    geometry
#> 1 1 2026-05-03 09:57:55 POINT (1 2)
#> 2 2 2026-05-03 09:57:55 POINT (5 8)

## summarise
x1 |>
  summarise(time = mean(time))
#> Spatiotemporal feature collection with 1 feature and 0 fields
#> Geometry type: MULTIPOINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Representing 2026-05-03 09:57:55.604221.
#>                  time                       geometry
#> 1 2026-05-03 09:57:55 MULTIPOINT ((1 2), (2 9), (...
  
x1 |>
  summarize(time = mean(time))
#> Spatiotemporal feature collection with 1 feature and 0 fields
#> Geometry type: MULTIPOINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Representing 2026-05-03 09:57:55.604221.
#>                  time                       geometry
#> 1 2026-05-03 09:57:55 MULTIPOINT ((1 2), (2 9), (...

## distinct
x1 |>
  distinct(geometry)
#> Simple feature collection with 3 features and 0 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#>      geometry
#> 1 POINT (1 2)
#> 2 POINT (5 8)
#> 3 POINT (2 9)

## gather
library(tidyr)
x1 |>
  mutate(a1 = 5:7) |>
  gather(key = "variable", value = "value", a, a1)
#> Spatiotemporal feature collection with 6 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#>                  time key value    geometry
#> 1 2026-05-03 09:57:55   a     1 POINT (1 2)
#> 2 2026-05-03 09:57:55   a     2 POINT (5 8)
#> 3 2026-05-03 09:57:55   a     3 POINT (2 9)
#> 4 2026-05-03 09:57:55  a1     5 POINT (1 2)
#> 5 2026-05-03 09:57:55  a1     6 POINT (5 8)
#> 6 2026-05-03 09:57:55  a1     7 POINT (2 9)

## pivot_longer
x1 |>
  mutate(a1 = 5:7) |>
  pivot_longer(cols = c("a", "a1"), names_to = "variable", values_to = "value")
#> Spatiotemporal feature collection with 6 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#> # A tibble: 6 × 4
#>   geometry time                variable value
#> *  <POINT> <dttm>              <chr>    <int>
#> 1    (1 2) 2026-05-03 09:57:55 a            1
#> 2    (1 2) 2026-05-03 09:57:55 a1           5
#> 3    (5 8) 2026-05-03 09:57:55 a            2
#> 4    (5 8) 2026-05-03 09:57:55 a1           6
#> 5    (2 9) 2026-05-03 09:57:55 a            3
#> 6    (2 9) 2026-05-03 09:57:55 a1           7

## spread
x1 |>
  mutate(a1 = 5:7) |>
  gather(key = "variable", value = "value", a, a1) |>
  spread(key = "variable", value = "value")
#> Spatiotemporal feature collection with 3 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#>                  time a a1    geometry
#> 1 2026-05-03 09:57:55 1  5 POINT (1 2)
#> 2 2026-05-03 09:57:55 2  6 POINT (5 8)
#> 3 2026-05-03 09:57:55 3  7 POINT (2 9)

## sample_n
set.seed(234)
x1 |>
  sample_n(size = 10, replace = TRUE)
#> Spatiotemporal feature collection with 10 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#>    a                time    geometry
#> 1  1 2026-05-03 09:57:55 POINT (1 2)
#> 2  3 2026-05-03 09:57:55 POINT (2 9)
#> 3  2 2026-05-03 09:57:55 POINT (5 8)
#> 4  2 2026-05-03 09:57:55 POINT (5 8)
#> 5  2 2026-05-03 09:57:55 POINT (5 8)
#> 6  2 2026-05-03 09:57:55 POINT (5 8)
#> 7  1 2026-05-03 09:57:55 POINT (1 2)
#> 8  1 2026-05-03 09:57:55 POINT (1 2)
#> 9  3 2026-05-03 09:57:55 POINT (2 9)
#> 10 2 2026-05-03 09:57:55 POINT (5 8)

## sample_frac
x1 |>
  sample_frac(size = 10, replace = TRUE) |>
  sample_frac(size = 0.1, replace = FALSE)
#> Spatiotemporal feature collection with 3 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 2 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#>   a                time    geometry
#> 1 1 2026-05-03 09:57:55 POINT (1 2)
#> 2 1 2026-05-03 09:57:55 POINT (1 2)
#> 3 3 2026-05-03 09:57:55 POINT (2 9)

## nest
x1 |>
  nest(a1 = -time)
#>                  time                        a1
#> 1 2026-05-03 09:57:55 1, 2, 3, 1, 2, 5, 8, 2, 9

## unnest
x1 |>
  mutate(a1 = list(1, c(1, 2), 5)) |>
  unnest(a1)
#> Spatiotemporal feature collection with 4 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#> # A tibble: 4 × 4
#>       a geometry time                   a1
#> * <int>  <POINT> <dttm>              <dbl>
#> 1     1    (1 2) 2026-05-03 09:57:55     1
#> 2     2    (5 8) 2026-05-03 09:57:55     1
#> 3     2    (5 8) 2026-05-03 09:57:55     2
#> 4     3    (2 9) 2026-05-03 09:57:55     5

## separate
x1 |>
  mutate(x = c(NA, "a.b", "a.d")) |>
  separate(x, c("A", "B"))
#> Spatiotemporal feature collection with 3 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#>   a                time    A    B    geometry
#> 1 1 2026-05-03 09:57:55 <NA> <NA> POINT (1 2)
#> 2 2 2026-05-03 09:57:55    a    b POINT (5 8)
#> 3 3 2026-05-03 09:57:55    a    d POINT (2 9)

## unite
x1 |>
  mutate(x = c(NA, "a.b", "a.d")) |>
  separate(x, c("A", "B")) |>
  unite(x, c("A", "B"))
#> Spatiotemporal feature collection with 3 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#>   a                time   col    geometry
#> 1 1 2026-05-03 09:57:55 NA_NA POINT (1 2)
#> 2 2 2026-05-03 09:57:55   a_b POINT (5 8)
#> 3 3 2026-05-03 09:57:55   a_d POINT (2 9)
  
## separate_rows
x1 |>
  mutate(z = c("1", "2,3,4", "5,6")) |>
  separate_rows(z, convert = TRUE)
#> Spatiotemporal feature collection with 6 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#> # A tibble: 6 × 4
#>       a geometry time                    z
#> * <int>  <POINT> <dttm>              <int>
#> 1     1    (1 2) 2026-05-03 09:57:55     1
#> 2     2    (5 8) 2026-05-03 09:57:55     2
#> 3     2    (5 8) 2026-05-03 09:57:55     3
#> 4     2    (5 8) 2026-05-03 09:57:55     4
#> 5     3    (2 9) 2026-05-03 09:57:55     5
#> 6     3    (2 9) 2026-05-03 09:57:55     6

## drop_na
x1 |>
  mutate(z = c(1, 2, NA)) |>
  drop_na(z)
#> Spatiotemporal feature collection with 2 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 5 ymax: 8
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#>   a                time z    geometry
#> 1 1 2026-05-03 09:57:55 1 POINT (1 2)
#> 2 2 2026-05-03 09:57:55 2 POINT (5 8)
  
x1 |>
  mutate(z = c(1, NA, NA)) |>
  drop_na(z)   
#> Spatiotemporal feature collection with 1 feature and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Representing 2026-05-03 09:57:55.604221.
#>   a                time z    geometry
#> 1 1 2026-05-03 09:57:55 1 POINT (1 2)

x1 |>
  mutate(time = replace(time, 1, NA)) |>
  drop_na(time)
#> Spatiotemporal feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 2 ymin: 8 xmax: 5 ymax: 9
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:55.604221 to 2026-05-03 09:57:55.604221.
#>   a                time    geometry
#> 1 2 2026-05-03 09:57:55 POINT (5 8)
#> 2 3 2026-05-03 09:57:55 POINT (2 9)
```
