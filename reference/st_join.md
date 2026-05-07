# Spatial join, spatial filter for `sftime` objects

Spatial join, spatial filter for `sftime` objects

## Usage

``` r
# S3 method for class 'sftime'
st_join(
  x,
  y,
  join = st_intersects,
  ...,
  suffix = c(".x", ".y"),
  left = TRUE,
  largest = FALSE
)

# S3 method for class 'sftime'
st_filter(x, y, ..., .predicate = st_intersects)
```

## Arguments

- x:

  An object of class `sftime` or `sf`.

- y:

  An object of class `sftime` or `sf`.

- join:

  A geometry predicate function with the same profile as
  [`st_intersects`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html);
  see details.

- ...:

  for `st_join`: arguments passed on to the `join` function or to
  `st_intersection` when `largest` is `TRUE`; for `st_filter` arguments
  passed on to the `.predicate` function, e.g. `prepared`, or a pattern
  for
  [st_relate](https://r-spatial.github.io/sf/reference/st_relate.html)

- suffix:

  length 2 character vector; see
  [merge](https://rdrr.io/r/base/merge.html)

- left:

  logical; if `TRUE` return the left join, otherwise an inner join; see
  details. see also
  [left_join](https://dplyr.tidyverse.org/reference/mutate-joins.html)

- largest:

  logical; if `TRUE`, return `x` features augmented with the fields of
  `y` that have the largest overlap with each of the features of `x`;
  see https://github.com/r-spatial/sf/issues/578

- .predicate:

  A geometry predicate function with the same profile as
  [`st_intersects`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html);
  see details.

## Value

An object of class `sftime`, joined based on geometry.

## Details

Alternative values for argument `join` are:

- [st_contains_properly](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)

- [st_contains](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)

- [st_covered_by](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)

- [st_covers](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)

- [st_crosses](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)

- [st_disjoint](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)

- [st_equals_exact](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)

- [st_equals](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)

- [st_is_within_distance](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)

- [st_nearest_feature](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)

- [st_overlaps](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)

- [st_touches](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)

- [st_within](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)

- any user-defined function of the same profile as the above

A left join returns all records of the `x` object with `y` fields for
non-matched records filled with `NA` values; an inner join returns only
records that spatially match.

## Examples

``` r
g1 <- st_sfc(st_point(c(1,1)), st_point(c(2,2)), st_point(c(3,3)))
x1 <- st_sftime(a = 1:3, geometry = g1, time = Sys.time())

g2 <- st_sfc(st_point(c(10,10)), st_point(c(2,2)), st_point(c(2,2)), st_point(c(3,3)))
x2 <- st_sftime(a = 11:14, geometry = g2, time = Sys.time())

## st_join

# left spatial join with st_intersects
st_join(x1, x2)
#> Spatiotemporal feature collection with 4 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 1 xmax: 3 ymax: 3
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-07 20:03:01.120072 to 2026-05-07 20:03:01.120072.
#>     a.x a.y              time.y    geometry              time.x
#> 1     1  NA                <NA> POINT (1 1) 2026-05-07 20:03:01
#> 2     2  12 2026-05-07 20:03:01 POINT (2 2) 2026-05-07 20:03:01
#> 2.1   2  13 2026-05-07 20:03:01 POINT (2 2) 2026-05-07 20:03:01
#> 3     3  14 2026-05-07 20:03:01 POINT (3 3) 2026-05-07 20:03:01

# inner spatial join with st_intersects
st_join(x1, x2, left = FALSE)
#> Spatiotemporal feature collection with 3 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 2 ymin: 2 xmax: 3 ymax: 3
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-07 20:03:01.120072 to 2026-05-07 20:03:01.120072.
#>     a.x a.y              time.y    geometry              time.x
#> 2     2  12 2026-05-07 20:03:01 POINT (2 2) 2026-05-07 20:03:01
#> 2.1   2  13 2026-05-07 20:03:01 POINT (2 2) 2026-05-07 20:03:01
#> 3     3  14 2026-05-07 20:03:01 POINT (3 3) 2026-05-07 20:03:01

## st_filter

st_filter(x1, x2)
#> Spatiotemporal feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 2 ymin: 2 xmax: 3 ymax: 3
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-07 20:03:01.120072 to 2026-05-07 20:03:01.120072.
#>   a    geometry                time
#> 1 2 POINT (2 2) 2026-05-07 20:03:01
#> 2 3 POINT (3 3) 2026-05-07 20:03:01
st_filter(x2, x1)
#> Spatiotemporal feature collection with 3 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 2 ymin: 2 xmax: 3 ymax: 3
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-07 20:03:01.122996 to 2026-05-07 20:03:01.122996.
#>    a    geometry                time
#> 1 12 POINT (2 2) 2026-05-07 20:03:01
#> 2 13 POINT (2 2) 2026-05-07 20:03:01
#> 3 14 POINT (3 3) 2026-05-07 20:03:01
```
