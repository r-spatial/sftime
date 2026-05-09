# Get, set, or replace time information

Get, set, or replace time information

## Usage

``` r
st_time(obj, ...)

st_time(x, ...) <- value

# S3 method for class 'sftime'
st_time(obj, ...)

# S3 method for class 'sf'
st_time(x, ..., time_column_name = "time") <- value

# S3 method for class 'sftime'
st_time(x, ...) <- value

st_set_time(x, value, ...)

st_drop_time(x)
```

## Arguments

- obj:

  An object of class `sftime`.

- ...:

  Additional arguments; Ignored.

- x:

  An object of class `sftime` or `sf`.

- value:

  An object for which
  [`is_sortable`](https://r-spatial.github.io/sftime/reference/is_sortable.md)
  returns `TRUE` or an object of class `character`, or `NULL`.

- time_column_name:

  Character value; The name of the column to set as active time column
  in `x`.

## Value

`st_time` returns the content of the active time column of an `sftime`
object. Assigning an object for which
[`is_sortable`](https://r-spatial.github.io/sftime/reference/is_sortable.md)
returns `TRUE` to an `sf` object creates an
[`sftime`](https://r-spatial.github.io/sftime/reference/st_sftime.md)
object. Assigning an object for which
[`is_sortable`](https://r-spatial.github.io/sftime/reference/is_sortable.md)
returns `TRUE` to an `sftime` object replaces the active time column by
this object.

## Details

In case `value` is character and `x` is of class `sftime`, the active
time column (as indicated by attribute `time_column`) is set to
`x[[value]]`.

The replacement function applied to `sftime` objects will overwrite the
active time column, if `value` is `NULL`, it will remove it and coerce
`x` to an `sftime` object.

`st_drop_time` drops the time column of its argument, and reclasses it
accordingly.

## Examples

``` r
# from sftime object
g <- st_sfc(st_point(1:2))
time <- Sys.time()
x <- st_sftime(a = 3, g, time = time)
st_time(x) 
#> [1] "2026-05-09 15:44:27 UTC"

## assign a vector with time information

# to sf object
x <- st_sf(a = 3, g)
st_time(x) <- time
x
#> Spatiotemporal feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Representing 2026-05-09 15:44:27.058873.
#>   a           g                time
#> 1 3 POINT (1 2) 2026-05-09 15:44:27

# to sftime object
x <- st_sftime(a = 3, g, time = time)
st_time(x) <- Sys.time()

## change the time column to another already existing column
st_time(x) <- "a"

## remove time column from sftime object
st_time(x) <- NULL

## pipe-friendly

# assign time column to sf object
x <- st_sf(a = 3, g)
x <- st_set_time(x, time)

# remove time column from sftime object
st_set_time(x, NULL)
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#>   a           g
#> 1 3 POINT (1 2)

## drop time column and class

# same as x <- st_set_time(x, NULL)
st_drop_time(x)
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#>   a           g
#> 1 3 POINT (1 2)
```
