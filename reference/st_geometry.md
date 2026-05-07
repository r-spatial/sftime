# Drops the geometry column of `sftime` objects

Drops the geometry column of an `sftime` object. This will also drop the
`sftime` class attribute and `time_column` attribute.

## Usage

``` r
# S3 method for class 'sftime'
st_drop_geometry(x, ...)
```

## Arguments

- x:

  An `sftime` object.

- ...:

  ignored

## Value

`x` without geometry column and without `sftime` and `sf` class.

## Examples

``` r
# dropping the geometry column will also drop the `sftime` class:
g <- st_sfc(st_point(1:2))
time <- Sys.time()
x <- st_sftime(a = 3, g, time = time)
st_drop_geometry(x)
#>   a                time
#> 1 3 2026-05-07 19:40:36
```
