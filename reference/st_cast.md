# Cast geometry to another type: either simplify, or cast explicitly

Cast geometry to another type: either simplify, or cast explicitly

## Usage

``` r
# S3 method for class 'sftime'
st_cast(x, to, ..., warn = TRUE, do_split = TRUE)
```

## Arguments

- x:

  An object of class `sftime`.

- to:

  character; target type, if missing, simplification is tried; when `x`
  is of type `sfg` (i.e., a single geometry) then `to` needs to be
  specified.

- ...:

  ignored

- warn:

  logical; if `TRUE`, warn if attributes are assigned to sub-geometries

- do_split:

  logical; if `TRUE`, allow splitting of geometries in sub-geometries

## Value

`x` with changed geometry type.

## Examples

``` r
# cast from POINT to LINESTRING
g <- st_sfc(st_point(1:2), st_point(c(2, 4)))
time <- Sys.time()
x <- 
  st_sftime(a = 3:4, g, time = time) |>
  dplyr::group_by(time) |>
  dplyr::summarize(do_union = TRUE) |>
  st_cast(to = "LINESTRING")
```
