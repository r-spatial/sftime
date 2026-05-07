# Transform method for `sftime` objects

Can be used to create or modify attribute variables; for transforming
geometries see
[`st_transform`](https://r-spatial.github.io/sf/reference/st_transform.html),
and all other functions starting with `st_`.

## Usage

``` r
# S3 method for class 'sftime'
transform(`_data`, ...)
```

## Arguments

- \_data:

  An object of class
  [`sftime`](https://r-spatial.github.io/sftime/reference/st_sftime.md).

- ...:

  Further arguments of the form `new_variable = expression`

## Value

`_data` (an `sftime` object) with modified attribute values (columns).

## Examples

``` r
# create an sftime object
g <- st_sfc(st_point(c(1, 2)), st_point(c(1, 3)), st_point(c(2, 3)), 
     st_point(c(2, 1)), st_point(c(3, 1)))
x <- 
   data.frame(a = 1:5, g, time = Sys.time() + 1:5, stringsAsFactors = FALSE)
x_sftime <- st_as_sftime(x)
x_sftime
#> Spatiotemporal feature collection with 5 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 1 xmax: 3 ymax: 3
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-07 20:03:05.187299 to 2026-05-07 20:03:09.187299.
#>   a                time    geometry
#> 1 1 2026-05-07 20:03:05 POINT (1 2)
#> 2 2 2026-05-07 20:03:06 POINT (1 3)
#> 3 3 2026-05-07 20:03:07 POINT (2 3)
#> 4 4 2026-05-07 20:03:08 POINT (2 1)
#> 5 5 2026-05-07 20:03:09 POINT (3 1)

# modify values in column a
transform(x_sftime, a = rev(a))
#> Spatiotemporal feature collection with 5 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 1 xmax: 3 ymax: 3
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-07 20:03:05.187299 to 2026-05-07 20:03:09.187299.
#>   a                time    geometry
#> 1 5 2026-05-07 20:03:05 POINT (1 2)
#> 2 4 2026-05-07 20:03:06 POINT (1 3)
#> 3 3 2026-05-07 20:03:07 POINT (2 3)
#> 4 2 2026-05-07 20:03:08 POINT (2 1)
#> 5 1 2026-05-07 20:03:09 POINT (3 1)
```
