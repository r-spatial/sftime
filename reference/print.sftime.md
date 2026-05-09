# Prints an `sftime` object

Prints an `sftime` object

## Usage

``` r
# S3 method for class 'sftime'
print(x, ..., n = getOption("sf_max_print", default = 10))
```

## Arguments

- x:

  An object of class `sftime`.

- ...:

  Currently unused arguments, for compatibility.

- n:

  Numeric value; maximum number of printed elements.

## Value

`x` (invisible).

## Examples

``` r
g <- st_sfc(st_point(c(1, 2)), st_point(c(1, 3)), st_point(c(2, 3)), 
     st_point(c(2, 1)), st_point(c(3, 1)))
tc <- Sys.time() + 1:5
x <- st_sftime(a = 1:5, g, time = tc)
print(x)
#> Spatiotemporal feature collection with 5 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 1 xmax: 3 ymax: 3
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-09 15:44:20.995744 to 2026-05-09 15:44:24.995744.
#>   a           g                time
#> 1 1 POINT (1 2) 2026-05-09 15:44:20
#> 2 2 POINT (1 3) 2026-05-09 15:44:21
#> 3 3 POINT (2 3) 2026-05-09 15:44:22
#> 4 4 POINT (2 1) 2026-05-09 15:44:23
#> 5 5 POINT (3 1) 2026-05-09 15:44:24
print(x[0, ])
#> Spatiotemporal feature collection with 0 features and 1 field
#> Geometry type: POINT
#> Bounding box:  xmin: NA ymin: NA xmax: NA ymax: NA
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from NA to NA.
#> [1] a    g    time
#> <0 rows> (or 0-length row.names)
```
