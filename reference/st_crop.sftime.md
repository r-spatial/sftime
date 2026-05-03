# Crop an `sftime` object to a specific rectangle

Crop an `sftime` object to a specific rectangle

## Usage

``` r
# S3 method for class 'sftime'
st_crop(x, y, ...)
```

## Arguments

- x:

  An object of class `sftime`.

- y:

  A numeric vector with named elements `xmin`, `ymin`, `xmax` and
  `ymax`, or an object of class `bbox`, or an object for which there is
  an [`st_bbox`](https://r-spatial.github.io/sf/reference/st_bbox.html)
  method to convert it to a `bbox` object.

- ...:

  Additional arguments; Ignored.

## Value

`x` cropped using `y`.

## Details

See [`st_crop`](https://r-spatial.github.io/sf/reference/st_crop.html).

## Examples

``` r
# modified from sf:
box <- c(xmin = 0, ymin = 0, xmax = 1, ymax = 1)
pol <- sf::st_sfc(sf::st_buffer(sf::st_point(c(0.5, 0.5)), 0.6))
pol_sftime <- st_sftime(a = 1, geom = pol, time = Sys.time() + 1:2 * 1000)
#> Warning: row names were found from a short variable and have been discarded

pol_sftime_cropped <- sf::st_crop(pol_sftime, sf::st_bbox(box))
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries

class(pol_sftime_cropped)
#> [1] "sftime"     "sf"         "data.frame"
plot(pol_sftime_cropped)
#> [INFO] Fewer time stamps in the data than asked for; argument 'number' set to: 2

#> NULL
```
