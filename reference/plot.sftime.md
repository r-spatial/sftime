# Plots an `sftime` object

`plot.sftime`

## Usage

``` r
# S3 method for class 'sftime'
plot(x, y, ..., number = 6, tcuts)
```

## Arguments

- x:

  The
  [`sftime`](https://r-spatial.github.io/sftime/reference/st_sftime.md)
  object to be plotted.

- y:

  A character value; The variable name to be plotted; if missing, the
  first variable is plotted.

- ...:

  Additional arguments; Passed on to
  [`plot.sf`](https://r-spatial.github.io/sf/reference/plot.html).

- number:

  A numeric value; The number of panels to be plotted, cannot be larger
  than the number of timestamps; ignored when `tcuts` is provided.

- tcuts:

  predefined temporal ranges assigned to each map; if missing, will be
  determined as equal spans according to `number`.

## Value

Returns `NULL` and creates as side effect a plot for `x`.

## Examples

``` r
set.seed(123)
coords <- matrix(runif(100), ncol = 2)
g <- st_sfc(lapply(1:50, function(i) st_point(coords[i, ]) ))
sft <- st_sftime(a = 1:50, g, time = as.POSIXct("2020-09-01 00:00:00") + 0:49 * 3600 * 6)

plot(sft)

#> NULL
```
