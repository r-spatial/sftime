# Combine or union feature geometries (including `sftime` objects)

Combine or union feature geometries (including `sftime` objects)

## Usage

``` r
# S3 method for class 'sftime'
st_union(x, y, ..., by_feature = FALSE, is_coverage = FALSE)
```

## Arguments

- x:

  An object of class `sftime`, `sf`, `sfc` or `sfg`.

- y:

  An object of class `sftime`, `sf`, `sfc` or `sfg` (optional).

- ...:

  See
  [`geos_combine`](https://r-spatial.github.io/sf/reference/geos_combine.html).

- by_feature:

  See
  [`geos_combine`](https://r-spatial.github.io/sf/reference/geos_combine.html).

- is_coverage:

  See
  [`geos_combine`](https://r-spatial.github.io/sf/reference/geos_combine.html).

## Value

If `y` is missing, `st_union(x)` returns a single geometry with resolved
boundaries, else the geometries for all unioned pairs of `x[i]` and
`y[j]`.

## Details

See
[`geos_combine`](https://r-spatial.github.io/sf/reference/geos_combine.html).

## Examples

``` r
# union simple features in an sftime object
g <- st_sfc(st_point(c(1, 2)), st_point(c(1, 3)), st_point(c(2, 3)), 
     st_point(c(2, 1)), st_point(c(3, 1)))
tc <- Sys.time() + 1:5
x <- st_sftime(a = 1:5, g, time = tc)

# only x provided (no y)
plot(st_union(st_buffer(x, dist = 1)))


# with arguments x and y provided
plot(st_union(st_buffer(x, dist = 1), st_buffer(x, dist = 0.5)), "a")
#> Warning: attribute variables are assumed to be spatially constant throughout all geometries
#> [INFO] Fewer time stamps in the data than asked for; argument 'number' set to: 5

#> NULL
```
