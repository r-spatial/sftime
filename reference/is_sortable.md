# Checks whether a vector or list is sortable

Checks whether a vector or list is sortable. This is the condition for a
vector to be usable as time column in a `sftime` object.

## Usage

``` r
is_sortable(x)
```

## Arguments

- x:

  The object to check.

## Value

`TRUE` if `x` passes the check, else `FALSE`.

## Details

Checks whether the provided object can be handled by
[`order`](https://rdrr.io/r/base/order.html). A couple of basic types
are whitelisted. However, custom types can be defined when they provide
a dedicated generic to [xtfrm](https://rdrr.io/r/base/xtfrm.html). Note
that a `list` can only be sorted with
[atomic](https://rdrr.io/r/base/vector.html) values. See the examples
below for a template.

## Examples

``` r
x <- Sys.time() + 5:1 * 3600 * 24
sort(x)
#> [1] "2026-05-08 19:40:30 UTC" "2026-05-09 19:40:30 UTC"
#> [3] "2026-05-10 19:40:30 UTC" "2026-05-11 19:40:30 UTC"
#> [5] "2026-05-12 19:40:30 UTC"
is_sortable(x)
#> [1] TRUE
```
