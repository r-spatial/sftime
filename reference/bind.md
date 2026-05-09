# Bind rows (features) of `sftime` objects

Bind rows (features) of `sftime` objects

Bind columns (variables) of `sftime` objects

## Usage

``` r
# S3 method for class 'sftime'
rbind(..., deparse.level = 1)

# S3 method for class 'sftime'
cbind(..., deparse.level = 1, sf_column_name = NULL, tc_column_name = NULL)
```

## Arguments

- ...:

  Objects to bind; note that for the `rbind` and `cbind` methods, all
  objects have to be of class `sftime`; see `dotsMethods`.

- deparse.level:

  An integer value; see [`rbind`](https://rdrr.io/r/base/cbind.html).

- sf_column_name:

  Character value; specifies the active geometry column; passed on to
  [`st_sftime`](https://r-spatial.github.io/sftime/reference/st_sftime.md).

- tc_column_name:

  Character value; specifies active time column; passed on to
  [`st_sftime`](https://r-spatial.github.io/sftime/reference/st_sftime.md).

## Value

`rbind` combines all `sftime` objects in `...` row-wise and returns the
combined `sftime` object.

`cbind` combines all `sftime` objects in `...` column-wise and returns
the combined `sftime` object. When called with multiple `sftime` objects
warns about multiple time and geometry columns present when the time and
geometry columns to use are not specified by using arguments
`tc_column_name` and `sf_column_name`; see also
[st_sftime](https://r-spatial.github.io/sftime/reference/st_sftime.md).

## Details

Both `rbind` and `cbind` have non-standard method dispatch (see
[cbind](https://rdrr.io/r/base/cbind.html)): the `rbind` or `cbind`
method for `sftime` objects is only called when all arguments to be
combined are of class `sftime`.

If you need to `cbind` e.g. a `data.frame` to an `sf`, use
[`data.frame`](https://rdrr.io/r/base/data.frame.html) directly and use
[`st_sftime`](https://r-spatial.github.io/sftime/reference/st_sftime.md)
on its result, or use
[`bind_cols`](https://dplyr.tidyverse.org/reference/bind_rows.html); see
examples.

## Examples

``` r
g1 <- st_sfc(st_point(1:2))
x1 <- st_sftime(a = 3, geometry = g1, time = Sys.time())

g2 <- st_sfc(st_point(c(4, 6)))
x2 <- st_sftime(a = 4, geometry = g2, time = Sys.time())

rbind(x1, x2) # works because both tc1 and tc2 have the same class
#> Spatiotemporal feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 4 ymax: 6
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-09 15:46:47.843065 to 2026-05-09 15:46:47.847203.
#>   a    geometry                time
#> 1 3 POINT (1 2) 2026-05-09 15:46:47
#> 2 4 POINT (4 6) 2026-05-09 15:46:47

if (FALSE) { # \dontrun{
st_time(x2) <- 1
rbind(x1, x2) # error because both tc1 and tc2 do not have the same class
} # }

cbind(x1, x2) 
#> Spatiotemporal feature collection with 1 feature and 3 fields
#> Active geometry column: geometry
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Representing 2026-05-09 15:46:47.843065.
#>   a a.1              time.1    geometry  geometry.1                time
#> 1 3   4 2026-05-09 15:46:47 POINT (1 2) POINT (4 6) 2026-05-09 15:46:47

if (require(dplyr)) {
  # returns a data frame because names of sf and time column are modified:
  dplyr::bind_cols(x1, x2) 
  
  # returns an sf object because the name of the time column is modified:
  dplyr::bind_cols(x1, x2 |> sf::st_drop_geometry()) 
  
  # returns an sftime object because names of sf and time column are both 
  # preserved:
  dplyr::bind_cols(x1, x2 |> st_drop_time() |> sf::st_drop_geometry()) 
}
#> Loading required package: dplyr
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
#> New names:
#> • `a` -> `a...1`
#> • `geometry` -> `geometry...2`
#> • `time` -> `time...3`
#> • `a` -> `a...4`
#> • `geometry` -> `geometry...5`
#> • `time` -> `time...6`
#> New names:
#> • `a` -> `a...1`
#> • `time` -> `time...3`
#> • `a` -> `a...4`
#> • `time` -> `time...5`
#> New names:
#> • `a` -> `a...1`
#> • `a` -> `a...4`
#> Spatiotemporal feature collection with 1 feature and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Representing 2026-05-09 15:46:47.843065.
#>   a...1 a...4    geometry                time
#> 1     3     4 POINT (1 2) 2026-05-09 15:46:47
  
df <- data.frame(x = 3)   
st_sftime(data.frame(x1, df))   
#> Spatiotemporal feature collection with 1 feature and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Representing 2026-05-09 15:46:47.843065.
#>   a x    geometry                time
#> 1 3 3 POINT (1 2) 2026-05-09 15:46:47
  
```
