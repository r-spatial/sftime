# Construct an `sftime` object from all its components

Construct an `sftime` object from all its components

## Usage

``` r
st_sftime(
  ...,
  agr = sf::NA_agr_,
  row.names,
  stringsAsFactors = TRUE,
  crs,
  precision,
  sf_column_name = NULL,
  time_column_name = NULL,
  check_ring_dir = FALSE,
  sfc_last = TRUE,
  time_column_last = TRUE
)

# S3 method for class 'sftime'
x[i, j, ..., drop = FALSE, op = sf::st_intersects]

# S3 method for class 'sftime'
x[[i]] <- value

# S3 method for class 'sftime'
x$i <- value
```

## Arguments

- ...:

  Column elements to be binded into an `sftime` object or a single
  `list` or `data.frame` with such columns. At least one of these
  columns shall be a geometry list-column of class `sfc` and one shall
  be a time column (to be specified with `time_column_name`).

- agr:

  A character vector; see details below.

- row.names:

  row.names for the created `sf` object.

- stringsAsFactors:

  A logical value; see
  [`st_read`](https://r-spatial.github.io/sf/reference/st_read.html).

- crs:

  Coordinate reference system, something suitable as input to
  [`st_crs`](https://r-spatial.github.io/sf/reference/st_crs.html).

- precision:

  A numeric value; see
  [`st_as_binary`](https://r-spatial.github.io/sf/reference/st_as_binary.html).

- sf_column_name:

  A character value; name of the active list-column with simple feature
  geometries; in case there is more than one and `sf_column_name` is
  `NULL`, the first one is taken.

- time_column_name:

  A character value; name of the active time column. In case
  `time_column_name` is `NULL`, the first
  [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) column is
  taken. If there is no `POSIXct` column, the first
  [`Date`](https://rdrr.io/r/base/Dates.html) column is taken.

- check_ring_dir:

  A logical value; see
  [`st_read`](https://r-spatial.github.io/sf/reference/st_read.html).

- sfc_last:

  A logical value; if `TRUE`, `sfc` columns are always put last,
  otherwise column order is left unmodified.

- time_column_last:

  A logical value; if `TRUE`, the active time column is always put last,
  otherwise column order is left unmodified. If both `sfc_last` and
  `time_column_last` are `TRUE`, the active time column is put last.

- x:

  An object of class `sf`.

- i:

  Record selection, see
  [\[.data.frame](https://rdrr.io/r/base/Extract.data.frame.html)

- j:

  Variable selection, see
  [\[.data.frame](https://rdrr.io/r/base/Extract.data.frame.html)

- drop:

  A logical value, default `FALSE`; if `TRUE` drop the geometry column
  and return a `data.frame`, else make the geometry sticky and return an
  `sf` object.

- op:

  A function; geometrical binary predicate function to apply when `i` is
  a simple feature object.

- value:

  An object to insert into `x` or with which to rename columns of `x`.

## Value

`st_sftime`: An object of class `sftime`.

Returned objects for subsetting functions: `[.sf` will return a
`data.frame` or vector if the geometry column (of class `sfc`) is
dropped (`drop=TRUE`), an `sfc` object if only the geometry column is
selected, and otherwise return an `sftime` object.

## Details

See also
[\[.data.frame](https://rdrr.io/r/base/Extract.data.frame.html); for
`[.sftime` `...` arguments are passed to `op`.

## Examples

``` r
## construction with an sfc object
library(sf)
g <- st_sfc(st_point(1:2))
tc <- Sys.time()
st_sftime(a = 3, g, time = tc)
#> Spatiotemporal feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Representing 2026-05-07 20:03:01.517311.
#>   a           g                time
#> 1 3 POINT (1 2) 2026-05-07 20:03:01

## construction with an sf object
if (FALSE) { # \dontrun{
st_sftime(st_sf(a = 3, g), time = tc) 
# error, because if ... contains a data.frame-like object, no other objects 
# may be passed through ... . Instead, add the time column before.
} # }

st_sftime(st_sf(a = 3, g, time = tc))
#> Spatiotemporal feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Representing 2026-05-07 20:03:01.517311.
#>   a           g                time
#> 1 3 POINT (1 2) 2026-05-07 20:03:01

## Subsetting
g <- st_sfc(st_point(c(1, 2)), st_point(c(1, 3)), st_point(c(2, 3)), 
     st_point(c(2, 1)), st_point(c(3, 1)))
tc <- Sys.time() + 1:5
x <- st_sftime(a = 1:5, g, time = tc)

# rows
x[1, ]
#> Spatiotemporal feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 2
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Representing 2026-05-07 20:03:02.527314.
#>   a           g                time
#> 1 1 POINT (1 2) 2026-05-07 20:03:02
class(x[1, ])
#> [1] "sftime"     "sf"         "data.frame"

x[x$a < 3, ]
#> Spatiotemporal feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 1 ymax: 3
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-07 20:03:02.527314 to 2026-05-07 20:03:03.527314.
#>   a           g                time
#> 1 1 POINT (1 2) 2026-05-07 20:03:02
#> 2 2 POINT (1 3) 2026-05-07 20:03:03
class(x[x$a < 3, ])
#> [1] "sftime"     "sf"         "data.frame"

# columns
x[, 1]
#> Simple feature collection with 5 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 1 xmax: 3 ymax: 3
#> CRS:           NA
#>   a           g
#> 1 1 POINT (1 2)
#> 2 2 POINT (1 3)
#> 3 3 POINT (2 3)
#> 4 4 POINT (2 1)
#> 5 5 POINT (3 1)
class(x[, 1]) # drops time column as for ordinary data.frame subsetting, 
#> [1] "sf"         "data.frame"
# keeps geometry column of sf object

x[, 3]
#> Spatiotemporal feature collection with 5 features and 0 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 1 xmax: 3 ymax: 3
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-07 20:03:02.527314 to 2026-05-07 20:03:06.527314.
#>                  time           g
#> 1 2026-05-07 20:03:02 POINT (1 2)
#> 2 2026-05-07 20:03:03 POINT (1 3)
#> 3 2026-05-07 20:03:04 POINT (2 3)
#> 4 2026-05-07 20:03:05 POINT (2 1)
#> 5 2026-05-07 20:03:06 POINT (3 1)
class(x[, 3]) # keeps time column because it is explicitly selected,
#> [1] "sftime"     "sf"         "data.frame"
# keeps geometry column of sf object, returns an sftime object

x[, 3, drop = TRUE] 
#> [1] "2026-05-07 20:03:02 UTC" "2026-05-07 20:03:03 UTC"
#> [3] "2026-05-07 20:03:04 UTC" "2026-05-07 20:03:05 UTC"
#> [5] "2026-05-07 20:03:06 UTC"
class(x[, 3, drop = TRUE]) # if the geometry column is dropped, not only the
#> [1] "POSIXct" "POSIXt" 
# sf class is dropped, but also the sftime class

x["a"]
#> Simple feature collection with 5 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 1 xmax: 3 ymax: 3
#> CRS:           NA
#>   a           g
#> 1 1 POINT (1 2)
#> 2 2 POINT (1 3)
#> 3 3 POINT (2 3)
#> 4 4 POINT (2 1)
#> 5 5 POINT (3 1)
class(x["a"]) # Time columns are not sticky: If a column is selected by a 
#> [1] "sf"         "data.frame"
# character vector and this does not contain the active time column, the time 
# column is dropped. 

x[c("a", "time")]
#> Spatiotemporal feature collection with 5 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 1 xmax: 3 ymax: 3
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-07 20:03:02.527314 to 2026-05-07 20:03:06.527314.
#>   a                time           g
#> 1 1 2026-05-07 20:03:02 POINT (1 2)
#> 2 2 2026-05-07 20:03:03 POINT (1 3)
#> 3 3 2026-05-07 20:03:04 POINT (2 3)
#> 4 4 2026-05-07 20:03:05 POINT (2 1)
#> 5 5 2026-05-07 20:03:06 POINT (3 1)
class(x[c("a", "time")]) # keeps the time column
#> [1] "sftime"     "sf"         "data.frame"

# with sf or sftime object 
pol = st_sfc(st_polygon(list(cbind(c(0,2,2,0,0),c(0,0,2,2,0)))))
h = st_sf(r = 5, pol)

x[h, ] 
#> Simple feature collection with 2 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 1 xmax: 2 ymax: 2
#> CRS:           NA
#>   a           g                time
#> 1 1 POINT (1 2) 2026-05-07 20:03:02
#> 4 4 POINT (2 1) 2026-05-07 20:03:05
class(x[h, ]) # returns sftime object
#> [1] "sf"         "data.frame"

h[x, ] 
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 2 ymax: 2
#> CRS:           NA
#>   r                            pol
#> 1 5 POLYGON ((0 0, 2 0, 2 2, 0 ...
class(h[x, ]) # returns sf object
#> [1] "sf"         "data.frame"

## Assigning values to columns

# assigning new values to a non-time column
x[["a"]] <- 5:1
class(x)
#> [1] "sftime"     "sf"         "data.frame"

# assigning allowed new values to the time column
x[["time"]] <- Sys.time() + 1:5
class(x)
#> [1] "sftime"     "sf"         "data.frame"

# assigning new values to the time column which invalidate the time column
x[["time"]] <- list(letters[1:2])
class(x)
#> [1] "sftime"     "sf"         "data.frame"

# assigning new values with `$`
x$time <- Sys.time() + 1:5
class(x)
#> [1] "sftime"     "sf"         "data.frame"
```
