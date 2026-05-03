# Convert a foreign object to an `sftime` object

Convert a foreign object to an `sftime` object

## Usage

``` r
st_as_sftime(x, ...)

# S3 method for class 'ST'
st_as_sftime(x, ...)

# S3 method for class 'Track'
st_as_sftime(x, ...)

# S3 method for class 'Tracks'
st_as_sftime(x, ...)

# S3 method for class 'TracksCollection'
st_as_sftime(x, ...)

# S3 method for class 'sftime'
st_as_sftime(x, ...)

# S3 method for class 'sf'
st_as_sftime(x, ..., time_column_name = NULL)

# S3 method for class 'stars'
st_as_sftime(x, ..., long = TRUE, time_column_name = NULL)

# S3 method for class 'data.frame'
st_as_sftime(
  x,
  ...,
  agr = NA_agr_,
  coords,
  wkt,
  dim = "XYZ",
  remove = TRUE,
  na.fail = TRUE,
  sf_column_name = NULL,
  time_column_name = NULL,
  time_column_last = FALSE
)

# S3 method for class 'ppp'
st_as_sftime(x, ..., time_column_name)

# S3 method for class 'psp'
st_as_sftime(x, ..., time_column_name)

# S3 method for class 'lpp'
st_as_sftime(x, ..., time_column_name)

# S3 method for class 'sftrack'
st_as_sftime(x, ...)

# S3 method for class 'sftraj'
st_as_sftime(x, ...)

# S3 method for class 'cubble_df'
st_as_sftime(x, ..., sfc = NULL, crs, silent = FALSE)
```

## Arguments

- x:

  An object to be converted into an object of class
  [`sftime`](https://r-spatial.github.io/sftime/reference/st_sftime.md).

- ...:

  Further arguments passed to methods.

- time_column_name:

  A character value; name of the active time column. In case there is
  more than one and `time_column_name` is `NULL`, the first one is
  taken.

- long:

  A logical value; See
  [`st_as_sf`](https://r-spatial.github.io/stars/reference/st_as_sf.html).
  Typically, `long` should be set to `TRUE` since time information
  typically is a dimension of a `stars` object.

- agr:

  A character vector; see the details section of
  [`st_sf`](https://r-spatial.github.io/sf/reference/sf.html).

- coords:

  In case of point data: names or numbers of the numeric columns holding
  coordinates.

- wkt:

  The name or number of the character column that holds WKT encoded
  geometries.

- dim:

  Passed on to
  [`st_point`](https://r-spatial.github.io/sf/reference/st.html) (only
  when argument `coords` is given).

- remove:

  A logical value; when `coords` or `wkt` is given, remove these columns
  from `x`?

- na.fail:

  A logical value; if `TRUE`, raise an error if coordinates contain
  missing values.

- sf_column_name:

  A character value; name of the active list-column with simple feature
  geometries; in case there is more than one and `sf_column_name` is
  `NULL`, the first one is taken.

- time_column_last:

  A logical value; if `TRUE`, the active time column is always put last,
  otherwise column order is left unmodified. If both `sfc_last` and
  `time_column_last` are `TRUE`, the active time column is put last.

- sfc:

  object of class `sfc` (see package sf)

- crs:

  Coordinate reference system, something suitable as input to
  [`st_crs`](https://r-spatial.github.io/sf/reference/st_crs.html).

- silent:

  logical; suppress message?

## Value

`x` converted to an `sftime` object.

`st_as_sftime.Tracks` furthermore adds a column `track_name` with the
names of the `tracks` slot of the input `Tracks` object.

`st_as_sftime.TracksCollection` furthermore adds the columns
`tracks_name` with the names of the `tracksCollection` slot and
`track_name` with the names of the `tracks` slot of the input `Tracks`
object.

## Examples

``` r
# modified from spacetime:
library(sp)
library(spacetime)

sp <- cbind(x = c(0,0,1), y = c(0,1,1))
row.names(sp) <- paste("point", 1:nrow(sp), sep="")
sp <- SpatialPoints(sp)
time <- as.POSIXct("2010-08-05") + 3600 * (10:12)
x <- STI(sp, time)

st_as_sftime(x)
#> Warning: 'tzone' attributes are inconsistent
#> Spatiotemporal feature collection with 3 features and 0 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 1 ymax: 1
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2010-08-05 10:00:00 to 2010-08-05 12:00:00.
#>   st_as_sfc.x.sp.                time
#> 1     POINT (0 0) 2010-08-05 10:00:00
#> 2     POINT (0 1) 2010-08-05 11:00:00
#> 3     POINT (1 1) 2010-08-05 12:00:00

# convert a Track object from package trajectories to an sftime object
library(trajectories)
x1_Track <- trajectories::rTrack(n = 100)
x1_Track@data$speed <- sort(rnorm(length(x1_Track)))
x1_sftime <- st_as_sftime(x1_Track)
#> Warning: 'tzone' attributes are inconsistent

# convert a Tracks object from package trajectories to an sftime object
x2_Tracks <- trajectories::rTracks(m = 6)
x2_sftime <- st_as_sftime(x2_Tracks)
#> Warning: 'tzone' attributes are inconsistent

# convert a TracksCollection object from package trajectories to an sftime object
x3_TracksCollection <- trajectories::rTracksCollection(p = 2, m = 3, n = 50)
x3_sftime <- st_as_sftime(x3_TracksCollection)
#> Warning: 'tzone' attributes are inconsistent

# convert an sftime object to an sftime object
st_as_sftime(x3_sftime)
#> Spatiotemporal feature collection with 300 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -57.87874 ymin: -105.2126 xmax: 15.68272 ymax: 40.13782
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 1970-01-01 to 1970-01-01 04:49:00.
#> First 10 features:
#>    ones tracks_name track_name             st_as_sfc.x.sp.                time
#> 1     1     Tracks1     Track1 POINT (0.8546559 -1.036985) 1970-01-01 00:00:00
#> 2     1     Tracks1     Track1  POINT (0.2559254 1.134189) 1970-01-01 00:00:00
#> 3     1     Tracks1     Track1 POINT (0.7723814 -2.861054) 1970-01-01 00:01:00
#> 4     1     Tracks1     Track1  POINT (-0.578062 1.354914) 1970-01-01 00:01:00
#> 5     1     Tracks1     Track1 POINT (0.4824357 -4.520175) 1970-01-01 00:02:00
#> 6     1     Tracks1     Track1  POINT (-2.030241 2.319898) 1970-01-01 00:02:00
#> 7     1     Tracks1     Track1    POINT (1.821826 -6.1945) 1970-01-01 00:03:00
#> 8     1     Tracks1     Track1  POINT (-1.977036 3.783254) 1970-01-01 00:03:00
#> 9     1     Tracks1     Track1  POINT (1.765991 -6.699866) 1970-01-01 00:04:00
#> 10    1     Tracks1     Track1  POINT (-2.123454 6.196938) 1970-01-01 00:04:00
  
# convert an sf object to an sftime object
g <- st_sfc(st_point(c(1, 2)), st_point(c(1, 3)), st_point(c(2, 3)), 
     st_point(c(2, 1)), st_point(c(3, 1)))
x4_sf <- st_sf(a = 1:5, g, time = Sys.time() + 1:5)
x4_sftime <- st_as_sftime(x4_sf) 

# convert a Tracks object from package trajectories to an sftime object
x5_stars <- stars::read_stars(system.file("nc/bcsd_obs_1999.nc", package = "stars"))
#> pr, tas, 
x5_sftime <- st_as_sftime(x5_stars, time_column_name = "time")

# this requires some thought to not accidentally drop time dimensions. For
# example, setting `merge = TRUE` will drop the time dimension and thus throw
# an error:
if (FALSE) { # \dontrun{
x5_sftime <- st_as_sftime(x5_stars, merge = TRUE, time_column_name = "time")
} # }

# convert a data frame to an sftime object
x5_df <- 
   data.frame(a = 1:5, g, time = Sys.time() + 1:5, stringsAsFactors = FALSE)
x5_sftime <- st_as_sftime(x5_df)

# convert a ppp object to an sftime object (modified from the sf package)
if (require(spatstat.geom)) {
  st_as_sftime(gorillas, time_column_name = "date")
}
#> Loading required package: spatstat.geom
#> Loading required package: spatstat.data
#> Loading required package: spatstat.univar
#> spatstat.univar 3.1-7
#> spatstat.geom 3.7-3
#> Spatiotemporal feature collection with 648 features and 3 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: 580457.9 ymin: 674172.8 xmax: 585934 ymax: 678739.2
#> CRS:           NA
#> Time column with class: 'Date'.
#> Ranging from 2006-01-06 to 2009-05-31.
#> First 10 features:
#>    group season  label                           geom       date
#> NA  <NA>   <NA> window POLYGON ((584712 674237.1, ...       <NA>
#> 1  major    dry  point      POINT (582518.4 676886.2) 2006-01-06
#> 2  major    dry  point        POINT (581823 677422.7) 2006-01-10
#> 3  major    dry  point        POINT (582131 676937.9) 2006-01-15
#> 4  major    dry  point        POINT (582111.9 677420) 2006-01-24
#> 5  minor    dry  point      POINT (582585.1 677509.7) 2006-01-27
#> 6  major    dry  point      POINT (582302.3 677521.6) 2006-01-28
#> 7  major    dry  point      POINT (583167.2 676730.5) 2006-02-01
#> 8  major    dry  point      POINT (583584.5 677207.1) 2006-02-03
#> 9  major    dry  point      POINT (583117.8 676850.3) 2006-02-13

# convert a psp object to an sftime object (modified from the spatstat.geom 
# package)
if (require(spatstat.geom)) {
  # modified from spatstat.geom:
  x_psp <- 
    psp(
      runif(10), runif(10), runif(10), runif(10), window=owin(), 
      marks = data.frame(time = Sys.time() + 1:10)
    )
  st_as_sftime(x_psp, time_column_name = "time")
}
#> Spatiotemporal feature collection with 11 features and 1 field
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 1 ymax: 1
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:50.520351 to 2026-05-03 09:57:59.520351.
#> First 10 features:
#>      label                           geom                time
#> NA  window POLYGON ((0 0, 1 0, 1 1, 0 ...                <NA>
#> 1  segment LINESTRING (0.3203339 0.798... 2026-05-03 09:57:50
#> 2  segment LINESTRING (0.4039816 0.366... 2026-05-03 09:57:51
#> 3  segment LINESTRING (0.03262274 0.38... 2026-05-03 09:57:52
#> 4  segment LINESTRING (0.4922266 0.673... 2026-05-03 09:57:53
#> 5  segment LINESTRING (0.1416749 0.439... 2026-05-03 09:57:54
#> 6  segment LINESTRING (0.7991584 0.712... 2026-05-03 09:57:55
#> 7  segment LINESTRING (0.4091124 0.435... 2026-05-03 09:57:56
#> 8  segment LINESTRING (0.9364347 0.665... 2026-05-03 09:57:57
#> 9  segment LINESTRING (0.7329098 0.523... 2026-05-03 09:57:58

# convert an lpp object to an sftime object (modified from the 
# spatstat.linnet package)
if (require(spatstat.geom) && require(spatstat.linnet)) {
  # modified from spatstat.linnet:
  
  # letter 'A' 
  v <- spatstat.geom::ppp(x=(-2):2, y=3*c(0,1,2,1,0), c(-3,3), c(-1,7))
  edg <- cbind(1:4, 2:5)
  edg <- rbind(edg, c(2,4))
  letterA <- spatstat.linnet::linnet(v, edges=edg)
  
  # points on letter A
  xx <- 
    spatstat.geom::ppp(
      x=c(-1.5,0,0.5,1.5), y=c(1.5,3,4.5,1.5), 
      marks = data.frame(time = Sys.time() + 1:4, a = 1:4), 
      window = spatstat.geom::owin(
         xrange = range(c(-1.5,0,0.5,1.5)), 
         yrange = range(c(1.5,3,4.5,1.5)))
    )
  x_lpp <- spatstat.linnet::lpp(xx, letterA)
  
  # convert to sftime
  st_as_sftime(x_lpp, time_column_name = "time")
}
#> Loading required package: spatstat.linnet
#> Loading required package: spatstat.random
#> spatstat.random 3.4-5
#> Loading required package: spatstat.explore
#> Loading required package: nlme
#> 
#> Attaching package: ‘nlme’
#> The following object is masked from ‘package:dplyr’:
#> 
#>     collapse
#> spatstat.explore 3.8-0
#> Loading required package: spatstat.model
#> Loading required package: rpart
#> spatstat.model 3.7-0
#> spatstat.linnet 3.5-0
#> Spatiotemporal feature collection with 10 features and 4 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -3 ymin: -1 xmax: 3 ymax: 7
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2026-05-03 09:57:50.761299 to 2026-05-03 09:57:53.761299.
#>      label seg  tp  a                           geom                time
#> 1   window  NA  NA NA POLYGON ((-3 -1, 3 -1, 3 7,...                <NA>
#> 2  segment  NA  NA NA        LINESTRING (-2 0, -1 3)                <NA>
#> 3  segment  NA  NA NA         LINESTRING (-1 3, 0 6)                <NA>
#> 4  segment  NA  NA NA          LINESTRING (0 6, 1 3)                <NA>
#> 5  segment  NA  NA NA          LINESTRING (1 3, 2 0)                <NA>
#> 6  segment  NA  NA NA         LINESTRING (-1 3, 1 3)                <NA>
#> 7    point   1 0.5  1               POINT (-1.5 1.5) 2026-05-03 09:57:50
#> 8    point   5 0.5  2                    POINT (0 3) 2026-05-03 09:57:51
#> 9    point   3 0.5  3                POINT (0.5 4.5) 2026-05-03 09:57:52
#> 10   point   4 0.5  4                POINT (1.5 1.5) 2026-05-03 09:57:53

# convert an sftrack object to an sftime object (modified from sftrack)
if (require(sftrack)) {

  # get an sftrack object
  data("raccoon")
  
  raccoon$timestamp <- as.POSIXct(raccoon$timestamp, "EST")
  
  burstz <- 
    list(id = raccoon$animal_id, month = as.POSIXlt(raccoon$timestamp)$mon)
    
  x_sftrack <- 
    as_sftrack(raccoon,
               group = burstz, time = "timestamp",
               error = NA, coords = c("longitude", "latitude")
  )
  
  # convert to sftime
  st_as_sftime(x_sftrack)
}
#> Loading required package: sftrack
#> Spatiotemporal feature collection with 445 features and 8 fields (with 168 geometries empty)
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -80.28149 ymin: 26.06761 xmax: -80.27046 ymax: 26.07706
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2019-01-18 19:02:30 to 2019-02-01 18:02:30.
#> First 10 features:
#>    animal_id latitude longitude height hdop vdop fix               sft_group
#> 1    TTP-058       NA        NA     NA  0.0  0.0  NO (id: TTP-058, month: 0)
#> 2    TTP-058 26.06945 -80.27906      7  6.2  3.2  2D (id: TTP-058, month: 0)
#> 3    TTP-058       NA        NA     NA  0.0  0.0  NO (id: TTP-058, month: 0)
#> 4    TTP-058       NA        NA     NA  0.0  0.0  NO (id: TTP-058, month: 0)
#> 5    TTP-058 26.06769 -80.27431    858  5.1  3.2  2D (id: TTP-058, month: 0)
#> 6    TTP-058 26.06867 -80.27930    350  1.9  3.2  3D (id: TTP-058, month: 0)
#> 7    TTP-058 26.06962 -80.27908     11  2.3  4.5  3D (id: TTP-058, month: 0)
#> 8    TTP-058 26.06963 -80.27902      9  2.7  3.9  3D (id: TTP-058, month: 0)
#> 9    TTP-058       NA        NA     NA  0.0  0.0  NO (id: TTP-058, month: 0)
#> 10   TTP-058 26.06982 -80.27900     NA  2.0  3.3  3D (id: TTP-058, month: 0)
#>                      geometry           timestamp
#> 1                 POINT EMPTY 2019-01-18 19:02:30
#> 2  POINT (-80.27906 26.06945) 2019-01-18 20:02:30
#> 3                 POINT EMPTY 2019-01-18 21:02:30
#> 4                 POINT EMPTY 2019-01-18 22:02:30
#> 5  POINT (-80.27431 26.06769) 2019-01-18 23:02:30
#> 6   POINT (-80.2793 26.06867) 2019-01-19 00:02:30
#> 7  POINT (-80.27908 26.06962) 2019-01-19 01:02:30
#> 8  POINT (-80.27902 26.06963) 2019-01-19 02:02:04
#> 9                 POINT EMPTY 2019-01-19 03:02:30
#> 10   POINT (-80.279 26.06982) 2019-01-19 12:02:30

# convert an sftraj object to an sftime object (modified from sftrack)
if (require(sftrack)) {

  # get an sftrack object
  data("raccoon")
  
  raccoon$timestamp <- as.POSIXct(raccoon$timestamp, "EST")
  
  burstz <- 
    list(id = raccoon$animal_id, month = as.POSIXlt(raccoon$timestamp)$mon)
  
  x_sftraj <- 
    as_sftraj(raccoon,
      time = "timestamp",
      error = NA, coords = c("longitude", "latitude"),
      group = burstz
    )
  
  # convert to sftime
  st_as_sftime(x_sftraj)
}
#> Spatiotemporal feature collection with 445 features and 8 fields (with 168 geometries empty)
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -80.28149 ymin: 26.06761 xmax: -80.27046 ymax: 26.07706
#> CRS:           NA
#> Time column with classes: 'POSIXct', 'POSIXt'.
#> Ranging from 2019-01-18 19:02:30 to 2019-02-01 18:02:30.
#> First 10 features:
#>    animal_id latitude longitude height hdop vdop fix               sft_group
#> 1    TTP-058       NA        NA     NA  0.0  0.0  NO (id: TTP-058, month: 0)
#> 2    TTP-058 26.06945 -80.27906      7  6.2  3.2  2D (id: TTP-058, month: 0)
#> 3    TTP-058       NA        NA     NA  0.0  0.0  NO (id: TTP-058, month: 0)
#> 4    TTP-058       NA        NA     NA  0.0  0.0  NO (id: TTP-058, month: 0)
#> 5    TTP-058 26.06769 -80.27431    858  5.1  3.2  2D (id: TTP-058, month: 0)
#> 6    TTP-058 26.06867 -80.27930    350  1.9  3.2  3D (id: TTP-058, month: 0)
#> 7    TTP-058 26.06962 -80.27908     11  2.3  4.5  3D (id: TTP-058, month: 0)
#> 8    TTP-058 26.06963 -80.27902      9  2.7  3.9  3D (id: TTP-058, month: 0)
#> 9    TTP-058       NA        NA     NA  0.0  0.0  NO (id: TTP-058, month: 0)
#> 10   TTP-058 26.06982 -80.27900     NA  2.0  3.3  3D (id: TTP-058, month: 0)
#>                          geometry           timestamp
#> 1                     POINT EMPTY 2019-01-18 19:02:30
#> 2      POINT (-80.27906 26.06945) 2019-01-18 20:02:30
#> 3                     POINT EMPTY 2019-01-18 21:02:30
#> 4                     POINT EMPTY 2019-01-18 22:02:30
#> 5  LINESTRING (-80.27431 26.06... 2019-01-18 23:02:30
#> 6  LINESTRING (-80.2793 26.068... 2019-01-19 00:02:30
#> 7  LINESTRING (-80.27908 26.06... 2019-01-19 01:02:30
#> 8      POINT (-80.27902 26.06963) 2019-01-19 02:02:04
#> 9                     POINT EMPTY 2019-01-19 03:02:30
#> 10 LINESTRING (-80.279 26.0698... 2019-01-19 12:02:30

# convert a cubble_df object from package cubble to an sftime object
if (requireNamespace("cubble", quietly = TRUE, versionCheck = list(op = ">=", version = "0.3.0"))) {

  # get a cubble_df object
  data("climate_aus", package = "cubble")
  
  # convert to sftime
  climate_aus_sftime <- 
    st_as_sftime(climate_aus[1:4, ])
    
  climate_aus_sftime <- 
    st_as_sftime(cubble::face_temporal(climate_aus)[1:4, ])
  
}
#> CRS missing: using OGC:CRS84 (WGS84) as default
#> CRS missing: using OGC:CRS84 (WGS84) as default
```
