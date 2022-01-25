## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages-----------------------------------------------------------------
# load required packages
library(sftime)
library(sf)
library(stars)
library(spacetime)

## ----sftime-class-1-----------------------------------------------------------
# example sfc object
x_sfc <- 
  sf::st_sfc(
    sf::st_point(1:2), 
    sf::st_point(c(1,3)), 
    sf::st_point(2:3), 
    sf::st_point(c(2,1))
  )

# create an sftime object directly from x_sfc
x_sftime1 <- sftime::st_sftime(a = 1:4, x_sfc, time = Sys.time()- 0:3 * 3600 * 24)

# first create the sf object and from this the sftime object
x_sf <- sf::st_sf(a = 1:4, x_sfc, time = x_sftime1$time)
x_sftime2 <- sftime::st_sftime(x_sf)

x_sftime3 <- sftime::st_as_sftime(x_sf) # alernative option

identical(x_sftime1, x_sftime2)
identical(x_sftime1, x_sftime3)

x_sftime1

## ----sftime-class-2-----------------------------------------------------------
# get the values from the time column
st_time(x_sftime1)

# set the values in the time column
st_time(x_sftime1) <- Sys.time()
st_time(x_sftime1)

# drop the time column to convert an sftime object to an sf object
st_drop_time(x_sftime1)
x_sftime1

# add a time column to an sf object converts it to an sftime object
st_time(x_sftime1, time_column_name = "time") <- Sys.time()
class(x_sftime1)

# These can also be used with pipes
x_sftime1 <-
  x_sftime1 %>%
  st_drop_time() %>%
  st_set_time(Sys.time(), time_column_name = "time")

## -----------------------------------------------------------------------------
# define the geometry column
g <- 
  st_sfc(
    st_point(c(1, 2)), 
    st_point(c(1, 3)), 
    st_point(c(2, 3)), 
    st_point(c(2, 1)), 
    st_point(c(3, 1))
  )

# crate sf object
x4_sf <- st_sf(a = 1:5, g, time = Sys.time() + 1:5)

# convert to sftime
x4_sftime <- st_as_sftime(x4_sf) 
class(x4_sftime)

## -----------------------------------------------------------------------------
# load sample data
x5_stars <- stars::read_ncdf(system.file("nc/bcsd_obs_1999.nc", package = "stars"), var = c("pr", "tas"))

# convert to sftime
x5_sftime <- st_as_sftime(x5_stars, time_column_name = "time")

## ---- error = TRUE------------------------------------------------------------
# failed conversion to sftime
x5_sftime <- st_as_sftime(x5_stars, merge = TRUE, time_column_name = "time")
x5_sftime <- st_as_sftime(x5_stars, long = FALSE, time_column_name = "time")

## -----------------------------------------------------------------------------
# get sample data
example(STI, package = "spacetime")
class(stidf)

# conversion to sftime
x1_sftime <- st_as_sftime(stidf)

## -----------------------------------------------------------------------------
# get a sample TracksCollection
x2_TracksCollection <- trajectories::rTracksCollection(p = 2, m = 3, n = 40)

# convert to sftime
x2_TracksCollection_sftime <- st_as_sftime(x2_TracksCollection)
x2_Tracks_sftime <- st_as_sftime(x2_TracksCollection@tracksCollection[[1]])
x2_Track_sftime <- st_as_sftime(x2_TracksCollection@tracksCollection[[1]]@tracks[[1]])

## ---- fig.width=7-------------------------------------------------------------
coords <- matrix(runif(100), ncol = 2)
g = st_sfc(lapply(1:50, function(i) st_point(coords[i,]) ))
sft <- st_sftime(a=1:50, g, time = as.POSIXct("2020-09-01 00:00:00") + 0:49 * 3600 * 6)

plot(sft, key.pos = 4)

plot(sft, number = 10, max.plot = 10)


## ---- eval=FALSE--------------------------------------------------------------
#  library(sftime)
#  tc <- st_tc(as.POSIXct("2020-09-01 08:00:00")-0:3*3600*24)
#  
#  tc

## ---- eval=FALSE--------------------------------------------------------------
#  tc[1]
#  
#  order(tc)
#  
#  sort(tc)

## ---- eval=FALSE--------------------------------------------------------------
#  # utility functions
#  as.character.interval <- function(x) {
#    paste0("[", x[1], ", ", x[2], "]")
#  }
#  
#  print.interval<- function(x) {
#    cat("Interval:", as.character(x))
#  }
#  
#  '[.intervals' <- function(x, i) {
#    sx <- unclass(x)[i]
#    class(sx) <- "intervals"
#    sx
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  # time interval definition
#  i1 <- c(5.3,12)
#  class(i1) <- "interval"
#  i2 <- c(3.1,6)
#  class(i2) <- "interval"
#  i3 <- c(1.4,6.9)
#  class(i3) <- "interval"
#  i4 <- c(1,21)
#  class(i4) <- "interval"
#  
#  intrvls <- list(i1, i2, i3, i4)
#  class(intrvls) <- c("intervals", class(intrvls))
#  # provide dedicated generic to xtfrm for class intervals
#  xtfrm.intervals <- function(is) sapply(is, xtfrm)

## ---- eval=FALSE--------------------------------------------------------------
#  # - sort by centre
#  xtfrm.interval <- function(i) mean(i)
#  
#  tc <- st_tc(intrvls)
#  tc
#  tc[1]
#  order(tc)
#  sort(tc)[1]

## ---- eval=FALSE--------------------------------------------------------------
#  # - sort by end
#  xtfrm.interval <- function(i) i[2]
#  tc <- st_tc(intrvls)
#  tc
#  tc[1]
#  order(tc)
#  sort(tc)[1]

## ---- eval=FALSE--------------------------------------------------------------
#  # - sort by start
#  xtfrm.interval <- function(i) i[1]
#  
#  tc <- st_tc(intrvls)
#  tc
#  tc[1]
#  order(tc)
#  sort(tc)[1]

## ---- eval=FALSE--------------------------------------------------------------
#  
#  g = st_sfc(st_point(1:2), st_point(c(1,3)), st_point(2:3), st_point(c(2,1)))
#  
#  # provide as.data.frame method. This is needed for sf::st_sf to not cause an error with lists when calling as.data.frame
#  as.data.frame.intervals <- function(x, ...) {
#    nm <- deparse1(substitute(x))
#    if (!"nm" %in% names(list(...)))
#      as.data.frame.vector(x, ..., nm = nm)
#    else as.data.frame.vector(x, ...)
#  }
#  
#  sft <- st_sftime(a = 1:4, g, time = sort(tc))
#  
#  sft <- st_sftime(g, time = st_tc(Sys.time()-0:3*3600*24))

## ---- fig.width=7, eval=FALSE-------------------------------------------------
#  coords <- matrix(runif(100), ncol = 2)
#  g = st_sfc(lapply(1:50, function(i) st_point(coords[i,]) ))
#  sf <- st_sf(a=1:50, g)
#  
#  sft <- st_sftime(cbind(sf, time = st_tc(as.POSIXct("2020-09-01 00:00:00")+0:49*3600*6)))
#  
#  plot(sft, key.pos = 4)
#  
#  plot(sft, number=10, max.plot=10)
#  

