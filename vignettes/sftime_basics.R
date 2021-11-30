## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(sftime)
tc <- st_tc(as.POSIXct("2020-09-01 08:00:00")-0:3*3600*24)

tc

## -----------------------------------------------------------------------------
tc[1]

order(tc)

sort(tc)

## -----------------------------------------------------------------------------
# utility functions
as.character.interval <- function(x) {
  paste0("[", x[1], ", ", x[2], "]")
}

print.interval<- function(x) {
  cat("Interval:", as.character(x))
}

'[.intervals' <- function(x, i) {
  sx <- unclass(x)[i]
  class(sx) <- "intervals"
  sx
}

## -----------------------------------------------------------------------------
# time interval definition
i1 <- c(5.3,12)
class(i1) <- "interval"
i2 <- c(3.1,6)
class(i2) <- "interval"
i3 <- c(1.4,6.9)
class(i3) <- "interval"
i4 <- c(1,21)
class(i4) <- "interval"

intrvls <- list(i1, i2, i3, i4)
class(intrvls) <- "intervals"
# provide dedicated generic to xtfrm for class intervals
xtfrm.intervals <- function(is) sapply(is, xtfrm)

## -----------------------------------------------------------------------------
# - sort by centre
xtfrm.interval <- function(i) mean(i)

tc <- st_tc(intrvls)
tc
tc[1]
order(tc)
sort(tc)[1]

## -----------------------------------------------------------------------------
# - sort by end
xtfrm.interval <- function(i) i[2]
tc <- st_tc(intrvls)
tc
tc[1]
order(tc)
sort(tc)[1]

## -----------------------------------------------------------------------------
# - sort by start
xtfrm.interval <- function(i) i[1]

tc <- st_tc(intrvls)
tc
tc[1]
order(tc)
sort(tc)[1]

## -----------------------------------------------------------------------------
g = st_sfc(st_point(1:2), st_point(c(1,3)), st_point(2:3), st_point(c(2,1)))

# provide as.data.frame method. This is needed for sf::st_sf to not cause an error with lists when calling as.data.frame
as.data.frame.intervals <- function(x, ...) {
  nm <- deparse1(substitute(x))
  if (!"nm" %in% names(list(...))) 
    as.data.frame.vector(x, ..., nm = nm)
  else as.data.frame.vector(x, ...)
}

sft <- st_sftime(a = 1:4, g, time = sort(tc))

sft <- st_sftime(g, time = st_tc(Sys.time()-0:3*3600*24))

## ---- fig.width=7-------------------------------------------------------------
coords <- matrix(runif(100), ncol = 2)
g = st_sfc(lapply(1:50, function(i) st_point(coords[i,]) ))
sf <- st_sf(a=1:50, g)

sft <- st_sftime(cbind(sf, time = st_tc(as.POSIXct("2020-09-01 00:00:00")+0:49*3600*6)))

plot(sft, key.pos = 4)

plot(sft, number=10, max.plot=10)


