library(sftime)

#### checks on tc  ####
# popular POSIXct
tc <- st_tc(as.POSIXct("2020-09-01 08:00:00")-0:3*3600*24)
tc
tc[1]
order(tc)
sort(tc)[1]

# custom interval class
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

# time interval definition
i1 <- c(5.3,12)
class(i1) <- "interval"
i2 <- c(3.1,6)
class(i2) <- "interval"
i3 <- c(1.4,2.9)
class(i3) <- "interval"
i4 <- c(1,2)
class(i4) <- "interval"

intrvls <- list(i1, i2, i3, i4)
class(intrvls) <- "intervals"

# provide dedicated generic to xtfrm for class intervals
xtfrm.intervals <- function(is) sapply(is, xtfrm)

# different sort definitions:
# - sort by centre
xtfrm.interval <- function(i) mean(i)

tc <- st_tc(intrvls)
tc
tc[1]
order(tc)
sort(tc)[1]

# - sort by end
xtfrm.interval <- function(i) i[2]
tc <- st_tc(intrvls)
tc
tc[1]
order(tc)
sort(tc)[1]

# - sort by start
xtfrm.interval <- function(i) i[1]

tc <- st_tc(intrvls)
tc
tc[1]
order(tc)
sort(tc)[1]

#### sftime construction ####
g = st_sfc(st_point(1:2), st_point(c(1,3)), st_point(2:3), st_point(c(2,1)))
sf <- st_sf(a=1:4, g)

stf <- st_sf_time(sf, sort(tc))

stf <- st_sf_time(sf, st_tc(Sys.time()-0:3*3600*24))

# # coercion
# library(spacetime)
# example(STI)
# 
# sft <- st_as_sftime(stidf)