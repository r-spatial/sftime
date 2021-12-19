#' Get, set, or replace time information
#'
#' @param obj An object of class \code{sftime} or \code{tc}.
#' @param x An object of class \code{sftime} or \code{sf}.
#' @param value An object of class \code{tc}, or \code{character}, or 
#' \code{NULL}.
#' @param ... Additional arguments; Ignored.
#' 
#' @details When applied to an \code{sf} object and when \code{value} is an 
#' object of class \code{tc}, \code{st_set_time} and \code{st_time<-} will first 
#' check for the existence of an attribute \code{tc_column} and overwrite that, 
#' or else look for list-columns of class \code{tc} and overwrite the first of 
#' that, or else write the time column to a new column named \code{time}.  
#' 
#' In case \code{value} is character and \code{x} is of class \code{sftime}, 
#' the "active" time column is set to \code{x[[value]]}.
#'
#' The replacement function applied to \code{sftime} objects will overwrite the 
#' time column, if \code{value} is \code{NULL}, it will remove it and coerce 
#' \code{x} to an \code{sftime} object.
#' 
#' @return \code{st_time} returns an object of class \code{\link[=st_tc]{tc}}. 
#' Assigning a \code{tc} object to an \code{sf} object creates an 
#' \code{\link[=st_sftime]{sftime}} object. Assigning a \code{tc} object to an \code{sftime}
#' object replaces the time column.  
#' @export
st_time <- function(obj, ...) UseMethod("st_time")

#' @rdname st_time
#' @export
`st_time<-` = function(x, value) UseMethod("st_time<-")

#' @rdname st_time
#' @export
#' @examples
#' ## extracting tc objects
#' 
#' # from tc object
#' st_time(st_tc(Sys.time() + 1:5)) 
#' 
st_time.tc <- function(obj, ...) obj

#' @rdname st_time
#' @export
#' @examples
#' # from sftime object
#' g <- st_sfc(st_point(1:2))
#' tc <- st_tc(Sys.time())
#' x <- st_sftime(st_sf(a = 3, g, time = tc))
#' st_time(x) 
#' 
st_time.sftime <- function(obj, ...) {
  ret <-  obj[[attr(obj, "tc_column")]]
  if (!inherits(ret, "tc")) # corrupt!
    stop('attr(obj, "tc_column") does not point to a geometry column.\nDid you rename it, without setting st_time(obj) <- "newname"?')
  ret
}

#' @rdname st_time
#' @export
#' @examples
#' ## assign tc objects
#' 
#' # to sf object
#' x <- st_sf(a = 3, g)
#' st_time(x) <- tc
#' x
#' 
`st_time<-.sf` <- function(x, value) {
  stopifnot(inherits(value, "tc"))
  stopifnot(nrow(x) == length(value))
  
  a <- vapply(x, function(v) inherits(v, "tc"), TRUE)
  if (any(a)) {
    w <- which(a)
    tc_col <- attr(x, "tc_column")
    if (! is.null(tc_col))
      x[[ tc_col ]] <- value
    else {
      if (length(w) > 1)
        warning("overwriting first time column")
      x[[ which(a)[1L] ]] <- value
    }
  } else {
    x$time <- value 
  }
  st_sftime(x)
}

#' @rdname st_time
#' @export
#' @examples
#' # to sftime object
#' x <- st_sftime(st_sf(a = 3, g, time = tc))
#' st_time(x) <- st_tc(Sys.time())
#' 
#' ## remove time column from sftime object
#' st_time(x) <- NULL
#' 
`st_time<-.sftime` = function(x, value) {
  if (! is.null(value)) {
    stopifnot(inherits(value, "tc") || is.character(value))
    if (inherits(value, "tc"))
      stopifnot(nrow(x) == length(value))
    if (is.character(value))
      stopifnot(inherits(x[[value]], "tc"))
  }
  
  if (!is.null(value) && is.character(value)) # set flag to another column:
    attr(x, "tc_column") <- value
  else # replace, remove, or set list-column
    x[[attr(x, "tc_column")]] <- value
  
  if (is.null(value))
    structure(x, tc_column = NULL, class = setdiff(class(x), "sftime"))
  else
    st_sftime(x)
}

#' @rdname st_time
#' @export
#' @examples 
#' ## pipe-friendly
#' 
#' # assign time column to sf object
#' x <- st_sf(a = 3, g)
#' x <- st_set_time(x, tc)
#' 
#' # remove time column from sftime object
#' st_set_time(x, NULL)
#' 
st_set_time <- function(x, value) {
  st_time(x) <- value
  x
}

#' @rdname st_time
#' @export
#' @details \code{st_drop_time} drops the time column of its argument, and 
#' reclasses it accordingly.
#' @examples 
#' ## drop time column and class
#' 
#' # same as x <- st_set_time(x, NULL)
#' st_drop_time(x)
#' 
st_drop_time = function(x) {
  if (!inherits(x, "sftime"))
    stop("st_drop_time only works with objects of class sftime")
  st_set_time(x, NULL)
}