#' Get, set, or replace time information
#'
#' @param obj An object of class \code{sftime}.
#' @param x An object of class \code{sftime} or \code{sf}.
#' @param ... Additional arguments; Ignored.
#' @param time_column_name Character value; The name of the column to set as 
#' active time column in \code{x}. 
#' @param value An object for which \code{\link{is_sortable}} returns 
#' \code{TRUE} or an object of class \code{character}, or \code{NULL}.
#' 
#' @details In case \code{value} is character and \code{x} is of class 
#' \code{sftime}, the active time column (as indicated by attribute 
#' \code{time_column}) is set to \code{x[[value]]}.
#'
#' The replacement function applied to \code{sftime} objects will overwrite the 
#' active time column, if \code{value} is \code{NULL}, it will remove it and 
#' coerce \code{x} to an \code{sftime} object.
#' 
#' @return \code{st_time} returns the content of the active time column of an
#' \code{sftime} object. 
#' Assigning an object for which \code{\link{is_sortable}} returns \code{TRUE} 
#' to an \code{sf} object creates an \code{\link[=st_sftime]{sftime}} object. 
#' Assigning an object for which \code{\link{is_sortable}} returns \code{TRUE} 
#' to an \code{sftime} object replaces the active time column by this object.  
#' @export
st_time <- function(obj, ...) UseMethod("st_time")

#' @rdname st_time
#' @export
`st_time<-` = function(x, ..., value) UseMethod("st_time<-")

#' @rdname st_time
#' @export
#' @examples
#' # from sftime object
#' g <- st_sfc(st_point(1:2))
#' time <- Sys.time()
#' x <- st_sftime(a = 3, g, time = time)
#' st_time(x) 
#' 
st_time.sftime <- function(obj, ...) {
  ret <-  obj[[attr(obj, "time_column")]]
  if (!is_sortable(ret)) # corrupt!
    stop('attr(obj, "time_column") does not point to a time column.\nDid you rename it, without setting st_time(obj) <- "newname"?')
  ret
}

#' @rdname st_time
#' @export
#' @examples
#' ## assign a vector with time information
#' 
#' # to sf object
#' x <- st_sf(a = 3, g)
#' st_time(x) <- time
#' x
#' 
`st_time<-.sf` <- function(x, ..., time_column_name = "time", value) {
  stopifnot(is_sortable(value))
  stopifnot(is.character(time_column_name) && length(time_column_name) == 1)
  
  x[[time_column_name]] <- value 
  st_sftime(x, time_column_name = time_column_name)
}

#' @rdname st_time
#' @export
#' @examples
#' # to sftime object
#' x <- st_sftime(a = 3, g, time = time)
#' st_time(x) <- Sys.time()
#' 
#' ## change the time column to another already existing column
#' st_time(x) <- "a"
#' 
#' ## remove time column from sftime object
#' st_time(x) <- NULL
#' 
`st_time<-.sftime` = function(x, ..., value) {
  
  if (! is.null(value)) {
    stopifnot(is_sortable(value) || is.character(value))
  }
  
  if (! is.null(value) && is.character(value) && length(value) == 1 && value %in% colnames(x)) {# set flag to another column
    attr(x, "time_column") <- value 
  } else {# replace, remove, or set list-column
    x[[attr(x, "time_column")]] <- value
  }
  
  if (is.null(value))
    structure(x, time_column = NULL, class = setdiff(class(x), "sftime"))
  else
    st_as_sftime(x)
}

#' @rdname st_time
#' @export
#' @examples 
#' ## pipe-friendly
#' 
#' # assign time column to sf object
#' x <- st_sf(a = 3, g)
#' x <- st_set_time(x, time)
#' 
#' # remove time column from sftime object
#' st_set_time(x, NULL)
#' 
st_set_time <- function(x, value, ...) {
  st_time(x, ...) <- value
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
    stop("`st_drop_time` only works with objects of class sftime")
  st_set_time(x, NULL)
}