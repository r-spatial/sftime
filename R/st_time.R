#' Get, set, or replace time information
#'
#' @param obj An object of class \code{sftime} or \code{tc}.
#' @param ... Additional arguments; Ignored.
#' 
#' @return An object of class \code{\link[=st_tc]{tc}}
#' @export
st_time <- function(obj, ...) UseMethod("st_time")

#' @rdname st_time
#' @export
st_time.tc <- function(obj, ...) obj