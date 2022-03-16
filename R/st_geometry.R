#' Drops the geometry column of \code{sftime} objects
#' 
#' Drops the geometry column of an \code{sftime} object. This will also drop 
#' the \code{sftime} class attribute and \code{time_column} attribute. 
#' 
#' @name st_geometry
#' @inheritParams sf::st_drop_geometry
#' @param x An \code{sftime} object.
#' @return \code{x} without geometry column and without \code{sftime} and 
#' \code{sf} class.
#' @examples 
#' # dropping the geometry column will also drop the `sftime` class:
#' g <- st_sfc(st_point(1:2))
#' time <- Sys.time()
#' x <- st_sftime(a = 3, g, time = time)
#' st_drop_geometry(x)
#' 
#' @export
st_drop_geometry.sftime <- function(x, ...) {
  class(x) <- setdiff(class(x), "sftime")
  NextMethod()
}