#' crop an \code{sftime} object to a specific rectangle
#' 
#' @param x An object of class \code{sftime}.
#' @param y A numeric vector with named elements \code{xmin}, \code{ymin}, 
#' \code{xmax} and \code{ymax}, or an object of class \code{bbox}, or an object 
#' for which there is an \code{\link[sf:st_bbox]{st_bbox}} method to convert it 
#' to a \code{bbox} object.
#' @param ... Additional arguments; Ignored.
#' @return \code{x} cropped using \code{y}.
#' @details
#' See \code{\link[sf:st_crop]{st_crop}}.
#' @examples
#' # modified from sf:
#' box <- c(xmin = 0, ymin = 0, xmax = 1, ymax = 1)
#' pol <- sf::st_sfc(sf::st_buffer(sf::st_point(c(0.5, 0.5)), 0.6))
#' pol_sftime <- st_sftime(a = 1, geom = pol, time = Sys.time() + 1:2 * 1000)
#' 
#' pol_sftime_cropped <- sf::st_crop(pol_sftime, sf::st_bbox(box))
#' 
#' class(pol_sftime_cropped)
#' plot(pol_sftime_cropped)
#' @export
st_crop.sftime <- function(x, y, ...) {
  reclass_sftime(NextMethod(), time_column_name = attr(x, "time_column"))
}