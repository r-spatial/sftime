#' Cast geometry to another type: either simplify, or cast explicitly
#' 
#' @name st_cast
#' @inheritParams sf::st_cast
#' @param x An object of class \code{sftime}.
#' @return \code{x} with changed geometry type.
#' @examples
#' # cast from POINT to LINESTRING
#' g <- st_sfc(st_point(1:2), st_point(c(2, 4)))
#' time <- Sys.time()
#' x <- 
#'   st_sftime(a = 3:4, g, time = time) %>%
#'   dplyr::group_by(time) %>%
#'   dplyr::summarize(do_union = TRUE) %>%
#'   st_cast(to = "LINESTRING")
#' @export
st_cast.sftime <- function(x, to, ..., warn = TRUE, do_split = TRUE) {
  reclass_sftime(NextMethod(), attr(x, "time_column"))
}