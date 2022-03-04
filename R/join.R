#' Helper function to adjust class and attributes of \code{sftime} objects when joining.
#' 
#' @param x An object to be reclassed to the \code{\link[=st_sftime]{sftime}} 
#' class.
#' @param time_colmn_name A character value; name of the original active time 
#' column in \code{x} before joining.
#' @param suffix_x A character value representing the suffix to add to the name
#' of the time column in the \code{time_column} attribute when name repair 
#' during joining changed the name of the time column.
#' 
#' @return \code{x} as \code{sftime} object with adjusted \code{time_column}
#' attribute.
#' 
#' @keywords internal
#' @noRd
sftime_join <- function(x, time_column_name, suffix_x = ".x") {
  
  if (!(time_column_name %in% names(x))) {
    time_column_name <- paste0(time_column_name, suffix_x)
    stopifnot(time_column_name %in% names(x))
  }

  st_as_sftime(x, time_column_name = time_column_name)
}

## Tidyverse joins (see also tidyverse.R)

#' @name tidyverse
#' @examples 
#' g1 <- st_sfc(st_point(1:2), st_point(c(5, 8)), st_point(c(2, 9)))
#' x1 <- st_sftime(a = 1:3, geometry = g1, time = Sys.time())
#' 
#' g2 <- st_sfc(st_point(c(4, 6)), st_point(c(4, 6)), st_point(c(4, 6)))
#' x2 <- st_sftime(a = 2:4, geometry = g2, time = Sys.time())
#' 
#' library(dplyr)
#' 
#' ## inner_join
#' inner_join(x1, as.data.frame(x2), by = "a") # note: the active time column is
#' # time.x and the active geometry column geometry.x
#' 
#' inner_join(x2, as.data.frame(x1), by = "a")
#' 
inner_join.sftime <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sftime_join(NextMethod(), time_column_name = attr(x, "time_column"), suffix_x = suffix[[1]])
}

#' @name tidyverse
#' @examples 
#' ## left_join
#' left_join(x1, as.data.frame(x2), by = "a")
#' 
#' left_join(x2, as.data.frame(x1), by = "a")
#' 
left_join.sftime <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sftime_join(NextMethod(), time_column_name = attr(x, "time_column"), suffix_x = suffix[[1]])
}

#' @name tidyverse
#' @examples 
#' ## right_join
#' right_join(x1, as.data.frame(x2), by = "a")
#' 
#' right_join(x2, as.data.frame(x1), by = "a")
#' 
right_join.sftime <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sftime_join(NextMethod(), time_column_name = attr(x, "time_column"), suffix_x = suffix[[1]])
}

#' @name tidyverse
#' @examples 
#' ## full_join
#' full_join(x1, as.data.frame(x2), by = "a")
#' 
#' full_join(x2, as.data.frame(x1), by = "a")
#' 
full_join.sftime <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sftime_join(NextMethod(), time_column_name = attr(x, "time_column"), suffix_x = suffix[[1]])
}

#' @name tidyverse
#' @examples
#' ## semi_join
#' semi_join(x1, as.data.frame(x2), by = "a")
#' 
#' semi_join(x2, as.data.frame(x1), by = "a")
#' 
semi_join.sftime <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sftime_join(NextMethod(), time_column_name = attr(x, "time_column"), suffix_x = suffix[[1]])
}

#' @name tidyverse
#' @examples
#' ## anti_join
#' anti_join(x1, as.data.frame(x2), by = "a")
#' 
#' anti_join(x2, as.data.frame(x1), by = "a")
#' 
anti_join.sftime <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sftime_join(NextMethod(), time_column_name = attr(x, "time_column"), suffix_x = suffix[[1]])
}


#' Spatial join, spatial filter for \code{sftime} objects
#' 
#' @name st_join
#' @param x An object of class \code{sftime} or \code{sf}.
#' @param y An object of class \code{sftime} or \code{sf}.
#' @param join A geometry predicate function with the same profile as 
#' \code{\link[sf:geos_binary_pred]{st_intersects}}; see details.
#' @inheritParams sf::st_join
#' @return An object of class \code{sftime}, joined based on geometry.
#' @details Alternative values for argument \code{join} are:
#' \itemize{
#'   \item \link[sf:geos_binary_pred]{st_contains_properly}
#'   \item \link[sf:geos_binary_pred]{st_contains}
#'   \item \link[sf:geos_binary_pred]{st_covered_by}
#'   \item \link[sf:geos_binary_pred]{st_covers}
#'   \item \link[sf:geos_binary_pred]{st_crosses}
#'   \item \link[sf:geos_binary_pred]{st_disjoint}
#'   \item \link[sf:geos_binary_pred]{st_equals_exact}
#'   \item \link[sf:geos_binary_pred]{st_equals}
#'   \item \link[sf:geos_binary_pred]{st_is_within_distance}
#'   \item \link[sf:geos_binary_pred]{st_nearest_feature}
#'   \item \link[sf:geos_binary_pred]{st_overlaps}
#'   \item \link[sf:geos_binary_pred]{st_touches}
#'   \item \link[sf:geos_binary_pred]{st_within}
#'   \item any user-defined function of the same profile as the above
#' }
#' A left join returns all records of the \code{x} object with \code{y} fields 
#' for non-matched records filled with \code{NA} values; an inner join returns 
#' only records that spatially match.
#' 
#' @examples
#' g1 <- st_sfc(st_point(c(1,1)), st_point(c(2,2)), st_point(c(3,3)))
#' x1 <- st_sftime(a = 1:3, geometry = g1, time = Sys.time())
#' 
#' g2 <- st_sfc(st_point(c(10,10)), st_point(c(2,2)), st_point(c(2,2)), st_point(c(3,3)))
#' x2 <- st_sftime(a = 11:14, geometry = g2, time = Sys.time())
#' 
#' ## st_join
#' 
#' # left spatial join with st_intersects
#' st_join(x1, x2)
#' 
#' # inner spatial join with st_intersects
#' st_join(x1, x2, left = FALSE)
#' 
#' @export
st_join.sftime <- function(x, y, join = st_intersects, ..., suffix = c(".x", ".y"), left = TRUE, largest = FALSE) {
  sftime_join(NextMethod(), time_column_name = attr(x, "time_column"), suffix_x = suffix[[1]])
}

#' @name st_join
#' @param .predicate A geometry predicate function with the same profile as 
#' \code{\link[sf:geos_binary_pred]{st_intersects}}; see details.
#' @examples
#' ## st_filter
#' 
#' st_filter(x1, x2)
#' st_filter(x2, x1)
#' 
#' @export
st_filter.sftime <- function(x, y, ..., .predicate = st_intersects) {
  reclass_sftime(NextMethod(), time_column_name = attr(x, "time_column"))
}