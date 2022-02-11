sftime_join <- function(g, time_column_name, suffix_x = ".x") {
  
  if (!(time_column_name %in% names(g))) {
    time_column_name <- paste0(time_column_name, suffix_x)
    stopifnot(time_column_name %in% names(g))
  }

  st_as_sftime(g, time_column_name = time_column_name)
}

#' Tidyverse methods for \code{sftime} objects
#'
#' Tidyverse methods for \code{sftime} objects. Geometries are sticky, use 
#' \code{\link{as.data.frame}} to let \code{dplyr}'s own methods drop them. Use 
#' these methods without the \code{.sftime} suffix and after loading the 
#' tidyverse package with the generic (or after loading package tidyverse).
#' @name tidyverse
#' @inheritParams sf::tidyverse
NULL

#' @name tidyverse
inner_join.sftime <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sftime_join(NextMethod(), time_column_name = attr(x, "time_column"), suffix_x = suffix[[1]])
}

#' @name tidyverse
left_join.sftime <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sftime_join(NextMethod(), time_column_name = attr(x, "time_column"), suffix_x = suffix[[1]])
}

#' @name tidyverse
right_join.sftime <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sftime_join(NextMethod(), time_column_name = attr(x, "time_column"), suffix_x = suffix[[1]])
}

#' @name tidyverse
full_join.sftime <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sftime_join(NextMethod(), time_column_name = attr(x, "time_column"), suffix_x = suffix[[1]])
}

#' @name tidyverse
semi_join.sftime <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sftime_join(NextMethod(), time_column_name = attr(x, "time_column"), suffix_x = suffix[[1]])
}

#' @name tidyverse
anti_join.sftime <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sftime_join(NextMethod(), time_column_name = attr(x, "time_column"), suffix_x = suffix[[1]])
}


#' Spatial join, spatial filter for \code{sftime} objects
#' 
#' @name st_join
#' @param x An object of class \code{sftime} or \code{sf}.
#' @param y An object of class \code{sftime} or \code{sf}.
#' @inheritParams sf::st_join
#' @return An object of class \code{sftime}, joined based on geometry.
#' @details Alternative values for argument \code{join} are:
#' \itemize{
#'   \item \link[sf]{st_contains_properly}
#'   \item \link[sf]{st_contains}
#'   \item \link[sf]{st_covered_by}
#'   \item \link[sf]{st_covers}
#'   \item \link[sf]{st_crosses}
#'   \item \link[sf]{st_disjoint}
#'   \item \link[sf]{st_equals_exact}
#'   \item \link[sf]{st_equals}
#'   \item \link[sf]{st_is_within_distance}
#'   \item \link[sf]{st_nearest_feature}
#'   \item \link[sf]{st_overlaps}
#'   \item \link[sf]{st_touches}
#'   \item \link[sf]{st_within}
#'   \item any user-defined function of the same profile as the above
#' }
#' A left join returns all records of the \code{x} object with \code{y} fields 
#' for non-matched records filled with \code{NA} values; an inner join returns 
#' only records that spatially match.
#' 
#' @export
st_join.sftime <- function(x, y, join = st_intersects, ..., suffix = c(".x", ".y"), left = TRUE, largest = FALSE) {
  sftime_join(NextMethod(), time_column_name = attr(x, "time_column"), suffix_x = suffix[[1]])
}

#' @name st_join
#' @export
st_filter.sftime <- function(x, y, ..., .predicate = st_intersects) {
  reclass_sftime(NextMethod(), time_column_name = attr(x, "time_column"))
}