# Tidyverse methods (See also join.R)

#' Tidyverse methods for \code{sftime} objects
#'
#' Tidyverse methods for \code{sftime} objects. Geometries are sticky, use 
#' \code{\link{as.data.frame}} to let \code{dplyr}'s own methods drop them. Use 
#' these methods without the \code{.sftime} suffix and after loading the 
#' tidyverse package with the generic (or after loading package tidyverse).
#' @name tidyverse
#' @inheritParams sf::tidyverse
#' @inheritParams tidyr::pivot_longer
#' @param x An object of class \code{sftime}.
#' @param .data An object of class \code{stime}.
#' @return 
#' \itemize{
#'   \item For \code{_join} methods: An object of class \code{sftime} 
#'   representing the joining result of \code{x} and \code{y}. See 
#'   \code{\link[dplyr]{mutate-joins}}.
#'   \item For \code{filter}: See \code{\link[dplyr]{filter}}.
#'   \item For \code{arrange}: See \code{\link[dplyr]{arrange}}. 
#'   \item For \code{group_by} and \code{ungroup}: A grouped \code{sftime} 
#'   object. See \code{\link[dplyr]{arrange}}. 
#'   \item For \code{rowwise}: An \code{sftime} object. See 
#'   \code{\link[dplyr]{rowwise}}. 
#'   \item For \code{mutate} and \code{transmute}: See 
#'   \code{\link[dplyr]{mutate}}.
#'   \item For \code{select}: See \code{\link[dplyr]{select}}. If the active 
#'   time column is not explicitly selected, a \code{sf} object is returned. 
#'   \item For \code{rename}: See \code{\link[dplyr]{rename}}.    
#'   \item For \code{slice}: See \code{\link[dplyr]{slice}}.
#'   \item For \code{summarize} and \code{summarise}: See 
#'   \code{\link[dplyr]{summarise}}.  
#'   \item For \code{distinct}: See \code{\link[dplyr]{distinct}}.
#'   \item For \code{gather}: See \code{\link[tidyr]{gather}}. 
#' }
#' 
NULL

#' @rdname tidyverse
filter.sftime <- function(.data, ..., .dots) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
arrange.sftime <- function(.data, ..., .dots) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
group_by.sftime <- function(.data, ..., add = FALSE) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
ungroup.sftime <- function(.data, ...) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
rowwise.sftime <- function(.data, ...) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
mutate.sftime <- function(.data, ..., .dots) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
transmute.sftime <- function(.data, ..., .dots) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
select.sftime <- function(.data, ...) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
rename.sftime <- function(.data, ...) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
slice.sftime <- function(.data, ..., .dots) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
summarise.sftime <- function(.data, ..., .dots, do_union = TRUE, is_coverage = FALSE) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
summarize.sftime <- summarise.sftime

#' @rdname tidyverse
distinct.sftime <- function(.data, ..., .keep_all = FALSE) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
gather.sftime <- function(data, key, value, ..., na.rm = FALSE, convert = FALSE, factor_key = FALSE) {
  reclass_sftime(NextMethod(), time_column_name = attr(data, "time_column"))
}

#' @rdname tidyverse
pivot_longer.sftime <- function (data, cols, names_to = "name", names_prefix = NULL,
                             names_sep = NULL, names_pattern = NULL, names_ptypes = NULL,
                             names_transform = NULL, names_repair = "check_unique",
                             values_to = "value", values_drop_na = FALSE, values_ptypes = NULL,
                             values_transform = NULL, ...) {
  reclass_sftime(NextMethod(), time_column_name = attr(data, "time_column"))
}

#' @rdname tidyverse
spread.sftime <- function(data, key, value, fill = NA, convert = FALSE, drop = TRUE,
                      sep = NULL) {
  reclass_sftime(NextMethod(), time_column_name = attr(data, "time_column"))
}

#' @rdname tidyverse
sample_n.sftime <- function(tbl, size, replace = FALSE, weight = NULL, .env = parent.frame()) {
  reclass_sftime(NextMethod(), time_column_name = attr(tbl, "time_column"))
}

#' @rdname tidyverse
sample_frac.sftime <- function(tbl, size = 1, replace = FALSE, weight = NULL, .env = parent.frame()) {
  reclass_sftime(NextMethod(), time_column_name = attr(tbl, "time_column"))
}

#' @rdname tidyverse
nest.sftime <- function (.data, ...) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
separate.sftime <- function(data, col, into, sep = "[^[:alnum:]]+", remove = TRUE,
                       convert = FALSE, extra = "warn", fill = "warn", ...) {
  reclass_sftime(NextMethod(), time_column_name = attr(data, "time_column"))
}

#' @rdname tidyverse
separate_rows.sftime <- function(data, ..., sep = "[^[:alnum:]]+", convert = FALSE) {
  reclass_sftime(NextMethod(), time_column_name = attr(data, "time_column"))
}

#' @name tidyverse
unite.sftime <- function(data, col, ..., sep = "_", remove = TRUE) {
  reclass_sftime(NextMethod(), time_column_name = attr(data, "time_column"))
}

#' @name tidyverse
unnest.sftime = function(data, ..., .preserve = NULL) {
  reclass_sftime(NextMethod(), time_column_name = attr(data, "time_column"))
}
