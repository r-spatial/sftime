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
#' @examples
#' ## filter
#' filter(x1, a <= 2)
#' 
 filter.sftime <- function(.data, ..., .dots) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
#' @examples
#' ## arrange
#' arrange(x1, dplyr::desc(a))
#' 
 arrange.sftime <- function(.data, ..., .dots) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
#' @examples
#' ## group_by
#' group_by(x1, time)
#' 
 group_by.sftime <- function(.data, ..., add = FALSE) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
#' @examples
#' ## ungroup
#' ungroup(group_by(x1, time))
#' 
 ungroup.sftime <- function(.data, ...) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
#' @examples
#' ## rowwise
#' x1 %>%
#'   mutate(a1 = 5:7) %>%
#'   rowwise() %>%
#'   mutate(a2 = mean(a, a1))
#' 
 rowwise.sftime <- function(.data, ...) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
#' @examples
#' ## mutate
#' x1 %>%
#'   mutate(a1 = 5:7)
#' 
 mutate.sftime <- function(.data, ..., .dots) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
#' @examples
#' ## transmute
#' x1 %>%
#'   transmute(a1 = 5:7)
#' 
 transmute.sftime <- function(.data, ..., .dots) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
#' @examples
#' ## select
#' x1 %>%
#'   select(-time) %>%
#'   select(geometry)
#' 
 select.sftime <- function(.data, ...) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
#' @examples
#' ## rename
#' x1 %>%
#'   rename(a1 = a)
#' 
 rename.sftime <- function(.data, ...) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
#' @examples
#' ## slice
#' x1 %>%
#'   slice(1:2)
#' 
 slice.sftime <- function(.data, ..., .dots) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
#' @examples
#' ## summarise
#' x1 %>%
#'   summarise(time = mean(time))
#'   
#' x1 %>%
#'   summarize(time = mean(time))
#' 
 summarise.sftime <- function(.data, ..., .dots, do_union = TRUE, is_coverage = FALSE) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
 summarize.sftime <- summarise.sftime

#' @rdname tidyverse
#' @examples
#' ## distinct
#' x1 %>%
#'   distinct(geometry)
#' 
 distinct.sftime <- function(.data, ..., .keep_all = FALSE) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @rdname tidyverse
#' @examples
#' ## gather
#' library(tidyr)
#' x1 %>%
#'   mutate(a1 = 5:7) %>%
#'   gather(key = "variable", value = "value", a, a1)
#' 
 gather.sftime <- function(data, key, value, ..., na.rm = FALSE, convert = FALSE, factor_key = FALSE) {
  reclass_sftime(NextMethod(), time_column_name = attr(data, "time_column"))
}

#' @rdname tidyverse
#' @examples
#' ## pivot_longer
#' x1 %>%
#'   mutate(a1 = 5:7) %>%
#'   pivot_longer(cols = c("a", "a1"), names_to = "variable", values_to = "value")
#' 
 pivot_longer.sftime <- function (data, cols, names_to = "name", names_prefix = NULL,
                             names_sep = NULL, names_pattern = NULL, names_ptypes = NULL,
                             names_transform = NULL, names_repair = "check_unique",
                             values_to = "value", values_drop_na = FALSE, values_ptypes = NULL,
                             values_transform = NULL, ...) {
  reclass_sftime(NextMethod(), time_column_name = attr(data, "time_column"))
}

#' @rdname tidyverse
#' @examples
#' ## spread
#' x1 %>%
#'   mutate(a1 = 5:7) %>%
#'   gather(key = "variable", value = "value", a, a1) %>%
#'   spread(key = "variable", value = "value")
#' 
 spread.sftime <- function(data, key, value, fill = NA, convert = FALSE, drop = TRUE,
                      sep = NULL) {
  reclass_sftime(NextMethod(), time_column_name = attr(data, "time_column"))
}

#' @rdname tidyverse
#' @examples
#' ## sample_n
#' set.seed(234)
#' x1 %>%
#'   sample_n(size = 10, replace = TRUE)
#' 
 sample_n.sftime <- function(tbl, size, replace = FALSE, weight = NULL, .env = parent.frame()) {
  reclass_sftime(NextMethod(), time_column_name = attr(tbl, "time_column"))
}

#' @rdname tidyverse
#' @examples
#' ## sample_frac
#' x1 %>%
#'   sample_frac(size = 10, replace = TRUE) %>%
#'   sample_frac(size = 0.1, replace = FALSE)
#' 
 sample_frac.sftime <- function(tbl, size = 1, replace = FALSE, weight = NULL, .env = parent.frame()) {
  reclass_sftime(NextMethod(), time_column_name = attr(tbl, "time_column"))
}

#' @rdname tidyverse
#' @examples
#' ## nest
#' x1 %>%
#'   nest(a1 = -time)
#' 
 nest.sftime <- function (.data, ...) {
  reclass_sftime(NextMethod(), time_column_name = attr(.data, "time_column"))
}

#' @name tidyverse
#' @examples
#' ## unnest
#' x1 %>%
#'   mutate(a1 = list(1, c(1, 2), 5)) %>%
#'   unnest(a1)
#' 
 unnest.sftime = function(data, ..., .preserve = NULL) {
  reclass_sftime(NextMethod(), time_column_name = attr(data, "time_column"))
}

#' @rdname tidyverse
#' @examples
#' ## separate
#' x1 %>%
#'   mutate(x = c(NA, "a.b", "a.d")) %>%
#'   separate(x, c("A", "B"))
#' 
 separate.sftime <- function(data, col, into, sep = "[^[:alnum:]]+", remove = TRUE,
                       convert = FALSE, extra = "warn", fill = "warn", ...) {
  
  time_column_name <- attr(data, "time_column")
  class(data) <- setdiff(class(data), "sftime")
  
  # modified from sftime (tidyverse.R)
  if (!requireNamespace("rlang", quietly = TRUE))
    stop("rlang required: install first?")
  col <- rlang::enquo(col)
  
  res <- tidyr::separate(data, !!col, into = into,
                  sep = sep, remove = remove, convert = convert, extra = extra, fill = fill, ...)
  reclass_sftime(res, time_column_name = time_column_name)
  
}

#' @name tidyverse
#' @examples
#' ## unite
#' x1 %>%
#'   mutate(x = c(NA, "a.b", "a.d")) %>%
#'   separate(x, c("A", "B")) %>%
#'   unite(x, c("A", "B"))
#'   
unite.sftime <- function(data, col, ..., sep = "_", remove = TRUE) {
  reclass_sftime(NextMethod(), time_column_name = attr(data, "time_column"))
}

#' @rdname tidyverse
#' @examples
#' ## separate_rows
#' x1 %>%
#'   mutate(z = c("1", "2,3,4", "5,6")) %>%
#'   separate_rows(z, convert = TRUE)
#' 
 separate_rows.sftime <- function(data, ..., sep = "[^[:alnum:]]+", convert = FALSE) {
  reclass_sftime(NextMethod(), time_column_name = attr(data, "time_column"))
}
