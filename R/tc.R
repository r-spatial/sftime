#### construction ####

#' Checks whether a vector or list is sortable
#' 
#' Checks whether a vector or list is sortable. This is the condition to coerce
#' a vector or list to a \code{tc} object.
#' 
#' @name is_sortable
#' @param x The object to check.
#' @return \code{TRUE} if \code{x} passes the check, else \code{FALSE}.
#' 
#' @details Checks whether the provided object can be handled by 
#' \code{\link{order}}. A couple of basic types are whitelisted. However, custom 
#' types can be defined when they provide a dedicated generic to \link{xtfrm}. 
#' Note that a \code{list} can only be sorted with \link{atomic} values. See the 
#' examples below for a template.
#' 
#' @examples
#' x <- Sys.time() + 5:1 * 3600*24
#' sort(x)
#' is_sortable(x)
#' 
#' @importFrom utils methods
is_sortable <- function(x) {
  # can x be sorted?
  # sort.default checks 'is.object(x)' and uses 'order' to subset and sort the object 
  # lists and vectors are no objects, sort then uses sort.int which can only handle atomic values
  
  # Examples:
  # x <- Sys.time() + 5:1 * 3600*24
  # x <- yearmon(2020+c(5:0)/12)
  # x <- yearqtr(2020+c(5:0)/4)
  # x <- factor(LETTERS[sample(26, replace = T)], levels=LETTERS[sample(26)])
  # sort(x)
  # order(x)
  # class(x)
  any(vapply(class(x), function(clsnm) clsnm %in% c("numeric", "POSIXct", "POSIXlt", "Date", "yearmon", "yearqtr", "factor"), TRUE)) || # have a list of wellknown exceptions
    paste("xtfrm", class(x), sep=".") %in% methods(class = class(x)) # check for function 'xtfrm.[CLASSNAME]' which is used by 'order' which in turn is used by sort.default
  
}

#' Construct a \code{tc} object from a vector with time information
#'
#' An object of class \code{tc} stores time information and can be used as
#' time column in a \code{\link[sftime:st_sftime]{sftime}} object.
#'
#' @param x A vector or list.
#' 
#' @return An object of class \code{tc}.
#' @export
st_tc <- function(x) {
  stopifnot(is_sortable(x))
  structure(x, class = c("tc", class(x)))
}


#### subsetting ####

#' Subsetting of \code{tc} objects
#'
#' @param x The \code{\link{tc}} object to be subsetted.
#' @param i Any subsetting expression supported by the temporal class provided 
#' by the \code{tc} object.
#' @param ... any further arguments for the underlying subsetting method.
#'
#' @return A \code{tc} object representing a subset of \code{x}.
#' @export
"[.tc" <- function(x, i, ...) {
  st_tc(NextMethod())
}

#### printing ####

#' \code{print} generic for \code{tc}
#'
#' @param x An object of class \code{tc}.
#' @param print_number_features A logical value; whether the number of features 
#' shall be printed (\code{TRUE}) or not (\code{FALSE}).
#' @param ... Currently unused arguments, for compatibility.
#' 
#' @return \code{x} (invisible).
#' @export
print.tc <- function(x, ..., print_number_features = FALSE) {
  
  stopifnot(is.logical(print_number_features) && length(print_number_features) == 1)
  
  ord <- order(x)
  x_min <- x[[which.min(ord)]]
  x_max <- x[[which.max(ord)]]
  x_class <- class(x)
  x_is_value <- length(x) == 1
  
  cat(paste0("Time column with ", 
             ifelse(!print_number_features, "",
                    paste0(length(x), ifelse(x_is_value, " feature of ", " features, each of "))),
             ifelse(length(x_class) == 2, "class", "classes"), ": \'", 
             paste0(x_class[-1], collapse="\', \'"), "\'.\n",
             ifelse(x_is_value, 
                    paste0("Representing ", x_min, ".\n" ), 
                    paste0("Ranging from ", x_min, " to ", x_max, ".\n" ))))
  
  invisible(x)
}