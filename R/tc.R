#### construction ####

#' Checks whether a vector or list is sortable
#' 
#' Checks whether a vector or list is sortable. This is the condition to coerce
#' a vector or list to a \code{tc} object.
#' 
#' @name is_sortable
#' @param x The object to check.
#' @return \code{TRUE} if \code{x} passes the check, else \code{FALSE}.
#' @keywords internal
#' 
#' @details Checks whether the provided object can be handled by 
#' \code{\link{order}}. A couple of basic types are whitelisted. However, custom 
#' types can be defined when they provide a dedicated generic to \link{xtfrm}. 
#' Note that a \code{list} can only be sorted with \link{atomic} values. See the 
#' examples below for a template.
#' 
#' @examples
#' x <- Sys.time() + 5:1 * 3600 * 24
#' sort(x)
#' is_sortable(x)
#' 
#' @importFrom utils methods
#' @export
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
  any(vapply(class(x), function(y) y %in% c("integer", "numeric", "POSIXct", "POSIXlt", "Date", "yearmon", "yearqtr", "factor"), TRUE)) || # have a list of wellknown exceptions
    any(vapply(class(x), function(y) paste("xtfrm", y, sep=".") %in% methods(class = y), TRUE)) # check for function 'xtfrm.[CLASSNAME]' which is used by 'order' which in turn is used by sort.default
  
}

#' Construct a \code{tc} object from a vector with time information
#'
#' An object of class \code{tc} stores time information and can be used as
#' time column in a \code{\link[=st_sftime]{sftime}} object.
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

#' Extract or replace elements in a \code{tc} object
#'
#' @name st_tc-extract
#' @param x The \code{\link[=st_tc]{tc}} object from which to extract 
#' element(s) or in which to replace elements.
#' @param i Any subsetting expression supported by the temporal class provided 
#' by the \code{tc} object.
#' @param ... any further arguments for the underlying subsetting method.
#'
#' @return A \code{tc} object representing a subset of \code{x} or \code{x} 
#' with replaced values.
#' @export
"[.tc" <- function(x, i, ...) {
  st_tc(NextMethod())
}

#' @rdname st_tc-extract
#' @param value An object with which to substitute elements in \code{x}. Must 
#' be of the same class as elements in \code{x}.
#' @export
"[<-.tc" <- function(x, i, value) {
  st_tc(NextMethod())
}


#### combine ####

#' Combining \code{tc} objects
#' 
#' @param ... One or multiple \code{tc} objects to combine.
#' @param recursive A logical value; ignored.
#' @export
c.tc <- function(..., recursive = FALSE) {
  x <- list(...)
  stopifnot(all(vapply(x, FUN = inherits, "tc", FUN.VALUE = logical(1L))))
  x_classes <- lapply(x, class)
  if(length(unique(x_classes)) != 1)
    stop("All tc objects to combine must have identical classes.")
  st_tc(NextMethod())
}


#### casting ####

#' Cast a \code{tc} object to a data frame
#' 
#' @param x An object of class \code{tc}.
#' @param ... Additional arguments; ignored.
#' 
#' @return A data frame with a column \code{time} with the values in \code{x}
#' @export
as.data.frame.tc <- function(x, ...) {
  ret <- data.frame(row.names = seq_along(x))
  ret$time <- x
  ret
}


#### printing and summarizing ####

#' \code{print} generic for \code{tc}
#'
#' @param x An object of class \code{tc}.
#' @param n An integer value; The first \code{n} elements on \code{x} to print.
#' @param print_number_features A logical value; whether the number of features 
#' shall be printed (\code{TRUE}) or not (\code{FALSE}).
#' @param ... Currently unused arguments, for compatibility.
#' 
#' @return \code{x} (invisible).
#' @export
print.tc <- function(x, ..., n = 5L , print_number_features = FALSE) {
  
  stopifnot(is.logical(print_number_features) && length(print_number_features) == 1)
  stopifnot(is.integer(n) && length(n) == 1)
  
  ord <- order(x)
  x_min <- x[[ord[[1]]]]
  x_max <- x[[ord[[length(ord)]]]]
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
  
  for(i in seq_len(min(n, length(x)))) {
    ret <- x[[i]]
    class(ret) <- setdiff(class(ret), "tc")
    message(ret)
  }
    
  invisible(x)
}

#' Compactly display the structure of \code{tc} objects
#'
#' @param object An object of class \code{tc}.
#' @param ... Additional arguments passed to the \code{str} method for the 
#' first element in \code{object}.
#'
#' @export
str.tc <- function(object, ...) {
  n <- length(object)
  cat(paste0(class(object)[1], " of length ", n))
  if (n > 0) {
    cat("; first list element: ")
    ret <- object[[1]]
    class(ret) <- setdiff(class(ret), "tc")
    utils::str(ret, ...)
  }
}

#' Range of \code{tc} objects.
#' 
#' @param ... \code{tc} objects.
#' @param na.rm A logical value; Indicating if \code{NA}s should be omitted.
#' 
#' @export
range.tc <- function(..., na.rm = FALSE) {
  x <- list(...)
  stopifnot(all(vapply(x, FUN = inherits, "tc", FUN.VALUE = logical(1L))))
  x_classes <- lapply(x, class)
  if(length(unique(x_classes)) != 1)
    stop("All tc objects to combine must have identical classes.")
  st_tc(NextMethod())
}

#' Summarize \code{tc} objects
#' 
#' @param object An object of class \code{tc}.
#' @param ... Additional arguments; Ignored.
#' @export
summary.tc <- function(object, ...) {
  print(range(object), n = 0L)
}


