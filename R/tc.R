#### construction ####

#' Check the time column
#' 
#' Checks whether the time column is sortable.
#' 
#' @name timeColumnIsSortable
#' @param x the object to check
#' @details checks whether the provided object can be handled by \link{order}. A couple of basic types are whitelisted. However, custom types can be defined when they provide a dedicated generic to \link{xtfrm}. Note that a \code{list} can only be sorted with \link{atomic} values. See the examples below for a template.
#' @importFrom utils methods
timeColumnIsSortable <- function (x) {
  # can x be sorted?
  # sort.default checks 'is.object(x)' and uses 'order' to subset and sort the object 
  # lists and vectors are no objects, sort then uses sort.int which can onyl handle atomic values
  # have a list of wellknown exceptions
  
  # Examples:
  # x <- Sys.time() + 5:1 * 3600*24
  # x <- yearmon(2020+c(5:0)/12)
  # x <- yearqtr(2020+c(5:0)/4)
  # x <- factor(LETTERS[sample(26, replace = T)], levels=LETTERS[sample(26)])
  # sort(x)
  # order(x)
  # class(x)
  if (any(sapply(class(x), function(clsnm) clsnm %in% c("numeric", "POSIXct", "POSIXlt", "Date", "yearmon", "yearqtr", "factor"))))
    return (TRUE)
  
  # check for function 'xtfrm.[CLASSNAME]' which is used by 'order' which in turn is used by sort.default
  if (paste("xtfrm", class(x), sep=".") %in% methods(class = class(x)))
    return (TRUE)
  
  return(FALSE)  
}

#' Create temporal column
#'
#' Check temporal column and set class
#'
#' @param times the time stamps to be used
#' @export
st_tc = function(times) {
  stopifnot(timeColumnIsSortable(times))
  
  class(times) <- c("tc", class(times))
  
  times
}


#### subsetting ####

#' Subsetting of time columns \code{tc}
#'
#' @param x the object to be subsetted
#' @param i any subsetting expression supported by the temporal class provided to the \code{tc}.
#' @param ... any further arguments for the underlying subsetting function
#'
#' @return a time column \code{tc} holding a subset of the orignal time column 
#' @export
"[.tc" = function(x, i, ...) {
  cls <- class(x)
  # remove 'tc' class flag
  class(x) <- cls[which(cls != "tc")]
  # do any subsetting the original class supports
  sx <- x[i, ...]
  # re-build as time column
  st_tc(sx)
}

#### printing ####
#' \code{print} generic for \code{tc}
#'
#' @param x an object of class \code{tc}
#' @param noFeature logical: whether the number of features shall not be printed (default: FALSE)
#' @param ... currently unused, for compatability
#'
#' @return human readable print on the console
#' @export
print.tc <- function(x, ..., noFeature = FALSE) {
  ord <- order(x)
  minVal <- x[which.min(ord)]
  maxVal <- x[which.max(ord)]
  cls <- class(x)
  oneFeature <- length(x) == 1
  cat(paste0("Time column with ", 
             ifelse(noFeature, "",
                    paste0( length(x), ifelse(oneFeature, " feature of ", " features each of "))),
             ifelse(length(cls) == 2, "class", "classes"), ": \'", 
             paste0(cls[-1], collapse="\', \'"), "\'.\n",
             ifelse(oneFeature, 
                    paste0("Representing ", minVal, ".\n" ), 
                    paste0("Ranging from ", minVal, " to ", maxVal, ".\n" ))))
}