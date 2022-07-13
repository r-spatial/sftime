#' Bind rows (features) of \code{sftime} objects
#' 
#' @name bind
#' @param ... Objects to bind; note that for the \code{rbind} and \code{cbind} 
#' methods, all objects have to be of class \code{sftime}; see 
#' \code{\link{dotsMethods}}.
#' @param deparse.level An integer value; see \code{\link{rbind}}.
#' @return \code{rbind} combines all \code{sftime} objects in \code{...} 
#' row-wise and returns the combined \code{sftime} object.
#' @details Both \code{rbind} and \code{cbind} have non-standard method dispatch 
#' (see \link[base]{cbind}): the \code{rbind} or \code{cbind} method for 
#' \code{sftime} objects is only called when all arguments to be combined are of 
#' class \code{sftime}.
#' @export
#' @examples
#' g1 <- st_sfc(st_point(1:2))
#' x1 <- st_sftime(a = 3, geometry = g1, time = Sys.time())
#' 
#' g2 <- st_sfc(st_point(c(4, 6)))
#' x2 <- st_sftime(a = 4, geometry = g2, time = Sys.time())
#' 
#' rbind(x1, x2) # works because both tc1 and tc2 have the same class
#' 
#' \dontrun{
#' st_time(x2) <- 1
#' rbind(x1, x2) # error because both tc1 and tc2 do not have the same class
#' }
#' 
rbind.sftime <- function(..., deparse.level = 1) {
  
  dots <- list(...)
  dots <- dots[!sapply(dots, is.null)]
  stopifnot(vapply(dots, inherits, "sftime", FUN.VALUE = TRUE))
  
  tc0 <- class(st_time(dots[[1]]))
  if (length(dots) > 1L) { # check all time columns are equal...
    equal_tc <- vapply(dots[-1L], function(x) identical(tc0, class(st_time(x))), TRUE)
    if (!all(equal_tc))
      stop("Arguments have different time column classes", call. = FALSE)
  }
  
  nr <- sapply(dots, NROW)
  tc_column <- if (any(nr > 0))
    attr(dots[[ which(nr > 0)[1] ]], "tc_column")
  else
    NULL
  
  st_sftime(do.call(rbind, lapply(dots, function(x) structure(x, class = setdiff(class(x), "sftime")))), time_column_name = tc_column)
  
}

#' Bind columns (variables) of \code{sftime} objects
#' 
#' @name bind
#' @param sf_column_name Character value; specifies the active geometry column; 
#' passed on to \code{\link{st_sftime}}.
#' @param tc_column_name Character value; specifies active time column; passed 
#' on to \code{\link{st_sftime}}.
#' @return \code{cbind} combines all \code{sftime} objects in \code{...} 
#' column-wise and returns the combined \code{sftime} object. When called with 
#' multiple \code{sftime} objects warns about multiple time and geometry columns 
#' present when the time and geometry columns to use are not specified by using 
#' arguments \code{tc_column_name} and \code{sf_column_name}; see also 
#' \link{st_sftime}.
#' @export
#' @details If you need to \code{cbind} e.g. a \code{data.frame} to an \code{sf}, 
#' use \code{\link{data.frame}} directly and use \code{\link{st_sftime}} on its 
#' result, or use \code{\link[dplyr:bind]{bind_cols}}; see examples.
#' @examples
#' cbind(x1, x2) 
#' 
#' if (require(dplyr)) {
#'   # returns a data frame because names of sf and time column are modified:
#'   dplyr::bind_cols(x1, x2) 
#'   
#'   # returns an sf object because the name of the time column is modified:
#'   dplyr::bind_cols(x1, x2 %>% sf::st_drop_geometry()) 
#'   
#'   # returns an sftime object because names of sf and time column are both 
#'   # preserved:
#'   dplyr::bind_cols(x1, x2 %>% st_drop_time() %>% sf::st_drop_geometry()) 
#' }
#'   
#' df <- data.frame(x = 3)   
#' st_sftime(data.frame(x1, df))   
#'   
cbind.sftime = function(..., deparse.level = 1, sf_column_name = NULL, tc_column_name = NULL) {
  st_sftime(data.frame(...), sf_column_name = sf_column_name, time_column_name = tc_column_name)
}
