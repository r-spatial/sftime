#### construction ####

#' Construct a \code{sftime} object from all its components
#'
#' @param ... Column elements to be binded into an \code{sftime} object or a 
#' single \code{list} or \code{data.frame} with such columns. At least one of 
#' these columns shall be a geometry list-column of class \code{sfc} and one 
#' shall be a time list-column of class \code{tc}.
#' @param crs Coordinate reference system, something suitable as input to 
#' \code{\link{st_crs}}.
#' @param agr A character vector; see details below.
#' @param row.names row.names for the created \code{sf} object.
#' @param stringsAsFactors A logical value; see \link{st_read}.
#' @param precision A numeric value; see \link{st_as_binary}
#' @param sf_column_name A character value; name of the active list-column with 
#' simple feature geometries; in case there is more than one and 
#' \code{sf_column_name} is \code{NULL}, the first one is taken.
#' @param tc_column_name A character value; name of the active 
#' time column (\code{\link[tc]{tc}} object). In case there is more than one 
#' \code{tc} object in \code{...} and \code{tc_column_name} is 
#' \code{NULL}, the first \code{tc} is taken.
#' @param sfc_last A logical value; if \code{TRUE}, \code{sfc} columns are 
#' always put last, otherwise column order is left unmodified.
#' @param tc_last A logical value; if \code{TRUE}, \code{tc} columns are always 
#' put last, otherwise column order is left unmodified. If both \code{sfc_last}
#' and \code{tc_last} are \code{TRUE}, \code{tc} columns are put last.
#' @param check_ring_dir A logical value; see \link{st_read}.
#'
#' @return An object of class \code{sftime}.
#' @export
#' @examples
#' ## construction with a sfc object
#' library(sf)
#' g <- st_sfc(st_point(1:2))
#' tc <- st_tc(Sys.time())
#' st_sftime(a = 3, g, time = tc)
#' ## construction with an sf object
#' \dontrun{st_sftime(st_sf(a = 3, g), time = tc) # error, because if ... contains a data.frame-like object, no other objects may be passed through ... . Instead, add the time column before.}
#' st_sftime(st_sf(a = 3, g, time = tc))
st_sftime <- function(..., 
                      agr = sf::NA_agr_, 
                      row.names,
                      stringsAsFactors = TRUE, 
                      crs, 
                      precision,
                      sf_column_name = NULL, 
                      tc_column_name = NULL, 
                      check_ring_dir = FALSE, 
                      sfc_last = TRUE,
                      tc_last = TRUE) {
  
  # checks
  stopifnot(is.null(tc_column_name) || (is.character(tc_column_name) && length(tc_column_name) == 1))
  stopifnot(is.logical(tc_last) && length(tc_last) == 1)
  
  # pass to sf::st_sf to get sf object
  x <- list(...)
  res <- sf::st_sf(..., 
                   agr = agr, 
                   row.names = row.names, 
                   stringsAsFactors = stringsAsFactors, 
                   crs = crs, 
                   precision = precision, 
                   sf_column_name = sf_column_name, 
                   sfc_last = sfc_last)
  
  # search time column(s)
  all_tc_columns <- vapply(res, function(x) inherits(x, "tc"), TRUE)
  if(!any(all_tc_columns)) stop("No time column found.")
  all_tc_columns <- which(unlist(all_tc_columns))
  
  # get info on active time column (modified from sf)
  if(!is.null(tc_column_name)) {
    stopifnot(tc_column_name %in% colnames(res))
    tc_column <- match(tc_column_name, all_tc_names)
    tc_name <- tc_column_name
  } else {
    tc_column <- all_tc_columns[[1L]]
    tc_name <- names(all_tc_columns)[[1L]]
  }
  
  # sort time column
  if(tc_last) {
    res <- sf::st_sf(cbind(unclass(res[, -all_tc_columns]),
                   sf::st_drop_geometry(res[, all_tc_columns])),
                   agr = agr, 
                   row.names = row.names, 
                   stringsAsFactors = stringsAsFactors, 
                   crs = crs, 
                   precision = precision, 
                   sf_column_name = sf_column_name, 
                   sfc_last = FALSE)
  }
  
  # add attributes
  attr(res, "tc_column") = tc_name
  if(!inherits(res, "sftime"))
    class(res) = c("sftime", class(res))
  
  res
}

#### subsetting ####



#### printing ####

#' Print a \code{sftime} object
#'
#' @param x An object of class \code{sftime}.
#' @param ... Currently unused arguments, for compatibility.
#' @param n Numeric value; maximum number of printed elements.
#' @export
print.sftime <- function(x, ..., n = getOption("sf_max_print", default = 10)) {
  geoms <- which(vapply(x, function(col) inherits(col, "sfc"), TRUE))
  nf <- length(x) - length(geoms) - 1
  app <- paste("and", nf, ifelse(nf == 1, "field", "fields"))
  if (any(!is.na(st_agr(x))))
    app <- paste0(app, "\n", "Attribute-geometry relationship: ", sf:::summarize_agr(x))
  if (length(geoms) > 1)
    app <- paste0(app, "\n", "Active geometry column: ", attr(x, "sf_column"))
  print(st_geometry(x), n = 0, what = "Spatiotemporal feature collection with", append = app)
  
  # temporal information
  print(x[, attr(x, "tc_column"), drop = TRUE], print_number_features = FALSE)
  if(n > 0) {
    if (inherits(x, "tbl_df"))
      NextMethod()
    else {
      y <- x
      if(nrow(y) > n) {
        cat(paste("First", n, "features:\n"))
        y <- x[1:n, , drop = FALSE]
      }
      print.data.frame(y, ...)
    }
  }
  invisible(x)
}


#### coercion ####

#' Convert foreign object to an sftime object
#'
#' Convert foreign object to an sftime object
#' 
#' @aliases st_as_sftime.ST
#' @param x object to be converted into an object class \code{sftime}
#' @export
#' @importFrom methods slotNames as
st_as_sftime = function(x) UseMethod("st_as_sftime")

#' @export
st_as_sftime.ST <- function(x) {
  hasData <- "data" %in% slotNames(x)
  
  if (!inherits(x, "STI")) {
    if (hasData)
      x <- as(x, "STIDF")
    else 
      x <- as(x, "STI")
  }
  
  times <- as.POSIXct(attr(x@time, "index"), origin="1970-01-01")
  
  if (hasData)  
    st_sftime(x@data, st_as_sfc(x@sp), time = st_tc(times))
  else
    st_sftime(st_as_sfc(x@sp), time = st_tc(times))
}


#### manipulate time column ####

#' Extract the active time column of an \code{sftime} object.
#'
#' @param x An \code{sftime} object.
#'
#' @return The active time column in \code{x} as \code{tc} object.
#' @export
st_get_time <- function(x) {
  stopifnot(inherits(x, "sftime"))
  as.data.frame(x)[, attr(x, "tc_column")]
}

## keep time while subsetting a column!