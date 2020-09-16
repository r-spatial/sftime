#### construction ####

#' combine sf and tc objects to an sftime object
#' 
#' @param sf an sf object
#' @param time an tc object, see \link{st_tc} for details
#'
#' @export
st_sf_time <- function(sf, time) {
  sf$time <- time
  class(sf) <- c("sftime", class(sf))
  sf
}


#' Construct a \code{sftime} object from all its components
#'
#' @param ... column elements to be binded into an \code{sftime} object or a single \code{list} or \code{data.frame} with such columns; at least one of these columns shall be a geometry list-column of class \code{sfc} and one shall be a temporal colum of class \code{tc}.
#' @param crs coordinate reference system, something suitable as input to \link{st_crs}
#' @param agr character vector; see details below.
#' @param row.names row.names for the created \code{sf} object
#' @param stringsAsFactors logical; see \link{st_read}
#' @param precision numeric; see \link{st_as_binary}
#' @param sf_column_name character; name of the active list-column with simple feature geometries; in case
#' there is more than one and \code{sf_column_name} is \code{NULL}, the first one is taken.
#' @param sfc_last logical; if \code{TRUE}, \code{sfc} columns are always put last, otherwise column order is left unmodified.
#' @param check_ring_dir see \link{st_read}
#'
#' @return an object of class \code{sftime}
#' @export
st_sftime <- function(..., agr = NA_agr_, row.names,
                      stringsAsFactors = TRUE, crs, precision,
                      sf_column_name = NULL, check_ring_dir = FALSE, sfc_last = TRUE) {
  lst <- list(...)
  
  # time
  timeCol <- which(unlist(lapply(lapply(lst, class), function(l) "tc" %in% l)))
  stopifnot(length(timeCol) == 1)
  tc <- lst[[timeCol]]
  
  # sf
  sfcCol = which(vapply(lst, function(x) inherits(x, "sfc"), TRUE))
  g <- lst[[sfcCol]]
  
  # data
  if (length((1:length(lst))[-c(timeCol, sfcCol)]) > 0) {
    sf <- st_sf(lst[[(1:length(lst))[-c(timeCol, sfcCol)]]], g, 
                agr = agr, row.names = row.names, 
                stringsAsFactors = stringsAsFactors, 
                crs = crs, precision = precision,
                sf_column_name = sf_column_name, 
                check_ring_dir = check_ring_dir, 
                sfc_last = sfc_last)
  } else { # no data
    sf <- st_sf(g, 
                agr = agr, row.names = row.names, 
                stringsAsFactors = stringsAsFactors, 
                crs = crs, precision = precision,
                sf_column_name = sf_column_name, 
                check_ring_dir = check_ring_dir, 
                sfc_last = sfc_last)
  }
  
  st_sf_time(sf, tc)
}

#### subsetting ####



#### printing ####

#' Print a \code{sftime} object
#'
#' @param x the object to be printed
#' @param ... currently unused, for compatability
#' @param n maximum number of printed elements
#'
#' @return a human readble print out on the console
#' @export
print.sftime <- function(x, ..., n = getOption("sf_max_print", default = 10)) {
  geoms = which(vapply(x, function(col) inherits(col, "sfc"), TRUE))
  nf = length(x) - length(geoms) - 1
  app = paste("and", nf, ifelse(nf == 1, "field", "fields"))
  if (any(!is.na(st_agr(x))))
    app = paste0(app, "\n", "Attribute-geometry relationship: ", sf:::summarize_agr(x))
  if (length(geoms) > 1)
    app = paste0(app, "\n", "Active geometry column: ", attr(x, "sf_column"))
  print(st_geometry(x), n = 0, what = "Spatiotemporal feature collection with", append = app)
  
  # temporal information
  print(x$time, noFeature=TRUE)
  if (n > 0) {
    if (inherits(x, "tbl_df"))
      NextMethod()
    else {
      y <- x
      if (nrow(y) > n) {
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
    st_sftime(x@data, st_as_sfc(x@sp), st_tc(times))
  else
    st_sftime(st_as_sfc(x@sp), st_tc(times))
}

## extract time column of a sftime object
#' Title
#'
#' @param x the sftime object
#'
#' @return te time column as tc object
#' @export
st_get_time <- function(x) {
  as.data.frame(x)[,"time"]
}

## keep time while subsetting a column!