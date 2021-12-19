#### construction ####

#' Construct an \code{sftime} object from all its components
#'
#' @param ... Column elements to be binded into an \code{sftime} object or a 
#' single \code{list} or \code{data.frame} with such columns. At least one of 
#' these columns shall be a geometry list-column of class \code{sfc} and one 
#' shall be a time list-column of class \code{tc}.
#' @param crs Coordinate reference system, something suitable as input to 
#' \code{\link[sf]{st_crs}}.
#' @param agr A character vector; see details below.
#' @param row.names row.names for the created \code{sf} object.
#' @param stringsAsFactors A logical value; see 
#' \code{\link[sf]{st_read}}.
#' @param precision A numeric value; see 
#' \code{\link[sf]{st_as_binary}}.
#' @param sf_column_name A character value; name of the active list-column with 
#' simple feature geometries; in case there is more than one and 
#' \code{sf_column_name} is \code{NULL}, the first one is taken.
#' @param tc_column_name A character value; name of the active 
#' time column (\code{\link[=st_tc]{tc}} object). In case there is more than one 
#' \code{tc} object in \code{...} and \code{tc_column_name} is 
#' \code{NULL}, the first \code{tc} is taken.
#' @param sfc_last A logical value; if \code{TRUE}, \code{sfc} columns are 
#' always put last, otherwise column order is left unmodified.
#' @param tc_last A logical value; if \code{TRUE}, \code{tc} columns are always 
#' put last, otherwise column order is left unmodified. If both \code{sfc_last}
#' and \code{tc_last} are \code{TRUE}, \code{tc} columns are put last.
#' @param check_ring_dir A logical value; see \code{\link[sf]{st_read}}.
#'
#' @return An object of class \code{sftime}.
#' @examples
#' ## construction with a sfc object
#' library(sf)
#' g <- st_sfc(st_point(1:2))
#' tc <- st_tc(Sys.time())
#' st_sftime(a = 3, g, time = tc)
#' 
#' ## construction with an sf object
#' \dontrun{st_sftime(st_sf(a = 3, g), time = tc) 
#' # error, because if ... contains a data.frame-like object, no other objects 
#' # may be passed through ... . Instead, add the time column before.}
#' 
#' st_sftime(st_sf(a = 3, g, time = tc))
#' 
#' 
#' @export
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
  all_tc_names <- NULL
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

reorder_sftime_class_ <- function(x, tc_col) {
  stopifnot(inherits(x, "sftime"))
  class(x) <- c("sftime", setdiff(class(x), "sftime"))
  attr(x, "tc_column") <- tc_col
  x
}

#' @name st_sftime
#' @param x An object of class \code{sf}.
#' @param i Record selection, see \link{[.data.frame}
#' @param j Variable selection, see \link{[.data.frame}
#' @param drop A logical value, default \code{FALSE}; if \code{TRUE} drop the 
#' geometry column and return a \code{data.frame}, else make the geometry sticky 
#' and return an \code{sf} object.
#' @param op A function; geometrical binary predicate function to apply when 
#' \code{i} is a simple feature object.
#' @details \code{[.sf} will return a \code{data.frame} or vector if the 
#' geometry column (of class \code{sfc}) is dropped (\code{drop=TRUE}), an 
#' \code{sfc} object if only the geometry column is selected, and otherwise 
#' return an \code{sftime} object; see also \link{[.data.frame}; for 
#' \code{[.sftime} \code{...} arguments are passed to \code{op}.
#' @examples
#' ## Subsetting
#' g <- st_sfc(st_point(c(1, 2)), st_point(c(1, 3)), st_point(c(2, 3)), 
#'      st_point(c(2, 1)), st_point(c(3, 1)))
#' tc <- st_tc(Sys.time() + 1:5)
#' x <- st_sftime(st_sf(a = 1:5, g, time = tc))
#' 
#' # rows
#' x[1, ]
#' class(x[1, ])
#' 
#' # columns
#' x[, 1]
#' class(x[, 1]) # drops time column as for ordinary data.frame subsetting, 
#' # keeps geometry column of sf object
#' x[, 3]
#' class(x[, 3]) # keeps time column because it is explicitly selected,
#' # keeps geometry column of sf object, returns an sftime object
#' x[, 3, drop = TRUE] 
#' class(x[, 3, drop = TRUE]) # if the geometry column is dropped, not only the
#' # sf class is dropped, but also the sftime class
#' 
#' # with sf or sftime object 
#' pol = st_sfc(st_polygon(list(cbind(c(0,2,2,0,0),c(0,0,2,2,0)))))
#' h = st_sf(r = 5, pol)
#' x[h, ] 
#' class(x[h, ]) # returns sftime object
#' h[x, ] 
#' class(h[x, ]) # returns sf object
#' 
#' @export
"[.sftime" <- function(x, i, j, ..., drop = FALSE, op = sf::st_intersects) {
  
  # retain info on time column
  tc_col <- attr(x, "tc_column")
  
  # perform subsetting for sf object
  attr(x, "tc_column") <- NULL
  if((!missing(j) && !drop && ((is.character(j) && any(j == tc_col)) || (is.numeric(j) && any(colnames(x)[j] == tc_col)))) ||
     !missing(i) && !drop) {
    reorder_sftime_class_(NextMethod(), tc_col = tc_col)
  } else if (drop) {
    class(x) <- setdiff(class(x), "sftime")
    NextMethod()
  } else {
    NextMethod()
  } # ---todo: what to do when i is an sftime object: match also time info
  
}

#' @name st_sftime
#' @param value An object to insert into \code{x}.
#' @export
"[[<-.sftime" <- function(x, i, value) {
  structure(NextMethod(), class = c("sftime", setdiff(class(x), "sftime")))
}

#' @name st_sftime
#' @export
"$<-.sftime" = function(x, i, value) {
  structure(NextMethod(), class = c("sftime", setdiff(class(x), "sftime")))
}

#### printing ####

#' Print a \code{sftime} object
#'
#' @param x An object of class \code{sftime}.
#' @param ... Currently unused arguments, for compatibility.
#' @param n Numeric value; maximum number of printed elements.
#' 
#' @return \code{x} (invisible).
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

#' Convert a foreign object to an \code{sftime} object
#'
#' Convert a foreign object to an \code{sftime} object.
#' 
#' @name st_as_sftime
#' @param x An object to be converted into an object of class 
#' \code{\link[=st_sftime]{sftime}}.
#' @param ... Further arguments passed to methods.
#' 
#' @export
#' @importFrom methods slotNames as
st_as_sftime = function(x, ...) UseMethod("st_as_sftime")

#' @name st_as_sftime
#' @export
st_as_sftime.ST <- function(x, ...) {
  has_data <- "data" %in% slotNames(x)
  
  if (!inherits(x, "STI")) {
    if (has_data)
      x <- as(x, "STIDF")
    else 
      x <- as(x, "STI")
  }
  
  times <- as.POSIXct(attr(x@time, "index"), origin = "1970-01-01")
  
  if (has_data)  
    st_sftime(x@data, st_as_sfc(x@sp), time = st_tc(times))
  else
    st_sftime(st_as_sfc(x@sp), time = st_tc(times))
}

#' @name st_as_sftime
#' @export
st_as_sftime.Track <- function(x, ...) {
  
  has_data <- "data" %in% slotNames(x)
  
  if (has_data)
    x <- as(x, "STIDF")
  else 
    x <- as(x, "STI")
  
  st_as_sftime(x)
  
} 

#' @name st_as_sftime
#' @export
st_as_sftime.Tracks <- function(x, ...) {
  
  track_name <-
    unlist(lapply(seq_along(x@tracks), function(i) rep(names(x@tracks)[[i]], x@tracksData$n[[i]])))
  
  cbind(st_as_sftime(as(x, "STIDF")), track_name = track_name)
  
} 

#' @name st_as_sftime
#' @export
st_as_sftime.TracksCollection <- function(x, ...) {
  
  track_names <-
    do.call(rbind, lapply(seq_along(x@tracksCollection), function(i) {
      n <- sum(x@tracksCollection[[i]]@tracksData$n)
      track_i <- x@tracksCollection[[i]]
      data.frame(
        tracks_name = rep(names(x@tracksCollection)[[i]], n),
        track_name = unlist(lapply(seq_along(track_i@tracks), function(j) rep(names(track_i@tracks)[[j]], track_i@tracksData$n[[j]]))),
        stringsAsFactors = FALSE
      )
    }))
  
  cbind(st_as_sftime(as(x, "STIDF")), track_names)
  
} 

#' @name st_as_sftime
#' @export
st_as_sftime.sftime <- function(x, ...) x

#' @name st_as_sftime
#' @param tc_column_name A character value; name of the active time column. In 
#' case there is more than one and \code{tc_column_name} is \code{NULL}, the 
#' first one is taken.
#' @export
st_as_sftime.sf <- function(x, ..., tc_column_name = NULL) {
  st_sftime(x, ..., tc_column_name = tc_column_name)
}

#' @name st_as_sftime
#' @param agr A character vector; see details section of \code{\link{st_sf}}.
#' @param coords In case of point data: names or numbers of the numeric columns 
#' holding coordinates.
#' @param wkt The name or number of the character column that holds WKT encoded 
#' geometries.
#' @param dim Passed on to \code{\link{st_point}} (only when argument 
#' \code{coords} is given).
#' @param remove A logical value; when \code{coords} or \code{wkt} is given, 
#' remove these columns from code{data.frame}?
#' @param na.fail A logical value; if \code{TRUE}, raise an error if coordinates 
#' contain missing values.
#' @param sf_column_name A character value; name of the active list-column with 
#' simple feature geometries; in case there is more than one and 
#' \code{sf_column_name} is \code{NULL}, the first one is taken.
#' @export
st_as_sftime.data.frame <- 
  function(x, 
           ..., 
           agr = NA_agr_, 
           coords, wkt,
           dim = "XYZ", 
           remove = TRUE, 
           na.fail = TRUE, 
           sf_column_name = NULL, 
           tc_column_name = NULL) {
    
    st_sftime(
      sf::st_as_sf(
        x, 
        ..., 
        agr = agr, 
        coords = coords, 
        wkt = wkt,
        dim = dim, 
        remove = remove, 
        na.fail = na.fail, 
        sf_column_name = sf_column_name
      ), 
      tc_column_name = tc_column_name)
  }
