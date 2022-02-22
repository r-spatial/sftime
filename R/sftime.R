#### construction ####

#' Checks whether a vector or list is sortable
#' 
#' Checks whether a vector or list is sortable. This is the condition for a 
#' vector to be usable as time column in a \code{sftime} object.
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

#' Construct an \code{sftime} object from all its components
#'
#' @param ... Column elements to be binded into an \code{sftime} object or a 
#' single \code{list} or \code{data.frame} with such columns. At least one of 
#' these columns shall be a geometry list-column of class \code{sfc} and one 
#' shall be a time column (to be specified with \code{time_column_name}).
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
#' @param time_column_name A character value; name of the active 
#' time column. In case \code{time_column_name} is \code{NULL}, the first 
#' \code{\link{POSIXct}} column is taken. If there is no \code{POSIXct} column,
#' the first \code{\link{Date}} column is taken.
#' @param sfc_last A logical value; if \code{TRUE}, \code{sfc} columns are 
#' always put last, otherwise column order is left unmodified.
#' @param time_column_last A logical value; if \code{TRUE}, the active time column is 
#' always put last, otherwise column order is left unmodified. If both \code{sfc_last}
#' and \code{time_column_last} are \code{TRUE}, the active time column is put last.
#' @param check_ring_dir A logical value; see \code{\link[sf]{st_read}}.
#'
#' @return An object of class \code{sftime}.
#' @examples
#' ## construction with a sfc object
#' library(sf)
#' g <- st_sfc(st_point(1:2))
#' tc <- Sys.time()
#' st_sftime(a = 3, g, time = tc)
#' 
#' ## construction with an sf object
#' \dontrun{
#' st_sftime(st_sf(a = 3, g), time = tc) 
#' # error, because if ... contains a data.frame-like object, no other objects 
#' # may be passed through ... . Instead, add the time column before.
#' }
#' 
#' st_sftime(st_sf(a = 3, g, time = tc))
#' 
#' @export
st_sftime <- function(..., 
                      agr = sf::NA_agr_, 
                      row.names,
                      stringsAsFactors = TRUE, 
                      crs, 
                      precision,
                      sf_column_name = NULL, 
                      time_column_name = NULL, 
                      check_ring_dir = FALSE, 
                      sfc_last = TRUE,
                      time_column_last = TRUE) {
  
  # checks
  stopifnot(is.null(time_column_name) || (is.character(time_column_name) && length(time_column_name) == 1))
  stopifnot(is.logical(time_column_last) && length(time_column_last) == 1)
  
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
  
  # get info on active time column (modified from sf)
  if(!is.null(time_column_name)) { # time column manually specified
    
    stopifnot(time_column_name %in% colnames(res))
    stopifnot(is_sortable(res[[time_column_name]]))
    res_time_column <- match(time_column_name, colnames(res))
    res_time_column_name <- time_column_name
    
  } else { #search for POSIXct and Date columns
    
    # search time column(s)
    all_time_column_names <- NULL
    all_time_columns <- vapply(res, function(x) inherits(x, "POSIXct"), TRUE)
    if(!any(all_time_columns)) {
      all_time_columns <- vapply(res, function(x) inherits(x, "Date"), TRUE)
    }
    if(!any(all_time_columns)) stop("No time column found.")
    all_time_columns <- which(unlist(all_time_columns))
    
    res_time_column <- all_time_columns[[1L]]
    res_time_column_name <- names(all_time_columns)[[1L]]
  }
  
  # sort time column
  if(time_column_last) {
    res_only_time_column <- sf::st_drop_geometry(res[, res_time_column])[, 1, drop = TRUE]
    res <- res[, -res_time_column]
    res[, res_time_column_name] <- res_only_time_column
    res <- sf::st_sf(res,
                     agr = agr, 
                     row.names = row.names, 
                     stringsAsFactors = stringsAsFactors, 
                     crs = crs, 
                     precision = precision, 
                     sf_column_name = sf_column_name, 
                     sfc_last = FALSE)
  }
  
  # add attributes
  attr(res, "time_column") <- res_time_column_name
  if(!inherits(res, "sftime"))
    class(res) <- c("sftime", class(res))
  
  res
}

#' Helper function for reclassing sftime objects
#' 
#' Reclasses sftime objects to the correct new class after modification. Checks
#' if the sftime object (the active time column) gets invalidated. If so, the
#' sftime class is dropped. If not, the object is reclassed to an sftime object.
#' 
#' @param x An object to be reclassed to the \code{\link[=st_sftime]{sftime}} class.
#' @param time_colmn_name A character value; name of the active time column.
#' @return \code{x} as \code{sftime} object if the column indicated by 
#' \code{time_colmn_name} is a valid time column (\code{\link{is_sortable}}) and
#' \code{x} without \code{time_column} attribute if not.
#' 
#' @keywords internal
#' @noRd
reclass_sftime <- function(x, time_column_name) {
  
  if(! time_column_name %in% colnames(x)) {
    structure(x, class = setdiff(class(x), "sftime"), time_column = NULL)
  } else {
    structure(x, class = c("sftime", setdiff(class(x), "sftime")), time_column = time_column_name)
  }
  
  # res <- structure(x, class = c("sftime", setdiff(class(x), "sftime")))
  
  # check if  time column is still intact
  # if(!is_sortable(res[, time_column_name, drop = TRUE])) {
  #   structure(res, class = setdiff(class(res), "sftime"), time_column = NULL)
  # } else {
  #   st_as_sftime(res, time_column_name = time_column_name)
  # }
  
}

#### subsetting ####

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
#' tc <- Sys.time() + 1:5
#' x <- st_sftime(a = 1:5, g, time = tc)
#' 
#' # rows
#' x[1, ]
#' class(x[1, ])
#' 
#' # columns
#' x[, 1]
#' class(x[, 1]) # drops time column as for ordinary data.frame subsetting, 
#' # keeps geometry column of sf object
#' 
#' x[, 3]
#' class(x[, 3]) # keeps time column because it is explicitly selected,
#' # keeps geometry column of sf object, returns an sftime object
#' 
#' x[, 3, drop = TRUE] 
#' class(x[, 3, drop = TRUE]) # if the geometry column is dropped, not only the
#' # sf class is dropped, but also the sftime class
#' 
#' x["a"]
#' class(x["a"]) # Time columns are not sticky: If a column is selected by a 
#' # character vector and this does not contain the active time column, the time 
#' # column is dropped. 
#' 
#' x[c("a", "time")]
#' class(x[c("a", "time")]) # keeps the time column
#' 
#' # with sf or sftime object 
#' pol = st_sfc(st_polygon(list(cbind(c(0,2,2,0,0),c(0,0,2,2,0)))))
#' h = st_sf(r = 5, pol)
#' 
#' x[h, ] 
#' class(x[h, ]) # returns sftime object
#' 
#' h[x, ] 
#' class(h[x, ]) # returns sf object
#' 
#' @export
"[.sftime" <- function(x, i, j, ..., drop = FALSE, op = sf::st_intersects) {
  
  # retain info on time column
  time_column <- attr(x, "time_column")
  
  # perform subsetting for sf object
  if((!missing(j) && !drop && ((is.character(j) && any(j == time_column)) || (is.numeric(j) && any(colnames(x)[j] == time_column)))) ||
     !missing(i) && !drop && ((is.character(i)) && any(i == time_column)  || is.numeric(i))) {
    structure(NextMethod(), class = class(x), time_column = time_column)
  } else {
    x <- structure(x, class = setdiff(class(x), "sftime"), time_column = NULL)
    NextMethod()
  } # ---todo: what to do when i is an sftime object: match also time info
  
}

#' @name st_sftime
#' @param value An object to insert into \code{x}.
#' @examples
#' ## Assigning values to columns
#' 
#' # assigning new values to a non-time column
#' x[["a"]] <- 5:1
#' class(x)
#' 
#' # assigning allowed new values to the time column
#' x[["time"]] <- Sys.time() + 1:5
#' class(x)
#' 
#' # assigning new values to the time column which invalidate the time column
#' x[["time"]] <- list(letters[1:2])
#' class(x)
#' 
#' @export
"[[<-.sftime" <- function(x, i, value) {
  time_column_name <- attr(x, "time_column")
  reclass_sftime(NextMethod(), time_column_name = time_column_name)
}

#' @name st_sftime
#' @export
"$<-.sftime" = function(x, i, value) {
  structure(NextMethod(), class = c("sftime", setdiff(class(x), "sftime")))
}

#### printing ####

#' Helper function to print time columns when printing sftime object
#'
#' @noRd
#' @keywords internal
#' @param x A time column from a \code{\link[=st_sftime]{sftime}} object.
#' @param n An integer value; The first \code{n} elements of \code{x} to print.
#' @param print_number_features A logical value; whether the number of features 
#' shall be printed (\code{TRUE}) or not (\code{FALSE}).
#' 
#' @return \code{x} (invisible).
print_time_column <- function(x, n = 5L, print_number_features = FALSE) {
  
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
  print_time_column(x[, attr(x, "time_column"), drop = TRUE], n = 0L, print_number_features = FALSE)
  
  if(n > 0) {
    if (inherits(x, "tbl_df"))
      NextMethod()
    else {
      y <- x
      if(nrow(y) > n) {
        cat(paste("First", n, "features:\n"))
        y <- x[seq_len(n), , drop = FALSE]
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
    st_sftime(x@data, st_as_sfc(x@sp), time = times)
  else
    st_sftime(st_as_sfc(x@sp), time = times)
}

#' @name st_as_sftime
#' @examples 
#' # convert a Track object from package trajectories to an sftime object
#' library(trajectories)
#' x1_Track <- trajectories::rTrack(n = 100)
#' x1_Track@data$speed <- sort(rnorm(length(x1_Track)))
#' x1_sftime <- st_as_sftime(x1_Track)
#' 
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
#' @examples 
#' # convert a Tracks object from package trajectories to an sftime object
#' x2_Tracks <- trajectories::rTracks(m = 6)
#' x2_sftime <- st_as_sftime(x2_Tracks)
#' 
#' @export
st_as_sftime.Tracks <- function(x, ...) {
  
  track_name <-
    unlist(lapply(seq_along(x@tracks), function(i) rep(names(x@tracks)[[i]], x@tracksData$n[[i]])))
  
  cbind(st_as_sftime(as(x, "STIDF")), track_name = track_name)
  
} 

#' @name st_as_sftime
#' @examples 
#' # convert a TracksCollection object from package trajectories to an sftime object
#' x3_TracksCollection <- trajectories::rTracksCollection(p = 2, m = 3, n = 50)
#' x3_sftime <- st_as_sftime(x3_TracksCollection)
#' 
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
#' @param time_column_name A character value; name of the active time column. In 
#' case there is more than one and \code{time_column_name} is \code{NULL}, the 
#' first one is taken.
#' @examples
#' # convert an sf object to an sftime object
#' g <- st_sfc(st_point(c(1, 2)), st_point(c(1, 3)), st_point(c(2, 3)), 
#'      st_point(c(2, 1)), st_point(c(3, 1)))
#' x4_sf <- st_sf(a = 1:5, g, time = Sys.time() + 1:5)
#' x4_sftime <- st_as_sftime(x4_sf) 
#' 
#' @export
st_as_sftime.sf <- function(x, ..., time_column_name = NULL) {
  st_sftime(x, ..., time_column_name = time_column_name)
}

#' @name st_as_sftime
#' @param long A logical value; See \code{\link[stars:st_as_sf]{st_as_sf}}. 
#' Typically, \code{long} should be set to \code{TRUE} since time information
#' typically is a dimension of a stars object.
#' @examples 
#' # convert a Tracks object from package trajectories to an sftime object
#' x5_stars <- stars::read_stars(system.file("nc/bcsd_obs_1999.nc", package = "stars"))
#' x5_sftime <- st_as_sftime(x5_stars, time_column_name = "time")
#' 
#' # this requires some thought to not accidentally drop time dimensions. For
#' # example, setting `merge = TRUE` will drop the time dimension and thus throw
#' # an error:
#' \dontrun{
#' x5_sftime <- st_as_sftime(x5_stars, merge = TRUE, time_column_name = "time")
#' }
#' 
#' @export
st_as_sftime.stars <- function(x, ..., long = TRUE, time_column_name = NULL) {
  res <- sf::st_as_sf(x, ..., long = long)
  if(!time_column_name %in% colnames(res)) 
    stop("`time_column_name` is not a column in the converted object.")
  st_sftime(res, time_column_name = time_column_name)
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
#' @inheritParams st_sftime
#' @examples
#' # convert a data frame to an sftime object
#' x5_df <- 
#'    data.frame(a = 1:5, g, time = Sys.time() + 1:5, stringsAsFactors = FALSE)
#' x5_sftime <- st_as_sftime(x5_df)
#' 
#' @export
st_as_sftime.data.frame <- 
  function(x, 
           ..., 
           agr = NA_agr_, 
           coords, 
           wkt,
           dim = "XYZ", 
           remove = TRUE, 
           na.fail = TRUE, 
           sf_column_name = NULL, 
           time_column_name = NULL) {
    
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
      time_column_name = time_column_name)
  }


#### transform attributes ####

#' Transform method for \code{sftime} objects
#'
#' Can be used to create or modify attribute variables; for transforming 
#' geometries see \code{\link[sf]{st_transform}}, and all other functions starting with 
#' \code{st_}.
#'
#' @param _data An object of class \code{\link[=st_sftime]{sftime}}.
#' @inheritParams sf::transform.sf
#' @examples
#' # create an sftime object
#' g <- st_sfc(st_point(c(1, 2)), st_point(c(1, 3)), st_point(c(2, 3)), 
#'      st_point(c(2, 1)), st_point(c(3, 1)))
#' x <- 
#'    data.frame(a = 1:5, g, time = Sys.time() + 1:5, stringsAsFactors = FALSE)
#' x_sftime <- st_as_sftime(x)
#' x_sftime
#' 
#' # modify values in column a
#' transform(x_sftime, a = rev(a))
#' 
#' @export
transform.sftime <- function (`_data`, ...) {
  reclass_sftime(NextMethod(), time_column_name = attr(`_data`, "time_column"))
}
