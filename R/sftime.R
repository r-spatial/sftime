first_time = function(x) {
	w = which(sapply(x, inherits, c("POSIXct", "Date")))
	if (length(w) == 0)
		stop("no time column found")
	if (length(w) > 1)
		stop("using first time column")
	names(x)[w][1]
}


#' construct an sf_time object from an sf object
#'
#' construct an sf_time object from an sf object
#' @param x object of class \code{sf}
#' @param time_column integer or character; index or name of the time column
#' @param ... ignored
#' @export
#' @examples
#' x = st_sf(a = 1:2, b = as.Date("2019-09-13") + 0:1, geom = st_sfc(st_point(0:1), st_point(1:2)))
#' sf_time(x)
#' sf_time(x, 2)
#' sf_time(x, "b")
sf_time = function(x, time_column = first_time(x), ...) {
	stopifnot(inherits(x, "sf"))

	if (!is.character(time_column))
		time_column = names(x)[time_column]

	stopifnot(time_column %in% names(x))

	structure(x, time_column = time_column, class = c("sf_time", class(x)))
}

#' @export
#' @name sf_time
#' @param n maximum number of features to print; can be set globally by \code{options(sf_max_print=...)}
print.sf_time = function(x, ..., n = getOption("sf_max_print", default = 10)) {

	geoms = which(vapply(x, function(col) inherits(col, "sfc"), TRUE))
	nf = length(x) - length(geoms) - 1
	app = paste("and", nf, ifelse(nf == 1, "field", "fields"))
	if (length(geoms) > 1)
		app = paste0(app, "\n", "Active geometry column: ", attr(x, "sf_column"))
	app = paste0(app, "\n",     "Time column:    ", attr(x, "time_column"))
	print(st_geometry(x), n = 0, what = "Simple feature + time collection with", append = app)
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
