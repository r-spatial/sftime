#' Plots an  \code{sftime} object
#'
#' \code{plot.sftime} 
#'
#' @aliases plot
#' @param x The \code{\link[=st_sftime]{sftime}} object to be plotted.
#' @param y A character value; The variable name to be plotted; if missing, the 
#' first variable is plotted.
#' @param ... Additional arguments; Passed on to \code{\link[sf:plot]{plot.sf}}.
#' @param number A numeric value; The number of panels to be plotted, cannot be 
#' larger than the number of timestamps; ignored when \code{tcuts} is provided.
#' @param tcuts predefined temporal ranges assigned to each map; if missing, 
#' will be determined as equal spans according to \code{number}.
#'
#' @importFrom graphics plot
#'
#' @return Returns \code{NULL} and creates as side effect a plot for \code{x}.
#' @examples
#' set.seed(123)
#' coords <- matrix(runif(100), ncol = 2)
#' g <- st_sfc(lapply(1:50, function(i) st_point(coords[i, ]) ))
#' sft <- st_sftime(a = 1:50, g, time = as.POSIXct("2020-09-01 00:00:00") + 0:49 * 3600 * 6)
#' 
#' plot(sft)
#' 
#' @export
plot.sftime <- function(x, y, ..., number = 6, tcuts) {
  
  if (missing(y))
    y <- colnames(x)[[1]]
  
  stopifnot(y %in% colnames(x))
  
  ts <- st_time(x)
  if(any(is.na(ts))) {
    message("[INFO] there are ", sum(is.na(ts)), " `NA` values in the active time column of `x`. These rows are dropped.")
  }
  
  x <- x[!is.na(ts), ]
  ts <- st_time(x)
  
  if (missing(tcuts)) {
    ts_ord <- order(ts)
    ts_fac <- tryCatch(as.factor(ts[ts_ord]), error = function(e) e)
    if (inherits(ts_fac, "error")) {
      ts_fac <- 
        factor(
          as.character(ts[ts_ord]), 
          levels = unique(as.character(ts[ts_ord])),
          ordered = TRUE
        )
    }
    
    ts_nlv <- length(levels(ts_fac))
    
    if (number > ts_nlv) {
      number <- ts_nlv
      message("[INFO] Fewer time stamps in the data than asked for; argument 'number' set to: ", ts_nlv)
    }
    
    tcuts <- seq(1, ts_nlv, length.out = number + 1)
    
    timeclass <- findInterval(as.numeric(ts_fac), tcuts, rightmost.closed = TRUE)
  } else {
    number <- length(tcuts) - 1
    timeclass <- findInterval(ts, tcuts, rightmost.closed = TRUE)
  }
  d_ord <- as.data.frame(x)[order(ts), y, drop = FALSE]
  
  data <- d_ord
  if (number > 1) { 
    for (i in 2:number) {
      data <- cbind(data, d_ord[, 1])
      data[timeclass != i, i] = NA
      if (i == number)
        data[timeclass != 1, 1] <- NA # deal with first time class
    }
  }
  
  names(data) <- ts_fac[!duplicated(timeclass)]
  d <- sf::st_sf(data, geometry = sf::st_geometry(x))
  
  plot(d, ...)
  
  NULL
}