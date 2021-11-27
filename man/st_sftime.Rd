% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sftime.R
\name{st_sftime}
\alias{st_sftime}
\title{Construct a \code{sftime} object from all its components}
\usage{
st_sftime(
  ...,
  agr = sf::NA_agr_,
  row.names,
  stringsAsFactors = TRUE,
  crs,
  precision,
  sf_column_name = NULL,
  tc_column_name = NULL,
  check_ring_dir = FALSE,
  sfc_last = TRUE,
  tc_last = TRUE
)
}
\arguments{
\item{...}{Column elements to be binded into an \code{sftime} object or a 
single \code{list} or \code{data.frame} with such columns. At least one of 
these columns shall be a geometry list-column of class \code{sfc} and one 
shall be a time list-column of class \code{tc}.}

\item{agr}{A character vector; see details below.}

\item{row.names}{row.names for the created \code{sf} object.}

\item{stringsAsFactors}{A logical value; see \link{st_read}.}

\item{crs}{Coordinate reference system, something suitable as input to 
\code{\link{st_crs}}.}

\item{precision}{A numeric value; see \link{st_as_binary}}

\item{sf_column_name}{A character value; name of the active list-column with 
simple feature geometries; in case there is more than one and 
\code{sf_column_name} is \code{NULL}, the first one is taken.}

\item{tc_column_name}{A character value; name of the active 
time column (\code{\link[tc]{tc}} object). In case there is more than one 
\code{tc} object in \code{...} and \code{tc_column_name} is 
\code{NULL}, the first \code{tc} is taken.}

\item{check_ring_dir}{A logical value; see \link{st_read}.}

\item{sfc_last}{A logical value; if \code{TRUE}, \code{sfc} columns are 
always put last, otherwise column order is left unmodified.}

\item{tc_last}{A logical value; if \code{TRUE}, \code{tc} columns are always 
put last, otherwise column order is left unmodified. If both \code{sfc_last}
and \code{tc_last} are \code{TRUE}, \code{tc} columns are put last.}
}
\value{
An object of class \code{sftime}.
}
\description{
Construct a \code{sftime} object from all its components
}
\examples{
## construction with a sfc object
library(sf)
g <- st_sfc(st_point(1:2))
tc <- st_tc(Sys.time())
st_sftime(a = 3, g, time = tc)
## construction with an sf object
\dontrun{st_sftime(st_sf(a = 3, g), time = tc) # error, because if ... contains a data.frame-like object, no other objects may be passed through ... . Instead, add the time column before.}
st_sftime(st_sf(a = 3, g, time = tc))
}