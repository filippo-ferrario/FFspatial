% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map2raster.R
\name{map2raster}
\alias{map2raster}
\title{Map metric to Raster using buffers}
\usage{
map2raster(obs_polygon, side.cell = 0.1, radius = 1, FUN, ...)
}
\arguments{
\item{obs_polygon}{polygon delimiting the area observed (e.g., a transect)}

\item{side.cell}{the size of the (squared) pixel side to be used to subdivide the observation window (i.e., the polygon defining the extent of the raster to be produced).}

\item{radius}{the radius of the buffer to be used.}

\item{FUN}{the funtion to be mapped on the obs_polygon.}

\item{...}{arguments to be passed to FUN.}
}
\description{
Apply a function to calculate a summary metric at buffers created along a regular grid of sampling points whithin the polygon of an observed area.
}
\seealso{
\link{PRindex_body}, \link[sf:st]{sf::st_point}, \link[sf:geos_unary]{sf::st_buffer}
}
\author{
Filippo Ferrario, \email{filippo.f3rrario@gmail.com}
}
