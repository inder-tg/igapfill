% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_LON.R
\name{get_LON}
\alias{get_LON}
\title{Gets RasterStack's longitude information}
\usage{
get_LON(stack)
}
\arguments{
\item{stack}{RasterStack}
}
\value{
Character vector of length equal to \code{ncol(stack)}.
}
\description{
This function constructs the input for one of the four dimensions 
required by \code{\link[gapfill]{Gapfill}}, namely, \emph{longitude}.
}
\note{
This function may be useful when employing \code{\link[gapfill]{Gapfill}}
independently of the current package.
}
\seealso{
\code{\link[igapfill]{get_3Darray}}, \code{\link[igapfill]{get_LAT}}
}
