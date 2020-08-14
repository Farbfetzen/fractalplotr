% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/l_system.R
\name{plot.l_system}
\alias{plot.l_system}
\title{Plot L-systems}
\usage{
\method{plot}{l_system}(x, ...)
}
\arguments{
\item{x}{A data frame of class "l_system" as returned from
\code{\link[=convert_l_system]{convert_l_system()}} with the columns x0, y0, x1, and y1.}

\item{...}{Other parameters passed on to \code{\link[graphics:segments]{graphics::segments()}}.}
}
\value{
None
}
\description{
Plot L-systems as line segments.
}
\examples{
L <- grow_l_system("X", list(`X` = "[@.7071-FX][@.7071+FX]"), 10)
L <- convert_l_system(L, pi * 0.2)
plot(L)

}