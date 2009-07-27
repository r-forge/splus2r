\name{stdev}
\alias{stdev}
\title{Standard deviation}
\concept{splus}
\usage{stdev(x, ...)}
\description{Calculates the standard deviation of a series.}

\arguments{
\item{x}{input series.}
\item{...}{optional arguments sent directly to the \code{colVars} function. You can control for example
the removal of NA values prior to analyis via the \code{na.rm} argument, and whether or not an unbiased estimate
is returned ala the \code{unbiased} argument.}
}

\value{
the standard deviation of the input series.
}
\seealso{
\code{\link{var}},\code{\link{colVars}},\code{\link{colStdevs}}.}

\examples{
stdev(c(pi, 1, 3))
}
\keyword{utilities}

