\name{colIds}
\alias{colIds}
\alias{colMaxs}
\alias{colMedians}
\alias{colMins}
\alias{colRanges}
\alias{rowIds}
\alias{rowMaxs}
\alias{rowMins}
\alias{rowRanges}
\title{Column and row summary function}
\concept{splus}
\usage{colIds(x, do.NULL=TRUE, prefix="col")
colMaxs(x, na.rm = FALSE, dims = 1, n = NULL)
colMedians(x, na.rm=FALSE)
colMins(x, na.rm = FALSE, dims = 1, n = NULL)
colRanges(x, na.rm = FALSE, dims = 1, n = NULL)
rowIds(x, do.NULL=TRUE, prefix="row")
rowMaxs(x, na.rm = FALSE, dims = 1, n = NULL)
rowMins(x, na.rm = FALSE, dims = 1, n = NULL)
rowRanges(x, na.rm = FALSE, dims = 1, n = NULL)
}
\description{Summarizes the columns or rows of a rectangular object.}

\arguments{
\item{x}{rectangular input object such as a matrix or data.frame.}

\item{dims}{if \code{x} has dimension higher than \code{2},
\code{dims} determines what dimensions are summarized.}

\item{do.NULL}{logical for rowIds or colIds.}

\item{n}{number of rows; treat \code{x} as a matrix with \code{n} rows.}

\item{na.rm}{logical, NA values are removed if TRUE.}

\item{prefix}{character string preface for column IDs returned by rowIds or colIds.}
}

\value{
corresponding summary by row or by column.
}

\seealso{
These functions have been removed from this package, because better
versions are available in the \code{aggregate} package:
  \code{\link[aggregate]{colStdevs}},
  \code{\link[aggregate]{colVars}},
  \code{\link[aggregate]{rowStdevs}},
  \code{\link[aggregate]{rowVars}},
}


\examples{
## create a matrix, add dimensions, and obtain
## various summaries
x <- matrix(sin(1:20), nrow=4)
dimnames(x) <- list(c("a","b","c","d"), paste("col", 1:5))
colIds(x)
colMaxs(x)
colMedians(x)
colMins(x)
colRanges(x)
rowIds(x)
rowMaxs(x)
rowMins(x)
rowRanges(x)
}
\keyword{utilities}
