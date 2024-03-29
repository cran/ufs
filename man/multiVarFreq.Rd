% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiVarFreq.R
\name{multiVarFreq}
\alias{multiVarFreq}
\title{Generate a table collapsing frequencies of multiple variables}
\usage{
multiVarFreq(data, items = NULL, labels = NULL, sortByMean = TRUE)
}
\arguments{
\item{data}{The dataframe containing the variables.}

\item{items}{The variable names.}

\item{labels}{Labels can be provided which will be set as row names when
provided.}

\item{sortByMean}{Whether to sort the rows by mean value for each variable
(only sensible if the possible values are numeric).}
}
\value{
The resulting dataframe, but with class 'multiVarFreq' prepended to
allow pretty printing.
}
\description{
This function can be used to efficiently combine the frequencies of
variables with the same possible values. The frequencies are collapsed into
a table with the variable names as row names and the possible values as
column (variable) names.
}
\examples{

multiVarFreq(mtcars, c('gear', 'carb'));

}
\seealso{
\code{\link[=table]{table()}}
}
\author{
Gjalt-Jorn Peters

Maintainer: Gjalt-Jorn Peters \href{mailto:gjalt-jorn@userfriendlyscience.com}{gjalt-jorn@userfriendlyscience.com}
}
\keyword{utilities}
