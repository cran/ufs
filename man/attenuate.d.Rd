% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attenuate.d.R
\name{attenuate.d}
\alias{attenuate.d}
\title{Attenuate a Cohen's d estimate for unreliability in the continuous variable}
\usage{
attenuate.d(d, reliability)
}
\arguments{
\item{d}{The value of Cohen's d (that would be obtained with perfect measurements)}

\item{reliability}{The reliability of the measurements of the continuous variable}
}
\value{
The attenuated value of Cohen's d
}
\description{
Measurement error (i.e. the complement of reliability) results in a downward bias
of observed effect sizes. This attenuation can be emulated by this function.
}
\examples{
attenuate.d(.5, .8);
}
\references{
Bobko, P., Roth, P. L., & Bobko, C. (2001). Correcting
the Effect Size of d for Range Restriction and Unreliability.
\emph{Organizational Research Methods, 4}(1), 46–61.
\doi{10.1177/109442810141003}
}
\author{
Gjalt-Jorn Peters & Stefan Gruijters
}
