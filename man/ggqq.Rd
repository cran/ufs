% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggqq.R
\name{ggqq}
\alias{ggqq}
\title{Easy ggplot Q-Q plot}
\usage{
ggqq(
  x,
  distribution = "norm",
  ...,
  ci = TRUE,
  line.estimate = NULL,
  conf.level = 0.95,
  sampleSizeOverride = NULL,
  observedOnX = TRUE,
  scaleExpected = TRUE,
  theoryLab = "Theoretical quantiles",
  observeLab = "Observed quantiles",
  theme = ggplot2::theme_bw()
)
}
\arguments{
\item{x}{A vector containing the values to plot.}

\item{distribution}{The distribution to (a 'd' and 'q' are prepended, and
the resulting functions are used, e.g. \code{\link{dnorm}} and
\code{\link{qnorm}} for the normal curve).}

\item{\dots}{Any additional arguments are passed to the quantile function
(e.g. \code{\link{qnorm}}). Because of these dots, any following arguments
must be named explicitly.}

\item{ci}{Whether to show the confidence interval.}

\item{line.estimate}{Whether to show the line showing the match with the
specified distribution (e.g. the normal distribution).}

\item{conf.level}{THe confidence of the confidence leven arround the
estimate for the specified distribtion.}

\item{sampleSizeOverride}{It can be desirable to get the confidence
intervals for a different sample size (when the sample size is very large,
for example, such as when this plot is generated by the function
\code{\link{normalityAssessment}}). That different sample size can be
specified here.}

\item{observedOnX}{Whether to plot the observed values (if \code{TRUE}) or
the theoretically expected values (if \code{FALSE}) on the X axis. The other
is plotted on the Y axis.}

\item{scaleExpected}{Whether the scale the expected values to match the
scale of the variable. This option is provided to be able to mimic SPSS' Q-Q
plots.}

\item{theoryLab}{The label for the theoretically expected values (on the Y
axis by default).}

\item{observeLab}{The label for the observed values (on the Y axis by
default).}

\item{theme}{The theme to use.}
}
\value{
A \code{\link{ggplot}} plot is returned.
}
\description{
This function creates a qq-plot with a confidence interval.
}
\details{
This is strongly based on the answer by user Floo0 to a Stack Overflow
question at Stack Exchange (see
\url{https://stackoverflow.com/questions/4357031/qqnorm-and-qqline-in-ggplot2/27191036#27191036}),
also posted at GitHub (see
\url{https://gist.github.com/rentrop/d39a8406ad8af2a1066c}). That code is in
turn based on the \code{qqPlot()} function from the \code{car} package.
}
\examples{

ggqq(mtcars$mpg);

}
\author{
John Fox and Floo0; implemented in this package (and tweaked a bit)
by Gjalt-Jorn Peters.

Maintainer: Gjalt-Jorn Peters \href{mailto:gjalt-jorn@userfriendlyscience.com}{gjalt-jorn@userfriendlyscience.com}
}
\keyword{hplot}
