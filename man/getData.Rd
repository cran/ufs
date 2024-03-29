% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getData.R
\name{getData}
\alias{getData}
\alias{getDat}
\title{Use a dialog to load data from an SPSS file}
\usage{
getData(
  filename = NULL,
  file = NULL,
  errorMessage = "[defaultErrorMessage]",
  applyRioLabels = TRUE,
  use.value.labels = FALSE,
  to.data.frame = TRUE,
  stringsAsFactors = FALSE,
  silent = FALSE,
  ...
)

getDat(..., dfName = "dat", backup = TRUE)
}
\arguments{
\item{filename, file}{It is possible to specify a path and filename to load
here. If not specified, the default R file selection dialogue is shown.
\code{file} is still available for backward compatibility but will
eventually be phased out.}

\item{errorMessage}{The error message that is shown if the file does not
exist or does not have the right extension; \verb{[defaultErrorMessage]} is
replaced with a default error message (and can be included in longer
messages).}

\item{applyRioLabels}{Whether to apply the labels supplied by Rio. This will
make variables that has value labels into factors.}

\item{use.value.labels}{Only useful when reading from SPSS files: whether to
read variables with value labels as factors (TRUE) or numeric vectors
(FALSE).}

\item{to.data.frame}{Only useful when reading from SPSS files: whether to
return a dataframe or not.}

\item{stringsAsFactors}{Whether to read strings as strings (FALSE) or
factors (TRUE).}

\item{silent}{Whether to suppress potentially useful information.}

\item{...}{Additional options, passed on to the function used to import the
data (which depends on the extension of the file).}

\item{dfName}{The name of the dataframe to create in the parent environment.}

\item{backup}{Whether to backup an object with name \code{dfName}, if one
already exists in the parent environment.}
}
\value{
getData returns the imported dataframe, with the filename from which it was
read stored in the 'filename' attribute.

getDat is a simple wrapper for \code{getData()} which creates a dataframe in
the parent environment, by default with the name 'dat'. Therefore, calling
\code{getDat()} in the console will allow the user to select a file, and the
data from the file will then be read and be available as 'dat'. If an object
with \code{dfName} (i.e. 'dat' by default) already exists, it will be backed
up with a warning. \code{getDat()} also invisibly returns the data.frame.
}
\description{
\code{getData()} and \code{getDat()} provide an easy way to load SPSS datafiles.
}
\note{
getData() currently can't read from LibreOffice or OpenOffice files.
There doesn't seem to be a platform-independent package that allows this.
Non-CRAN package ROpenOffice from OmegaHat should be able to do the trick,
but fails to install (manual download and installation using
https://www.omegahat.org produces "ERROR: dependency 'Rcompression' is not
available for package 'ROpenOffice'" - and manual download and installation
of RCompression produces "Please define LIB_ZLIB; ERROR: configuration
failed for package 'Rcompression'"). If you have any suggestions, please let
me know!
}
\examples{


\dontrun{
### Open a dialogue to read an SPSS file
getData();
}

}
\keyword{file}
\keyword{univar}
\keyword{utilities}
