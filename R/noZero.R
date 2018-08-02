#' Remove one or more zeroes before the decimal point
#'
#' @param str The character string to process.
#'
#' @return The processed string.
#' @seealso \code{\link{formatCI}}, \code{\link{formatR}}, \code{\link{formatPvalue}}
#' @export
#'
#' @examples noZero("0.3");

noZero <- function (str) {
  return(gsub("0*\\.", ".", str));
}
