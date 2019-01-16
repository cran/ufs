#' Disattentuate a Cohen's d estimate for unreliability in the continuous variable
#'
#' @param d The (attenuated) value of Cohen's d
#' @param reliability The reliability of the measurements of the continuous variable
#'
#' @return The disattenuated value of Cohen's d
#' @export
#'
#' @examples
#' disattenuate.d(.5, .8);
disattenuate.d <- function(d, reliability) {
  return(d / sqrt(reliability));
}
