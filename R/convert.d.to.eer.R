#' @export
convert.d.to.eer <- function(d, cer, eventDesirable=TRUE, eventIfHigher=TRUE) {
  if (eventIfHigher) {
    return(stats::pnorm((stats::qnorm(cer) + d)));
  } else {
    return(1 - stats::pnorm((stats::qnorm(1-cer) + d)));
  }
}
