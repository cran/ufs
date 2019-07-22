#' @rdname nncConversion
#' @export
convert.cer.to.d <- function(cer, eer, eventDesirable=TRUE, eventIfHigher=TRUE) {
  if (eventIfHigher) {
    return(stats::qnorm(eer) - stats::qnorm(cer));
  } else {
    return(stats::qnorm(cer) - stats::qnorm(eer));
  }
}
