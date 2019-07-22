#' @rdname nncConversion
#' @export
convert.eer.to.d <- function(eer, cer, eventDesirable=TRUE, eventIfHigher=TRUE) {
  if (eventIfHigher) {
    return(stats::qnorm(eer) - stats::qnorm(cer));
  } else {
    return(stats::qnorm(cer) - stats::qnorm(eer));
  }
}
