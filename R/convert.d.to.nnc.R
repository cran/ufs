#' Helper functions for Numbers Needed for Change
#'
#' These functions are used by [behaviorchange::nnc()] to compute the Numbers
#' Needed for Change, but are also available for manual use.
#'
#' @aliases convert.d.to.nnc convert.d.to.eer convert.cer.to.d convert.eer.to.d
#' @param d The value of Cohen's *d*.
#' @param cer The Control Event Rate.
#' @param eer The Experimental Event Rate.
#' @param r The correlation between the determinant and behavior (for mediated
#' Numbers Needed for Change).
#' @param eventDesirable Whether an event is desirable or undesirable.
#' @param eventIfHigher Whether scores above or below the threshold are
#' considered 'an event'.
#' @return The converted value.
#' @author Gjalt-Jorn Peters & Stefan Gruijters
#'
#' Maintainer: Gjalt-Jorn Peters <gjalt-jorn@@userfriendlyscience.com>
#' @seealso [behaviorchange::nnc()]
#' @references Gruijters, S. L., & Peters, G. Y. (2019). Gauging the
#' impact of behavior change interventions: A tutorial on the Numbers
#' Needed to Treat. *PsyArXiv.* doi:[10.31234/osf.io/2bau7](https://doi.org/10.31234/osf.io/2bau7)
#' @keywords utilities
#' @rdname nncConversion
#' @examples
#'
#' convert.d.to.eer(d=.5, cer=.25);
#' convert.d.to.nnc(d=.5, cer=.25);
#'
#' @export convert.d.to.nnc
convert.d.to.nnc <- function(d, cer, r = 1, eventDesirable=TRUE, eventIfHigher=TRUE) {

  ### Based on http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0019070
  ### Consistent with http://rpsychologist.com/d3/cohend/

  d <- convert.r.to.d(convert.d.to.r(d) * r);

  if (is.null(cer)) {
    # if (eventDesirable) {
    #   return(1 / (2 * pnorm(d / sqrt(2)) - 1));
    # } else {
    cat0("Not implemented yet!");
    # }
  } else {
    eer <- convert.d.to.eer(d, cer, eventDesirable=eventDesirable, eventIfHigher=eventIfHigher);
    if (eventDesirable) {
      nnc <- 1 / (eer - cer);
    } else {
      nnc <- 1 / (cer - eer);
    }
  }
  attr(nnc, 'eer') <- eer;
  return(nnc);
}
