#' Set caption numbering
#'
#' @param captionName THe name of the caption; normally `fig.cap` or `tab.cap`.
#' @param prefix,suffix The prefix and suffix; any occurrences of `\%s` will be replaced by the
#' number.
#' @param optionName THe name to use for the option that keeps track of the numbering.
#' @param resetCounterTo Whether to reset the counter (as stored in the options), and if so,
#' to what value (set to `FALSE` to prevent resetting).
#'
#' @return `NULL`, invisibly.
#' @export
#'
#' @examples setFigCapNumbering();
#'
#' ### For table captions
#' setFigCapNumbering("tab.cap", "Table %s: ");
setFigCapNumbering <- function (captionName = "fig.cap",
                                prefix = "Figure %s: ",
                                suffix = "",
                                optionName = paste0("setCaptionNumbering_",
                                                    captionName),
                                resetCounterTo = 1) {
  if (!is.null(resetCounterTo) && is.numeric(resetCounterTo)) {
    do.call("options", as.list(structure(resetCounterTo,
                                         names = optionName)))
  }
  hookFunction <-
    function(options) {
      ### Get counter value
      cntr <- getOption(optionName, 1);
      if (!is.numeric(cntr)) {
        cntr <- 1;
      }
      ### Store as latin or roman number in prefix and/or suffix
      newPrefix <-
        sprintf(prefix,
                ifelse(getOption("figure_counter_roman",
                                 FALSE),
                       as.character(utils::as.roman(cntr)),
                       as.character(cntr)));
      newSuffix <-
        sprintf(suffix,
                ifelse(getOption("figure_counter_roman",
                                 FALSE),
                       as.character(utils::as.roman(cntr)),
                       as.character(cntr)));
      ### Store new counter value
      do.call("options",
              stats::setNames(list(cntr + 1),
                              optionName));
      ### Get specified caption and build new full caption
      newCaption <-
        paste0(newPrefix,
               options[[captionName]],
               newSuffix);
      ### Set new caption in options and return options
      options[[captionName]] <-
        newCaption;
      return(options);
    }
  do.call(knitr::opts_hooks$set,
          stats::setNames(list(hookFunction),
                          captionName));
  return(invisible(NULL));
}
