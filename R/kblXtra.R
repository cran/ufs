#' Wrapper for kableExtra for consistent `ufs` table styling
#'
#' @param x The dataframe to print
#' @param digits,format,escape,table.attr Defaults that are
#' passed to [knitr::kable()]
#' @param print Wther to print the table
#' @param viewer Whether to show the table in the viewer
#' @param ... Additional arguments are passed to [knitr::kable()]
#'
#' @return The table, invisibly.
#' @export
#'
#' @examples kblXtra(mtcars);
kblXtra <- function(x,
                    digits = 2,
                    format="html",
                    escape = FALSE,
                    print=TRUE,
                    viewer = FALSE,
                    table.attr = "style='border:0px solid black !important;'",
                    ...) {

  oldKableViewOption <- getOption("kableExtra_view_html", NULL);
  options(kableExtra_view_html = viewer);

  res <-
    kableExtra::kable_styling(
      knitr::kable(
        x,
        digits = digits,
        format = format,
        escape = escape,
        table.attr = table.attr,
        ...
      ),
      bootstrap_options = "condensed"
    );

  if (print || viewer) {
    print(res);
  }

  if (!is.null(oldKableViewOption)) {
    options(kableExtra_view_html = oldKableViewOption);
  }

  return(invisible(res));

}
