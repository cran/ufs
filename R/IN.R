#' Case insensitive version of \%in\%
#'
#' @param find The element(s) to look up in the vector or matrix.
#' @param table The vector or matrix in which to look up the element(s).
#'
#' @usage find \%IN\% table
#'
#' @return A logical vector.
#' @rdname IN
#' @export
#'
#' @examples letters[1:4] %IN% LETTERS
"%IN%" <- function(find, table) {
  return(toupper(find) %in% toupper(table));
}
