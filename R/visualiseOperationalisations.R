# visualiseOperationalisations <- function(shortNames,
#                                          longNames = shortNames) {
#   res <-
#     DiagrammeR::create_graph(directed=FALSE) %>%
#       DiagrammeR::add_full_graph(
#         n = length(shortNames),
#         type = "connected",
#         label = shortNames,
#         rel = "connected_to");
#   print(DiagrammeR::render_graph(res));
#   invisible(res);
# }
#
# visualiseOperationalisations(LETTERS[1:20])
