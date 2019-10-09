#' Augment Edges
#' 
#' Adds edge attributes derived from vertex attributes by appending with "_from" and "_to"
#'
#' @param .data 
#' @param ... names of the vertex attributes to be added to edges
#'
#' @return a graph with new edge attributes.
#' @export
#'
#' @examples
augment_edges <- function(.data, ...) {
  
  to_join <- select(.data, ...) %>% 
    mutate(.id = row_number())
  .data %>% 
    activate(edges) %>% 
    left_join(
      dplyr::rename_at(to_join, dplyr::vars(-.id), list(~paste0(., '_from'))), 
      by = c('from' = '.id'), copy = TRUE
      ) %>% 
    left_join(
      dplyr::rename_at(to_join, dplyr::vars(-.id), list(~paste0(., '_to'))), 
      by = c('to' = '.id'), copy = TRUE
    ) %>% 
    activate(nodes)
}
