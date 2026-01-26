# Function to remove duplicated observations from a data frame
#' @param data A data frame
#' @return A data frame with duplicated observations removed
#'
#' This function takes a data frame as input and removes duplicated observations
#' based on the specified key columns.
#' The key columns are defined by the `product_id` and `store_id` variables.
#' The function returns a new data frame with the duplicated observations
#' removed.

remove_duplicated_observations <- function(data) {
  contain_duplicates <- data |>
    are_duplicated(index = collected, key = c(product_id, store_id))
  data <- data |>
    dplyr::filter(!contain_duplicates)
}
