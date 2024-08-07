#' Calculate Walk Percentage
#'
#' This function computes the walk percentage, which is the proportion of at-bats that result
#' in walks. It is calculated as the number of walks divided by the number of at-bats.
#'
#' @param walks Numeric vector, the number of walks.
#' @param at_bats Numeric vector, the number of at-bats.
#'
#' @return Numeric vector, the walk percentage rounded to three decimal places.
#'         If the number of at-bats is zero, the function returns NA to prevent division by zero.
#' @export
#'
#' @examples
#' walk_percentage(50, 200)
#' walk_percentage(c(50, 30), c(200, 150))

walk_percentage <- function(walks, at_bats) {
  # Check that inputs are numeric
  if (!is.numeric(walks) || !is.numeric(at_bats)) {
    stop("Both 'walks' and 'at_bats' must be numeric.")
  }

  # Check for the same length of vectors
  if (length(walks) != length(at_bats)) {
    stop("The length of 'walks' and 'at_bats' must be the same.")
  }

  # Handle potential division by zero
  walk_percentage <- ifelse(at_bats == 0, NA, (walks / at_bats) * 100)

  # Return the value rounded to one decimal place for each element
  return(round(walk_percentage, 1))
}
