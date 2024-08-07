#' Calculate Batting Average
#'
#' This function calculates the batting average, which is the ratio of hits to at-bats,
#' for each player provided in the vectors of hits and at-bats. It ensures that all
#' at-bats entries are greater than zero to avoid division by zero errors, and that
#' both inputs are non-negative.
#'
#' @param hits Numeric vector, the number of successful hits for each player.
#' @param at_bats Numeric vector, the number of times at bat for each player.
#'
#' @return Numeric vector, the batting averages rounded to three decimal places for each player.
#' @export
#'
#' @examples
#' batting_average(c(50, 100), c(150, 300))
#' batting_average(c(0, 0), c(1, 0))  # Error: 'at_bats' cannot be zero or negative.

batting_average <- function(hits, at_bats) {
  # Ensure inputs are numeric
  if (!is.numeric(hits) || !is.numeric(at_bats)) {
    stop("Both 'hits' and 'at_bats' must be numeric.")
  }

  # Check for appropriate values
  if (any(at_bats <= 0)) {
    stop("'at_bats' cannot be zero or negative.")
  }

  # Calculate batting average element-wise
  batting_avg <- hits / at_bats

  # Return the value rounded to three decimal places for each element
  return(round(batting_avg, 3))
}
