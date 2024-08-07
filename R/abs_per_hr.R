#' Calculate At-Bats Per Home Run
#'
#' This function calculates the average number of at-bats required for a player
#' to hit one home run for each pair of homeruns and at_bats in the vectors provided.
#' It checks that all entries in the vectors are non-negative numerics and that
#' all homerun entries are positive.
#'
#' @param homeruns Numeric vector, the total number of home runs for each player.
#' @param at_bats Numeric vector, the total number of at-bats for each player.
#'
#' @return Numeric vector, the average number of at-bats per home run for each player,
#'         rounded to one decimal place.
#' @export
#'
#' @examples
#' at_bats_per_homerun(c(8, 400), c(1, 20))
#' at_bats_per_homerun(c(-300, 400), c(10, 20)) # Error: 'at_bats' must be non-negative.
#' at_bats_per_homerun(c(300, 400), c(-5, 20))  # Error: 'homeruns' must be positive.

at_bats_per_homerun <- function(at_bats, homeruns) {
  # Ensure inputs are numeric
  if (!is.numeric(at_bats) || !is.numeric(homeruns)) {
    stop("Both 'at_bats' and 'homeruns' must be numeric.")
  }

  # Check for appropriate values
  if (any(at_bats < 0)) {
    stop("'at_bats' must be non-negative.")
  }

  if (any(homeruns < 0)) {
    stop("'homeruns' must be non-negative.")
  }

  # Calculate at-bats per home run element-wise
  abs_per_hr <- ifelse(homeruns == 0, NA, at_bats / homeruns)

  # Return the value rounded to one decimal place for each element
  return(round(abs_per_hr, 1))
}
