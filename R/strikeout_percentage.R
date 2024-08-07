#' Calculate Strikeout Percentage
#'
#' This function computes the strikeout percentage, which is the proportion of at-bats that result
#' in strikeouts, expressed as a percentage. It is calculated as the number of strikeouts divided
#' by the number of at-bats, multiplied by 100.
#'
#' @param strikeouts Numeric vector, the number of strikeouts.
#' @param at_bats Numeric vector, the number of at-bats.
#'
#' @return Numeric vector, the strikeout percentage rounded to one decimal place.
#'         If the number of at-bats is zero, the function returns NA to prevent division by zero.
#' @export
#'
#' @examples
#' strikeout_percentage(50, 200)
#' strikeout_percentage(c(50, 30), c(200, 100))

strikeout_percentage <- function(strikeouts, at_bats) {
  # Check that inputs are numeric
  if (!is.numeric(strikeouts) || !is.numeric(at_bats)) {
    stop("Both 'strikeouts' and 'at_bats' must be numeric.")
  }

  # Check for the same length of vectors
  if (length(strikeouts) != length(at_bats)) {
    stop("The length of 'strikeouts' and 'at_bats' must be the same.")
  }

  # Handle potential division by zero
  strikeout_percentage <- ifelse(at_bats == 0, NA, round((strikeouts / at_bats) * 100, 1))

  return(strikeout_percentage)
}
