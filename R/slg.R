#' Calculate Slugging Percentage
#'
#' This function calculates the slugging percentage, a measure of the total bases a batter achieves
#' per at bat. It is calculated as the total number of bases divided by at bats.
#'
#' @param total_bases Numeric vector, the total number of bases achieved.
#' @param at_bats Numeric vector, the number of at bats.
#'
#' @return Numeric vector, the slugging percentage rounded to three decimal places. If the number of
#'         at bats is zero, the function returns NA to prevent division by zero.
#' @export
#'
#' @examples
#' slugging_percentage(120, 300)
#' slugging_percentage(c(120, 150), c(300, 350))

slugging_percentage <- function(total_bases, at_bats) {
  # Check that inputs are numeric
  if (!is.numeric(total_bases) || !is.numeric(at_bats)) {
    stop("Both 'total_bases' and 'at_bats' must be numeric.")
  }

  # Check for the same length of vectors
  if (length(total_bases) != length(at_bats)) {
    stop("The length of 'total_bases' and 'at_bats' must be the same.")
  }

  # Handle potential division by zero
  slugging_percentage <- ifelse(at_bats == 0, NA, round(total_bases / at_bats, 3))

  return(slugging_percentage)
}
