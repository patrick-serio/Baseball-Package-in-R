#' Calculate Isolated Power
#'
#' This function calculates isolated power (ISO), a measure of a batter's raw power, defined as
#' the number of extra bases per at bat. The function can calculate batting average and slugging
#' percentage if not provided, assuming that the necessary other statistics (at_bats, hits, and
#' total_bases) are available.
#'
#' @param at_bats Numeric vector, the number of at bats.
#' @param hits Numeric vector, the number of hits.
#' @param total_bases Numeric vector, the number of total bases achieved.
#' @param batting_average Numeric vector, the batting average (optional).
#' @param slugging_percentage Numeric vector, the slugging percentage (optional).
#'
#' @return Numeric vector, the isolated power value.
#' @export
#'
#' @examples
#' isolated_power(at_bats = c(500, 450), hits = c(150, 135), total_bases = c(250, 230))
#' isolated_power(at_bats = 500, total_bases = 250, batting_average = 0.300)

isolated_power <- function(at_bats = NULL, hits = NULL, total_bases = NULL, batting_average = NULL, slugging_percentage = NULL) {
  # Validate and calculate batting average if not provided
  if (is.null(batting_average)) {
    if (is.null(at_bats) || is.null(hits) || length(at_bats) != length(hits)) {
      stop("To calculate batting average, 'at_bats' and 'hits' must be provided and must have the same length.")
    }
    batting_average <- hits / at_bats
  }

  # Validate and calculate slugging percentage if not provided
  if (is.null(slugging_percentage)) {
    if (is.null(at_bats) || is.null(total_bases) || length(at_bats) != length(total_bases)) {
      stop("To calculate slugging percentage, 'at_bats' and 'total_bases' must be provided and must have the same length.")
    }
    slugging_percentage <- total_bases / at_bats
  }

  # Calculate isolated power
  isolated_power <- slugging_percentage - batting_average

  # Return the value rounded to three decimal places for each element
  return(round(isolated_power, 3))
}
