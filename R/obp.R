#' Calculate On-Base Percentage
#'
#' This function calculates the on-base percentage (OBP), which measures how frequently a batter
#' reaches base per plate appearance. The formula used is (H + BB + HBP) / (AB + BB + HBP + SF),
#' where H is hits, BB is walks, HBP is hit by pitches, AB is at bats, and SF is sacrifice flies.
#'
#' @param hits Numeric vector, the number of hits.
#' @param walks Numeric vector, the number of walks.
#' @param hit_by_pitches Numeric vector, the number of times hit by pitch.
#' @param at_bats Numeric vector, the number of at bats.
#' @param sac_flys Numeric vector, the number of sacrifice flies.
#'
#' @return Numeric vector, the on-base percentage rounded to three decimal places. If the denominator
#'         in the calculation is zero, the function returns NA to avoid division by zero errors.
#' @export
#'
#' @examples
#' on_base_percentage(50, 10, 5, 200, 3)
#' on_base_percentage(c(50, 30), c(10, 20), c(5, 0), c(200, 100), c(3, 1))

on_base_percentage <- function(hits, walks, hit_by_pitches, at_bats, sac_flys) {
  # Validate input types and ensure vectors are the same length
  if (!is.numeric(hits) || !is.numeric(walks) || !is.numeric(hit_by_pitches) || !is.numeric(at_bats) || !is.numeric(sac_flys)) {
    stop("All inputs must be numeric.")
  }
  if (length(hits) != length(walks) || length(walks) != length(hit_by_pitches) || length(hit_by_pitches) != length(at_bats) || length(at_bats) != length(sac_flys)) {
    stop("All input vectors must be of the same length.")
  }

  # Calculate the denominator
  denominator <- at_bats + walks + hit_by_pitches + sac_flys

  # Handle potential division by zero
  if (any(denominator == 0)) {
    return(rep(NA, length(denominator)))  # Return NA where denominator is zero
  }

  # Calculate OBP
  on_base_percentage <- (hits + walks + hit_by_pitches) / denominator

  # Return rounded result
  return(round(on_base_percentage, 3))
}
