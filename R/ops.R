#' Calculate On-Base Plus Slugging Percentage (OPS)
#'
#' This function calculates the On-Base Plus Slugging (OPS) percentage, a composite statistic used
#' to measure a baseball player's overall batting ability. It sums the on-base percentage (OBP) and
#' the slugging percentage (SLG). If not provided, the function will attempt to calculate OBP and
#' SLG using the provided stats.
#'
#' @param hits Numeric vector, the number of hits (required if on_base_percentage is missing).
#' @param walks Numeric vector, the number of walks (required if on_base_percentage is missing).
#' @param hit_by_pitches Numeric vector, the number of times hit by pitch (required if on_base_percentage is missing).
#' @param at_bats Numeric vector, the number of at bats (required for both OBP and SLG calculations).
#' @param sac_flys Numeric vector, the number of sacrifice flies (required if on_base_percentage is missing).
#' @param total_bases Numeric vector, the total number of bases (required if slugging_percentage is missing).
#' @param on_base_percentage Numeric vector, precomputed on-base percentage (optional).
#' @param slugging_percentage Numeric vector, precomputed slugging percentage (optional).
#'
#' @return Numeric vector, the OPS value.
#' @export
#'
#' @examples
#' on_base_plus_slugging_percentage(hits = 50, walks = 30, hit_by_pitches = 5, at_bats = 150,
#'                                  sac_flys = 3, total_bases = 120)
#' on_base_plus_slugging_percentage(at_bats = 150, total_bases = 120, on_base_percentage = 0.320,
#'                                  slugging_percentage = 0.480)

on_base_plus_slugging_percentage <- function(hits = NULL, walks = NULL, hit_by_pitches = NULL, at_bats = NULL, sac_flys = NULL, total_bases = NULL, on_base_percentage = NULL, slugging_percentage = NULL) {
  # Calculate on base percentage if not provided
  if (is.null(on_base_percentage)) {
    if (!is.null(hits) && !is.null(walks) && !is.null(hit_by_pitches) && !is.null(at_bats) && !is.null(sac_flys)) {
      on_base_percentage <- on_base_percentage(hits, walks, hit_by_pitches, at_bats, sac_flys)
    } else {
      stop("Not enough information to calculate on base percentage.")
    }
  }

  # Calculate slugging percentage if not provided
  if (is.null(slugging_percentage)) {
    if (!is.null(total_bases) && !is.null(at_bats)) {
      slugging_percentage <- slugging_percentage(total_bases, at_bats)
    } else {
      stop("Not enough information to calculate slugging percentage.")
    }
  }

  # Ensure that OBP and SLG have been calculated or provided
  if (is.null(on_base_percentage) || is.null(slugging_percentage)) {
    stop("On-base percentage and slugging percentage must be available to compute OPS.")
  }

  # Calculate on base plus slugging percentage
  on_base_plus_slugging <- on_base_percentage + slugging_percentage

  return(on_base_plus_slugging)
}
