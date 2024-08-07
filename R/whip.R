#' Calculate Walks and Hits Per Inning Pitched (WHIP)
#'
#' This function calculates WHIP, which quantifies the number of walks and hits a pitcher
#' allows per inning pitched. It is a common measure of a pitcher's ability to prevent batters
#' from reaching base.
#'
#' @param walks Numeric vector, the number of walks allowed by the pitcher.
#' @param hits Numeric vector, the number of hits allowed by the pitcher.
#' @param innings_pitched Numeric vector, the innings pitched in traditional recording format.
#'
#' @return Numeric vector, the WHIP value rounded to three decimal places.
#'
#' @export
#'
#' @examples
#' whip(20, 50, 45.1)
#' whip(c(20, 30), c(50, 60), c(45.1, 50.2))

whip <- function(walks, hits, innings_pitched) {
  # Ensure inputs are numeric
  if (!is.numeric(walks) || !is.numeric(hits) || !is.numeric(innings_pitched)) {
    stop("All inputs ('walks', 'hits', 'innings_pitched') must be numeric.")
  }

  # Convert innings pitched first using the helper function
  adjusted_innings <- convert_innings(innings_pitched)

  # Ensure adjusted innings are not zero to prevent division by zero
  if (any(adjusted_innings == 0)) {
    stop("Innings pitched cannot be zero after adjustment.")
  }

  # Calculate WHIP
  whip <- (walks + hits) / adjusted_innings

  # Return the value rounded to two decimal places
  return(round(whip, 2))
}
