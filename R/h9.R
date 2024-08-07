#' Calculate Hits Per Nine Innings Pitched
#'
#' This function calculates the number of hits allowed per nine innings pitched. It uses the
#' `convert_innings` function to adjust the innings pitched from the traditional recording format
#' to a decimal format for accurate calculation.
#'
#' @param hits Numeric vector, the number of hits allowed by the pitcher.
#' @param innings_pitched Numeric vector, the innings pitched in recorded format.
#'
#' @return Numeric vector, the number of hits per nine innings, rounded to one decimal place.
#' @export
#'
#' @examples
#' hits_per9(8, 9)
#' hits_per9(c(8, 5), c(9 ,7.2))

hits_per9 <- function(hits, innings_pitched) {
  # Ensure inputs are numeric
  if (!is.numeric(hits) || !is.numeric(innings_pitched)) {
    stop("Both 'hits' and 'innings_pitched' must be numeric.")
  }

  # Convert innings pitched first using the helper function
  adjusted_innings <- convert_innings(innings_pitched)

  # Ensure adjusted innings are not zero to prevent division by zero
  if (any(adjusted_innings == 0)) {
    stop("Innings pitched cannot be zero after adjustment.")
  }

  # Calculate hits per 9 innings
  h_per9 <- (hits / adjusted_innings) * 9

  # Return the value rounded to one decimal place
  return(round(h_per9, 1))
}
