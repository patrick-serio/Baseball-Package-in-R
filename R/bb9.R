#' Calculate Walks Per Nine Innings Pitched
#'
#' This function calculates the number of walks per nine innings pitched.
#' It uses the `convert_innings` function to adjust the innings pitched from the traditional format
#' to a decimal format before performing the calculation.
#'
#' @param walks Numeric vector, the number of walks allowed by the pitcher.
#' @param innings_pitched Numeric vector, the innings pitched in recorded format.
#'
#' @return Numeric vector, the number of walks per nine innings, rounded to one decimal place.
#' @export
#'
#' @examples
#' walks_per9(7, 30)
#' walks_per9(c(7, 35), c(40, 160))

walks_per9 <- function(walks, innings_pitched) {
  # Ensure inputs are numeric
  if (!is.numeric(walks) || !is.numeric(innings_pitched)) {
    stop("Both 'walks' and 'innings_pitched' must be numeric.")
  }

  # Convert innings pitched first using the helper function
  adjusted_innings <- convert_innings(innings_pitched)

  # Ensure adjusted innings are not zero to prevent division by zero
  if (any(adjusted_innings == 0)) {
    stop("Innings pitched cannot be zero after adjustment.")
  }

  # Calculate walks per 9 innings
  bb_per9 <- (walks / adjusted_innings) * 9

  # Return the value rounded to one decimal place
  return(round(bb_per9, 1))
}
