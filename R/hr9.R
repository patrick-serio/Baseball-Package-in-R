#' Calculate Home Runs Per Nine Innings Pitched
#'
#' This function calculates the number of home runs allowed per nine innings pitched. It uses the
#' `convert_innings` function to adjust the innings pitched from the traditional recording format
#' to a decimal format before performing the calculation.
#'
#' @param home_runs Numeric vector, the number of home runs allowed by the pitcher.
#' @param innings_pitched Numeric vector, the innings pitched in recorded format.
#'
#' @return Numeric vector, the number of home runs per nine innings, rounded to one decimal place.
#' @export
#'
#' @examples
#' homeruns_per9(3, 9)
#' homeruns_per9(c(3, 4), c(9, 7.2))

homeruns_per9 <- function(home_runs, innings_pitched) {
  # Ensure inputs are numeric
  if (!is.numeric(home_runs) || !is.numeric(innings_pitched)) {
    stop("Both 'home_runs' and 'innings_pitched' must be numeric.")
  }

  # Convert innings pitched first using the helper function
  adjusted_innings <- convert_innings(innings_pitched)

  # Ensure adjusted innings are not zero to prevent division by zero
  if (any(adjusted_innings == 0)) {
    stop("Innings pitched cannot be zero after adjustment.")
  }

  # Calculate home runs per 9 innings
  hr_per9 <- (home_runs / adjusted_innings) * 9

  # Return the value rounded to one decimal place
  return(round(hr_per9, 1))
}
