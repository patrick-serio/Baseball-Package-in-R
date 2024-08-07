#' Calculate Earned Run Average (ERA)
#'
#' This function calculates the Earned Run Average (ERA), which is the mean number of earned runs
#' a pitcher would give up over nine innings, based on the earned runs allowed and the innings pitched.
#' Innings are adjusted from the traditional recorded format to a decimal format for accurate calculation.
#'
#' @param earned_runs Numeric vector, the number of earned runs allowed by the pitcher.
#' @param innings_pitched Numeric vector, the innings pitched in recorded format.
#'
#' @return Numeric vector, the ERA of the pitcher.
#' @export
#'
#' @examples
#' era(5, 9)
#' era(c(5, 3), c(9, 7.2))

era <- function(earned_runs, innings_pitched) {
  # Ensure inputs are numeric
  if (!is.numeric(earned_runs) || !is.numeric(innings_pitched)) {
    stop("Both 'earned_runs' and 'innings_pitched' must be numeric.")
  }

  # Convert innings pitched first using the helper function
  adjusted_innings <- convert_innings(innings_pitched)

  # Ensure adjusted innings are not zero to prevent division by zero
  if (any(adjusted_innings == 0)) {
    stop("Innings pitched cannot be zero after adjustment.")
  }

  # Calculate ERA
  era_value <- (earned_runs / adjusted_innings) * 9

  # Return ERA value rounded to two decimal places
  return(round(era_value, 2))
}
