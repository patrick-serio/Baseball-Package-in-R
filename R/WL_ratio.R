#' Calculate Win-Loss Percentage
#'
#' This function computes the winning percentage, which is the proportion of total games won.
#' It is calculated as the number of wins divided by the total number of games (wins + losses).
#'
#' @param wins Numeric vector, the number of wins.
#' @param losses Numeric vector, the number of losses.
#'
#' @return Numeric vector, the win-loss percentage rounded to three decimal places.
#'         If the total number of games (wins + losses) is zero, the function returns NA to prevent division by zero.
#' @export
#'
#' @examples
#' win_loss_percentage(10, 5)
#' win_loss_percentage(c(10, 20), c(5, 20))

win_loss_percentage <- function(wins, losses) {
  # Check that inputs are numeric
  if (!is.numeric(wins) || !is.numeric(losses)) {
    stop("Both 'wins' and 'losses' must be numeric.")
  }

  # Check for negative values which are not valid in this context
  if (any(wins < 0) || any(losses < 0)) {
    stop("Neither 'wins' nor 'losses' can be negative.")
  }

  # Calculate total games to handle division by zero
  total_games <- wins + losses
  if (any(total_games == 0)) {
    return(NA)
  }

  # Calculate win-loss percentage
  win_loss_percentage <- wins / total_games

  # Return the value rounded to three decimal places
  return(round(win_loss_percentage, 3))
}
