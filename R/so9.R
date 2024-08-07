#' Calculate Strikeouts Per Nine Innings Pitched
#'
#' This function calculates the number of strikeouts a pitcher averages per nine innings pitched.
#' It first adjusts the innings pitched from a traditional recording format to a decimal format
#' for precise calculation.
#'
#' @param strikeouts Numeric vector, the number of strikeouts.
#' @param innings_pitched Numeric vector, the innings pitched in traditional format.
#'
#' @return Numeric vector, the strikeouts per nine innings, rounded to one decimal place.
#'
#' @export
#'
#' @examples
#' strikeouts_per9(50, 45.1)
#' strikeouts_per9(c(50, 75), c(45.1, 50.2))

strikeouts_per9 <- function(strikeouts, innings_pitched) {
  # Ensure inputs are numeric
  if (!is.numeric(strikeouts) || !is.numeric(innings_pitched)) {
    stop("Both 'strikeouts' and 'innings_pitched' must be numeric.")
  }

  # Convert innings pitched first using the helper function
  adjusted_innings <- convert_innings(innings_pitched)

  # Ensure adjusted innings are not zero to prevent division by zero
  if (any(adjusted_innings == 0)) {
    stop("Innings pitched cannot be zero after adjustment.")
  }

  # Calculate strikeouts per 9 innings
  k_per9 <- (strikeouts / adjusted_innings) * 9

  # Return the value rounded to one decimal place
  return(round(k_per9, 1))
}
