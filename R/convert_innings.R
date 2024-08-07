#' Convert Innings Pitched from Recorded to Decimal Format
#'
#' This function converts innings pitched from the traditional recording format (where fractions
#' of innings are recorded as 0.1 and 0.2 for one and two outs, respectively) to a decimal format
#' where fractions represent parts of three outs in an inning (e.g., 0.33 and 0.67). Inputs that
#' are not 0.1 or 0.2 in the fractional part are considered to have zero outs in that fraction.
#'
#' @param innings_pitched Numeric vector, innings pitched in recorded format.
#'
#' @return Numeric or numeric vector, innings pitched in decimal format.
#' @export
#'
#' @examples
#' convert_innings(5.1)  # Expected output: 5.33
#' convert_innings(6.2)  # Expected output: 6.67
#' convert_innings(7)  # Expected output: 7
#' convert_innings(c(5.1, 6.2, 7))  # Expected output: c(5.33, 6.67, 7.00)
#' convert_innings("5.1")  # Error: 'innings_pitched' must be numeric.

convert_innings <- function(innings_pitched) {
  # Ensure input is numeric
  if (!is.numeric(innings_pitched)) {
    stop("'innings_pitched' must be numeric.")
  }

  # Separate the integer part from the fractional part
  whole_part <- floor(innings_pitched)
  fractional_part <- innings_pitched - whole_part

  # Round fractional part to nearest hundredth
  fractional_part <- round(fractional_part, 2)

  # Convert fractional part from 0.1 and 0.2 to 0.33 and 0.67
  converted_fraction <- ifelse(fractional_part == 0.1, 1/3,
                               ifelse(fractional_part == 0.2, 2/3, 0))

  # Combine the whole part with the converted fractional part
  decimal_format <- whole_part + converted_fraction

  # Return the value rounded to two decimal places for each element
  return(round(decimal_format, 2))
}
