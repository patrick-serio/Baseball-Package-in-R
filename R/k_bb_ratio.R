#' Calculate Strikeout to Walk Ratio
#'
#' This function calculates the ratio of strikeouts to walks. It is a common statistic used
#' to evaluate a pitcher's control and effectiveness.
#'
#' @param strikeouts Numeric vector, the number of strikeouts.
#' @param walks Numeric vector, the number of walks.
#'
#' @return Numeric vector, the strikeout to walk ratio, rounded to two decimal places. If
#'         walks is zero, the function returns NA to avoid division by zero errors.
#' @export
#'
#' @examples
#' strikeout_walk_ratio(50, 10)
#' strikeout_walk_ratio(c(50, 30), c(10, 0))

strikeout_walk_ratio <- function(strikeouts, walks) {
  # Check that inputs are numeric and have the same length
  if (!is.numeric(strikeouts) || !is.numeric(walks)) {
    stop("Both 'strikeouts' and 'walks' must be numeric.")
  }
  if (length(strikeouts) != length(walks)) {
    stop("Inputs 'strikeouts' and 'walks' must be of the same length.")
  }

  # Avoid division by zero by replacing 0 in 'walks' with NA
  k_bb_ratio <- ifelse(walks == 0, NA, round(strikeouts / walks, 2))

  return(k_bb_ratio)
}
