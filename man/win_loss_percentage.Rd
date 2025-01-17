% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WL_ratio.R
\name{win_loss_percentage}
\alias{win_loss_percentage}
\title{Calculate Win-Loss Percentage}
\usage{
win_loss_percentage(wins, losses)
}
\arguments{
\item{wins}{Numeric vector, the number of wins.}

\item{losses}{Numeric vector, the number of losses.}
}
\value{
Numeric vector, the win-loss percentage rounded to three decimal places.
        If the total number of games (wins + losses) is zero, the function returns NA to prevent division by zero.
}
\description{
This function computes the winning percentage, which is the proportion of total games won.
It is calculated as the number of wins divided by the total number of games (wins + losses).
}
\examples{
win_loss_percentage(10, 5)
win_loss_percentage(c(10, 20), c(5, 20))
}
