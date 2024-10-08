% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_multilateral_indicators.R
\name{mmontgomery}
\alias{mmontgomery}
\title{Calculating the multilateral Montgomery price and quantity indicators}
\usage{
mmontgomery(
  data,
  start,
  end,
  wstart = start,
  matched = FALSE,
  window = 13,
  interval = FALSE,
  contributions = FALSE,
  prec = 2
)
}
\arguments{
\item{data}{The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.}

\item{start}{The base period (as character) limited to the year and month, e.g. "2020-03".}

\item{end}{The research period (as character) limited to the year and month, e.g. "2020-04".}

\item{wstart}{The first period of the time window (as character) limited to the year and month, e.g. "2019-12".}

\item{matched}{A logical parameter indicating whether the matched sample approach is to be used (if yes, the parameter has the value TRUE).}

\item{window}{The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).}

\item{interval}{A logical parameter indicating whether calculations are to be made for the whole time interval (TRUE) or no (FALSE).}

\item{contributions}{A logical parameter indicating whether contributions of individual products are to be displayed. If it is \code{TRUE}, then contributions are calculated for the the base period \code{start} and the current period \code{end}.}

\item{prec}{A numeric vector indicating precision, i.e. the number of decimal places for presenting results.}
}
\value{
This function returns the multilateral Montgomery price and quantity indicators and optionally also the price and quantity contributions of individual products.
}
\description{
This function returns the multilateral Montgomery price and quantity indicators and optionally also the price and quantity contributions of individual products.
}
\examples{
\donttest{mmontgomery(milk, "2018-12", "2019-12", matched=TRUE, contributions=TRUE)}
\donttest{mmontgomery(coffee, start="2018-12", end="2019-03", interval=TRUE)}
}
\references{
{Montgomery, J. K., (1929). \emph{Is There a Theoretically Correct Price Index of a Group of Commodities?} Rome, International Institute of Agriculture}

{Fox, K.J., (2006). \emph{A Method for Transitive and Additive Multilateral Comparisons: A Transitive Bennet Indicator}. Journal of Economics, 87(1), 73-87.}

{Białek, J., Pawelec, N. (2024). \emph{The use of transitive Montgomery Indicators for scanner data analysis}. Argumenta Oeconomica,  2(53).}
}
