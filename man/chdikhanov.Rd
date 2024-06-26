% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_chain_indices.R
\name{chdikhanov}
\alias{chdikhanov}
\title{Calculating the monthly chained Dikhanov price index}
\usage{
chdikhanov(data, start, end, interval = FALSE)
}
\arguments{
\item{data}{The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.}

\item{start}{The base period (as character) limited to the year and month, e.g. "2020-03".}

\item{end}{The research period (as character) limited to the year and month, e.g. "2020-04".}

\item{interval}{A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).}
}
\value{
The function returns a value (or vector of values) of the monthly chained Dikhanov price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
}
\description{
This function returns a value (or vector of values) of the monthly chained Dikhanov price index.
}
\examples{
chdikhanov(sugar, start="2018-12", end="2019-04")
\donttest{chdikhanov(milk, start="2018-12", end="2020-01", interval=TRUE)}
}
\references{
{Dikhanov, Y., (2024). \emph{A New Elementary Index Number}. Paper presented at the 18th Meeting of the Ottawa Group on Price Indices, Ottawa, Canada.}
}
