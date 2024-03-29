% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_extensions_multilateral_indices.R
\name{gekslm_fbmw}
\alias{gekslm_fbmw}
\title{Extending the multilateral GEKS-LM price index by using the FBMW method.}
\usage{
gekslm_fbmw(data, start, end, sigma)
}
\arguments{
\item{data}{The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).}

\item{start}{The base period (as character) limited to the year and month, e.g. "2019-12".}

\item{end}{The research period (as character) limited to the year and month, e.g. "2020-04".}

\item{sigma}{The elasticity of substitution (a parameter used in the Lloyd-Moulton index formula).}
}
\value{
This function returns a value of the multilateral GEKS-LM price index extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
}
\description{
This function returns a value of the multilateral GEKS-LM price index extended by using the FBMW (Fixed Base Moving Window) method.
}
\examples{
\donttest{geksqm_fbmw(milk, start="2019-12", end="2020-04", r=1.6)}
}
\references{
{Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}

{Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}

{Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}

{Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
}
