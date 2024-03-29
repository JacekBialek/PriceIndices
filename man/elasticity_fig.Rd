% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_data_processing.R
\name{elasticity_fig}
\alias{elasticity_fig}
\title{Presenting elasticities of substitution for time interval}
\usage{
elasticity_fig(
  data,
  start,
  end,
  method = c("lm"),
  fixedbase = TRUE,
  figure = TRUE,
  date_breaks = "1 month",
  names = c(),
  left = -10,
  right = 10,
  precision = 1e-06
)
}
\arguments{
\item{data}{The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities} (as positive numeric) and \code{prodID} (as numeric, factor or character).}

\item{start}{The base period (as character) limited to the year and month, e.g. "2020-03".}

\item{end}{The research period (as character) limited to the year and month, e.g. "2020-04".}

\item{method}{A vector indicating index formulas for which the CES index will be equated to calculate the elasticity. Acceptable options are \code{lm}, \code{f}, \code{t}, \code{w} and \code{sv} or their combinations.}

\item{fixedbase}{A logical parameter indicating whether the procedure is to work for subsequent months from the considered time interval (\code{fixedbase}=FALSE). Otherwise the period defined by \code{start} plays a role of fixed base month (\code{fixedbase}=TRUE)}

\item{figure}{A logical parameter indicating whether the function returns a figure (TRUE) or a data frame (FALSE) with values of elasticity of substitution.}

\item{date_breaks}{A string giving the distance between breaks on the X axis like "1 month" (default value) or "4 months".}

\item{names}{A character string indicating names of indices used for elasticity approximation (see the \code{method} parameter).}

\item{left}{The beginning of an interval for estimation of each elasticity of substitution (its default value is -10)}

\item{right}{The end of an interval for estimation of each elasticity of substitution (its default value is 10)}

\item{precision}{The precision of estimation (a 'stop' condition for the procedure). A default value of this parameter is 0.000001.}
}
\value{
The function provides a data frame or a figure presenting elasticities of substitution calculated for time interval (see the \code{figure} parameter). The elasticities of substitution can be calculated for subsequent months or for a fixed base month (see the \code{start} parameter) and rest of months from the given time interval (it depends on the \code{fixedbase} parameter). The above-mentioned parameters for compared months are calculated by using the \code{elasticity} function.
}
\description{
The function provides a data frame or a figure presenting elasticities of substitution calculated for time interval.
}
\examples{
\donttest{elasticity_fig (milk,start="2018-12",end="2019-04",figure=TRUE, 
method=c("lm","f"),names=c("LM","Fisher"))}
\donttest{elasticity_fig (milk,start="2018-12",end="2019-06",figure=FALSE)}
}
\references{
{de Haan, J., Balk, B.M., Hansen, C.B. (2010). \emph{Retrospective Approximations of Superlative Price Indexes for Years Where Expenditure Data Is Unavailable.} In: Biggeri, L., Ferrari, G. (eds) Price Indexes in Time and Space. Contributions to Statistics. Physica-Verlag HD.}

{(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
}
