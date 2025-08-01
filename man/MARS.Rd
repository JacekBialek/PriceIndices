% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_data_processing.R
\name{MARS}
\alias{MARS}
\title{Data stratification via the MARS method}
\usage{
MARS(
  data = data.frame(),
  start,
  end,
  attributes = c(),
  n = 3,
  strategy = "two_months"
)
}
\arguments{
\item{data}{The user's data frame with information about products. It must contain attributes: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric), \code{prodID} (as numeric or character) and the attributes indicated by the 'attributes' parameter.}

\item{start}{The base period \code{0} (as character) limited to the year and month, e.g. "2020-03".}

\item{end}{The research period \code{t} (as character) limited to the year and month, e.g. "2020-04".}

\item{attributes}{A character vector with column names specifying the product attributes.}

\item{n}{Parameter needed only if \code{last_months} strategy is selected. This parameter specifies how many last months are to be taken into account for calculating the average MARS value.}

\item{strategy}{A variable that determines how to calculate the degree of product match, the degree of homogeneity (the weighted R squared measure) and the final MARS score. Available options are: \code{two_months} (only base and current periods are considered, i.e. the MARS score is computed for periods \code{0} and {t}), \code{interval_base} (MARS scores are calculated for each pair of periods: (0,1), (0,2), ...(0,t) and the geometric mean of these values is returned), \code{interval_chain} (MARS scores are calculated for each pair of periods: (0,1), (1,2), ...(t-1,t) and the geometric mean of these values is returned), \code{interval_pairs} (MARS scores are calculated for each pair of periods: (a,b) from the interval {0,1,2,...,t} and the geometric mean of these values is returned), \code{last_months} (MARS scores are calculated for each pair of periods: (t-n,t), (t-n+1,2), ...(t-1,t) and the geometric mean of these values is returned).}
}
\value{
This function groups prodIDs into strata (‘products’) by balancing two measures: an explained variance (R squared) measure for the ‘homogeneity’ of prodIDs within products, while the second expresses the degree to which products can be ‘matched’ over time with respect to a comparison period. The resulting product ‘match adjusted R squared’ (MARS) combines explained variance in product prices with product match over time, so that different stratification schemes can be ranked according to the combined measure. Any combination of attributes is taken into account when creating stratas. For example, for a set of attributes {A, B, C}, the stratas created by the following attribute combinations are considered: A, B, C, A-B, A-C, B-C, A-B-C.The function returns a list with the following elements: \code{scores} (with scores for degrees of product match and product homogeneity, as well as for MARS measure), \code{best_partition} (with the name of the partition for which the highest indication of the MARS measure was obtained), and \code{data_MARS} (with a data frame obtained by replacing the original prodIDs with identifiers created based on the selected best partition).
}
\description{
This function groups prodIDs into strata (‘products’) by balancing two measures: an explained variance (R squared) measure for the ‘homogeneity’ of prodIDs within products, while the second expresses the degree to which products can be ‘matched’ over time with respect to a comparison period.
}
\examples{
df<-MARS(data=dataMARS, 
          start="2025-05", end="2025-09",
          attributes=c("brand","size","fabric"),
          strategy="two_months")
#Results:
df$scores
df$best_partition
df$data_MARS
}
\references{
{Chessa, A.G. (2022). \emph{A Product Match Adjusted R Squared Method for Defining Products with Transaction Data}. Journal of Official Statistics, 37(2), 411–432.}
}
