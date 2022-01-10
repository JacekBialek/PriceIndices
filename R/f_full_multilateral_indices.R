#' @title  Calculating the multilateral GEKS price index
#'
#' @description This function returns a value of the multilateral GEKS price index (to be more precise: the GEKS index based on the Fisher formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geks
#' @return This function returns a value of the multilateral GEKS price index (to be more precise: the GEKS index based on the Fisher formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' \donttest{geks(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{geks(milk, start="2018-12", end="2019-12")}
#' @export

geks <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- c()
  while (wstart <= wend)
  {
  t <- substr(wstart, 0, 7)
  dates <- c(dates, t)
  lubridate::month(wstart) <-
  lubridate::month(wstart) + 1
  }
  #main body
  gks <-
  function (tt)
  return (c(fisher(data, tt, end), fisher(data, tt, start)))
  vec <- sapply(dates, gks)
  geks <- prod(vec[1, ] / vec[2, ])
  geks <- geks ^ (1 / window)
  return(geks)
  }

#' @title  Calculating the multilateral GEKS price index based on the Walsh formula (GEKS-W)
#'
#' @description This function returns a value of the multilateral GEKS-W price index, i.e. the GEKS price index based on the superlative Walsh index formula. 
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksw
#' @return This function returns a value of the multilateral GEKS-W price index (to be more precise: the GEKS index based on the Walsh formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Walsh, C. M. (1901). \emph{The Measurement of General Exchange Value}. The MacMillan Company, New York.}
#'
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' \donttest{geksw(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{geksw(milk, start="2018-12", end="2019-12")}
#' @export

geksw <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- c()
  while (wstart <= wend)
  {
  t <- substr(wstart, 0, 7)
  dates <- c(dates, t)
  lubridate::month(wstart) <-
  lubridate::month(wstart) + 1
  }
  #main body
  gksw <-
  function (tt)
  return (c(walsh(data, tt, end), walsh(data, tt, start)))
  vec <- sapply(dates, gksw)
  geksw <- prod(vec[1, ] / vec[2, ])
  geksw <- geksw ^ (1 / window)
  return(geksw)
  }

#' @title  Calculating the multilateral GEKS price index based on the Jevons formula (typical notation: GEKS-J) 
#'
#' @description This function returns a value of the multilateral GEKS-J price index (to be more precise: the GEKS index based on the Jevons formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} is needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksj
#' @return This function returns a value of the multilateral GEKS-J price index (to be more precise: the GEKS index based on the Jevons formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' \donttest{geksj(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{geksj(milk, start="2018-12", end="2019-12")}
#' @export

geksj <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- c()
  while (wstart <= wend)
  {
  t <- substr(wstart, 0, 7)
  dates <- c(dates, t)
  lubridate::month(wstart) <-
  lubridate::month(wstart) + 1
  }
  #main body
  gks <-
  function (tt)
  return (c(jevons(data, tt, end), jevons(data, tt, start)))
  vec <- sapply(dates, gks)
  geksj <- prod(vec[1, ] / vec[2, ])
  geksj <- geksj ^ (1 / window)
  return(geksj)
  }

#' @title  Calculating the multilateral GEKS price index based on the Tornqvist formula (typical notation: GEKS-T or CCDI)
#'
#' @description This function returns a value of the multilateral CCDI price index, i.e. the GEKS price index based on the superlative Tornqvist index formula. 
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname ccdi
#' @return This function returns a value of the multilateral CCDI price index (to be more precise: the GEKS index based on the Tornqvist formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Caves, D.W., Christensen, L.R. and Diewert, W.E. (1982). \emph{Multilateral comparisons of output, input, and productivity using superlative index numbers.} Economic Journal 92, 73-86.}
#' @examples 
#' \donttest{ccdi(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{ccdi(milk, start="2018-12", end="2019-12")}
#' @export

ccdi <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- c()
  while (wstart <= wend)
  {
  t <- substr(wstart, 0, 7)
  dates <- c(dates, t)
  lubridate::month(wstart) <-
  lubridate::month(wstart) + 1
  }
  #main body
  cc <-
  function (tt)
  return (c(tornqvist(data, tt, end), tornqvist(data, tt, start)))
  vec <- sapply(dates, cc)
  ccdi <- prod(vec[1, ] / vec[2, ])
  ccdi <- ccdi ^ (1 / window)
  return(ccdi)
  }

#' @title  Calculating the quality adjusted unit value index (QU index)
#'
#' @description This function returns a value of the quality adjusted unit value index (QU index) for a given set of adjustment factors.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param v The data frame with adjustment factors for at least all matched prodIDs. It must contain two columns: \code{prodID} (as numeric or character) with unique product IDs and \code{values} (as positive numeric) with corresponding adjustment factors.
#' @rdname QU
#' @return This function returns a value of the quality adjusted unit value index (QU index) for a given set of adjustment factors (adjusted factors must be available for all matched prodIDs).  
#' @references
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' ## Creating a data frame with artificial adjustment factors 
#' ## (random numbers from uniform distribution U[1,2])
#' \donttest{prodID<-unique(milk$prodID)}
#' \donttest{values<-stats::runif(length(prodID),1,2)}
#' \donttest{v<-data.frame(prodID,values)}
#' ## Calculating the QU index for the created data frame 'v'
#' \donttest{QU(milk, start="2018-12", end="2019-12", v)}
#' @export

QU<-function(data, start, end, v)
{
if (start==end) return (1)  
if (nrow(data)==0) stop("A data frame is empty")
start<-paste(start,"-01",sep="")
end<-paste(end,"-01",sep="")
start<-as.Date(start)
end<-as.Date(end)
data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))   
Gstart<-matched(data,start,start) 
Gend<-matched(data,end,end)
sale_end<-sales(data,period=end,set=Gend)
sale_start<-sales(data,period=start,set=Gstart)
quantity_end<-quantities(data,period=end,set=Gend)
quantity_start<-quantities(data,period=start,set=Gstart)
#main body
a<-sum(sale_end)
b<-sum(sale_start)
f<-function (prod) {vl<-dplyr::filter(v,v$prodID==prod)
vl<-sum(vl$values)}
val_end<-sapply(Gend,f)
val_start<-sapply(Gstart,f)
c<-sum(val_end*quantity_end)
d<-sum(val_start*quantity_start)
return ((a/b)/(c/d))
}


#' @title  Calculating the multilateral Geary-Khamis price index
#'
#' @description This function returns a value of the multilateral Geary-Khamis price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname gk
#' @return This function returns a value of the multilateral Geary-Khamis price index which considers the time window defined by \code{wstart} and \code{window} parameters. The Geary-Khamis price index is calculated by using a special iterative algorithm from \code{Chessa (2016)}. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Geary, R. G. (1958). \emph{A Note on Comparisons of Exchange Rates and Purchasing Power between Countries.} Journal of the Royal Statistical Society, Series A, 121, 97-99.}
#'
#' {Khamis, S. H. (1970). \emph{Properties and Conditions for the Existence of a new Type of Index Number.} Sankhya Series B32, 81-98.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' \donttest{gk(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{gk(milk, start="2018-12", end="2019-12")}
#' @export

gk <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #calculating end of the window
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  lubridate::day(wend) <-
  lubridate::days_in_month(wend)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstart<=start")
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  #data filtration
  d <-
  dplyr::filter(data, data$time >= wstart & data$time <= wend)
  prodID <- unique(d$prodID)
  if (length(prodID) < 2)
  stop ("At least two prodIDs must be available during the considered time interval")
  #main body
  #initial values of indices
  index1 <- rep(1, window)
  index2 <- rep(2, window)
  #set of dates
  dates <- c()
  wst <- wstart
  while (wst <= wend)
  {
  t <- substr(wst, 0, 7)
  dates <- c(dates, t)
  lubridate::month(wst) <-
  lubridate::month(wst) + 1
  }
  s <-
  function(tt)
  return (sales(d, period = tt, set = prodID))
  q <-
  function(tt)
  return (quantities(d, period = tt, set = prodID))
  expenditure <- sapply(dates, s)
  quantity <- sapply(dates, q)
  #quantity weights - quality adjusted factors vi
  while (sqrt(sum((index1 - index2) ^ 2)) >
  0.005)
  {
  val <- function (i)  {
  xx <-
  function (tt)
  return (expenditure[i, tt] / index1[which(dates == tt)])
  yy <-
  function (tt)
  return (quantity[i, tt])
  x <- sum(sapply(dates, xx))
  y <- sum(sapply(dates, yy))
  return (x / y)
  }
  num_prod <- seq(1:length(prodID))
  values <- sapply(num_prod, val)
  v <- data.frame(prodID, values)
  #series  of indices
  indd <-
  function(tt)
  return (QU(d, substr(wstart, 0, 7), tt, v))
  ind <- sapply(dates, indd)
  index2 <- index1
  index1 <- ind
  }
  result <-
  index1[which(dates == substr(end, 0, 7))] / index1[which(dates == substr(start, 0, 7))]
  result <- result[[1]]
  return (result)
  }
  

#' @title  Calculating the multilateral TPD price index
#'
#' @description This function returns a value of the multilateral TPD (Time Product Dummy) price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname tpd
#' @return This function returns a value of the multilateral TPD price index which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). Please note that a Weighted Least Squares (WLS) regression is run with the expenditure shares in each period serving as weights.To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).  
#' @references
#' {de  Haan,  J.  and  F.  Krsinich  (2014). \emph{Time  Dummy  Hedonic  and  Quality-Adjusted  Unit Value Indexes: Do They Really Differ?} Paper presented at the Society for Economic Measurement Conference, 18-20 August 2014, Chicago, U.S.}
#' @examples 
#' \donttest{tpd(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{tpd(milk, start="2018-12", end="2019-12")}
#' @export

tpd <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #calculating end of the window
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  lubridate::day(wend) <-
  lubridate::days_in_month(wend)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstart<=start")
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  #data filtration,i.e. we obtan products which are available during the time window
  d <-
  dplyr::filter(data, data$time >= wstart & data$time <= wend)
  products <- unique(d$prodID)
  if (length(products) < 2)
  stop ("At least two prodIDs must be available during the considered time interval")
  #main body
  dates <-
  c()  #vector with all dates written as 7 signs from the time interval [1,T]
  wst <- wstart
  lubridate::month(wst) <-
  lubridate::month(wst) + 1
  while (wst <= wend)
  {
  dates <- c(dates, substr(wst, 0, 7))
  lubridate::month(wst) <-
  lubridate::month(wst) + 1
  }
  #dates of availability of products
  av <-
  list() #list of dates when the given i-th product is available
  for (i in (1:length(products)))
  {
  d0 <- dplyr::filter(d, d$prodID == products[i])
  time <- unique(d0$time)
  time <- substr(time, 0, 7)
  av[[i]] <- time
  }
  av_dates <-
  c() #avaiable dates as one vector
  for (i in (1:length(products)))
  av_dates <- c(av_dates, av[[i]])
  #vector connected with the alfa parameter in TPD model
  alfa <-
  replicate(length(av_dates), 1) #unit vector
  #unit vectors corresponding to products
  vec <- list()
  for (i in (1:length(products)))
  vec[[i]] <- replicate(length(av[[i]]), 1)
  #gamma vectors connected with Di (i=1,2,...N-1)
  gm <- list()
  for (i in (1:(length(products) - 1))) {
  bin <- c()
  for (k in (1:length(products)))
  {
  if (i == k)
  bin <- c(bin, vec[[k]])
  else
  bin <- c(bin, vec[[k]] - 1)
  }
  gm[[i]] <- bin
  }
  #sigma vectors for time moments 1,2,...,T
  sigma <- list()
  for (i in (1:length(dates)))  {
  sg <- c()
  for (k in (1:length(av_dates))) {
  if (dates[i] == av_dates[k])
  sg <- c(sg, 1)
  else
  sg <- c(sg, 0)
  }
  sigma[[i]] <- sg
  }
  #vector of log prices
  logprices <- c()
  for (i in (1:length(products))) {
  for (k in (1:length(av[[i]])))
  logprices <-
  c(logprices, log(price(
  d, period = av[[i]][k], ID = products[i]
  )))
  }
  #vector of expenditures
  weights <- c()
  for (i in (1:length(products))) {
  for (k in (1:length(av[[i]])))
  weights <-
  c(
  weights,
  price(d, period = av[[i]][k], ID = products[i]) * quantity(d, period = av[[i]][k], ID =
  products[i])
  )
  }
  weights <- diag(weights)
  #creating matrix X
  x <- alfa
  for (i in (1:(length(products) - 1)))
  x <- c(x, gm[[i]])
  for (i in (1:length(dates)))
  x <- c(x, sigma[[i]])
  x <-
  matrix(x,
  nrow = length(av_dates),
  ncol = length(products) + length(dates))
  #estimation of parameters
  b <- t(x) %*% weights
  b <- b %*% x
  b <- solve(b)
  b <- b %*% t(x)
  b <- b %*% weights
  b <- b %*% logprices
  #index calculation
  if (wstart == start)
  return (exp(b[length(products) + dist(wstart, end)]))
  else
  return (exp(b[length(products) + dist(wstart, end)]) / exp(b[length(products) +
  dist(wstart, start)]))
  }


#' @title  Calculating the multilateral SPQ price index 
#'
#' @description This function returns a value of the multilateral SPQ price index which is based on the relative price and quantity dissimilarity measure.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. '2019-03'.
#' @param end The research period (as character) limited to the year and month, e.g. '2019-07'.
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE). 
#' @rdname SPQ
#' @return This function returns a value of the multilateral SPQ price index which is based on the relative price and quantity dissimilarity measure (see \code{References}). If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}}, \code{\link{final_index}} or \code{\link{final_index2}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or \code{\link{final_index2}} function). 
#' @references
#' {Diewert, E. (2020). \emph{The Chain Drift Problem and Multilateral Indexes.} Chapter 6 in: Consumer Price Index Theory (draft)}
#'
#' @examples 
#' SPQ(sugar, start="2018-12",end="2019-02")
#' \donttest{SPQ(milk, start="2018-12",end="2019-12",interval=TRUE)}
#' @export

SPQ <- function (data, start, end, interval = FALSE)
{
if (nrow(data) == 0)
stop("A data frame is empty")
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
if (start > end)
stop("parameters must satisfy: start<=end")
if (start == end)
return (1)

times <- c()
while (start <= end)

{
times <- c(times, substr(start, 0, 7))
lubridate::month(start) <- lubridate::month(start) + 1
}
if (length(times) == 2)
{
if (interval == FALSE)
return (fisher(data, start = times[1], end = times[2]))
else
return (c(1, fisher(
data, start = times[1], end = times[2]
)))
}
spq <- c(1)
spq <- c(spq, fisher(data, start = times[1], end = times[2]))
for (i in 3:length(times))
{
#main body

#delta sp for r=1,2,...,i-1 (we drop the last element)
sp <-
dissimilarity_fig(
data,
start = times[1],
end = times[i],
type = "pq",
benchmark = "end",
figure = FALSE
)$dissimilarity[-i]
#position of the minimal element
pos_min <- max(which(sp == min(sp)))
spq <-
c(spq, fisher(data, start = times[pos_min], end = times[i]) * spq[pos_min])
}
if (interval == TRUE)
return (spq)
else
return (spq[length(spq)])
}


#' @title  Calculating the multilateral weighted WGEKS price index
#'
#' @description This function returns a value of the multilateral weighted WGEKS price index (to be more precise: the weighted GEKS index based on the Fisher formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname wgeks
#' @return This function returns a value of the multilateral weighted WGEKS price index (to be more precise: the weighted GEKS index based on the Fisher formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' \donttest{wgeks(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{wgeks(milk, start="2018-12", end="2019-12")}
#' @export

wgeks <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- c()
  while (wstart <= wend)
  {
  t <- substr(wstart, 0, 7)
  dates <- c(dates, t)
  lubridate::month(wstart) <-
  lubridate::month(wstart) + 1
  }
  #main body
  wgks <-
  function (tt)
  return (c(fisher(data, start=tt, end=end), fisher(data, start=tt, end=start)))
  vec <- sapply(dates, wgks)
  sales_in_time <-
  function (tt)
  return (sum(sales(data, tt)))
  expenditures <-
  sapply(dates, sales_in_time)
  expenditures <-
  expenditures / sum(expenditures)
  wgeks <-
  prod((vec[1, ] / vec[2, ]) ^ expenditures)
  return(wgeks)
  }
  
#' @title  Calculating the multilateral GEKS-L price index
#'
#' @description This function returns a value of the multilateral GEKS-L price index (to be more precise: the GEKS index based on the Laspeyres formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksl
#' @return This function returns a value of the multilateral GEKS-L price index (to be more precise: the GEKS index based on the Laspeyres formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function). 
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' \donttest{geksl(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{geksl(milk, start="2018-12", end="2019-12")}
#' @export

geksl <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- c()
  while (wstart <= wend)
  {
  t <- substr(wstart, 0, 7)
  dates <- c(dates, t)
  lubridate::month(wstart) <-
  lubridate::month(wstart) + 1
  }
  #main body
  gksl <-
  function (tt)
  return (c(nl(data, start=tt, end=end), nl(data, start=tt, end=start)))
  vec <- sapply(dates, gksl)
  geksl <- prod(vec[1, ] / vec[2, ])
  geksl <- geksl ^ (1 / window)
  return(geksl)
  }

#' @title  Calculating the multilateral weighted WGEKS-L price index
#'
#' @description This function returns a value of the multilateral weighted WGEKS-L price index (to be more precise: the weighted GEKS index based on the Laspeyres formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname wgeksl
#' @return This function returns a value of the multilateral weighted WGEKS-L price index (to be more precise: the weighted GEKS index based on the Laspeyres formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' \donttest{wgeksl(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{wgeksl(milk, start="2018-12", end="2019-12")}
#' @export

wgeksl <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- c()
  while (wstart <= wend)
  {
  t <- substr(wstart, 0, 7)
  dates <- c(dates, t)
  lubridate::month(wstart) <-
  lubridate::month(wstart) + 1
  }
  #main body
  wgksl <-
  function (tt)
  return (c(nl(data, start=tt, end=end), nl(data, start=tt, end=start)))
  vec <- sapply(dates, wgksl)
  sales_in_time <-
  function (tt)
  return (sum(sales(data, tt)))
  expenditures <-
  sapply(dates, sales_in_time)
  expenditures <-
  expenditures / sum(expenditures)
  wgeksl <-
  prod((vec[1, ] / vec[2, ]) ^ expenditures)
  return(wgeksl)
  }


#' @title  Calculating the multilateral GEKS-GL price index
#'
#' @description This function returns a value of the multilateral GEKS-GL price index (to be more precise: the GEKS index based on the geometric Laspeyres formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksgl
#' @return This function returns a value of the multilateral GEKS-GL price index (to be more precise: the GEKS index based on the geometric Laspeyres formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function). 
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' \donttest{geksgl(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{geksgl(milk, start="2018-12", end="2019-12")}
#' @export

geksgl <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- c()
  while (wstart <= wend)
  {
  t <- substr(wstart, 0, 7)
  dates <- c(dates, t)
  lubridate::month(wstart) <-
  lubridate::month(wstart) + 1
  }
  #main body
  gksgl <-
  function (tt)
  return (c(geolaspeyres(data, start=tt, end=end), geolaspeyres(data, start=tt, end=start)))
  vec <- sapply(dates, gksgl)
  geksgl <- prod(vec[1, ] / vec[2, ])
  geksgl <- geksgl ^ (1 / window)
  return(geksgl)
  }

#' @title  Calculating the multilateral weighted WGEKS-GL price index
#'
#' @description This function returns a value of the multilateral weighted WGEKS-GL price index (to be more precise: the weighted GEKS index based on the geometric Laspeyres formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname wgeksgl
#' @return This function returns a value of the multilateral weighted WGEKS-GL price index (to be more precise: the weighted GEKS index based on the geometric Laspeyres formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' \donttest{wgeksgl(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{wgeksgl(milk, start="2018-12", end="2019-12")}
#' @export

wgeksgl <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- c()
  while (wstart <= wend)
  {
  t <- substr(wstart, 0, 7)
  dates <- c(dates, t)
  lubridate::month(wstart) <-
  lubridate::month(wstart) + 1
  }
  #main body
  wgksgl <-
  function (tt)
  return (c(geolaspeyres(data, start=tt, end=end), geolaspeyres(data, start=tt, end=start)))
  vec <- sapply(dates, wgksgl)
  sales_in_time <-
  function (tt)
  return (sum(sales(data, tt)))
  expenditures <-
  sapply(dates, sales_in_time)
  expenditures <-
  expenditures / sum(expenditures)
  wgeksgl <-
  prod((vec[1, ] / vec[2, ]) ^ expenditures)
  return(wgeksgl)
  }


#' @title  Calculating the multilateral GEKS-AQU price index
#'
#' @description This function returns a value of the multilateral GEKS-AQU price index (to be more precise: the GEKS index based on the asynchronous quality adjusted unit value formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksaqu
#' @return This function returns a value of the multilateral GEKS-AQU price index (to be more precise: the GEKS index based on the asynchronous quality adjusted unit value formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function). 
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' \donttest{geksaqu(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{geksaqu(milk, start="2018-12", end="2019-12")}
#' @export

geksaqu <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- c()
  while (wstart <= wend)
  {
  t <- substr(wstart, 0, 7)
  dates <- c(dates, t)
  lubridate::month(wstart) <-
  lubridate::month(wstart) + 1
  }
  #data frame with quality adjusted factors
  availableID<-available(data,period1=dates[1],period2=dates[length(dates)],interval=TRUE)
  
  fi<-function (ID) {
    nom<-c()
    denom<-c()
    for (d in dates) {
      nom<-c(nom, sales(data,period=d,set=ID))
      denom<-c(denom, quantities(data,period=d,set=ID))
    }
   return (sum(nom)/sum(denom)) 
   }
  
  vi<-sapply(availableID, fi)
  v<-data.frame(prodID=availableID, value=vi)
  
  #main body
  gksaqu <-
  function (tt)
  return (c(aqu(data, start=tt, end=end,v), aqu(data, start=tt, end=start,v)))
  vec <- sapply(dates, gksaqu)
  geksaqu <- prod(vec[1, ] / vec[2, ])
  geksaqu <- geksaqu ^ (1 / window)
  return(geksaqu)
  }


#' @title  Calculating the multilateral weighted WGEKS-AQU price index
#'
#' @description This function returns a value of the multilateral weighted WGEKS-AQU price index (to be more precise: the weighted GEKS index based on the asynchronous quality adjusted unit value formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname wgeksaqu
#' @return This function returns a value of the multilateral weighted WGEKS-AQU price index (to be more precise: the weighted GEKS index based on the asynchronous quality adjusted unit value formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' \donttest{wgeksaqu(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{wgeksaqu(milk, start="2018-12", end="2019-12")}
#' @export

wgeksaqu <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- c()
  while (wstart <= wend)
  {
  t <- substr(wstart, 0, 7)
  dates <- c(dates, t)
  lubridate::month(wstart) <-
  lubridate::month(wstart) + 1
  }
  
  #data frame with quality adjusted factors
  availableID<-available(data,period1=dates[1],period2=dates[length(dates)],interval=TRUE)
  
  fi<-function (ID) {
    nom<-c()
    denom<-c()
    for (d in dates) {
      nom<-c(nom, sales(data,period=d,set=ID))
      denom<-c(denom, quantities(data,period=d,set=ID))
    }
   return (sum(nom)/sum(denom)) 
   }
  
  vi<-sapply(availableID, fi)
  v<-data.frame(prodID=availableID, value=vi)
  
  #main body
  wgksaqu <-
  function (tt)
  return (c(aqu(data, start=tt, end=end,v), aqu(data, start=tt, end=start,v)))
  vec <- sapply(dates, wgksaqu)
  sales_in_time <-
  function (tt)
  return (sum(sales(data, tt)))
  expenditures <-
  sapply(dates, sales_in_time)
  expenditures <-
  expenditures / sum(expenditures)
  wgeksaqu <- prod((vec[1, ] / vec[2, ])^expenditures)
  return(wgeksaqu)
  }


#' @title  Calculating the multilateral GEKS-AQI price index
#'
#' @description This function returns a value of the multilateral GEKS-AQI price index (to be more precise: the GEKS index based on the asynchronous quality adjusted price index formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksaqi
#' @return This function returns a value of the multilateral GEKS-AQI price index (to be more precise: the GEKS index based on the asynchronous quality adjusted price index formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function). 
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' \donttest{geksaqi(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{geksaqi(milk, start="2018-12", end="2019-12")}
#' @export

geksaqi <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- c()
  while (wstart <= wend)
  {
  t <- substr(wstart, 0, 7)
  dates <- c(dates, t)
  lubridate::month(wstart) <-
  lubridate::month(wstart) + 1
  }
  #data frame with quality adjusted factors
  availableID<-available(data,period1=dates[1],period2=dates[length(dates)],interval=TRUE)
  
  fi<-function (ID) {
    nom<-c()
    denom<-c()
    for (d in dates) {
      nom<-c(nom, sales(data,period=d,set=ID))
      denom<-c(denom, quantities(data,period=d,set=ID))
    }
   return (sum(nom)/sum(denom)) 
   }
  
  vi<-sapply(availableID, fi)
  v<-data.frame(prodID=availableID, value=vi)
  
  #main body
  gksaqi <-
  function (tt)
  return (c(aqi(data, start=tt, end=end,v), aqi(data, start=tt, end=start,v)))
  vec <- sapply(dates, gksaqi)
  geksaqi <- prod(vec[1, ] / vec[2, ])
  geksaqi <- geksaqi ^ (1 / window)
  return(geksaqi)
  }


#' @title  Calculating the multilateral weighted WGEKS-AQI price index
#'
#' @description This function returns a value of the multilateral weighted WGEKS-AQI price index (to be more precise: the weighted GEKS index based on the asynchronous quality adjusted price index formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname wgeksaqi
#' @return This function returns a value of the multilateral weighted WGEKS-AQI price index (to be more precise: the weighted GEKS index based on the asynchronous quality adjusted price index formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' \donttest{wgeksaqi(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{wgeksaqi(milk, start="2018-12", end="2019-12")}
#' @export

wgeksaqi <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- c()
  while (wstart <= wend)
  {
  t <- substr(wstart, 0, 7)
  dates <- c(dates, t)
  lubridate::month(wstart) <-
  lubridate::month(wstart) + 1
  }
  
  #data frame with quality adjusted factors
  availableID<-available(data,period1=dates[1],period2=dates[length(dates)],interval=TRUE)
  
  fi<-function (ID) {
    nom<-c()
    denom<-c()
    for (d in dates) {
      nom<-c(nom, sales(data,period=d,set=ID))
      denom<-c(denom, quantities(data,period=d,set=ID))
    }
   return (sum(nom)/sum(denom)) 
   }
  
  vi<-sapply(availableID, fi)
  v<-data.frame(prodID=availableID, value=vi)
  
  #main body
  wgksaqi <-
  function (tt)
  return (c(aqi(data, start=tt, end=end,v), aqi(data, start=tt, end=start,v)))
  vec <- sapply(dates, wgksaqi)
  sales_in_time <-
  function (tt)
  return (sum(sales(data, tt)))
  expenditures <-
  sapply(dates, sales_in_time)
  expenditures <-
  expenditures / sum(expenditures)
  wgeksaqi <- prod((vec[1, ] / vec[2, ])^expenditures)
  return(wgeksaqi)
  }

#' @title  Calculating the multilateral GEKS-GAQI price index
#'
#' @description This function returns a value of the multilateral GEKS-GAQI price index (to be more precise: the GEKS index based on the geometric asynchronous quality adjusted price index formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksgaqi
#' @return This function returns a value of the multilateral GEKS-GAQI price index (to be more precise: the GEKS index based on the geometric asynchronous quality adjusted price index formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function). 
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' \donttest{geksgaqi(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{geksgaqi(milk, start="2018-12", end="2019-12")}
#' @export

geksgaqi <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- c()
  while (wstart <= wend)
  {
  t <- substr(wstart, 0, 7)
  dates <- c(dates, t)
  lubridate::month(wstart) <-
  lubridate::month(wstart) + 1
  }
  #data frame with quality adjusted factors
  availableID<-available(data,period1=dates[1],period2=dates[length(dates)],interval=TRUE)
  
  fi<-function (ID) {
    nom<-c()
    denom<-c()
    for (d in dates) {
      nom<-c(nom, sales(data,period=d,set=ID))
      denom<-c(denom, quantities(data,period=d,set=ID))
    }
   return (sum(nom)/sum(denom)) 
   }
  
  vi<-sapply(availableID, fi)
  v<-data.frame(prodID=availableID, value=vi)
  
  #main body
  gksgaqi <-
  function (tt)
  return (c(gaqi(data, start=tt, end=end,v), gaqi(data, start=tt, end=start,v)))
  vec <- sapply(dates, gksgaqi)
  geksgaqi <- prod(vec[1, ] / vec[2, ])
  geksgaqi <- geksgaqi ^ (1 / window)
  return(geksgaqi)
  }

#' @title  Calculating the multilateral weighted WGEKS-GAQI price index
#'
#' @description This function returns a value of the multilateral weighted WGEKS-GAQI price index (to be more precise: the weighted GEKS index based on the geometric asynchronous quality adjusted price index formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname wgeksgaqi
#' @return This function returns a value of the multilateral weighted WGEKS-GAQI price index (to be more precise: the weighted GEKS index based on the geometric asynchronous quality adjusted price index formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' \donttest{wgeksgaqi(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{wgeksgaqi(milk, start="2018-12", end="2019-12")}
#' @export

wgeksgaqi <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- c()
  while (wstart <= wend)
  {
  t <- substr(wstart, 0, 7)
  dates <- c(dates, t)
  lubridate::month(wstart) <-
  lubridate::month(wstart) + 1
  }
  
  #data frame with quality adjusted factors
  availableID<-available(data,period1=dates[1],period2=dates[length(dates)],interval=TRUE)
  
  fi<-function (ID) {
    nom<-c()
    denom<-c()
    for (d in dates) {
      nom<-c(nom, sales(data,period=d,set=ID))
      denom<-c(denom, quantities(data,period=d,set=ID))
    }
   return (sum(nom)/sum(denom)) 
   }
  
  vi<-sapply(availableID, fi)
  v<-data.frame(prodID=availableID, value=vi)
  
  #main body
  wgksgaqi <-
  function (tt)
  return (c(gaqi(data, start=tt, end=end,v), gaqi(data, start=tt, end=start,v)))
  vec <- sapply(dates, wgksgaqi)
  sales_in_time <-
  function (tt)
  return (sum(sales(data, tt)))
  expenditures <-
  sapply(dates, sales_in_time)
  expenditures <-
  expenditures / sum(expenditures)
  wgeksgaqi <- prod((vec[1, ] / vec[2, ])^expenditures)
  return(wgeksgaqi)
  }

