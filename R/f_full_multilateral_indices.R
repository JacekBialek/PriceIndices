#' @title  Calculating the multilateral GEKS price index
#'
#' @description This function returns a value of the multilateral GEKS price index (to be more precise: the GEKS index based on the Fisher formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geks
#' @return This function returns a value of the multilateral GEKS price index (to be more precise: the GEKS index based on the Fisher formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
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
  #distances of start and end from wstart plus 1
  d_start<-dist(wstart,start)+1
  d_end<-dist(wstart,end)+1
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  p<-function (tm) prices(data, period=tm, ID=TRUE)
  q<-function (tm) quantities(data, period=tm, ID=TRUE)
  p_list<-lapply(dates, p)  
  q_list<-lapply(dates, q)
  #main body
  gks <- function (tt)
  {
   if (tt==d_end) {
   ID_tt_start<-intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
   pm_start<-dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
   pm_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
   qm_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
   qm_start<-dplyr::filter(q_list[[d_start]], by %in% ID_tt_start)$q
   pm_F<-sum(qm_tt*pm_start)*sum(qm_start*pm_start)/(sum(qm_tt*pm_tt)*sum(qm_start*pm_tt))
   return ((1/pm_F)^0.5)  
   }
   if (tt==d_start) {
   ID_tt_end<-intersect(p_list[[tt]]$by, p_list[[d_end]]$by)   
   pl_end<-dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
   pl_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
   ql_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
   ql_end<-dplyr::filter(q_list[[d_end]], by %in% ID_tt_end)$q
   pl_F<-sum(ql_tt*pl_end)*sum(ql_end*pl_end)/(sum(ql_tt*pl_tt)*sum(ql_end*pl_tt))
   return (pl_F^0.5)   
   }
   if ((!(tt==d_end)) & (!(tt==d_start)))  
   {ID_tt_start<-intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
   ID_tt_end<-intersect(p_list[[tt]]$by, p_list[[d_end]]$by)   
   pl_end<-dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
   pl_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
   ql_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
   ql_end<-dplyr::filter(q_list[[d_end]], by %in% ID_tt_end)$q
   pm_start<-dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
   pm_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
   qm_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
   qm_start<-dplyr::filter(q_list[[d_start]], by %in% ID_tt_start)$q
   pl_F<-sum(ql_tt*pl_end)*sum(ql_end*pl_end)/(sum(ql_tt*pl_tt)*sum(ql_end*pl_tt))
   pm_F<-sum(qm_tt*pm_start)*sum(qm_start*pm_start)/(sum(qm_tt*pm_tt)*sum(qm_start*pm_tt))
   return ((pl_F/pm_F)^0.5)
  }
  }
  geks<-sapply(seq(1,window),gks)
  geks <- prod(geks) ^ (1 / window)
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
#' @return This function returns a value of the multilateral GEKS-W price index (to be more precise: the GEKS index based on the Walsh formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
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
  #distances of start and end from wstart plus 1
  d_start<-dist(wstart,start)+1
  d_end<-dist(wstart,end)+1
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  p<-function (tm) prices(data, period=tm, ID=TRUE)
  q<-function (tm) quantities(data, period=tm, ID=TRUE)
  p_list<-lapply(dates, p)  
  q_list<-lapply(dates, q)
  #main body
  gksw <- function (tt)
  {
   if (tt==d_end) {
   ID_tt_start<-intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
   pm_start<-dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
   pm_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
   qm_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
   qm_start<-dplyr::filter(q_list[[d_start]], by %in% ID_tt_start)$q
   pm_W<-sum(pm_start*(qm_tt*qm_start)^0.5)/sum(pm_tt*(qm_tt*qm_start)^0.5)
   return (1/pm_W)  
   }
   if (tt==d_start) {
   ID_tt_end<-intersect(p_list[[tt]]$by, p_list[[d_end]]$by)   
   pl_end<-dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
   pl_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
   ql_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
   ql_end<-dplyr::filter(q_list[[d_end]], by %in% ID_tt_end)$q
   pl_W<-sum(pl_end*(ql_tt*ql_end)^0.5)/sum(pl_tt*(ql_tt*ql_end)^0.5)
   return (pl_W)   
   }
   if ((!(tt==d_end)) & (!(tt==d_start)))  
   {ID_tt_start<-intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
   ID_tt_end<-intersect(p_list[[tt]]$by, p_list[[d_end]]$by)   
   pl_end<-dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
   pl_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
   ql_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
   ql_end<-dplyr::filter(q_list[[d_end]], by %in% ID_tt_end)$q
   pm_start<-dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
   pm_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
   qm_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
   qm_start<-dplyr::filter(q_list[[d_start]], by %in% ID_tt_start)$q
   pl_W<-sum(pl_end*(ql_tt*ql_end)^0.5)/sum(pl_tt*(ql_tt*ql_end)^0.5)
   pm_W<-sum(pm_start*(qm_tt*qm_start)^0.5)/sum(pm_tt*(qm_tt*qm_start)^0.5)
   return (pl_W/pm_W)
  }
  }
  geksw<-sapply(seq(1,window),gksw)
  geksw <- prod(geksw) ^ (1 / window)
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
#' @return This function returns a value of the multilateral GEKS-J price index (to be more precise: the GEKS index based on the Jevons formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
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
  #distances of start and end from wstart plus 1
  d_start<-dist(wstart,start)+1
  d_end<-dist(wstart,end)+1
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  p<-function (tm) prices(data, period=tm, ID=TRUE)
  q<-function (tm) quantities(data, period=tm, ID=TRUE)
  p_list<-lapply(dates, p)  
  q_list<-lapply(dates, q)
  #main body
  gksj <- function (tt)
  {
   if (tt==d_end) {
   ID_tt_start<-intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
   pm_start<-dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
   pm_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
   pm_J<-prod((pm_start/pm_tt)^(1/length(pm_start)))
   return (1/pm_J)  
   }
   if (tt==d_start) {
   ID_tt_end<-intersect(p_list[[tt]]$by, p_list[[d_end]]$by)   
   pl_end<-dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
   pl_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
   pl_J<-prod((pl_end/pl_tt)^(1/length(pl_end)))
   return (pl_J)   
   }
   if ((!(tt==d_end)) & (!(tt==d_start)))  
   {ID_tt_start<-intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
   ID_tt_end<-intersect(p_list[[tt]]$by, p_list[[d_end]]$by)   
   pl_end<-dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
   pl_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
   pm_start<-dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
   pm_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
   pl_J<-prod((pl_end/pl_tt)^(1/length(pl_end)))
   pm_J<-prod((pm_start/pm_tt)^(1/length(pm_start)))
   return (pl_J/pm_J)
  }
  }
  geksj<-sapply(seq(1,window),gksj)
  geksj <- prod(geksj) ^ (1 / window)
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
#' @return This function returns a value of the multilateral CCDI price index (to be more precise: the GEKS index based on the Tornqvist formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
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
  #distances of start and end from wstart plus 1
  d_start<-dist(wstart,start)+1
  d_end<-dist(wstart,end)+1
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  p<-function (tm) prices(data, period=tm, ID=TRUE)
  ex<-function (tm) expenditures(data, period=tm, ID=TRUE)
  p_list<-lapply(dates, p)  
  ex_list<-lapply(dates, ex)
  #main body
  ccd <- function (tt)
  {
   if (tt==d_end) {
   ID_tt_start<-intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
   pm_start<-dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
   pm_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
   exm_tt<-dplyr::filter(ex_list[[tt]], by %in% ID_tt_start)$expend
   exm_start<-dplyr::filter(ex_list[[d_start]], by %in% ID_tt_start)$expend
   summ_ex_tt<-sum(exm_tt) 
   summ_ex_start<-sum(exm_start)
   pm_T<-prod((pm_start/pm_tt)^((exm_tt/summ_ex_tt+exm_start/summ_ex_start)/2))
   return (1/pm_T)  
   }
   if (tt==d_start) {
   ID_tt_end<-intersect(p_list[[tt]]$by, p_list[[d_end]]$by)   
   pl_end<-dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
   pl_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
   exl_tt<-dplyr::filter(ex_list[[tt]], by %in% ID_tt_end)$expend
   exl_end<-dplyr::filter(ex_list[[d_end]], by %in% ID_tt_end)$expend
   suml_ex_tt<-sum(exl_tt) 
   suml_ex_end<-sum(exl_end)
   pl_T<-prod((pl_end/pl_tt)^((exl_tt/suml_ex_tt+exl_end/suml_ex_end)/2))
   return (pl_T)   
   }
   if ((!(tt==d_end)) & (!(tt==d_start)))  
   {ID_tt_start<-intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
   ID_tt_end<-intersect(p_list[[tt]]$by, p_list[[d_end]]$by)   
   pl_end<-dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
   pl_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
   exl_tt<-dplyr::filter(ex_list[[tt]], by %in% ID_tt_end)$expend
   exl_end<-dplyr::filter(ex_list[[d_end]], by %in% ID_tt_end)$expend
   suml_ex_tt<-sum(exl_tt) 
   suml_ex_end<-sum(exl_end)
   pm_start<-dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
   pm_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
   exm_tt<-dplyr::filter(ex_list[[tt]], by %in% ID_tt_start)$expend
   exm_start<-dplyr::filter(ex_list[[d_start]], by %in% ID_tt_start)$expend
   summ_ex_tt<-sum(exm_tt) 
   summ_ex_start<-sum(exm_start)
   pl_T<-prod((pl_end/pl_tt)^((exl_tt/suml_ex_tt+exl_end/suml_ex_end)/2))
   pm_T<-prod((pm_start/pm_tt)^((exm_tt/summ_ex_tt+exm_start/summ_ex_start)/2))
   return (pl_T/pm_T)
  }
  }
  ccdi<-sapply(seq(1,window),ccd)
  ccdi <- prod(ccdi) ^ (1 / window)
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
prodID<-time<-NULL
data$time<-substr(data$time, 0, 7)
data1<-dplyr::filter(data, time==start)
data2<-dplyr::filter(data, time==end)
data<-rbind(data1, data2)
df_start<-dplyr::summarise(dplyr::group_by(data1,prodID),
                          expenditures=sum(prices*quantities),
                          quantities=sum(quantities),.groups="drop")
df_end<-dplyr::summarise(dplyr::group_by(data2,prodID),
                          expenditures=sum(prices*quantities),
                          quantities=sum(quantities),.groups="drop")
quantity_end<-df_end$quantities
quantity_start<-df_start$quantities
sale_end<-df_end$expenditures
sale_start<-df_start$expenditures
Gstart<-unique(data1$prodID) 
Gend<-unique(data2$prodID)
#main body
a<-sum(sale_end)
b<-sum(sale_start)
v_end<-dplyr::filter(v, prodID %in% Gend)
val_end<-v_end$values
v_start<-dplyr::filter(v, prodID %in% Gstart)
val_start<-v_start$values
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
#' @return This function returns a value of the multilateral Geary-Khamis price index which considers the time window defined by \code{wstart} and \code{window} parameters. The Geary-Khamis price index is calculated by using a special iterative algorithm from \code{Chessa (2016)}. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
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
  time<-expend<-NULL
  expend<-NULL
  quant<-NULL
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  d$time<-substr(d$time,0,7)
  wstart<-substr(wstart,0,7)
  start<-substr(start,0,7)
  end<-substr(end,0,7)
  #quantity weights - quality adjusted factors vi
  while (max(abs(index1 - index2)) >
  0.000001)
  {
  gr<-dplyr::summarise(dplyr::group_by(d, time, prodID),expend=sum(prices*quantities) / index1[which(dates == unique(time))],quant=sum(quantities),.groups="drop")
  gr2<-dplyr::summarise(dplyr::group_by(gr, prodID), value=sum(expend)/sum(quant),.groups="drop")
  v <- data.frame(prodID=gr2$prodID, values=gr2$value)
  #series  of indices
  indd <-
  function(tt)
  return (QU(d, wstart, tt, v))
  ind <- sapply(dates, indd)
  index2 <- index1
  index1 <- ind
  }
  result <-
  index1[which(dates == end)] / index1[which(dates == start)]
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
#' @return This function returns a value of the multilateral TPD price index which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). Please note that a Weighted Least Squares (WLS) regression is run with the expenditure shares in each period serving as weights.To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).  
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
  prodID<-NULL
  time<-NULL
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
  products <- sort(unique(d$prodID))
  if (length(products) < 2)
  stop ("At least two prodIDs must be available during the considered time interval")
  #main body
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  dates<-dates[2:length(dates)]
  d$time<-as.character(d$time)
  d$time<-substr(d$time, 0, 7)
  #dates of availability of products
  df<-dplyr::group_by(d,prodID,time)
  df<-dplyr::summarise(df, pr=sum(prices*quantities)/sum(quantities), weig=sum(prices*quantities),.groups="drop")
 av<-function (i) {
  sub<-dplyr::filter(d, prodID==i)
  return (as.character(unique(sub$time)))
  }
  av_dates<-sapply(products, av)
  av_dates<-as.vector(unlist(av_dates))
  #vector connected with the alfa parameter in TPD model
  alfa <-
  replicate(length(av_dates), 1) #unit vector
  #unit vectors corresponding to products
  vec <- list()
  i<-0
  for (prod in products) {i<-i+1
    vec[[i]] <- replicate(length(av(prod)), 1)
  }  
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
  logprices <- log(df$pr)
  #system of weights
  weights=df$weig
  df_last<-data.frame(av_dates=av_dates, weights=weights)
  df_last<-dplyr::mutate(
  dplyr::group_by(
  df_last,av_dates),
  w=weights/sum(weights))
  weights <- diag(df_last$w)
  #creating matrix X
  x <- alfa
  for (i in (1:(length(products) - 1)))
  x <- c(x, gm[[i]])
  for (i in (1:length(dates)))
  x <- c(x, sigma[[i]])
  x<-
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


#' @title  Calculating the unweighted multilateral TPD price index
#'
#' @description This function returns a value of the unweighted multilateral TPD (Time Product Dummy) price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname utpd
#' @return This function returns a value of the unweighted multilateral TPD price index which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). Please note, that the estimation procedure runs the Ordinary Least Squares (OLS) method instead of the Weighted Least Squares (WLS) method like in the case of the TPD index. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).  
#' @references
#' {de  Haan,  J.  and  F.  Krsinich  (2014). \emph{Time  Dummy  Hedonic  and  Quality-Adjusted  Unit Value Indexes: Do They Really Differ?} Paper presented at the Society for Economic Measurement Conference, 18-20 August 2014, Chicago, U.S.}
#' @examples 
#' \donttest{utpd(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{utpd(milk, start="2018-12", end="2019-12")}
#' @export

utpd <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  prodID<-NULL
  time<-NULL
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
  products <- sort(unique(d$prodID))
  if (length(products) < 2)
  stop ("At least two prodIDs must be available during the considered time interval")
  #main body
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  dates<-dates[2:length(dates)]
  d$time<-as.character(d$time)
  d$time<-substr(d$time, 0, 7)
  #dates of availability of products
  df<-dplyr::group_by(d,prodID,time)
  df<-dplyr::summarise(df, pr=sum(prices*quantities)/sum(quantities), weig=sum(prices*quantities),.groups="drop")
 av<-function (i) {
  sub<-dplyr::filter(d, prodID==i)
  return (as.character(unique(sub$time)))
  }
  av_dates<-sapply(products, av)
  av_dates<-as.vector(unlist(av_dates))
  #vector connected with the alfa parameter in TPD model
  alfa <-
  replicate(length(av_dates), 1) #unit vector
  #unit vectors corresponding to products
  vec <- list()
  i<-0
  for (prod in products) {i<-i+1
    vec[[i]] <- replicate(length(av(prod)), 1)
  }  
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
  logprices <- log(df$pr)
  #creating matrix X
  x <- alfa
  for (i in (1:(length(products) - 1)))
  x <- c(x, gm[[i]])
  for (i in (1:length(dates)))
  x <- c(x, sigma[[i]])
  x<-
  matrix(x,
  nrow = length(av_dates),
  ncol = length(products) + length(dates))
  #estimation of parameters
  b <- t(x) %*% x
  b <- solve(b)
  b <- b %*% t(x)
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
#' @return This function returns a value of the multilateral SPQ price index which is based on the relative price and quantity dissimilarity measure (see \code{References}). If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function). 
#' @references
#' {Diewert, E. (2020). \emph{The Chain Drift Problem and Multilateral Indexes.} Chapter 6 in: Consumer Price Index Theory (draft)}
#'
#' @examples 
#' \donttest{SPQ(sugar, start="2018-12",end="2019-02")}
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
#' @return This function returns a value of the multilateral weighted WGEKS price index (to be more precise: the weighted GEKS index based on the Fisher formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
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
  #distances of start and end from wstart plus 1
  d_start<-dist(wstart,start)+1
  d_end<-dist(wstart,end)+1
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  p<-function (tm) prices(data, period=tm, ID=TRUE)
  q<-function (tm) quantities(data, period=tm, ID=TRUE)
  p_list<-lapply(dates, p)  
  q_list<-lapply(dates, q)
  #main body
  gks <- function (tt)
  {
   if (tt==d_end) {
   ID_tt_start<-intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
   pm_start<-dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
   pm_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
   qm_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
   qm_start<-dplyr::filter(q_list[[d_start]], by %in% ID_tt_start)$q
   pm_F<-sum(qm_tt*pm_start)*sum(qm_start*pm_start)/(sum(qm_tt*pm_tt)*sum(qm_start*pm_tt))
   return ((1/pm_F)^0.5)  
   }
   if (tt==d_start) {
   ID_tt_end<-intersect(p_list[[tt]]$by, p_list[[d_end]]$by)   
   pl_end<-dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
   pl_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
   ql_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
   ql_end<-dplyr::filter(q_list[[d_end]], by %in% ID_tt_end)$q
   pl_F<-sum(ql_tt*pl_end)*sum(ql_end*pl_end)/(sum(ql_tt*pl_tt)*sum(ql_end*pl_tt))
   return (pl_F^0.5)   
   }
   if ((!(tt==d_end)) & (!(tt==d_start)))  
   {ID_tt_start<-intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
   ID_tt_end<-intersect(p_list[[tt]]$by, p_list[[d_end]]$by)   
   pl_end<-dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
   pl_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
   ql_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
   ql_end<-dplyr::filter(q_list[[d_end]], by %in% ID_tt_end)$q
   pm_start<-dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
   pm_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
   qm_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
   qm_start<-dplyr::filter(q_list[[d_start]], by %in% ID_tt_start)$q
   pl_F<-sum(ql_tt*pl_end)*sum(ql_end*pl_end)/(sum(ql_tt*pl_tt)*sum(ql_end*pl_tt))
   pm_F<-sum(qm_tt*pm_start)*sum(qm_start*pm_start)/(sum(qm_tt*pm_tt)*sum(qm_start*pm_tt))
   return ((pl_F/pm_F)^0.5)
  }
  }
  wgeks<-sapply(seq(1,window),gks)
  sales_in_time <-
  function (tt)
  return (sum(expenditures(data, tt)))
  expenditures_ <-
  sapply(dates, sales_in_time)
  expenditures_ <-
  expenditures_ / sum(expenditures_)
  wgeks <-
  prod((wgeks)^expenditures_)
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
#' @return This function returns a value of the multilateral GEKS-L price index (to be more precise: the GEKS index based on the Laspeyres formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function). 
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Białek, J. (2022). \emph{The general class of multilateral indices and its two special cases.} Paper presented at the 17th Meeting of the Ottawa Group on Price Indices, Rome, Italy.}
#'
#' {Białek, J. (2022). \emph{Improving quality of the scanner CPI: proposition of new multilateral methods}, Quality & Quantity, https://doi.org/10.1007/s11135-022-01506-6.}
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
  #distances of start and end from wstart plus 1
  d_start<-dist(wstart,start)+1
  d_end<-dist(wstart,end)+1
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  p<-function (tm) prices(data, period=tm, ID=TRUE)
  q<-function (tm) quantities(data, period=tm, ID=TRUE)
  p_list<-lapply(dates, p)  
  q_list<-lapply(dates, q)
  #main body
  gksl <- function (tt)
  {
   if (tt==d_end) {
   ID_tt_start<-intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
   pm_start<-dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
   pm_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
   qm_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
   pm_L<-sum(qm_tt*pm_start)/sum(qm_tt*pm_tt)
   return (1/pm_L)  
   }
   if (tt==d_start) {
   ID_tt_end<-intersect(p_list[[tt]]$by, p_list[[d_end]]$by)   
   pl_end<-dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
   pl_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
   ql_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
   pl_L<-sum(ql_tt*pl_end)/sum(ql_tt*pl_tt)
   return (pl_L)   
   }
   if ((!(tt==d_end)) & (!(tt==d_start)))  
   {ID_tt_start<-intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
   ID_tt_end<-intersect(p_list[[tt]]$by, p_list[[d_end]]$by)   
   pl_end<-dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
   pl_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
   ql_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
   pm_start<-dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
   pm_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
   qm_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
   pl_L<-sum(ql_tt*pl_end)/sum(ql_tt*pl_tt)
   pm_L<-sum(qm_tt*pm_start)/sum(qm_tt*pm_tt)
   return (pl_L/pm_L)
  }
  }
  geksl<-sapply(seq(1,window),gksl)
  geksl <- prod(geksl) ^ (1 / window)
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
#' @return This function returns a value of the multilateral weighted WGEKS-L price index (to be more precise: the weighted GEKS index based on the Laspeyres formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Białek, J. (2022). \emph{The general class of multilateral indices and its two special cases.} Paper presented at the 17th Meeting of the Ottawa Group on Price Indices, Rome, Italy.}
#'
#' {Białek, J. (2022). \emph{Improving quality of the scanner CPI: proposition of new multilateral methods}, Quality & Quantity, https://doi.org/10.1007/s11135-022-01506-6.}
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
  #distances of start and end from wstart plus 1
  d_start<-dist(wstart,start)+1
  d_end<-dist(wstart,end)+1
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  p<-function (tm) prices(data, period=tm, ID=TRUE)
  q<-function (tm) quantities(data, period=tm, ID=TRUE)
  p_list<-lapply(dates, p)  
  q_list<-lapply(dates, q)
  #main body
  gksl <- function (tt)
  {
   if (tt==d_end) {
   ID_tt_start<-intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
   pm_start<-dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
   pm_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
   qm_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
   pm_L<-sum(qm_tt*pm_start)/sum(qm_tt*pm_tt)
   return (1/pm_L)  
   }
   if (tt==d_start) {
   ID_tt_end<-intersect(p_list[[tt]]$by, p_list[[d_end]]$by)   
   pl_end<-dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
   pl_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
   ql_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
   pl_L<-sum(ql_tt*pl_end)/sum(ql_tt*pl_tt)
   return (pl_L)   
   }
   if ((!(tt==d_end)) & (!(tt==d_start)))  
   {ID_tt_start<-intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
   ID_tt_end<-intersect(p_list[[tt]]$by, p_list[[d_end]]$by)   
   pl_end<-dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
   pl_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
   ql_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
   pm_start<-dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
   pm_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
   qm_tt<-dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
   pl_L<-sum(ql_tt*pl_end)/sum(ql_tt*pl_tt)
   pm_L<-sum(qm_tt*pm_start)/sum(qm_tt*pm_tt)
   return (pl_L/pm_L)
  }
  }
  wgeksl<-sapply(seq(1,window),gksl)
  sales_in_time <-
  function (tt)
  return (sum(expenditures(data, tt)))
  expenditures_ <-
  sapply(dates, sales_in_time)
  expenditures_ <-
  expenditures_ / sum(expenditures_)
  wgeksl <-
  prod((wgeksl)^expenditures_)
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
#' @return This function returns a value of the multilateral GEKS-GL price index (to be more precise: the GEKS index based on the geometric Laspeyres formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function). 
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Białek, J. (2022). \emph{The general class of multilateral indices and its two special cases.} Paper presented at the 17th Meeting of the Ottawa Group on Price Indices, Rome, Italy.}
#'
#' {Białek, J. (2022). \emph{Improving quality of the scanner CPI: proposition of new multilateral methods}, Quality & Quantity, https://doi.org/10.1007/s11135-022-01506-6.}
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
  #distances of start and end from wstart plus 1
  d_start<-dist(wstart,start)+1
  d_end<-dist(wstart,end)+1
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  p<-function (tm) prices(data, period=tm, ID=TRUE)
  ex<-function (tm) expenditures(data, period=tm, ID=TRUE)
  p_list<-lapply(dates, p)  
  ex_list<-lapply(dates, ex)
  #main body
  gksgl <- function (tt)
  {
   if (tt==d_end) {
   ID_tt_start<-intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
   pm_start<-dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
   pm_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
   exm_tt<-dplyr::filter(ex_list[[tt]], by %in% ID_tt_start)$expend
   summ_ex_tt<-sum(exm_tt) 
   pm_GL<-prod((pm_start/pm_tt)^(exm_tt/summ_ex_tt))
   return (1/pm_GL)  
   }
   if (tt==d_start) {
   ID_tt_end<-intersect(p_list[[tt]]$by, p_list[[d_end]]$by)   
   pl_end<-dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
   pl_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
   exl_tt<-dplyr::filter(ex_list[[tt]], by %in% ID_tt_end)$expend
   suml_ex_tt<-sum(exl_tt) 
   pl_GL<-prod((pl_end/pl_tt)^(exl_tt/suml_ex_tt))
   return (pl_GL)   
   }
   if ((!(tt==d_end)) & (!(tt==d_start)))  
   {ID_tt_start<-intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
   ID_tt_end<-intersect(p_list[[tt]]$by, p_list[[d_end]]$by)   
   pl_end<-dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
   pl_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
   exl_tt<-dplyr::filter(ex_list[[tt]], by %in% ID_tt_end)$expend
   exl_end<-dplyr::filter(ex_list[[d_end]], by %in% ID_tt_end)$expend
   suml_ex_tt<-sum(exl_tt) 
   suml_ex_end<-sum(exl_end)
   pm_start<-dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
   pm_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
   exm_tt<-dplyr::filter(ex_list[[tt]], by %in% ID_tt_start)$expend
   summ_ex_tt<-sum(exm_tt) 
   pl_GL<-prod((pl_end/pl_tt)^(exl_tt/suml_ex_tt))
   pm_GL<-prod((pm_start/pm_tt)^(exm_tt/summ_ex_tt))
   return (pl_GL/pm_GL)
  }
  }
  geksgl<-sapply(seq(1,window),gksgl)
  geksgl <- prod(geksgl) ^ (1 / window)
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
#' @return This function returns a value of the multilateral weighted WGEKS-GL price index (to be more precise: the weighted GEKS index based on the geometric Laspeyres formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Białek, J. (2022). \emph{The general class of multilateral indices and its two special cases.} Paper presented at the 17th Meeting of the Ottawa Group on Price Indices, Rome, Italy.}
#'
#' {Białek, J. (2022). \emph{Improving quality of the scanner CPI: proposition of new multilateral methods}, Quality & Quantity, https://doi.org/10.1007/s11135-022-01506-6.}
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
  #distances of start and end from wstart plus 1
  d_start<-dist(wstart,start)+1
  d_end<-dist(wstart,end)+1
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  p<-function (tm) prices(data, period=tm, ID=TRUE)
  ex<-function (tm) expenditures(data, period=tm, ID=TRUE)
  p_list<-lapply(dates, p)  
  ex_list<-lapply(dates, ex)
  #main body
  gksgl <- function (tt)
  {
   if (tt==d_end) {
   ID_tt_start<-intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
   pm_start<-dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
   pm_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
   exm_tt<-dplyr::filter(ex_list[[tt]], by %in% ID_tt_start)$expend
   summ_ex_tt<-sum(exm_tt) 
   pm_GL<-prod((pm_start/pm_tt)^(exm_tt/summ_ex_tt))
   return (1/pm_GL)  
   }
   if (tt==d_start) {
   ID_tt_end<-intersect(p_list[[tt]]$by, p_list[[d_end]]$by)   
   pl_end<-dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
   pl_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
   exl_tt<-dplyr::filter(ex_list[[tt]], by %in% ID_tt_end)$expend
   suml_ex_tt<-sum(exl_tt) 
   pl_GL<-prod((pl_end/pl_tt)^(exl_tt/suml_ex_tt))
   return (pl_GL)   
   }
   if ((!(tt==d_end)) & (!(tt==d_start)))  
   {ID_tt_start<-intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
   ID_tt_end<-intersect(p_list[[tt]]$by, p_list[[d_end]]$by)   
   pl_end<-dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
   pl_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
   exl_tt<-dplyr::filter(ex_list[[tt]], by %in% ID_tt_end)$expend
   exl_end<-dplyr::filter(ex_list[[d_end]], by %in% ID_tt_end)$expend
   suml_ex_tt<-sum(exl_tt) 
   suml_ex_end<-sum(exl_end)
   pm_start<-dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
   pm_tt<-dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
   exm_tt<-dplyr::filter(ex_list[[tt]], by %in% ID_tt_start)$expend
   summ_ex_tt<-sum(exm_tt) 
   pl_GL<-prod((pl_end/pl_tt)^(exl_tt/suml_ex_tt))
   pm_GL<-prod((pm_start/pm_tt)^(exm_tt/summ_ex_tt))
   return (pl_GL/pm_GL)
  }
  }
  wgeksgl<-sapply(seq(1,window),gksgl)
  sales_in_time <-
  function (tt)
  return (sum(expenditures(data, tt)))
  expenditures_ <-
  sapply(dates, sales_in_time)
  expenditures_ <-
  expenditures_ / sum(expenditures_)
  wgeksgl <-
  prod((wgeksgl)^expenditures_)
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
#' @return This function returns a value of the multilateral GEKS-AQU price index (to be more precise: the GEKS index based on the asynchronous quality adjusted unit value formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function). 
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Białek, J. (2022). \emph{The general class of multilateral indices and its two special cases.} Paper presented at the 17th Meeting of the Ottawa Group on Price Indices, Rome, Italy.}
#'
#' {Białek, J. (2023). \emph{Quality adjusted GEKS-type indices for price comparisons based on scanner data.} Statistics in Transition – new series, 24(3), 151-169.}
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
  prodID<-NULL
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  lubridate::day(wend) <- lubridate::days_in_month(wend)
  data<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
  #data frame with quality adjusted factors
  v<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop")
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
#' @return This function returns a value of the multilateral weighted WGEKS-AQU price index (to be more precise: the weighted GEKS index based on the asynchronous quality adjusted unit value formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Białek, J. (2022). \emph{The general class of multilateral indices and its two special cases.} Paper presented at the 17th Meeting of the Ottawa Group on Price Indices, Rome, Italy.}
#'
#' {Białek, J. (2023). \emph{Quality adjusted GEKS-type indices for price comparisons based on scanner data.} Statistics in Transition – new series, 24(3), 151-169.}
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
  prodID<-NULL
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  lubridate::day(wend) <- lubridate::days_in_month(wend)
  data<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
  #data frame with quality adjusted factors
  v<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop")
  #main body
  wgksaqu <-
  function (tt)
  return (c(aqu(data, start=tt, end=end,v), aqu(data, start=tt, end=start,v)))
  vec <- sapply(dates, wgksaqu)
  sales_in_time <-
  function (tt)
  return (sum(expenditures(data, tt)))
  expenditures_ <-
  sapply(dates, sales_in_time)
  expenditures_ <-
  expenditures_ / sum(expenditures_)
  wgeksaqu <- prod((vec[1, ] / vec[2, ])^expenditures_)
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
#' @return This function returns a value of the multilateral GEKS-AQI price index (to be more precise: the GEKS index based on the asynchronous quality adjusted price index formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function). 
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Białek, J. (2022). \emph{The general class of multilateral indices and its two special cases.} Paper presented at the 17th Meeting of the Ottawa Group on Price Indices, Rome, Italy.}
#'
#' {Białek, J. (2023). \emph{Quality adjusted GEKS-type indices for price comparisons based on scanner data.} Statistics in Transition – new series, 24(3), 151-169.}
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
  prodID<-NULL
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  lubridate::day(wend) <- lubridate::days_in_month(wend)
  data<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
  #data frame with quality adjusted factors
  v<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop")
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
#' @return This function returns a value of the multilateral weighted WGEKS-AQI price index (to be more precise: the weighted GEKS index based on the asynchronous quality adjusted price index formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Białek, J. (2022). \emph{The general class of multilateral indices and its two special cases.} Paper presented at the 17th Meeting of the Ottawa Group on Price Indices, Rome, Italy.}
#'
#' {Białek, J. (2023). \emph{Quality adjusted GEKS-type indices for price comparisons based on scanner data.} Statistics in Transition – new series, 24(3), 151-169.}
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
  prodID<-NULL
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  lubridate::day(wend) <- lubridate::days_in_month(wend)
  data<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
  #data frame with quality adjusted factors
  v<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop")
  #main body
  wgksaqi <-
  function (tt)
  return (c(aqi(data, start=tt, end=end,v), aqi(data, start=tt, end=start,v)))
  vec <- sapply(dates, wgksaqi)
  sales_in_time <-
  function (tt)
  return (sum(expenditures(data, tt)))
  expenditures_ <-
  sapply(dates, sales_in_time)
  expenditures_ <-
  expenditures_ / sum(expenditures_)
  wgeksaqi <- prod((vec[1, ] / vec[2, ])^expenditures_)
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
#' @return This function returns a value of the multilateral GEKS-GAQI price index (to be more precise: the GEKS index based on the geometric asynchronous quality adjusted price index formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function). 
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
  prodID<-NULL
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  lubridate::day(wend) <- lubridate::days_in_month(wend)
  data<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
  #data frame with quality adjusted factors
  v<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop")
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
#' @return This function returns a value of the multilateral weighted WGEKS-GAQI price index (to be more precise: the weighted GEKS index based on the geometric asynchronous quality adjusted price index formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
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
  prodID<-NULL
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  lubridate::day(wend) <- lubridate::days_in_month(wend)
  data<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
  #data frame with quality adjusted factors
  v<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop")
  #main body
  wgksgaqi <-
  function (tt)
  return (c(aqu(data, start=tt, end=end,v), aqu(data, start=tt, end=start,v)))
  vec <- sapply(dates, wgksgaqi)
  sales_in_time <-
  function (tt)
  return (sum(expenditures(data, tt)))
  expenditures_ <-
  sapply(dates, sales_in_time)
  expenditures_ <-
  expenditures_ / sum(expenditures_)
  wgeksgaqi <- prod((vec[1, ] / vec[2, ])^expenditures_)
  return(wgeksgaqi)
  }


#' @title  Calculating the multilateral GEKS-IQM price index
#'
#' @description This function returns a value of the multilateral GEKS-IQM price index (to be more precise: the GEKS index based on the implicit quadratic mean of order r price index IQMp).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter used in the implicit quadratic mean of order r price index.
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksiqm
#' @return This function returns a value of the multilateral GEKS-IQM price index (to be more precise: the GEKS index based on the the implicit quadratic mean of order r price index IQMp) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' \donttest{geksiqm(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{geksiqm(milk, start="2018-12", end="2019-12", r=1.6)}
#' @export

geksiqm <-
  function(data,
  start,
  end,
  r=2,
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gks <-
  function (tt)
  return (c(IQMp(data, tt, end, r, FALSE), IQMp(data, tt, start, r, FALSE)))
  vec <- sapply(dates, gks)
  geks_iqm <- prod(vec[1, ] / vec[2, ])
  geks_iqm <- geks_iqm ^ (1 / window)
  return(geks_iqm)
  }

#' @title  Calculating the multilateral GEKS-QM price index
#'
#' @description This function returns a value of the multilateral GEKS-QM price index (to be more precise: the GEKS index based on the quadratic mean of order r price index QMp).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter used in the implicit quadratic mean of order r price index.
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksqm
#' @return This function returns a value of the multilateral GEKS-QM price index (to be more precise: the GEKS index based on the the quadratic mean of order r price index QMp) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' \donttest{geksqm(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{geksqm(milk, start="2018-12", end="2019-12", r=1.6)}
#' @export

geksqm <-
  function(data,
  start,
  end,
  r=2,
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gks <-
  function (tt)
  return (c(QMp(data, tt, end, r, FALSE), QMp(data, tt, start, r, FALSE)))
  vec <- sapply(dates, gks)
  geks_qm <- prod(vec[1, ] / vec[2, ])
  geks_qm <- geks_qm ^ (1 / window)
  return(geks_qm)
  }


#' @title  Calculating the multilateral GEKS-LM price index
#'
#' @description This function returns a value of the multilateral GEKS-LM price index (to be more precise: the GEKS index based on the Lloyd-Moulton price index).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param sigma The elasticity of substitution (a parameter used in the Lloyd-Moulton index formula).
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname gekslm
#' @return This function returns a value of the multilateral GEKS-LM price index (to be more precise: the GEKS index based on the Lloyd-Moulton price index) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lloyd, P. J. (1975). \emph{Substitution Effects and Biases in Nontrue Price Indices.}  The American Economic Review, 65, 301-313.}
#'
#' {Moulton,  B.  R.  (1996). \emph{Constant  Elasticity  Cost-of-Living  Index  in  Share-Relative  Form.}  Washington DC: U. S. Bureau of Labor Statistics, mimeograph}
#' @examples 
#' \donttest{gekslm(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{gekslm(milk, start="2018-12", end="2019-12", sigma=0.5)}
#' @export

gekslm <-
  function(data,
  start,
  end,
  sigma=0.7,
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
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gks <-
  function (tt)
  return (c(lloyd_moulton(data, tt, end, sigma, FALSE), lloyd_moulton(data, tt, start, sigma, FALSE)))
  vec <- sapply(dates, gks)
  geks_lm <- prod(vec[1, ] / vec[2, ])
  geks_lm <- geks_lm ^ (1 / window)
  return(geks_lm)
  }


#' @title  Multiplicative decomposing the GEKS-type indices
#'
#' @description This function returns multiplicative decompositions of the selected GEKS-type indices.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param formula A parameter indicating which multilateral formulas are to be decomposed. In the current version of the package, the multiplicative decomposition includes the following GEKS-type indices: GEKS, CCDI, GEKS-W, GEKS-L, GEKS-GL, GEKS-LM, GEKS-AQI, and GEKS-GAQI. Thus, this parameter can take values like: “geks”, ‘ccdi’, ‘geksw’ ‘geksl’, ‘geksgl’, ‘gekslm’, 'geksaqi','geksgaqi'. 
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param sigma The elasticity of substitution (a parameter used in the Lloyd-Moulton index formula). The default value is 0.7.
#' @param index.value The parameter indicating whether price index values are to be displayed (at the end of the returned \code{multiplicative} data frame).
#' @rdname m_decomposition
#' @return This function returns a list with three elements: \code{multiplicative} - a data frame containing multiplicative decompositions of the indicated GEKS-type indices, \code{normalized} - normalized multiplicative decompositions of the indicated indices (their product is always 1), \code{impact} - relative impacts of commodities on the price index value (in p.p.).    
#' @references
#' {Webster, M., Tarnow-Mordi, R. C. (2019). \emph{Decomposing Multilateral Price Indexes into the Contributions of Individual Commodities}, Journal of Official Statistics, 35(2), 461-486.}
#' @examples 
#' \donttest{m_decomposition(milk, start="2018-12",end="2019-12",formula=c("geks","ccdi"))$multiplicative}
#' @export


m_decomposition <-
  function(data,
  start,
  end,
  wstart = start,
  formula=c(),
  window = 13,
  sigma=0.7,
  index.value=TRUE)  {
  #checking conditions
  allowed_formula<-c("geks","ccdi","geksw","geksl","geksgl","gekslm","geksaqi","geksgaqi")
  if (!(length(intersect(formula, allowed_formula))==length(formula)) | (length(formula)==0)) 
  stop("Bad specification of the 'formula' parameter")
  if (start == end)
  stop ("Parameters 'start' and 'end' are the same!")
  if (nrow(data) == 0)
  stop("A data frame is empty")
  price<-quantity<-NULL
  time<-prodID<-NULL
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  if (sigma == 1)
  stop("A specification of the parameter 'sigma' is wrong")
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
  lubridate::day(wend)<-lubridate::days_in_month(wend)
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates.<-substr(dates, 0, 7) #only year and month
  TT<-length(dates.) #number of periods 
  columns<-c("product",formula)
  columns2<-c("product")
  #main body
  data<-dplyr::filter(data, time>=wstart & time<=wend)
  data$time<-as.character(data$time)
  data$time<-substr(data$time, 0, 7)
  if (("geksaqi" %in% formula) | ("geksgaqi" %in% formula)) v_data<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop")
  data<-dplyr::summarise(dplyr::group_by(data, time, prodID),
                         prices=sum(prices*quantities)/sum(quantities),
                         quantities=sum(quantities),.groups="drop")
  av<-unique(data$prodID) #all available products
  n_av=length(av)
  n_row = n_av
  n_col = 1
  mat = matrix(0, nrow = n_row, ncol = n_col)
  result<-data.frame(mat)
  colnames(result)<-"product"
  result$product<-av
  av_in_time<-function (t) {
    d.<-dplyr::filter(data, time==t)
    return(d.$prodID)
  } #products available in a period t
  av_t<-lapply(dates., av_in_time)
 #logarithmic mean
  L<-function (x,y)
  {if (x==y) return (x)
  else return ((x-y)/(log(x)-log(y)))
  }
  #price function (universal for all decompositions)
  price_t<-function (t) {
  p_t<-c()
  set<-av_t[[which(dates.==t)]]
  for (prod in av)
  {  
    if (prod %in% set) {
                      df<-dplyr::filter(data, time==t & prodID==prod)
                      p_t<-c(p_t, df$prices)
                       }
  else p_t<-c(p_t,1)  
  }
  return (p_t)
  }
  price<-lapply(dates.,price_t) #prices for all products and all periods
  #individaul treating of indices
  #CCDI (GEKS-T)
  if ("ccdi" %in% formula)
  {
  #weights for individual products
  wT_tau_t<-function (tau, t)
  {
  wT<-c()
  set<-intersect(av_t[[which(dates.==tau)]],av_t[[which(dates.==t)]])
  df<-dplyr::filter(data, prodID %in% set)
  dftau<-dplyr::filter(df, time==tau)
  dft<-dplyr::filter(df, time==t)
  sales_tau<-sum(dftau$prices*dftau$quantities)
  sales_t<-sum(dft$prices*dft$quantities)
  for (prod in av)  {
    if (prod %in% set) {
    #main work
    dftau_i<-dplyr::filter(dftau, prodID==prod)
    dft_i<-dplyr::filter(dft, prodID==prod)
    wtau<-dftau_i$prices*dftau_i$quantities
    wt<-dft_i$prices*dft_i$quantities
    wT<-c(wT, 0.5*(wtau/sales_tau+wt/sales_t))
   }
  else wT<-c(wT,0)
  }
  return(wT)
  #end wT function
  }  
  s<-which(dates.==start)
  t<-which(dates.==end)
  wT_start<-lapply(dates.,wT_tau_t, t=start)
  wT_end<-lapply(dates.,wT_tau_t, t=end)
  ccdi<-c()
  for (i in 1:n_av)
  {
  suma_start<-0
  suma_end<-0
  prod_st<-1
  for (tt in 1:TT) {suma_start<-suma_start+wT_start[[tt]][i]
                    suma_end<-suma_end+wT_end[[tt]][i]
                    prod_st<-prod_st*price[[tt]][i]^(wT_start[[tt]][i]-wT_end[[tt]][i])
                    }
  suma_start<-suma_start/TT
  suma_end<-suma_end/TT
  prod_st<-prod_st^(1/(TT))
  ccdi<-c(ccdi, (price[[t]][i]^suma_end)/(price[[s]][i]^suma_start)*prod_st)
  }
  columns2<-c(columns2, "ccdi")
  result$CCDI<-ccdi
  #end CCDI decomposition   
  }  
  #GEKS (GEKS-F)
  if ("geks" %in% formula)
  {
  #weights for individual products
  wF_tau_t<-function (tau, t)
  {
  t_tau<-which(dates.==tau)
  t_t<-which(dates.==t)
  wF<-c()
  wtau<-c()
  wtau_t<-c()
  L1<-c()
  L2<-c()
  set<-intersect(av_t[[t_tau]],av_t[[t_t]])
  df<-dplyr::filter(data, prodID %in% set)
  dftau<-dplyr::filter(df, time==tau)
  dft<-dplyr::filter(df, time==t)
  #Laspeyres and Paasche indices
  p_tau<-dftau$prices
  p_t<-dft$prices
  q_tau<-dftau$quantities
  q_t<-dft$quantities
  lasp<-sum(q_tau*p_t)/sum(q_tau*p_tau)
  paasch<-sum(q_t*p_t)/sum(q_t*p_tau)
  #weights
  for (prod in av)  {
    if (prod %in% set) {
  i_i<-which(av==prod)
  dftau_i<-dplyr::filter(dftau, prodID==prod)
  dft_i<-dplyr::filter(dft, prodID==prod)
  wtau<-c(wtau,dftau_i$prices*dftau_i$quantities)
  wtau_t<-c(wtau_t,dftau_i$prices*dft_i$quantities)
  L1<-c(L1,L(price[[t_t]][i_i]/price[[t_tau]][i_i],lasp))
  L2<-c(L2,L(price[[t_t]][i_i]/price[[t_tau]][i_i],paasch))
  }
  else
  {
  wtau<-c(wtau,0)
  wtau_t<-c(wtau_t,0)
  L1<-c(L1,0)
  L2<-c(L2,0)
  }
  }
  wtau<-wtau/sum(wtau)
  wtau_t<-wtau_t/sum(wtau_t)
  wtau<-wtau*L1
  wtau_t<-wtau_t*L2
  wF<-0.5*(wtau/sum(wtau)+wtau_t/sum(wtau_t))
  return(wF)
  #end wF function
  }  
  s<-which(dates.==start)
  t<-which(dates.==end)
  wF_start<-lapply(dates.,wF_tau_t, t=start)
  wF_end<-lapply(dates.,wF_tau_t, t=end)
  geks<-c()
  for (i in 1:n_av)
  {
  suma_start<-0
  suma_end<-0
  prod_st<-1
  for (tt in 1:TT) {suma_start<-suma_start+wF_start[[tt]][i]
                    suma_end<-suma_end+wF_end[[tt]][i]
                    prod_st<-prod_st*price[[tt]][i]^(wF_start[[tt]][i]-wF_end[[tt]][i])
                    }
  suma_start<-suma_start/TT
  suma_end<-suma_end/TT
  prod_st<-prod_st^(1/(TT))
  geks<-c(geks, (price[[t]][i]^suma_end)/(price[[s]][i]^suma_start)*prod_st)
  }
  columns2<-c(columns2, "geks")
  result$GEKS<-geks
  #end GEKS decomposition   
  }  
  #GEKS-W 
  if ("geksw" %in% formula)
  {
  #weights for individual products
  w_tau_t<-function (tau, t)
  {
  t_tau<-which(dates.==tau)
  t_t<-which(dates.==t)
  W<-c()
  waw<-c()
  L<-c()
  set<-intersect(av_t[[t_tau]],av_t[[t_t]])
  df<-dplyr::filter(data, prodID %in% set)
  dftau<-dplyr::filter(df, time==tau)
  dft<-dplyr::filter(df, time==t)
  #Walsh price index
  p_tau<-dftau$prices
  p_t<-dft$prices
  q_tau<-dftau$quantities
  q_t<-dft$quantities
  walsh<-sum((q_tau*q_t)^0.5*p_t)/sum((q_tau*q_t)^0.5*p_tau)
  #weights
  for (prod in av)  {
    if (prod %in% set) {
  i_i<-which(av==prod)
  dftau_i<-dplyr::filter(dftau, prodID==prod)
  dft_i<-dplyr::filter(dft, prodID==prod)
  waw<-c(waw,dftau_i$prices*(dftau_i$quantities*dft_i$quantities)^0.5)
  L<-c(L,L(price[[t_t]][i_i]/price[[t_tau]][i_i],walsh))
  }
  else
  {
  waw<-c(waw,0)
  L<-c(L,0)
  }
  }
  waw<-waw/sum(waw)
  waw<-waw*L
  W<-waw/sum(waw)
  return(W)
  #end w function
  }  
  s<-which(dates.==start)
  t<-which(dates.==end)
  w_start<-lapply(dates.,w_tau_t, t=start)
  w_end<-lapply(dates.,w_tau_t, t=end)
  geksw<-c()
  for (i in 1:n_av)
  {
  suma_start<-0
  suma_end<-0
  prod_st<-1
  for (tt in 1:TT) {suma_start<-suma_start+w_start[[tt]][i]
                    suma_end<-suma_end+w_end[[tt]][i]
                    prod_st<-prod_st*price[[tt]][i]^(w_start[[tt]][i]-w_end[[tt]][i])
                    }
  suma_start<-suma_start/TT
  suma_end<-suma_end/TT
  prod_st<-prod_st^(1/(TT))
  geksw<-c(geksw, (price[[t]][i]^suma_end)/(price[[s]][i]^suma_start)*prod_st)
  }
  columns2<-c(columns2, "geksw")
  result$GEKS_W<-geksw
  #end GEKS-W decomposition   
  }  
  #GEKS-L 
  if ("geksl" %in% formula)
  {
  #weights for individual products
  wL_tau_t<-function (tau, t)
  {
  t_tau<-which(dates.==tau)
  t_t<-which(dates.==t)
  wL<-c()
  wtau<-c()
  L<-c()
  set<-intersect(av_t[[t_tau]],av_t[[t_t]])
  df<-dplyr::filter(data, prodID %in% set)
  dftau<-dplyr::filter(df, time==tau)
  dft<-dplyr::filter(df, time==t)
  #Laspeyres index
  p_tau<-dftau$prices
  p_t<-dft$prices
  q_tau<-dftau$quantities
  q_t<-dft$quantities
  lasp<-sum(q_tau*p_t)/sum(q_tau*p_tau)
  #weights
  for (prod in av)  {
    if (prod %in% set) {
  i_i<-which(av==prod)
  dftau_i<-dplyr::filter(dftau, prodID==prod)
  wtau<-c(wtau,dftau_i$prices*dftau_i$quantities)
  L<-c(L,L(price[[t_t]][i_i]/price[[t_tau]][i_i],lasp))
  }
  else
  {
  wtau<-c(wtau,0)
  L<-c(L,0)
  }
  }
  wtau<-wtau/sum(wtau)
  wtau<-wtau*L
  wL<-wtau/sum(wtau)
  return(wL)
  #end wL function
  }  
  s<-which(dates.==start)
  t<-which(dates.==end)
  wL_start<-lapply(dates.,wL_tau_t, t=start)
  wL_end<-lapply(dates.,wL_tau_t, t=end)
  geksl<-c()
  for (i in 1:n_av)
  {
  suma_start<-0
  suma_end<-0
  prod_st<-1
  for (tt in 1:TT) {suma_start<-suma_start+wL_start[[tt]][i]
                    suma_end<-suma_end+wL_end[[tt]][i]
                    }
                    prod_st<-prod_st*price[[tt]][i]^(wL_start[[tt]][i]-wL_end[[tt]][i])
  suma_start<-suma_start/TT
  suma_end<-suma_end/TT
  prod_st<-prod_st^(1/(TT))
  geksl<-c(geksl, (price[[t]][i]^suma_end)/(price[[s]][i]^suma_start)*prod_st)
  }
  columns2<-c(columns2, "geksl")
  result$GEKS_L<-geksl
  #end GEKS-L decomposition   
  }  
  #GEKS-GL 
  if ("geksgl" %in% formula)
  {
  #weights for individual products
  wGL_tau_t<-function (tau, t)
  {
  wGL<-c()
  set<-intersect(av_t[[which(dates.==tau)]],av_t[[which(dates.==t)]])
  df<-dplyr::filter(data, prodID %in% set)
  dftau<-dplyr::filter(df, time==tau)
  sales_tau<-sum(dftau$prices*dftau$quantities)
  for (prod in av)  {
    if (prod %in% set) {
    #main work
    dftau_i<-dplyr::filter(dftau, prodID==prod)
    wtau<-dftau_i$prices*dftau_i$quantities
    wGL<-c(wGL, wtau/sales_tau)
   }
  else wGL<-c(wGL,0)
  }
  return(wGL)
  #end wGL function
  }  
  s<-which(dates.==start)
  t<-which(dates.==end)
  wGL_start<-lapply(dates.,wGL_tau_t, t=start)
  wGL_end<-lapply(dates.,wGL_tau_t, t=end)
  geksgl<-c()
  for (i in 1:n_av)
  {
  suma_start<-0
  suma_end<-0
  prod_st<-1
  for (tt in 1:TT) {suma_start<-suma_start+wGL_start[[tt]][i]
                    suma_end<-suma_end+wGL_end[[tt]][i]
                    prod_st<-prod_st*price[[tt]][i]^(wGL_start[[tt]][i]-wGL_end[[tt]][i])
                    }
  suma_start<-suma_start/TT
  suma_end<-suma_end/TT
  prod_st<-prod_st^(1/(TT))
  geksgl<-c(geksgl, (price[[t]][i]^suma_end)/(price[[s]][i]^suma_start)*prod_st)
  }
  columns2<-c(columns2, "geksgl")
  result$GEKS_GL<-geksgl
  #end GEKSGL decomposition   
  }  
  #GEKS-LM 
  if ("gekslm" %in% formula)
  {
  #weights for individual products
  wLM_tau_t<-function (tau, t)
  {
  t_tau<-which(dates.==tau)
  t_t<-which(dates.==t)
  wLM<-c()
  wtau<-c()
  L<-c()
  set<-intersect(av_t[[t_tau]],av_t[[t_t]])
  df<-dplyr::filter(data, prodID %in% set)
  dftau<-dplyr::filter(df, time==tau)
  dft<-dplyr::filter(df, time==t)
  #LM price index
  p_tau<-dftau$prices
  p_t<-dft$prices
  q_tau<-dftau$quantities
  lm<-(1/sum(p_tau*q_tau))*sum(p_tau*q_tau*(p_t/p_tau)^(1-sigma))
  #weights
  for (prod in av)  {
    if (prod %in% set) {
  i_i<-which(av==prod)
  dftau_i<-dplyr::filter(dftau, prodID==prod)
  wtau<-c(wtau,dftau_i$prices*dftau_i$quantities)
  L<-c(L,L((price[[t_t]][i_i]/price[[t_tau]][i_i])^(1-sigma),lm^(1-sigma)))
  }
  else
  {
  wtau<-c(wtau,0)
  L<-c(L,0)
  }
  }
  wtau<-wtau/sum(wtau)
  wtau<-wtau*L
  wLM<-wtau/sum(wtau)
  return(wLM)
  #end wL function
  }  
  s<-which(dates.==start)
  t<-which(dates.==end)
  wLM_start<-lapply(dates.,wLM_tau_t, t=start)
  wLM_end<-lapply(dates.,wLM_tau_t, t=end)
  gekslm<-c()
  for (i in 1:n_av)
  {
  suma_start<-0
  suma_end<-0
  prod_st<-1
  for (tt in 1:TT) {suma_start<-suma_start+wLM_start[[tt]][i]
                    suma_end<-suma_end+wLM_end[[tt]][i]
                    prod_st<-prod_st*price[[tt]][i]^(wLM_start[[tt]][i]-wLM_end[[tt]][i])
                    }
  suma_start<-suma_start/TT
  suma_end<-suma_end/TT
  prod_st<-prod_st^(1/(TT))
  gekslm<-c(gekslm, (price[[t]][i]^suma_end)/(price[[s]][i]^suma_start)*prod_st)
  }
  columns2<-c(columns2, "gekslm")
  result$GEKS_LM<-gekslm
  #end GEKS-LM decomposition   
  }
  #GEKS-AQI 
  if ("geksaqi" %in% formula)
  {
  #weights for individual products
  wAQI_tau_t<-function (tau, t)
  {
  t_tau<-which(dates.==tau)
  t_t<-which(dates.==t)
  WAQI<-c()
  wQ<-c()
  L<-c()
  set<-intersect(av_t[[t_tau]],av_t[[t_t]])
  df<-dplyr::filter(data, prodID %in% set)
  v<-dplyr::filter(v_data, prodID %in% set)
  dftau<-dplyr::filter(df, time==tau)
  dft<-dplyr::filter(df, time==t)
  #AQI price index
  p_tau<-dftau$prices
  p_t<-dft$prices
  q_tau<-dftau$quantities
  aqi<-sum(v$values*q_tau * p_t/p_tau) / sum(v$values*q_tau)
  #weights
  for (prod in av)  {
    if (prod %in% set) {
  i_i<-which(av==prod)
  dftau_i<-dplyr::filter(dftau, prodID==prod)
  v_i<-dplyr::filter(v, prodID==prod)
  wQ<-c(wQ, v_i$values*dftau_i$quantities)
  L<-c(L,L(price[[t_t]][i_i]/price[[t_tau]][i_i],aqi))
  }
  else
  {
  wQ<-c(wQ,0)
  L<-c(L,0)
  }
  }
  wQ<-wQ/sum(wQ)
  wQ<-wQ*L
  WAQI<-wQ/sum(wQ)
  return(WAQI)
  #end wAQI function
  }  
  s<-which(dates.==start)
  t<-which(dates.==end)
  w_start<-lapply(dates.,wAQI_tau_t, t=start)
  w_end<-lapply(dates.,wAQI_tau_t, t=end)
  geksaqi<-c()
  for (i in 1:n_av)
  {
  suma_start<-0
  suma_end<-0
  prod_st<-1
  for (tt in 1:TT) {suma_start<-suma_start+w_start[[tt]][i]
                    suma_end<-suma_end+w_end[[tt]][i]
                    prod_st<-prod_st*price[[tt]][i]^(w_start[[tt]][i]-w_end[[tt]][i])
                    }
  suma_start<-suma_start/TT
  suma_end<-suma_end/TT
  prod_st<-prod_st^(1/(TT))
  geksaqi<-c(geksaqi, (price[[t]][i]^suma_end)/(price[[s]][i]^suma_start)*prod_st)
  }
  columns2<-c(columns2, "geksaqi")
  result$GEKS_AQI<-geksaqi
  #end GEKS-AQI decomposition   
  }  
  #GEKS-GAQI 
  if ("geksgaqi" %in% formula)
  {
  #weights for individual products
  wGAQI_tau_t<-function (tau, t)
  {
  t_tau<-which(dates.==tau)
  t_t<-which(dates.==t)
  wGAQI<-c()
  set<-intersect(av_t[[t_tau]],av_t[[t_t]])
  df<-dplyr::filter(data, prodID %in% set)
  dftau<-dplyr::filter(df, time==tau)
  #main work: weights
  for (prod in av)  {
    if (prod %in% set) {
  i_i<-which(av==prod)
  dftau_i<-dplyr::filter(dftau, prodID==prod)
  v_data_i<-dplyr::filter(v_data, prodID==prod)
  wGAQI<-c(wGAQI, v_data_i$values*dftau_i$quantities)
  }
  else wGAQI<-c(wGAQI,0)
  }
  wGAQI<-wGAQI/sum(wGAQI)
  return(wGAQI)
  #end wGAQI function
  }  
  s<-which(dates.==start)
  t<-which(dates.==end)
  w_start<-lapply(dates.,wGAQI_tau_t, t=start)
  w_end<-lapply(dates.,wGAQI_tau_t, t=end)
  geksgaqi<-c()
  for (i in 1:n_av)
  {
  suma_start<-0
  suma_end<-0
  prod_st<-1
  for (tt in 1:TT) {suma_start<-suma_start+w_start[[tt]][i]
                    suma_end<-suma_end+w_end[[tt]][i]
                    prod_st<-prod_st*price[[tt]][i]^(w_start[[tt]][i]-w_end[[tt]][i])
                    }
  suma_start<-suma_start/TT
  suma_end<-suma_end/TT
  prod_st<-prod_st^(1/(TT))
  geksgaqi<-c(geksgaqi, (price[[t]][i]^suma_end)/(price[[s]][i]^suma_start)*prod_st)
  }
  columns2<-c(columns2, "geksgaqi")
  result$GEKS_GAQI<-geksgaqi
  #end GEKS-GAQI decomposition   
  }  
  result<-result[,match(columns,columns2)]
  l<-list()
  result2<-result[1,]
  result2[1,1]<-"index value (product)"
  result_sum<-result[1,]
  for (i in 2:ncol(result)) result2[1,i]<-prod(result[,i])
  #multiplicative contributions
  if (index.value==TRUE) {
  result3<-rbind(result, result2)
  l$multiplicative<-result3
  }
  else l$multiplicative<-result
  #normalized multiplicative contributions
  for (i in 2:ncol(result)) result[,i]<-result[,i]/(result2[1,i]^(1/n_av))
  l$normalized<-result
  #relative impacts
  for (i in 2:ncol(result)) result[,i]<-(result[,i]-1)*100
  l$impact<-result
  return (l)
  }
