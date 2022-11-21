#' @title  Extending the multilateral GEKS price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral GEKS price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname geks_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral GEKS price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#' @examples 
#' \donttest{geks_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{geks_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

geks_splice <-
  function (data,
  start,
  end,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, geks(data, t0, t, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * geks(data, t1, t, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * geks(data, tT, t, wstart = tT, window) / geks(data, tT, t1, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * geks(data, th, t, wstart = tT, window) / geks(data, th, t1, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * geks(data, tm, t, wstart = tT, window) / geks(data, tm, t1, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * geks(data, tT, t, wstart =
  tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * geks(data, th, t, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * geks(data, tm, t, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }


#' @title  Extending the multilateral GEKS price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral GEKS price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geks_fbew
#' @return This function returns a value of the multilateral GEKS price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' \donttest{geks_fbew(milk, start="2018-12", end="2019-08")}
#' @export

geks_fbew <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geks(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}

#' @title  Extending the multilateral GEKS price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral GEKS price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geks_fbmw
#' @return This function returns a value of the multilateral GEKS price index extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#' @examples 
#' \donttest{geks_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

geks_fbmw <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geks_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}

#' @title  Extending the multilateral GEKS-W price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral GEKS-W price index (GEKS based on the Walsh formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname geksw_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral GEKS-W price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#' @examples 
#' \donttest{geksw_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{geksw_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

geksw_splice <-
  function (data,
  start,
  end,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, geksw(data, t0, t, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * geksw(data, t1, t, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * geksw(data, tT, t, wstart = tT, window) / geksw(data, tT, t1, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * geksw(data, th, t, wstart = tT, window) / geksw(data, th, t1, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <- lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * geksw(data, tm, t, wstart = tT, window) / geksw(data, tm, t1, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * geksw(data, tT, t, wstart =
  tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * geksw(data, th, t, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * geksw(data, tm, t, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }
  

#' @title  Extending the multilateral GEKS-W price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral GEKS-W price index (GEKS based on the Walsh formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksw_fbew
#' @return This function returns a value of the multilateral GEKS-W price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Walsh, C. M. (1901). \emph{The Measurement of General Exchange Value}. The MacMillan Company, New York.}
#'
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' \donttest{geksw_fbew(milk, start="2018-12", end="2019-08")}
#' @export

geksw_fbew <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geksw(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}

#' @title  Extending the multilateral GEKS-W price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral GEKS-W price index (GEKS based on the Walsh formula) extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksw_fbmw
#' @return This function returns a value of the multilateral GEKS-W price index extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Walsh, C. M. (1901). \emph{The Measurement of General Exchange Value}. The MacMillan Company, New York.}
#'
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#' @examples 
#' \donttest{geksw_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

geksw_fbmw <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geksw_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral GEKS-J price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral GEKS-J price index (GEKS based on the Jevons formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} is needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname geksj_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral GEKS-J price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#' @examples 
#' \donttest{geksj_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{geksj_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

geksj_splice <-
  function (data,
  start,
  end,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, geksj(data, t0, t, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * geksj(data, t1, t, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * geksj(data, tT, t, wstart = tT, window) / geksj(data, tT, t1, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * geksj(data, th, t, wstart = tT, window) / geksj(data, th, t1, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * geksj(data, tm, t, wstart = tT, window) / geksj(data, tm, t1, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * geksj(data, tT, t, wstart =
  tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * geksj(data, th, t, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * geksj(data, tm, t, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }
  

#' @title  Extending the multilateral GEKS-J price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral GEKS-J price index (i.e. the GEKS price index based on the Jevons formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} is needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksj_fbew
#' @return This function returns a value of the multilateral GEKS-J price index (i.e. the GEKS price index based on the Jevons formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' \donttest{geksj_fbew(milk, start="2018-12", end="2019-08")}
#' @export

geksj_fbew <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geksj(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}

#' @title  Extending the multilateral GEKS-J price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral GEKS-J price index (i.e. the GEKS price index based on the Jevons formula) extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} is needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksj_fbmw
#' @return This function returns a value of the multilateral GEKS-J price index (i.e. the GEKS price index based on the Jevons formula) extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#' @examples 
#' \donttest{geksj_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

geksj_fbmw <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geksj_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
  
}


#' @title  Extending the multilateral CCDI price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral CCDI price index (GEKS based on the Tornqvist formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname ccdi_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral CCDI price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Caves, D.W., Christensen, L.R. and Diewert, W.E. (1982). \emph{Multilateral comparisons of output, input, and productivity using superlative index numbers.} Economic Journal 92, 73-86.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#' @examples 
#' \donttest{ccdi_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{ccdi_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

ccdi_splice <-
  function (data,
  start,
  end,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, ccdi(data, t0, t, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * ccdi(data, t1, t, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * ccdi(data, tT, t, wstart = tT, window) / ccdi(data, tT, t1, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * ccdi(data, th, t, wstart = tT, window) / ccdi(data, th, t1, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <- lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * ccdi(data, tm, t, wstart = tT, window) / ccdi(data, tm, t1, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * ccdi(data, tT, t, wstart =
  tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * ccdi(data, th, t, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * ccdi(data, tm, t, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }


#' @title  Extending the multilateral CCDI price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral CCDI price index (GEKS based on the Tornqvist formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname ccdi_fbew
#' @return This function returns a value of the multilateral CCDI price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Caves, D.W., Christensen, L.R. and Diewert, W.E. (1982). \emph{Multilateral comparisons of output, input, and productivity using superlative index numbers.} Economic Journal 92, 73-86.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' \donttest{ccdi_fbew(milk, start="2018-12", end="2019-08")}
#' @export

ccdi_fbew <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <- min(end, last)
  ind <-
  ind * ccdi(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}

#' @title  Extending the multilateral CCDI price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral CCDI price index (GEKS based on the Tornqvist formula) extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname ccdi_fbmw
#' @return This function returns a value of the multilateral CCDI price index extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Caves, D.W., Christensen, L.R. and Diewert, W.E. (1982). \emph{Multilateral comparisons of output, input, and productivity using superlative index numbers.} Economic Journal 92, 73-86.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#' @examples 
#' \donttest{ccdi_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

ccdi_fbmw <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <- min(end, last)
  ind <-
  ind * ccdi_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral Geary-Khamis price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral Geary-Khamis price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname gk_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral Geary-Khamis price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#' @examples 
#' \donttest{gk_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{gk_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

gk_splice <-
  function (data,
  start,
  end,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, gk(data, t0, t, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * gk(data, t1, t, wstart = tT, window))
  if (splice == "window")
  set <-
  c(set,
  set[length(set)] * gk(data, tT, t, wstart = tT, window) / gk(data, tT, t1, wstart =
  tT1, window))
  if (splice == "half")
  set <-
  c(set,
  set[length(set)] * gk(data, th, t, wstart = tT, window) / gk(data, th, t1, wstart =
  tT1, window))
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <- lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * gk(data, tm, t, wstart = tT, window) / gk(data, tm, t1, wstart = tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * gk(data, tT, t, wstart = tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * gk(data, th, t, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * gk(data, tm, t, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }


#' @title  Extending the multilateral Geary-Khamis price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral Geary-Khamis price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname gk_fbew
#' @return This function returns a value of the multilateral Geary-Khamis price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Geary, R. G. (1958). \emph{A Note on Comparisons of Exchange Rates and Purchasing Power between Countries.} Journal of the Royal Statistical Society, Series A, 121, 97-99.}
#'
#' {Khamis, S. H. (1970). \emph{Properties and Conditions for the Existence of a new Type of Index Number.} Sankhya Series B32, 81-98.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' \donttest{gk_fbew(milk, start="2018-12", end="2019-08")}
#' @export

gk_fbew <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <- min(end, last)
  ind <-
  ind * gkreal(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}

#' @title  Extending the multilateral Geary-Khamis price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral Geary-Khamis price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname gk_fbmw
#' @return This function returns a value of the multilateral Geary-Khamis price index extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Geary, R. G. (1958). \emph{A Note on Comparisons of Exchange Rates and Purchasing Power between Countries.} Journal of the Royal Statistical Society, Series A, 121, 97-99.}
#'
#' {Khamis, S. H. (1970). \emph{Properties and Conditions for the Existence of a new Type of Index Number.} Sankhya Series B32, 81-98.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#' @examples 
#' \donttest{gk_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

gk_fbmw <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <- min(end, last)
  ind <-
  ind * gk_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral TPD price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral TPD price index (Time Product Dummy index) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname tpd_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral TPD price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {de  Haan,  J.  and  F.  Krsinich  (2014). \emph{Time  Dummy  Hedonic  and  Quality-Adjusted  Unit Value Indexes: Do They Really Differ?} Paper presented at the Society for Economic Measurement Conference, 18-20 August 2014, Chicago, U.S.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#' @examples 
#' \donttest{tpd_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{tpd_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

tpd_splice <-
  function (data,
  start,
  end,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, tpd(data, t0, t, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * tpd(data, t1, t, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * tpd(data, tT, t, wstart = tT, window) / tpd(data, tT, t1, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * tpd(data, th, t, wstart = tT, window) / tpd(data, th, t1, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * tpd(data, tm, t, wstart = tT, window) / tpd(data, tm, t1, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * tpd(data, tT, t, wstart = tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * tpd(data, th, t, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * tpd(data, tm, t, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }


#' @title  Extending the multilateral TPD price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral TPD price index (Time Product Dummy index) extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname tpd_fbew
#' @return This function returns a value of the multilateral TPD price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).  
#' @references
#' {de  Haan,  J.  and  F.  Krsinich  (2014). \emph{Time  Dummy  Hedonic  and  Quality-Adjusted  Unit Value Indexes: Do They Really Differ?} Paper presented at the Society for Economic Measurement Conference, 18-20 August 2014, Chicago, U.S.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' \donttest{tpd_fbew(milk, start="2018-12", end="2019-08")}
#' @export

tpd_fbew <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <- min(end, last)
  ind <-
  ind * tpd(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}

#' @title  Extending the multilateral TPD price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral TPD price index (Time Product Dummy index) extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname tpd_fbmw
#' @return This function returns a value of the multilateral TPD price index extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {de  Haan,  J.  and  F.  Krsinich  (2014). \emph{Time  Dummy  Hedonic  and  Quality-Adjusted  Unit Value Indexes: Do They Really Differ?} Paper presented at the Society for Economic Measurement Conference, 18-20 August 2014, Chicago, U.S.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#' @examples 
#' \donttest{tpd_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

tpd_fbmw <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <- min(end, last)
  ind <-
  ind * tpd_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral unweighted TPD price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the unweighted multilateral TPD price index (Time Product Dummy index) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname utpd_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the unweighted multilateral TPD price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {de  Haan,  J.  and  F.  Krsinich  (2014). \emph{Time  Dummy  Hedonic  and  Quality-Adjusted  Unit Value Indexes: Do They Really Differ?} Paper presented at the Society for Economic Measurement Conference, 18-20 August 2014, Chicago, U.S.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#' @examples 
#' \donttest{utpd_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{utpd_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

utpd_splice <-
  function (data,
  start,
  end,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, utpd(data, t0, t, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * utpd(data, t1, t, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * utpd(data, tT, t, wstart = tT, window) / utpd(data, tT, t1, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * utpd(data, th, t, wstart = tT, window) / utpd(data, th, t1, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * utpd(data, tm, t, wstart = tT, window) / utpd(data, tm, t1, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * utpd(data, tT, t, wstart = tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * utpd(data, th, t, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * utpd(data, tm, t, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }


#' @title  Extending the unweighted multilateral TPD price index by using the FBEW method.
#'
#' @description This function returns a value of the unweighted multilateral TPD price index (Time Product Dummy index) extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname utpd_fbew
#' @return This function returns a value of the unweighted multilateral TPD price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).  
#' @references
#' {de  Haan,  J.  and  F.  Krsinich  (2014). \emph{Time  Dummy  Hedonic  and  Quality-Adjusted  Unit Value Indexes: Do They Really Differ?} Paper presented at the Society for Economic Measurement Conference, 18-20 August 2014, Chicago, U.S.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' \donttest{utpd_fbew(milk, start="2018-12", end="2019-08")}
#' @export

utpd_fbew <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <- min(end, last)
  ind <-
  ind * utpd(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}

#' @title  Extending the unweighted multilateral TPD price index by using the FBMW method.
#'
#' @description This function returns a value of the unweighted multilateral TPD price index (Time Product Dummy index) extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname utpd_fbmw
#' @return This function returns a value of the unweighted multilateral TPD price index extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {de  Haan,  J.  and  F.  Krsinich  (2014). \emph{Time  Dummy  Hedonic  and  Quality-Adjusted  Unit Value Indexes: Do They Really Differ?} Paper presented at the Society for Economic Measurement Conference, 18-20 August 2014, Chicago, U.S.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#' @examples 
#' \donttest{utpd_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

utpd_fbmw <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <- min(end, last)
  ind <-
  ind * utpd_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}

#' @title  Extending the multilateral weighted GEKS price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral weighted GEKS price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname wgeks_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral weighted GEKS price index (the weighted GEKS index based on the Fisher formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#'
#' @examples 
#' \donttest{wgeks_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{wgeks_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

wgeks_splice <-
  function (data,
  start,
  end,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, wgeks(data, t0, t, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * wgeks(data, t1, t, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * wgeks(data, tT, t, wstart = tT, window) / wgeks(data, tT, t1, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * wgeks(data, th, t, wstart = tT, window) / wgeks(data, th, t1, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * wgeks(data, tm, t, wstart = tT, window) / wgeks(data, tm, t1, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * wgeks(data, tT, t, wstart =
  tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * wgeks(data, th, t, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * wgeks(data, tm, t, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }


#' @title  Extending the multilateral weighted GEKS price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral weighted GEKS price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname wgeks_fbew
#' @return This function returns a value of the multilateral weighted GEKS price index (the weighted GEKS index based on the Fisher formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#'
#' @examples 
#' \donttest{wgeks_fbew(milk, start="2018-12", end="2019-08")}
#' @export

wgeks_fbew <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * wgeks(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}

#' @title  Extending the multilateral weighted GEKS price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral weighted GEKS price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname wgeks_fbmw
#' @return This function returns a value of the multilateral weighted GEKS price index (the weighted GEKS index based on the Fisher formula) extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#'
#' @examples 
#' \donttest{wgeks_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

wgeks_fbmw <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * wgeks_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral GEKS-L price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral GEKS-L price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname geksl_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral GEKS-L price index (the GEKS index based on the Laspeyres formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#'
#' @examples 
#' \donttest{geksl_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{geksl_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

geksl_splice <-
  function (data,
  start,
  end,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, geksl(data, t0, t, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * geksl(data, t1, t, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * geksl(data, tT, t, wstart = tT, window) / geksl(data, tT, t1, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * geksl(data, th, t, wstart = tT, window) / geksl(data, th, t1, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * geksl(data, tm, t, wstart = tT, window) / geksl(data, tm, t1, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * geksl(data, tT, t, wstart =
  tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * geksl(data, th, t, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * geksl(data, tm, t, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }
  

#' @title  Extending the multilateral GEKS-L price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral GEKS-L price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksl_fbew
#' @return This function returns a value of the multilateral GEKS-L price index (the GEKS index based on the Laspeyres formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#'
#' @examples 
#' \donttest{geksl_fbew(milk, start="2018-12", end="2019-08")}
#' @export

geksl_fbew <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geksl(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}

#' @title  Extending the multilateral GEKS-L price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral GEKS-L price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksl_fbmw
#' @return This function returns a value of the multilateral GEKS-L price index (the GEKS index based on the Laspeyres formula) extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#'
#' @examples 
#' \donttest{geksl_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

geksl_fbmw <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geksl_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral weighted GEKS-L price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral weighted GEKS-L price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname wgeksl_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral weighted GEKS-L price index (the weighted GEKS index based on the Laspeyres formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#'
#' @examples 
#' \donttest{wgeksl_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{wgeksl_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

wgeksl_splice <-
  function (data,
  start,
  end,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, wgeksl(data, t0, t, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * wgeksl(data, t1, t, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * wgeksl(data, tT, t, wstart = tT, window) / wgeksl(data, tT, t1, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * wgeksl(data, th, t, wstart = tT, window) / wgeksl(data, th, t1, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * wgeksl(data, tm, t, wstart = tT, window) / wgeksl(data, tm, t1, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * wgeksl(data, tT, t, wstart =
  tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * wgeksl(data, th, t, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * wgeksl(data, tm, t, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }

#' @title  Extending the multilateral weighted GEKS-L price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral weighted GEKS-L price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname wgeksl_fbew
#' @return This function returns a value of the multilateral weighted GEKS-L price index (the weighted GEKS index based on the Laspeyres formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#'
#' @examples 
#' \donttest{wgeksl_fbew(milk, start="2018-12", end="2019-08")}
#' @export

wgeksl_fbew <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * wgeksl(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}



#' @title  Extending the multilateral weighted GEKS-L price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral weighted GEKS-L price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname wgeksl_fbmw
#' @return This function returns a value of the multilateral weighted GEKS-L price index (the GEKS index based on the Laspeyres formula) extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#'
#' @examples 
#' \donttest{wgeksl_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

wgeksl_fbmw <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * wgeksl_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral GEKS-GL price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral GEKS-GL price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname geksgl_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral GEKS-GL price index (the GEKS index based on the geometric Laspeyres formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#'
#' @examples 
#' \donttest{geksgl_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{geksgl_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

geksgl_splice <-
  function (data,
  start,
  end,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, geksgl(data, t0, t, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * geksgl(data, t1, t, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * geksgl(data, tT, t, wstart = tT, window) / geksgl(data, tT, t1, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * geksgl(data, th, t, wstart = tT, window) / geksgl(data, th, t1, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * geksgl(data, tm, t, wstart = tT, window) / geksgl(data, tm, t1, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * geksgl(data, tT, t, wstart =
  tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * geksgl(data, th, t, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * geksgl(data, tm, t, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }
  

#' @title  Extending the multilateral GEKS-GL price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral GEKS-GL price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksgl_fbew
#' @return This function returns a value of the multilateral GEKS-GL price index (the GEKS index based on the geometric Laspeyres formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#'
#' @examples 
#' \donttest{geksgl_fbew(milk, start="2018-12", end="2019-08")}
#' @export

geksgl_fbew <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geksgl(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral GEKS-GL price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral GEKS-GL price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksgl_fbmw
#' @return This function returns a value of the multilateral GEKS-GL price index (the GEKS index based on the geometric Laspeyres formula) extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#'
#' @examples 
#' \donttest{geksgl_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

geksgl_fbmw <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geksgl_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral weighted GEKS-GL price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral weighted GEKS-GL price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname wgeksgl_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral weighted GEKS-GL price index (the weighted GEKS index based on the geometric Laspeyres formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#'
#' @examples 
#' \donttest{wgeksgl_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{wgeksgl_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

wgeksgl_splice <-
  function (data,
  start,
  end,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, wgeksgl(data, t0, t, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * wgeksgl(data, t1, t, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * wgeksgl(data, tT, t, wstart = tT, window) / wgeksgl(data, tT, t1, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * wgeksgl(data, th, t, wstart = tT, window) / wgeksgl(data, th, t1, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * wgeksgl(data, tm, t, wstart = tT, window) / wgeksgl(data, tm, t1, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * wgeksgl(data, tT, t, wstart =
  tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * wgeksgl(data, th, t, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * wgeksgl(data, tm, t, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }


#' @title  Extending the multilateral weighted GEKS-GL price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral weighted GEKS-GL price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname wgeksgl_fbew
#' @return This function returns a value of the multilateral weighted GEKS-GL price index (the weighted GEKS index based on the geometric Laspeyres formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#'
#' @examples 
#' \donttest{wgeksgl_fbew(milk, start="2018-12", end="2019-08")}
#' @export

wgeksgl_fbew <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * wgeksgl(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral weighted GEKS-GL price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral weighted GEKS-GL price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname wgeksgl_fbmw
#' @return This function returns a value of the multilateral weighted GEKS-GL price index (the GEKS index based on the geometric Laspeyres formula) extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#'
#' @examples 
#' \donttest{wgeksgl_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

wgeksgl_fbmw <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * wgeksgl_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral GEKS-AQU price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral GEKS-AQU price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean","window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname geksaqu_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral GEKS-AQU price index (the GEKS index based on the asynchronous quality adjusted unit value formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#'
#' @examples 
#' \donttest{geksaqu_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{geksaqu_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

geksaqu_splice <-
  function (data,
  start,
  end,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, geksaqu(data, t0, t, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * geksaqu(data, t1, t, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * geksaqu(data, tT, t, wstart = tT, window) / geksaqu(data, tT, t1, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * geksaqu(data, th, t, wstart = tT, window) / geksaqu(data, th, t1, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * geksaqu(data, tm, t, wstart = tT, window) / geksaqu(data, tm, t1, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * geksaqu(data, tT, t, wstart =
  tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * geksaqu(data, th, t, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * geksaqu(data, tm, t, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }
  

#' @title  Extending the multilateral GEKS-AQU price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral GEKS-AQU price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksaqu_fbew
#' @return This function returns a value of the multilateral GEKS-AQU price index (the GEKS index based on the asynchronous quality adjusted unit value formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating,please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#'
#' @examples 
#' \donttest{geksaqu_fbew(milk, start="2018-12", end="2019-08")}
#' @export

geksaqu_fbew <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geksaqu(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}

#' @title  Extending the multilateral GEKS-AQU price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral GEKS-AQU price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksaqu_fbmw
#' @return This function returns a value of the multilateral GEKS-AQU price index (the GEKS index based on the asynchronous quality adjusted unit value formula) extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#'
#' @examples 
#' \donttest{geksaqu_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

geksaqu_fbmw <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geksaqu_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral weighted GEKS-AQU price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral weighted GEKS-AQU price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname wgeksaqu_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral weighted GEKS-AQU price index (the weighted GEKS index based on the asynchronous quality adjusted unit value formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#'
#' @examples 
#' \donttest{wgeksaqu_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{wgeksaqu_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

wgeksaqu_splice <-
  function (data,
  start,
  end,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, wgeksaqu(data, t0, t, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * wgeksaqu(data, t1, t, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * wgeksaqu(data, tT, t, wstart = tT, window) / wgeksaqu(data, tT, t1, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * wgeksaqu(data, th, t, wstart = tT, window) / wgeksaqu(data, th, t1, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * wgeksaqu(data, tm, t, wstart = tT, window) / wgeksaqu(data, tm, t1, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * wgeksaqu(data, tT, t, wstart =
  tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * wgeksaqu(data, th, t, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * wgeksaqu(data, tm, t, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }


#' @title  Extending the multilateral weighted GEKS-AQU price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral weighted GEKS-AQU price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname wgeksaqu_fbew
#' @return This function returns a value of the multilateral weighted GEKS-AQU price index (the weighted GEKS index based on the asynchronous quality adjusted unit value formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#'
#' @examples 
#' \donttest{wgeksaqu_fbew(milk, start="2018-12", end="2019-08")}
#' @export

wgeksaqu_fbew <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * wgeksaqu(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral weighted GEKS-AQU price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral weighted GEKS-AQU price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname wgeksaqu_fbmw
#' @return This function returns a value of the multilateral weighted GEKS-AQU price index (the GEKS index based on the asynchronous quality adjusted unit value formula) extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#'
#' @examples 
#' \donttest{wgeksaqu_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

wgeksaqu_fbmw <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * wgeksaqu_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral GEKS-AQI price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral GEKS-AQI price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean","window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname geksaqi_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral GEKS-AQI price index (the GEKS index based on the asynchronous quality adjusted price index formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#'
#' @examples 
#' \donttest{geksaqi_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{geksaqi_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

geksaqi_splice <-
  function (data,
  start,
  end,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, geksaqi(data, t0, t, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * geksaqi(data, t1, t, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * geksaqi(data, tT, t, wstart = tT, window) / geksaqi(data, tT, t1, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * geksaqi(data, th, t, wstart = tT, window) / geksaqi(data, th, t1, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * geksaqi(data, tm, t, wstart = tT, window) / geksaqi(data, tm, t1, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * geksaqi(data, tT, t, wstart =
  tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * geksaqi(data, th, t, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * geksaqi(data, tm, t, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }
  

#' @title  Extending the multilateral GEKS-AQI price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral GEKS-AQI price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksaqi_fbew
#' @return This function returns a value of the multilateral GEKS-AQI price index (the GEKS index based on the asynchronous quality adjusted price index formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#'
#' @examples 
#' \donttest{geksaqi_fbew(milk, start="2018-12", end="2019-08")}
#' @export

geksaqi_fbew <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geksaqi(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral GEKS-AQI price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral GEKS-AQI price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksaqi_fbmw
#' @return This function returns a value of the multilateral GEKS-AQI price index (the GEKS index based on the asynchronous quality adjusted price index formula) extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#'
#' @examples 
#' \donttest{geksaqi_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

geksaqi_fbmw <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geksaqi_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}

#' @title  Extending the multilateral weighted GEKS-AQI price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral weighted GEKS-AQI price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname wgeksaqi_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral weighted GEKS-AQI price index (the weighted GEKS index based on the asynchronous quality adjusted price index formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#'
#' @examples 
#' \donttest{wgeksaqi_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{wgeksaqi_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

wgeksaqi_splice <-
  function (data,
  start,
  end,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, wgeksaqi(data, t0, t, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * wgeksaqi(data, t1, t, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * wgeksaqi(data, tT, t, wstart = tT, window) / wgeksaqi(data, tT, t1, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * wgeksaqi(data, th, t, wstart = tT, window) / wgeksaqi(data, th, t1, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * wgeksaqi(data, tm, t, wstart = tT, window) / wgeksaqi(data, tm, t1, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * wgeksaqi(data, tT, t, wstart =
  tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * wgeksaqi(data, th, t, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * wgeksaqi(data, tm, t, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }


#' @title  Extending the multilateral weighted GEKS-AQI price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral weighted GEKS-AQI price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname wgeksaqi_fbew
#' @return This function returns a value of the multilateral weighted GEKS-AQI price index (the weighted GEKS index based on the asynchronous quality adjusted price index formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#'
#' @examples 
#' \donttest{wgeksaqi_fbew(milk, start="2018-12", end="2019-08")}
#' @export

wgeksaqi_fbew <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * wgeksaqi(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}



#' @title  Extending the multilateral weighted GEKS-AQI price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral weighted GEKS-AQI price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname wgeksaqi_fbmw
#' @return This function returns a value of the multilateral weighted GEKS-AQI price index (the GEKS index based on the asynchronous quality adjusted price index formula) extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#'
#' @examples 
#' \donttest{wgeksaqi_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

wgeksaqi_fbmw <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * wgeksaqi_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral GEKS-GAQI price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral GEKS-GAQI price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean","window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname geksgaqi_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral GEKS-GAQI price index (the GEKS index based on the geometric asynchronous quality adjusted price index formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#'
#' @examples 
#' \donttest{geksgaqi_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{geksgaqi_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

geksgaqi_splice <-
  function (data,
  start,
  end,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, geksgaqi(data, t0, t, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * geksgaqi(data, t1, t, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * geksgaqi(data, tT, t, wstart = tT, window) / geksgaqi(data, tT, t1, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * geksgaqi(data, th, t, wstart = tT, window) / geksgaqi(data, th, t1, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * geksgaqi(data, tm, t, wstart = tT, window) / geksgaqi(data, tm, t1, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * geksgaqi(data, tT, t, wstart =
  tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * geksgaqi(data, th, t, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * geksgaqi(data, tm, t, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }


#' @title  Extending the multilateral GEKS-GAQI price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral GEKS-GAQI price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksgaqi_fbew
#' @return This function returns a value of the multilateral GEKS-GAQI price index (the GEKS index based on the geometric asynchronous quality adjusted price index formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#'
#' @examples 
#' \donttest{geksgaqi_fbew(milk, start="2018-12", end="2019-08")}
#' @export

geksgaqi_fbew <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geksgaqi(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral GEKS-GAQI price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral GEKS-GAQI price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksgaqi_fbmw
#' @return This function returns a value of the multilateral GEKS-GAQI price index (the GEKS index based on the geometric asynchronous quality adjusted price index formula) extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#'
#' @examples 
#' \donttest{geksgaqi_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

geksgaqi_fbmw <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geksgaqi_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral weighted GEKS-GAQI price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral weighted GEKS-GAQI price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname wgeksgaqi_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral weighted GEKS-GAQI price index (the weighted GEKS index based on the geometric asynchronous quality adjusted price index formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#'
#' @examples 
#' \donttest{wgeksgaqi_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{wgeksgaqi_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

wgeksgaqi_splice <-
  function (data,
  start,
  end,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, wgeksgaqi(data, t0, t, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * wgeksgaqi(data, t1, t, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * wgeksgaqi(data, tT, t, wstart = tT, window) / wgeksgaqi(data, tT, t1, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * wgeksgaqi(data, th, t, wstart = tT, window) / wgeksgaqi(data, th, t1, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * wgeksgaqi(data, tm, t, wstart = tT, window) / wgeksgaqi(data, tm, t1, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * wgeksgaqi(data, tT, t, wstart =
  tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * wgeksgaqi(data, th, t, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * wgeksgaqi(data, tm, t, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }

#' @title  Extending the multilateral weighted GEKS-GAQI price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral weighted GEKS-GAQI price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname wgeksgaqi_fbew
#' @return This function returns a value of the multilateral weighted GEKS-GAQI price index (the weighted GEKS index based on the geometric asynchronous quality adjusted price index formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#'
#' @examples 
#' \donttest{wgeksgaqi_fbew(milk, start="2018-12", end="2019-08")}
#' @export

wgeksgaqi_fbew <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * wgeksgaqi(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral weighted GEKS-GAQI price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral weighted GEKS-GAQI price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname wgeksgaqi_fbmw
#' @return This function returns a value of the multilateral weighted GEKS-GAQI price index (the GEKS index based on the geometric asynchronous quality adjusted price index formula) extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#'
#' @examples 
#' \donttest{wgeksgaqi_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

wgeksgaqi_fbmw <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * wgeksgaqi_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7))
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral GEKS-IQM price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral GEKS-IQM price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter used in the implicit quadratic mean of order r price index.
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname geksiqm_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral GEKS-IQM price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#' @examples 
#' \donttest{geksiqm_splice(milk, start="2018-12", end="2020-02", r=0.8, splice="half")}
#' \donttest{geksiqm_splice(milk, start="2018-12", end="2020-02", window=10, interval=TRUE)}
#' @export

geksiqm_splice <-
  function (data,
  start,
  end,
  r=2,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, geksiqm(data, t0, t, r, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * geksiqm(data, t1, t, r, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * geksiqm(data, tT, t, r, wstart = tT, window) / geksiqm(data, tT, t1, r, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * geksiqm(data, th, t, r, wstart = tT, window) / geksiqm(data, th, t1, r, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * geksiqm(data, tm, t, r, wstart = tT, window) / geksiqm(data, tm, t1, r, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * geksiqm(data, tT, t, r, wstart =
  tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * geksiqm(data, th, t, r, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * geksiqm(data, tm, t, r, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }


#' @title  Extending the multilateral GEKS-IQM price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral GEKS-IQM price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter used in the implicit quadratic mean of order r price index.
#' @rdname geksiqm_fbew
#' @return This function returns a value of the multilateral GEKS-IQM price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' \donttest{geksiqm_fbew(milk, start="2018-12", end="2019-08", r=1.2)}
#' @export

geksiqm_fbew <- function(data, start, end, r)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geksiqm(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  r,
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}

#' @title  Extending the multilateral GEKS-IQM price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral GEKS-IQM price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter used in the implicit quadratic mean of order r price index.
#' @rdname geksiqm_fbmw
#' @return This function returns a value of the multilateral GEKS-IQM price index extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#' @examples 
#' \donttest{geksiqm_fbmw(milk, start="2019-12", end="2020-04", r=1.6)}
#' @export

geksiqm_fbmw <- function(data, start, end, r)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geksiqm_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7), r)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}

#' @title  Extending the multilateral GEKS-QM price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral GEKS-QM price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter used in the implicit quadratic mean of order r price index.
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname geksqm_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral GEKS-QM price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#' @examples 
#' \donttest{geksqm_splice(milk, start="2018-12", end="2020-02", r=0.8, splice="half")}
#' \donttest{geksqm_splice(milk, start="2018-12", end="2020-02", window=10, interval=TRUE)}
#' @export

geksqm_splice <-
  function (data,
  start,
  end,
  r=2,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, geksqm(data, t0, t, r, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * geksqm(data, t1, t, r, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * geksqm(data, tT, t, r, wstart = tT, window) / geksqm(data, tT, t1, r, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * geksqm(data, th, t, r, wstart = tT, window) / geksqm(data, th, t1, r, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * geksqm(data, tm, t, r, wstart = tT, window) / geksqm(data, tm, t1, r, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * geksqm(data, tT, t, r, wstart =
  tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * geksqm(data, th, t, r, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * geksqm(data, tm, t, r, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }


#' @title  Extending the multilateral GEKS-QM price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral GEKS-QM price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter used in the implicit quadratic mean of order r price index.
#' @rdname geksqm_fbew
#' @return This function returns a value of the multilateral GEKS-QM price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' \donttest{geksqm_fbew(milk, start="2018-12", end="2019-08", r=1.2)}
#' @export

geksqm_fbew <- function(data, start, end, r)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geksqm(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  r,
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}

#' @title  Extending the multilateral GEKS-QM price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral GEKS-QM price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter used in the implicit quadratic mean of order r price index.
#' @rdname geksqm_fbmw
#' @return This function returns a value of the multilateral GEKS-QM price index extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#' @examples 
#' \donttest{geksqm_fbmw(milk, start="2019-12", end="2020-04", r=1.6)}
#' @export

geksqm_fbmw <- function(data, start, end, r)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * geksqm_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7), r)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}


#' @title  Extending the multilateral GEKS-LM price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral GEKS-LM price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param sigma The elasticity of substitution (a parameter used in the Lloyd-Moulton index formula).
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname gekslm_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral GEKS-LM price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#' @examples 
#' \donttest{gekslm_splice(milk, start="2018-12", end="2020-02", sigma=0.8, splice="half")}
#' \donttest{gekslm_splice(milk, start="2018-12", end="2020-02", window=10, interval=TRUE)}
#' @export

gekslm_splice <-
  function (data,
  start,
  end,
  sigma=0.7,
  window = 13,
  splice = "movement",
  interval = FALSE)
  {
  asplice <-
  c(
  "movement",
  "window",
  "half",
  "mean",
  "window_published",
  "half_published",
  "mean_published"
  ) #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  t0 <- start
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  set <- c(1)
  #main body
  while (start < end)
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t <- substr(start, 0, 7)
  if (start <= wend)
  set <- c(set, gekslm(data, t0, t, sigma, wstart = t0, window))
  else {
  t1 <- start
  lubridate::month(t1) <-
  lubridate::month(t1) - 1
  tT <- start
  lubridate::month(tT) <-
  lubridate::month(tT) - (window - 1)
  tT1 <- start
  lubridate::month(tT1) <-
  lubridate::month(tT1) - (window - 1) - 1
  th <- start
  lubridate::month(th) <-
  lubridate::month(th) - floor(window / 2)
  t1 <- substr(t1, 0, 7)
  tT <- substr(tT, 0, 7)
  tT1 <- substr(tT1, 0, 7)
  th <- substr(th, 0, 7)
  if (splice == "movement")
  set <- c(set, set[length(set)] * gekslm(data, t1, t, sigma, wstart = tT, window))
  if (splice == "window")
  set <-
  c(
  set,
  set[length(set)] * gekslm(data, tT, t, sigma, wstart = tT, window) / gekslm(data, tT, t1, sigma, wstart =
  tT1, window)
  )
  if (splice == "half")
  set <-
  c(
  set,
  set[length(set)] * gekslm(data, th, t, sigma, wstart = tT, window) / gekslm(data, th, t1, sigma, wstart =
  tT1, window)
  )
  if (splice == "mean") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <-
  var * gekslm(data, tm, t, sigma, wstart = tT, window) / gekslm(data, tm, t1, sigma, wstart =
  tT1, window)
  }
  var <- var ^ (1 / (window - 1))
  set <-
  c(set, set[length(set)] * var)
  }
  if (splice == "window_published")
  set <-
  c(set, set[length(set) + 1 - (window - 1)] * gekslm(data, tT, t, sigma, wstart =
  tT, window))
  if (splice == "half_published")
  set <-
  c(set, set[length(set) + 1 - floor(window / 2)] * gekslm(data, th, t, sigma, wstart =
  tT, window))
  if (splice == "mean_published") {
  var <- 1
  for (m in 1:(window - 1)) {
  tm <- start
  lubridate::month(tm) <-
  lubridate::month(tm) - m
  tm <- substr(tm, 0, 7)
  var <- var * set[length(set) + 1 - m] * gekslm(data, tm, t, sigma, wstart = tT, window)
  }
  var <- var ^ (1 / (window - 1))
  set <- c(set, var)
  }
  }
  }
  if (interval == FALSE)
  return (set[length(set)])
  else
  return(set)
  }


#' @title  Extending the multilateral GEKS-LM price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral GEKS-LM price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param sigma The elasticity of substitution (a parameter used in the Lloyd-Moulton index formula)..
#' @rdname gekslm_fbew
#' @return This function returns a value of the multilateral GEKS-LM price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' \donttest{gekslm_fbew(milk, start="2018-12", end="2019-08", sigma=1.2)}
#' @export

gekslm_fbew <- function(data, start, end, sigma)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * gekslm(data,
  substr(start, 0, 7),
  substr(new, 0, 7),
  sigma,
  window = dist(start, new) + 1)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}

#' @title  Extending the multilateral GEKS-LM price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral GEKS-LM price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param sigma The elasticity of substitution (a parameter used in the Lloyd-Moulton index formula).
#' @rdname gekslm_fbmw
#' @return This function returns a value of the multilateral GEKS-LM price index extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#' @examples 
#' \donttest{geksqm_fbmw(milk, start="2019-12", end="2020-04", r=1.6)}
#' @export

gekslm_fbmw <- function(data, start, end, sigma)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  ind <- 1
  last <- as.Date(start)
  years <-
  lubridate::year(end) - lubridate::year(start)
  #main body
  for (i in 1:years) {
  lubridate::year(last) <- lubridate::year(last) + 1
  new <-
  min(end, last)
  ind <-
  ind * gekslm_fbmw2(data, substr(start, 0, 7), substr(new, 0, 7), sigma)
  lubridate::year(start) <-
  lubridate::year(start) + 1
  }
  return (ind)
}
