#' @title  Calculating the unweighted Jevons price index
#'
#' @description This function returns a value (or vector of values) of the unweighted bilateral Jevons price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname jevons
#' @return The function returns a value (or vector of values) of the unweighted bilateral Jevons price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).      
#' @references
#' {Jevons, W. S., (1865). \emph{The variation of prices and the value of the currency since 1782}. J. Statist. Soc. Lond., 28, 294-320.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' jevons(milk, start="2018-12", end="2020-01")
#' \donttest{jevons(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

jevons <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  date <- c(start)
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  jev <- prod((price_end / price_start)^(1 / length(id)))
  result <-
  c(result, jev)
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  jev <- prod((price_end / price_start)^(1 / length(id)))
  return(jev)
  }
  }

#' @title  Calculating the unweighted Dutot price index
#'
#' @description This function returns a value (or vector of values) of the unweighted bilateral Dutot price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname dutot
#' @return The function returns a value (or vector of values) of the unweighted bilateral Dutot price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).      
#' @references
#' {Dutot, C. F., (1738). \emph{Reflexions Politiques sur les Finances et le Commerce}. The Hague: Les Freres Vaillant et Nicolas Prevost, Vol. 1.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' dutot(sugar, start="2018-12", end="2019-12")
#' \donttest{dutot(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

dutot <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  result <-
  c(result, sum(price_end) / sum(price_start))
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  return(sum(price_end) / sum(price_start))
  }
  }

#' @title  Calculating the unweighted Carli price index
#'
#' @description This function returns a value (or vector of values) of the unweighted bilateral Carli price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname carli
#' @return The function returns a value (or vector of values) of the unweighted bilateral Carli price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Carli, G. (1804). \emph{Del valore e della proporzione de'metalli monetati}. Scrittori Classici Italiani di Economia Politica, 13, 297-336.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' carli(sugar, start="2018-12", end="2019-12")
#' \donttest{carli(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

carli <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  result <-
  c(result, sum(price_end / price_start) / length(id))
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  return(sum(price_end / price_start) /
  length(id))
  }
  }

#' @title  Calculating the unweighted CSWD price index
#'
#' @description This function returns a value (or vector of values) of the unweighted Carruthers-Sellwood-Ward-Dalen (CSWD) price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname cswd
#' @return The function returns a value (or vector of values) of the unweighted bilateral CSWD price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Carruthers, A.G., Sellwood, D. J, Ward, P. W. (1980). \emph{Recent developments in the retail price index}. Journal of the Royal Statistical Society. Series D (The Statisticain), 29(1), 1-32.}
#'
#' {Dalen, J. (1992). \emph{Recent developments in the retail price index}. The Statistician, 29(1),  1-32.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' cswd(sugar, start="2018-12", end="2019-12")
#' \donttest{cswd(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

cswd <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  a <- sum(price_end / price_start)
  b <- sum(price_start / price_end)
  result <- c(result, (a / b) ^ (1 / 2))
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  a <- sum(price_end / price_start)
  b <- sum(price_start / price_end)
  return((a / b) ^ (1 / 2))
  }
  }
  
#' @title  Calculating the unweighted harmonic price index
#'
#' @description This function returns a value (or vector of values) of the unweighted "unnamed" harmonic price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname harmonic
#' @return The function returns a value (or vector of values) of the unweighted bilateral harmonic price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' harmonic(sugar, start="2018-12", end="2019-12")
#' \donttest{harmonic(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

harmonic <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  result <-
  c(result, length(id) / sum(price_start / price_end))
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  return(length(id) / sum(price_start /
  price_end))
  }
  }

#' @title  Calculating the unweighted BMW price index
#'
#' @description This function returns a value (or vector of values) of the unweighted Balk-Mehrhoff-Walsh (BMW)  price index.
#' @param data User's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname bmw
#' @return The function returns a value (or vector of values) of the unweighted bilateral BMW price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).  
#' @references
#' {Mehrhoff, J.(2007). \emph{A linear approximation to the Jevons index}. In: Von der Lippe (2007): Index Theory and Price Statistics, Peter Lang: Berlin, Germany.}
#'
#' {(2018). \emph{Harmonised Index of Consumer Prices (HICP). Methodological Manual}. Publication Office of the European union, Luxembourg.}
#' @examples 
#' bmw(sugar, start="2018-12", end="2019-12")
#' \donttest{bmw(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

bmw <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  a <-
  sum((price_start / price_end) ^ 0.5)
  b <-
  sum((price_end / price_start) * ((price_start / price_end) ^ 0.5))
  result <- c(result, b / a)
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else
  {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  a <-
  sum((price_start / price_end) ^ 0.5)
  b <-
  sum((price_end / price_start) * ((price_start / price_end) ^ 0.5))
  return(b / a)
  }
  }

#' @title  Calculating the unweighted Dikhanov price index
#'
#' @description This function returns a value (or vector of values) of the unweighted bilateral Dikhanov price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname dikhanov
#' @return The function returns a value (or vector of values) of the unweighted bilateral Dikhanov price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).      
#' @references
#' {Dikhanov, Y., (2024). \emph{A New Elementary Index Number}. Paper presented at the 18th Meeting of the Ottawa Group on Price Indices, Ottawa, Canada.}
#' @examples 
#' dikhanov(sugar, start="2018-12", end="2019-12")
#' \donttest{dikhanov(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

dikhanov <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  result <-
  c(result, sum((price_end/price_start)^0.5) / sum((price_start/price_end)^0.5))
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  return(sum((price_end/price_start)^0.5) / sum((price_start/price_end)^0.5))
  }
  }


#' @title  Calculating the bilateral Laspeyres price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Laspeyres price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname laspeyres
#' @return The function returns a value (or vector of values) of the bilateral Laspeyres price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Laspeyres, E. (1871). \emph{Die Berechnung einer mittleren Waarenpreissteigerung}. Jahrbucher fur Nationalokonomie und Statistik 16, 296-314.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' laspeyres(sugar, start="2018-12", end="2019-12")
#' \donttest{laspeyres(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

laspeyres <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  quantity_start <-
  quantities(data2, period = start, set = id)
  result <-
  c(result,
  sum(quantity_start * price_end) / sum(quantity_start * price_start))
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <-
  matched(data, start, end, type = "prodID", interval = FALSE)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  quantity_start <-
  quantities(data, period = start, set = id)
  return(sum(quantity_start * price_end) /
  sum(quantity_start * price_start))
  }
  }
  
#' @title  Calculating the bilateral Paasche price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Paasche price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname paasche
#' @return The function returns a value (or vector of values) of the bilateral Paasche price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Paasche, H. (1874). \emph{Uber die Preisentwicklung der letzten Jahre nach den Hamburger Borsennotirungen}. Jahrbucher fur Nationalokonomie und Statistik, 12, 168-178.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' paasche(sugar, start="2018-12", end="2019-12")
#' \donttest{paasche(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

paasche <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  quantity_end <-
  quantities(data2, period = end, set = id)
  result <-
  c(result,
  sum(price_end * quantity_end) / sum(price_start * quantity_end))
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <-
  matched(data, start, end, type = "prodID", interval = FALSE)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  quantity_end <-
  quantities(data, period = end, set = id)
  return(sum(price_end * quantity_end) /
  sum(price_start * quantity_end))
  }
  }

#' @title  Calculating the bilateral Fisher price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Fisher price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname fisher
#' @return The function returns a value (or vector of values) of the bilateral Fisher price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating,please use the \code{\link{final_index}} function).   
#' @references
#' {Fisher, I. (1922). \emph{The Making of Index Numbers}. Boston: Houghton Mifflin.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' fisher(sugar, start="2018-12", end="2019-12")
#' \donttest{fisher(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

fisher <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  data21 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  )) 
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data22<-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  data2<-dplyr::bind_rows(data21, data22) 
  id <-intersect(unique(data21$prodID),unique(data22$prodID))
  price_end <-
  prices(data22, period = end, set = id)
  price_start <-
  prices(data21, period = start, set = id)
  quantity_end <-
  quantities(data22, period = end, set = id)
  quantity_start <-
  quantities(data21, period = start, set = id)
  result <-
  c(result, ((
  sum(price_end * quantity_end) / sum(price_start * quantity_end)
  ) * (
  sum(price_end * quantity_start) / sum(price_start * quantity_start)
  )) ^ (0.5))
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data1 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  )) 
  data2<-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  data<-dplyr::bind_rows(data1, data2)
  id <-intersect(unique(data1$prodID),unique(data2$prodID))
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data1, period = start, set = id)
  quantity_end <-
  quantities(data2, period = end, set = id)
  quantity_start <-
  quantities(data1, period = start, set = id)
  return (((
  sum(price_end * quantity_end) / sum(price_start * quantity_end)
  ) * (
  sum(price_end * quantity_start) / sum(price_start * quantity_start)
  )) ^ (0.5))
  }
  }


#' @title  Calculating the bilateral Tornqvist price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Tornqvist price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname tornqvist
#' @return The function returns a value (or vector of values) of the bilateral Tornqvist price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Tornqvist, L. (1936). \emph{The Bank of Finland's Consumption Price Index}. Bank of Finland Monthly Bulletin 10, 1-8.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' tornqvist(sugar, start="2018-12", end="2019-12")
#' \donttest{tornqvist(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

tornqvist <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  sales_end <-
  expenditures(data2, period = end, set = id)
  sales_start <-
  expenditures(data2, period = start, set = id)
  sum_start <- sum(sales_start)
  sum_end <- sum(sales_end)
  tornq <-
  prod((price_end / price_start) ^ (0.5 * ((sales_start / sum_start) + (sales_end /
  sum_end)
  )))
  result <- c(result, tornq)
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  sales_end <-
  expenditures(data, period = end, set = id)
  sales_start <-
  expenditures(data, period = start, set = id)
  sum_start <- sum(sales_start)
  sum_end <- sum(sales_end)
  tornq <-
  prod((price_end / price_start) ^ (0.5 * ((sales_start / sum_start) + (sales_end /
  sum_end)
  )))
  return(tornq)
  }
  }

#' @title  Calculating the bilateral geo-logarithmic Laspeyres price index
#'
#' @description This function returns a value (or vector of values) of the bilateral geo-logarithmic Laspeyres price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname geolaspeyres
#' @return The function returns a value (or vector of values) of the bilateral geo-logarithmic Laspeyres price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' geolaspeyres(sugar, start="2018-12", end="2019-12")
#' \donttest{geolaspeyres(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

geolaspeyres <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  sales_start <-
  expenditures(data2, period = start, set = id)
  sum_start <- sum(sales_start)
  result <-
  c(result, prod((price_end / price_start) ^ (sales_start / sum_start)))
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  sales_start <-
  expenditures(data, period = start, set = id)
  sum_start <- sum(sales_start)
  return(prod((price_end / price_start) ^
  (sales_start / sum_start)))
  }
  }
  
#' @title  Calculating the bilateral geo-logarithmic Paasche price index
#'
#' @description This function returns a value (or vector of values) of the bilateral geo-logarithmic Paasche price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname geopaasche
#' @return The function returns a value (or vector of values) of the bilateral geo-logarithmic Paasche price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' geopaasche(sugar, start="2018-12", end="2019-12")
#' \donttest{geopaasche(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

geopaasche <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  sales_end <-
  expenditures(data2, period = end, set = id)
  sum_end <- sum(sales_end)
  result <-
  c(result, prod((price_end / price_start) ^ (sales_end / sum_end)))
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  sales_end <-
  expenditures(data, period = end, set = id)
  sum_end <- sum(sales_end)
  return(prod((price_end / price_start) ^
  (sales_end / sum_end)))
  }
  }

#' @title  Calculating the bilateral Drobisch price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Drobisch price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname drobisch
#' @return The function returns a value (or vector of values) of the bilateral Drobisch price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
 #' {Drobisch, M. W. (1871). \emph{Ueber einige Einwurfe gegen die in diesen Jahrbuchern veroffentlichte neue Methode, die Veranderungen der Waarenpreise und des Geldwerths zu berechten}.Jahrbucher fur Nationalokonomie und Statistik, Vol. 16, s. 416-427.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' drobisch(sugar, start="2018-12", end="2019-12")
#' \donttest{drobisch(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

drobisch <-
  function(data, start, end, interval = FALSE) {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  a <-
  laspeyres(data, substr(start, 0, 7), substr(end, 0, 7))
  b <-
  paasche(data, substr(start, 0, 7), substr(end, 0, 7))
  result <- c(result, (a + b) / 2)
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  a <- laspeyres(data, substr(start, 0, 7), substr(end, 0, 7))
  b <-
  paasche(data, substr(start, 0, 7), substr(end, 0, 7))
  return((a + b) / 2)
  }
  }

#' @title  Calculating the bilateral Marshall-Edgeworth price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Marshall-Edgeworth price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname marshall_edgeworth
#' @return The function returns a value (or vector of values) of the bilateral Marshall-Edgeworth price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
 #' {Marshall, A. (1887). \emph{Remedies for Fluctuations of General Prices}. Contemporary Review, 51, 355-375.}
#'
#' {Edgeworth, F. Y. (1887). \emph{Measurement of Change in Value of Money I}. The first Memorandum presented to the British Association for the Advancement of Science; reprinted in Papers Relating to Political Economy, Vol. 1, New York, Burt Franklin, s. 1925.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' marshall_edgeworth(sugar, start="2018-12", end="2019-12")
#' \donttest{marshall_edgeworth(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

marshall_edgeworth <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  quantity_end <-
  quantities(data2, period = end, set = id)
  quantity_start <-
  quantities(data2, period = start, set = id)
  result <-
  c(result, sum(
  price_end * (quantity_start + quantity_end) / sum(price_start * (quantity_start +
  quantity_end))
  ))
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else
  {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  quantity_end <-
  quantities(data, period = end, set = id)
  quantity_start <-
  quantities(data, period = start, set = id)
  return(sum(
  price_end * (quantity_start + quantity_end) / sum(price_start * (quantity_start +
  quantity_end))
  ))
  }
  }
  
#' @title  Calculating the bilateral Walsh price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Walsh price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname walsh
#' @return The function returns a value (or vector of values) of the bilateral Walsh price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).  
#' @references
#' {Walsh, C. M. (1901). \emph{The Measurement of General Exchange Value}. The MacMillan Company, New York.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' walsh(sugar, start="2018-12", end="2019-12")
#' \donttest{walsh(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

walsh <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  quantity_end <-
  quantities(data2, period = end, set = id)
  quantity_start <-
  quantities(data2, period = start, set = id)
  result <-
  c(result, sum(
  price_end * (quantity_start * quantity_end) ^ (0.5)) / sum(price_start *
  (quantity_start * quantity_end) ^ (0.5))
  )
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  quantity_end <-
  quantities(data, period = end, set = id)
  quantity_start <-
  quantities(data, period = start, set = id)
  return(sum(
  price_end * (quantity_start * quantity_end) ^ (0.5)) / sum(price_start *
  (quantity_start * quantity_end) ^ (0.5))
  )
  }
  }
  
#' @title  Calculating the bilateral Bialek price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Bialek price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname bialek
#' @return The function returns a value (or vector of values) of the bilateral Bialek price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Von der Lippe, P. (2012). \emph{Some short notes on the price index of Jacek Bialek}. Econometrics (Ekonometria). 1(35), 76-83.}
#'
#' {Bialek, J. (2013). \emph{Some Remarks on the Original Price Index Inspired by the Notes of Peter von der Lippe}. Econometrics (Ekonometria), 3(41), 40-54.}
#'
#' {Bialek, J. (2014). \emph{Simulation Study of an Original Price Index Formula}. Communications in Statistics - Simulation and Computation, 43(2), 285-297}
#' @examples 
#' bialek(sugar, start="2018-12", end="2019-12")
#' \donttest{bialek(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

bialek <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  quantity_end <-
  quantities(data2, period = end, set = id)
  quantity_start <-
  quantities(data2, period = start, set = id)
  a1 <-
  sum(price_end * pmin(quantity_start, quantity_end))
  a2 <-
  sum(price_start * pmin(quantity_start, quantity_end))
  b1 <-
  sum(price_end * pmax(quantity_start, quantity_end))
  b2 <-
  sum(price_start * pmax(quantity_start, quantity_end))
  result <-
  c(result, ((a1 * b1) / (a2 * b2)) ^ (1 / 2))
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  quantity_end <-
  quantities(data, period = end, set = id)
  quantity_start <-
  quantities(data, period = start, set = id)
  a1 <-
  sum(price_end * pmin(quantity_start, quantity_end))
  a2 <-
  sum(price_start * pmin(quantity_start, quantity_end))
  b1 <-
  sum(price_end * pmax(quantity_start, quantity_end))
  b2 <-
  sum(price_start * pmax(quantity_start, quantity_end))
  return(((a1 * b1) / (a2 * b2)) ^ (1 /
  2))
  }
  }

#' @title  Calculating the bilateral Banajree price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Banajree price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname banajree
#' @return The function returns a value (or vector of values) of the bilateral Banajree price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function)..   
#' @references
#' {Banajree, K. S. (1977). \emph{On the factorial approach providing the true index of cost of living.} Gottingen : Vandenhoeck und Ruprecht.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' banajree(sugar, start="2018-12", end="2019-12")
#' \donttest{banajree(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

banajree <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  quantity_end <-
  quantities(data2, period = end, set = id)
  quantity_start <-
  quantities(data2, period = start, set = id)
  vt <- sum(price_end * quantity_end)
  v0 <-
  sum(price_start * quantity_start)
  vt0 <-
  sum(price_end * quantity_start)
  v0t <-
  sum(price_start * quantity_end)
  result <-
  c(result, (vt / v0) * (v0 + vt0) / (vt + v0t))
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  quantity_end <-
  quantities(data, period = end, set = id)
  quantity_start <-
  quantities(data, period = start, set = id)
  vt <- sum(price_end * quantity_end)
  v0 <-
  sum(price_start * quantity_start)
  vt0 <-
  sum(price_end * quantity_start)
  v0t <-
  sum(price_start * quantity_end)
  return((vt / v0) * (v0 + vt0) / (vt +
  v0t))
  }
  }

#' @title  Calculating the bilateral Davies price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Davies price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname davies
#' @return The function returns a value (or vector of values) of the bilateral Davies price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Davies, G. R. (1924). \emph{The Problem of a Standard Index Number Formula.} Journal of the American Statistical Association, 19 (146), 180-188.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' davies(sugar, start="2018-12", end="2019-12")
#' \donttest{davies(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

davies <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  quantity_end <-
  quantities(data2, period = end, set = id)
  quantity_start <-
  quantities(data2, period = start, set = id)
  vt <- sum(price_end * quantity_end)
  v0 <-
  sum(price_start * quantity_start)
  a <-
  sum(quantity_start * (price_end * price_start) ^ (0.5))
  b <-
  sum(quantity_end * (price_end * price_start) ^ (0.5))
  result <- c(result, (vt / v0) * (a /
  b))
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else
  {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  quantity_end <-
  quantities(data, period = end, set = id)
  quantity_start <-
  quantities(data, period = start, set = id)
  vt <- sum(price_end * quantity_end)
  v0 <-
  sum(price_start * quantity_start)
  a <-
  sum(quantity_start * (price_end * price_start) ^ (0.5))
  b <-
  sum(quantity_end * (price_end * price_start) ^ (0.5))
  return((vt / v0) * (a / b))
  }
  }

#' @title  Calculating the bilateral Stuvel price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Stuvel price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname stuvel
#' @return The function returns a value (or vector of values) of the bilateral Stuvel price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Stuvel, G. (1957). \emph{A New Index Number Formula.} Econometrica, 25, 123-131.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' stuvel(sugar, start="2018-12", end="2019-12")
#' \donttest{stuvel(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

stuvel <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  quantity_end <-
  quantities(data2, period = end, set = id)
  quantity_start <-
  quantities(data2, period = start, set = id)
  vt <- sum(price_end * quantity_end)
  v0 <-
  sum(price_start * quantity_start)
  v0t <-
  sum(price_start * quantity_end)
  vt0 <-
  sum(price_end * quantity_start)
  result <-
  c(result, ((vt0 / v0) - (v0t / v0)) / 2 + ((((vt0 / v0) - (v0t / v0)
  ) / 2) ^ 2  + (vt / v0)) ^ (1 / 2))
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  quantity_end <-
  quantities(data, period = end, set = id)
  quantity_start <-
  quantities(data, period = start, set = id)
  vt <- sum(price_end * quantity_end)
  v0 <-
  sum(price_start * quantity_start)
  v0t <-
  sum(price_start * quantity_end)
  vt0 <-
  sum(price_end * quantity_start)
  return(((vt0 / v0) - (v0t / v0)) / 2 + ((((vt0 /
  v0) - (v0t / v0)
  ) / 2) ^ 2  + (vt / v0)) ^ (1 / 2))
  }
  }

#' @title  Calculating the bilateral Palgrave price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Palgrave price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname palgrave
#' @return The function returns a value (or vector of values) of the bilateral Palgrave price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Palgrave, R. H. I. (1886). \emph{Currency and Standard of Value in England, France and India and the Rates of Exchange Between these Countries.} Memorandum submitted to the Royal Commission on Depression of trade and Industry, Third Report, Appendix B, 312-390.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' palgrave(sugar, start="2018-12", end="2019-12")
#' \donttest{palgrave(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

palgrave <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  quantity_end <-
  quantities(data2, period = end, set = id)
  quantity_start <-
  quantities(data2, period = start, set = id)
  vt <- sum(price_end * quantity_end)
  a <-
  sum(price_end ^ 2 * quantity_end / price_start)
  result <- c(result, a / vt)
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  quantity_end <-
  quantities(data, period = end, set = id)
  quantity_start <-
  quantities(data, period = start, set = id)
  vt <- sum(price_end * quantity_end)
  a <-
  sum(price_end ^ 2 * quantity_end / price_start)
  return(a / vt)
  }
  }

#' @title  Calculating the bilateral Geary-Khamis price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Geary-Khamis price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname geary_khamis
#' @return The function returns a value (or vector of values) of the bilateral Geary-Khamis price index depending on the \code{interval} parameter (please use \code{\link{gk}} function to calculate the multilateral Geary-Khamis price index). If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}}  function).   
#' @references
#' {Geary, R. G. (1958). \emph{A Note on Comparisons of Exchange Rates and Purchasing Power between Countries.} Journal of the Royal Statistical Society, Series A, 121, 97-99.}
#'
#' {Khamis, S. H. (1970). \emph{Properties and Conditions for the Existence of a new Type of Index Number.} Sankhya Series B32, 81-98.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' geary_khamis(sugar, start="2018-12", end="2019-12")
#' \donttest{geary_khamis(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

geary_khamis <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  quantity_end <-
  quantities(data2, period = end, set = id)
  quantity_start <-
  quantities(data2, period = start, set = id)
  a <-
  sum(price_end * quantity_start * quantity_end / (quantity_start + quantity_end))
  b <-
  sum(price_start * quantity_start * quantity_end / (quantity_start + quantity_end))
  result <- c(result, a / b)
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  quantity_end <-
  quantities(data, period = end, set = id)
  quantity_start <-
  quantities(data, period = start, set = id)
  a <-
  sum(price_end * quantity_start * quantity_end / (quantity_start + quantity_end))
  b <-
  sum(price_start * quantity_start * quantity_end / (quantity_start + quantity_end))
  return(a / b)
  }
  }

#' @title  Calculating the bilateral Lehr price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Lehr price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname lehr
#' @return The function returns a value (or vector of values) of the bilateral Lehr price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Lehr, J. (1885). \emph{Beitrage zur Statistik der Preise, insbesondere des Geldes und des Holzes.} J. D. Sauerlander, Frankfurt am Main.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' lehr(sugar, start="2018-12", end="2019-12")
#' \donttest{lehr(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

lehr <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data2, start, end)
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  quantity_end <-
  quantities(data2, period = end, set = id)
  quantity_start <-
  quantities(data2, period = start, set = id)
  a <- sum(price_end * quantity_end)
  b <-
  sum((price_start * quantity_start + price_end * quantity_end) * quantity_end /
  (quantity_start + quantity_end)
  )
  c <-
  sum(price_start * quantity_start)
  d <-
  sum((price_start * quantity_start + price_end * quantity_end) * quantity_start /
  (quantity_start + quantity_end)
  )
  result <- c(result, (a / b) / (c / d))
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  )
  )
  id <- matched(data, start, end)
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  quantity_end <-
  quantities(data, period = end, set = id)
  quantity_start <-
  quantities(data, period = start, set = id)
  a <- sum(price_end * quantity_end)
  b <-
  sum((price_start * quantity_start + price_end * quantity_end) * quantity_end /
  (quantity_start + quantity_end)
  )
  c <- sum(price_start * quantity_start)
  d <-
  sum((price_start * quantity_start + price_end * quantity_end) * quantity_start /
  (quantity_start + quantity_end)
  )
  return((a / b) / (c / d))
  }
  }
 
#' @title  Calculating the bilateral Vartia-I price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Vartia-I price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname vartia
#' @return The function returns a value (or vector of values) of the bilateral Vartia-I price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Vartia, Y. 0. (1976). \emph{Ideal Log-Change Index Numbers .}  Scandinavian Journal of Statistics 3(3), 121-126.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' vartia(sugar, start="2018-12", end="2019-12")
#' \donttest{vartia(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

  vartia <-
    function(data, start, end, interval = FALSE)  {
    if (start == end)
    return (1)
    if (nrow(data) == 0)
    stop("A data frame is empty")
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    #returning vector of values
    if (interval == TRUE) {
    result <- c(1)
    end2 <- end
    end <- start
    lubridate::month(end) <-
    lubridate::month(end) + 1
    while (end <= end2)
    {
    t <- substr(end, 0, 7)
    date <- c(date, t)
    data2 <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    )
    )
    id <- matched(data2, start, end)
    price_end <-
    prices(data2, period = end, set = id)
    price_start <-
    prices(data2, period = start, set = id)
    quantity_end <-
    quantities(data2, period = end, set = id)
    quantity_start <-
    quantities(data2, period = start, set = id)
    vt <- sum(price_end * quantity_end)
    v0 <-
    sum(price_start * quantity_start)
    sale_start <-
    price_start * quantity_start
    sale_end <- price_end * quantity_end
    sale <-
    data.frame(sale_start, sale_end)
    vecL <- apply(sale, 1, LL)
    summ <-
    sum(vecL * log(price_end / price_start))
    vartia <- exp(summ / L(vt, v0))
    result <- c(result, vartia)
    lubridate::month(end) <-
    lubridate::month(end) + 1
    }
    return(result)
    }
    #returning one value
    else {
    data <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    )
    )
    id <- matched(data, start, end)
    price_end <-
    prices(data, period = end, set = id)
    price_start <-
    prices(data, period = start, set = id)
    quantity_end <-
    quantities(data, period = end, set = id)
    quantity_start <-
    quantities(data, period = start, set = id)
    vt <- sum(price_end * quantity_end)
    v0 <-
    sum(price_start * quantity_start)
    sale_start <-
    price_start * quantity_start
    sale_end <- price_end * quantity_end
    sale <-
    data.frame(sale_start, sale_end)
    vecL <- apply(sale, 1, LL)
    summ <-
    sum(vecL * log(price_end / price_start))
    vartia <- exp(summ / L(vt, v0))
    return(vartia)
    }
    }
    
#' @title  Calculating the bilateral Vartia-II (Sato-Vartia) price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Vartia-II (Sato-Vartia) price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname sato_vartia
#' @return The function returns a value (or vector of values) of the bilateral Vartia-II (Sato-Vartia) price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Sato, K. (1976). \emph{The Ideal Log-Change Index Number.}  The Review of Economics and Statistics, 58(2), 223-228.}
#'
#' {Vartia, Y. 0. (1976). \emph{Ideal Log-Change Index Numbers .}  Scandinavian Journal of Statistics 3(3), 121-126.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' sato_vartia(sugar, start="2018-12", end="2019-12")
#' \donttest{sato_vartia(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

  sato_vartia <-
    function(data, start, end, interval = FALSE)  {
    if (start == end)
    return (1)
    if (nrow(data) == 0)
    stop("A data frame is empty")
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    #returning vector of values
    if (interval == TRUE) {
    result <- c(1)
    end2 <- end
    end <- start
    lubridate::month(end) <-
    lubridate::month(end) + 1
    while (end <= end2)
    {
    t <- substr(end, 0, 7)
    date <- c(date, t)
    data2 <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    )
    )
    id <- matched(data2, start, end)
    price_end <-
    prices(data2, period = end, set = id)
    price_start <-
    prices(data2, period = start, set = id)
    quantity_end <-
    quantities(data2, period = end, set = id)
    quantity_start <-
    quantities(data2, period = start, set = id)
    vt <- sum(price_end * quantity_end)
    v0 <-
    sum(price_start * quantity_start)
    x <- price_end * quantity_end / vt
    y <- price_start * quantity_start / v0
    z <- data.frame(x, y)
    vecL <- apply(z, 1, LL)
    sum1 <-
    sum(vecL * log(price_end / price_start))
    sum2 <- sum(vecL)
    result <-
    c(result, exp(sum1 / sum2))
    lubridate::month(end) <-
    lubridate::month(end) + 1
    }
    return(result)
    }
    #returning one value
    else {
    data <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    )
    )
    id <- matched(data, start, end)
    price_end <-
    prices(data, period = end, set = id)
    price_start <-
    prices(data, period = start, set = id)
    quantity_end <-
    quantities(data, period = end, set = id)
    quantity_start <-
    quantities(data, period = start, set = id)
    vt <- sum(price_end * quantity_end)
    v0 <-
    sum(price_start * quantity_start)
    x <- price_end * quantity_end / vt
    y <- price_start * quantity_start / v0
    z <- data.frame(x, y)
    vecL <- apply(z, 1, LL)
    sum1 <-
    sum(vecL * log(price_end / price_start))
    sum2 <- sum(vecL)
    return(exp(sum1 / sum2))
    }
    }

#' @title  Calculating the bilateral Lloyd-Moulton price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Lloyd-Moulton price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param sigma The elasticity of substitution parameter (as numeric).
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname lloyd_moulton
#' @return The function returns a value (or vector of values) of the bilateral Lloyd-Moulton price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Lloyd, P. J. (1975). \emph{Substitution Effects and Biases in Nontrue Price Indices.}  The American Economic Review, 65, 301-313.}
#'
#' {Moulton,  B.  R.  (1996). \emph{Constant  Elasticity  Cost-of-Living  Index  in  Share-Relative  Form.}  Washington DC: U. S. Bureau of Labor Statistics, mimeograph}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' lloyd_moulton(sugar, start="2018-12", end="2019-12",sigma=0.9)
#' \donttest{lloyd_moulton(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

  lloyd_moulton <-
    function(data,
    start,
    end,
    sigma = 0.7,
    interval = FALSE)  {
    if (start == end)
    return (1)
    if (nrow(data) == 0)
    stop("A data frame is empty")
    if (sigma == 1)
    stop("A specification of the parameter 'sigma' is wrong")
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    #returning vector of values
    if (interval == TRUE) {
    result <- c(1)
    end2 <- end
    end <- start
    lubridate::month(end) <-
    lubridate::month(end) + 1
    while (end <= end2)
    {
    t <- substr(end, 0, 7)
    date <- c(date, t)
    data2 <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    )
    )
    id <- matched(data2, start, end)
    price_end <-
    prices(data2, period = end, set = id)
    price_start <-
    prices(data2, period = start, set = id)
    quantity_start <-
    quantities(data2, period = start, set = id)
    v0 <-
    sum(price_start * quantity_start)
    sum <-
    sum(price_start * quantity_start / v0 * (price_end / price_start) ^ (1 -
    sigma))
    sum <-
    sum ^ (1 / (1 - sigma))
    result <- c(result, sum)
    lubridate::month(end) <-
    lubridate::month(end) + 1
    }
    return(result)
    }
    #returning one value
    else {
    data <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    )
    )
    id <- matched(data, start, end)
    price_end <-
    prices(data, period = end, set = id)
    price_start <-
    prices(data, period = start, set = id)
    quantity_start <-
    quantities(data, period = start, set = id)
    v0 <-
    sum(price_start * quantity_start)
    sum <-
    sum(price_start * quantity_start / v0 * (price_end / price_start) ^ (1 -
    sigma))
    sum <-
    sum ^ (1 / (1 - sigma))
    return(sum)
    }
    }

#' @title  Calculating the bilateral Young price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Young price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the Young price index formula (as character) limited to the year and month, e.g. "2020-01"
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname young
#' @return The function returns a value (or vector of values) of the bilateral Young price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Young, A. H. (1992). \emph{Alternative Measures of Change in Real Output and Prices.}  Survey of Current Business, 72, 32-48.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' young(sugar, start="2019-01", end="2020-01",base="2018-12")
#' \donttest{young(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

  young <-
    function(data,
    start,
    end,
    base = start,
    interval = FALSE)  {
    if (start == end)
    return (1)
    if (nrow(data) == 0)
    stop("A data frame is empty")
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    base <- paste(base, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    base <- as.Date(base)
    #returning vector of values
    if (interval == TRUE) {
    result <- c(1)
    end2 <- end
    end <- start
    lubridate::month(end) <-
    lubridate::month(end) + 1
    while (end <= end2)
    {
    t <- substr(end, 0, 7)
    date <- c(date, t)
    data2 <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(base) &
    lubridate::month(data$time) == lubridate::month(base)
    )
    )
    id <-
    intersect(matched(data2, start, end), matched(data, base, end))
    price_end <-
    prices(data2, period = end, set = id)
    price_start <-
    prices(data2, period = start, set = id)
    sale_base <-
    expenditures(data2, period = base, set = id)
    vold <- sum(sale_base)
    result <-
    c(result, sum(sale_base / vold * (price_end / price_start)))
    lubridate::month(end) <-
    lubridate::month(end) + 1
    }
    return(result)
    }
    #returning one value
    else {
    data <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(base) &
    lubridate::month(data$time) == lubridate::month(base)
    )
    )
    id <-
    intersect(matched(data, start, end), matched(data, base, end))
    price_end <-
    prices(data, period = end, set = id)
    price_start <-
    prices(data, period = start, set = id)
    sale_base <-
    expenditures(data, period = base, set = id)
    vold <- sum(sale_base)
    return(sum(sale_base / vold * (price_end /
    price_start)))
    }
    }

#' @title  Calculating the bilateral geometric Young price index
#'
#' @description This function returns a value (or vector of values) of the bilateral geometric Young price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the geometric Young price index formula (as character) limited to the year and month, e.g. "2020-01"
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname geoyoung
#' @return The function returns a value (or vector of values) of the bilateral geometric Young price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Young, A. H. (1992). \emph{Alternative Measures of Change in Real Output and Prices.}  Survey of Current Business, 72, 32-48.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' geoyoung(sugar, start="2019-01", end="2020-01",base="2018-12")
#' \donttest{geoyoung(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

  geoyoung <-
    function(data,
    start,
    end,
    base = start,
    interval = FALSE)  {
    if (start == end)
    return (1)
    if (nrow(data) == 0)
    stop("A data frame is empty")
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    base <- paste(base, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    base <- as.Date(base)
    #returning vector of values
    if (interval == TRUE) {
    result <- c(1)
    end2 <- end
    end <- start
    lubridate::month(end) <-
    lubridate::month(end) + 1
    while (end <= end2)
    {
    t <- substr(end, 0, 7)
    date <- c(date, t)
    data2 <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(base) &
    lubridate::month(data$time) == lubridate::month(base)
    )
    )
    id <-
    intersect(matched(data2, start, end), matched(data2, base, end))
    price_end <-
    prices(data2, period = end, set = id)
    price_start <-
    prices(data2, period = start, set = id)
    sale_base <-
    expenditures(data2, period = base, set = id)
    sale_base <-
    sale_base / sum(sale_base)
    result <-
    c(result, prod((price_end / price_start) ^ (sale_base)))
    lubridate::month(end) <-
    lubridate::month(end) + 1
    }
    return(result)
    }
    #returning one value
    else {
    data <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(base) &
    lubridate::month(data$time) == lubridate::month(base)
    )
    )
    id <-
    intersect(matched(data, start, end), matched(data, base, end))
    price_end <-
    prices(data, period = end, set = id)
    price_start <-
    prices(data, period = start, set = id)
    sale_base <-
    expenditures(data, period = base, set = id)
    sale_base <-
    sale_base / sum(sale_base)
    return (prod((price_end / price_start) ^
    (sale_base)))
    }
    }

#' @title  Calculating the bilateral Lowe price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Lowe price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the Lowe price index formula (as character) limited to the year and month, e.g. "2020-01".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname lowe
#' @return The function returns a value (or vector of values) of the bilateral Lowe price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' lowe(sugar, start="2019-01", end="2020-01",base="2018-12")
#' \donttest{lowe(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

  lowe <-
    function(data,
    start,
    end,
    base = start,
    interval = FALSE)  {
    if (start == end)
    return (1)
    if (nrow(data) == 0)
    stop("A data frame is empty")
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    base <- paste(base, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    base <- as.Date(base)
    #returning vector of values
    if (interval == TRUE) {
    result <- c(1)
    end2 <- end
    end <- start
    lubridate::month(end) <-
    lubridate::month(end) + 1
    while (end <= end2)
    {
    t <- substr(end, 0, 7)
    date <- c(date, t)
    data2 <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(base) &
    lubridate::month(data$time) == lubridate::month(base)
    )
    )
    id <-
    intersect(matched(data2, start, end), matched(data2, base, end))
    price_end <-
    prices(data2, period = end, set = id)
    price_start <-
    prices(data2, period = start, set = id)
    quantity_base <-
    quantities(data2, period = base, set = id)
    sale_base <-
    price_start * quantity_base
    vold <- sum(sale_base)
    result <-
    c(result, sum(sale_base / vold * (price_end / price_start)))
    lubridate::month(end) <-
    lubridate::month(end) + 1
    }
    return(result)
    }
    #returning one value
    else
    {
    data <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(base) &
    lubridate::month(data$time) == lubridate::month(base)
    )
    )
    id <-
    intersect(matched(data, start, end), matched(data, base, end))
    price_end <-
    prices(data, period = end, set = id)
    price_start <-
    prices(data, period = start, set = id)
    quantity_base <-
    quantities(data, period = base, set = id)
    sale_base <-
    price_start * quantity_base
    vold <- sum(sale_base)
    return(sum(sale_base / vold * (price_end /
    price_start)))
    }
    }

#' @title  Calculating the bilateral geometric Lowe price index
#'
#' @description This function returns a value (or vector of values) of the bilateral geometric Lowe price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the geometric Lowe price index formula (as character) limited to the year and month, e.g. "2020-01"
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname geolowe
#' @return The function returns a value (or vector of values) of the bilateral geometric Lowe price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' geolowe(sugar, start="2019-01", end="2020-01",base="2018-12")
#' \donttest{geolowe(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

  geolowe <-
    function(data,
    start,
    end,
    base = start,
    interval = FALSE)  {
    if (start == end)
    return (1)
    if (nrow(data) == 0)
    stop("A data frame is empty")
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    base <- paste(base, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    base <- as.Date(base)
    #returning vector of values
    if (interval == TRUE) {
    result <- c(1)
    end2 <- end
    end <- start
    lubridate::month(end) <-
    lubridate::month(end) + 1
    while (end <= end2)
    {
    t <- substr(end, 0, 7)
    date <- c(date, t)
    data2 <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(base) &
    lubridate::month(data$time) == lubridate::month(base)
    )
    )
    id <-
    intersect(matched(data2, start, end), matched(data2, base, end))
    price_end <-
    prices(data2, period = end, set = id)
    price_start <-
    prices(data2, period = start, set = id)
    quantity_base <-
    quantities(data2, period = base, set = id)
    sale_base <-
    price_start * quantity_base
    sale_base <-
    sale_base / sum(sale_base)
    result <-
    c(result, prod((price_end / price_start) ^ (sale_base)))
    lubridate::month(end) <-
    lubridate::month(end) + 1
    }
    
    return(result)
    }
    #returning one value
    else {
    data <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(base) &
    lubridate::month(data$time) == lubridate::month(base)
    )
    )
    id <-
    intersect(matched(data, start, end), matched(data, base, end))
    price_end <-
    prices(data, period = end, set = id)
    price_start <-
    prices(data, period = start, set = id)
    quantity_base <-
    quantities(data, period = base, set = id)
    sale_base <-
    price_start * quantity_base
    sale_base <-
    sale_base / sum(sale_base)
    return (prod((price_end / price_start) ^
    (sale_base)))
    }
    }

#' @title  Calculating the bilateral AG Mean price index
#'
#' @description This function returns a value (or vector of values) of the bilateral AG Mean price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param sigma The elasticity of substitution parameter (as numeric)
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname agmean
#' @return The function returns a value (or vector of values) of the bilateral AG Mean price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Lent J., & Dorfman,A. H. (2009). \emph{Using a Weighted Average of Base Period Price Indexes to Approximate a Superlative Index.} Journal of Official Statistics, 25(1), 139-149.}
#' @examples 
#' agmean(sugar, start="2019-01", end="2020-01",sigma=0.5)
#' \donttest{agmean(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

  agmean <-
    function(data,
    start,
    end,
    sigma = 0.7,
    interval = FALSE)  {
    if (start == end)
    return (1)
    if (nrow(data) == 0)
    stop("A data frame is empty")
    if (sigma == 1)
    stop("A specification of the parameter 'sigma' is wrong")
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    #returning vector of values
    if (interval == TRUE) {
    result <- c(1)
    end2 <- end
    end <- start
    lubridate::month(end) <-
    lubridate::month(end) + 1
    while (end <= end2)
    {
    t <- substr(end, 0, 7)
    date <- c(date, t)
    ag <-
    geolaspeyres(data, substr(start, 0, 7), substr(end, 0, 7)) * sigma + laspeyres(data, substr(start, 0, 7), substr(end, 0, 7)) *
    (1 - sigma)
    result <- c(result, ag)
    lubridate::month(end) <-
    lubridate::month(end) + 1
    }
    return(result)
    }
    #returning one value
    else {
    ag <-
    geolaspeyres(data, substr(start, 0, 7), substr(end, 0, 7)) * sigma + laspeyres(data, substr(start, 0, 7), substr(end, 0, 7)) *
    (1 - sigma)
    return (ag)
    }
    }
    
#' @title  Calculating the bilateral hybrid price index
#'
#' @description This function returns a value (or a vector of values) of the bilateral hybrid price index. The hybrid index was proposed by Bialek (2020) and it uses correlation coefficients between prices and quantities.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. '2020-03'.
#' @param end The research period (as character) limited to the year and month, e.g. '2020-04'.
#' @param base The prior period used in the hybrid price index formula (as character) limited to the year and month, e.g. '2020-01'.
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname hybrid
#' @return The function returns a value (or a vector of values) of the bilateral hybrid price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Bialek, J. (2020). \emph{Proposition of a Hybrid Price Index Formula for the Consumer Price Index Measurement}. Equilibrium. Quarterly Journal of Economics and Economic Policy, 15(4), 697-716.}
#'
#' @examples 
#' hybrid(sugar, start="2019-12", end="2020-08", base="2018-12")
#' \donttest{hybrid(milk, start="2019-12", end="2020-08", base="2018-12", interval=TRUE)}
#' @export

hybrid <-
  function(data,
  start,
  end,
  base = start,
  interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  base <- paste(base, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  base <- as.Date(base)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(base) &
  lubridate::month(data$time) == lubridate::month(base)
  )
  )
  id <-
  intersect(matched(data2, start, end), matched(data2, base, end))
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  quantity_base<-
  quantities(data2, period = base, set =
  id)
  sale_base <-
  expenditures(data2, period = base, set = id) 
  sale_start <-
  price_start * quantity_base
  sale_end <-
  price_end * quantity_base
  vol_base <- sum(sale_base)
  vol_start <- sum(sale_start)
  vol_end <- sum(sale_end)
  corr_base <-
  stats::cor(prices(data2, period = base, set = id),
  quantities(data2, period = base, set = id))
  corr_start <-
  stats::cor(prices(data2, period = start, set = id),
  quantities(data2, period = base, set = id))
  corr_end <-
  stats::cor(prices(data2, period = end, set = id),
  quantities(data2, period = base, set = id))
  if ((abs(corr_base) + abs(corr_start) +
  abs(corr_end)) == 0)
  {
  corr_base <- 1 / 3
  corr_start <- 1 / 3
  corr_end <- 1 / 3
  }
  weight <- c()
  for (i in 1:length(id))
  weight <-
  c(
  weight,
  abs(corr_base) / (abs(corr_base) + abs(corr_start) + abs(corr_end)) * sale_base[i] /
  vol_base + abs(corr_start) / (abs(corr_base) + abs(corr_start) + abs(corr_end)) *
  sale_start[i] / vol_start + abs(corr_end) / (abs(corr_base) + abs(corr_start) +
  abs(corr_end)) * sale_end[i] / vol_end
  )
  result <-
  c(result, sum(weight * (price_end / price_start)))
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(base) &
  lubridate::month(data$time) == lubridate::month(base)
  )
  )
  id <-
  intersect(matched(data, start, end), matched(data, base, end))
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  quantity_base<-
  quantities(data, period = base, set =
  id)
  sale_base <-
  expenditures(data, period = base, set = id) 
  sale_start <-
  price_start * quantity_base
  sale_end <-
  price_end * quantity_base
  vol_base <- sum(sale_base)
  vol_start <- sum(sale_start)
  vol_end <- sum(sale_end)
  corr_base <-
  stats::cor(prices(data, period = base, set = id),
  quantities(data, period = base, set = id))
  corr_start <-
  stats::cor(prices(data, period = start, set = id),
  quantities(data, period = base, set = id))
  corr_end <-
  stats::cor(prices(data, period = end, set = id),
  quantities(data, period = base, set = id))
  if ((abs(corr_base) + abs(corr_start) +
  abs(corr_end)) == 0)
  {
  corr_base <- 1 / 3
  corr_start <- 1 / 3
  corr_end <- 1 / 3
  }
  weight <- c()
  for (i in 1:length(id))
  weight <-
  c(
  weight,
  abs(corr_base) / (abs(corr_base) + abs(corr_start) + abs(corr_end)) * sale_base[i] /
  vol_base + abs(corr_start) / (abs(corr_base) + abs(corr_start) + abs(corr_end)) *
  sale_start[i] / vol_start + abs(corr_end) / (abs(corr_base) + abs(corr_start) +
  abs(corr_end)) * sale_end[i] / vol_end
  )
  return(sum(weight * (price_end / price_start)))
  }
  }
  
#' @title  Calculating the bilateral geohybrid price index
#'
#' @description This function returns a value (or vector of values) of the bilateral geohybrid price index. The geohybrid index was proposed by Bialek (2020) and it uses correlation coefficients between prices and quantities.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the geohybrid price index formula (as character) limited to the year and month, e.g. "2020-01"
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname geohybrid
#' @return The function returns a value (or vector of values) of the bilateral geohybrid price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Bialek, J. (2020). \emph{Proposition of a Hybrid Price Index Formula for the Consumer Price Index Measurement}. Equilibrium. Quarterly Journal of Economics and Economic Policy, 15(4), 697-716.}
#'
#' @examples 
#' geohybrid(sugar, start="2019-12", end="2020-08", base="2018-12")
#' \donttest{geohybrid(milk, start="2019-12", end="2020-08", base="2018-12", interval=TRUE)}
#' @export

geohybrid <-
  function(data,
  start,
  end,
  base = start,
  interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  base <- paste(base, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  base <- as.Date(base)
  #returning vector of values
  if (interval == TRUE) {
  result <- c(1)
  end2 <- end
  end <- start
  lubridate::month(end) <-
  lubridate::month(end) + 1
  while (end <= end2)
  {
  t <- substr(end, 0, 7)
  date <- c(date, t)
  data2 <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(base) &
  lubridate::month(data$time) == lubridate::month(base)
  )
  )
  id <-
  intersect(matched(data2, start, end), matched(data2, base, end))
  price_end <-
  prices(data2, period = end, set = id)
  price_start <-
  prices(data2, period = start, set = id)
  quantity_base<-
  quantities(data2, period = base, set =
  id)
  sale_base <-
  expenditures(data2, period = base, set = id) 
  sale_start <-
  price_start * quantity_base
  sale_end <-
  price_end * quantity_base
  vol_base <- sum(sale_base)
  vol_start <- sum(sale_start)
  vol_end <- sum(sale_end)
  corr_base <-
  stats::cor(prices(data2, period = base, set = id),
  quantities(data2, period = base, set = id))
  corr_start <-
  stats::cor(prices(data2, period = start, set = id),
  quantities(data2, period = base, set = id))
  corr_end <-
  stats::cor(prices(data2, period = end, set = id),
  quantities(data2, period = base, set = id))
  if ((abs(corr_base) + abs(corr_start) +
  abs(corr_end)) == 0)
  {
  corr_base <- 1 / 3
  corr_start <- 1 / 3
  corr_end <- 1 / 3
  }
  weight <- c()
  for (i in 1:length(id))
  weight <-
  c(
  weight,
  abs(corr_base) / (abs(corr_base) + abs(corr_start) + abs(corr_end)) * sale_base[i] /
  vol_base + abs(corr_start) / (abs(corr_base) + abs(corr_start) + abs(corr_end)) *
  sale_start[i] / vol_start + abs(corr_end) / (abs(corr_base) + abs(corr_start) +
  abs(corr_end)) * sale_end[i] / vol_end
  )
  result <-
  c(result, prod((price_end / price_start) ^ weight))
  lubridate::month(end) <-
  lubridate::month(end) + 1
  }
  return(result)
  }
  #returning one value
  else {
  data <-
  dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(start) &
  lubridate::month(data$time) == lubridate::month(start)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(end) &
  lubridate::month(data$time) == lubridate::month(end)
  ) |
  (
  lubridate::year(data$time) == lubridate::year(base) &
  lubridate::month(data$time) == lubridate::month(base)
  )
  )
  id <-
  intersect(matched(data, start, end), matched(data, base, end))
  price_end <-
  prices(data, period = end, set = id)
  price_start <-
  prices(data, period = start, set = id)
  quantity_base<-
  quantities(data, period = base, set =
  id)
  sale_base <-
  expenditures(data, period = base, set = id) 
  sale_start <-
  price_start * quantity_base
  sale_end <-
  price_end * quantity_base
  vol_base <- sum(sale_base)
  vol_start <- sum(sale_start)
  vol_end <- sum(sale_end)
  corr_base <-
  stats::cor(prices(data, period = base, set = id),
  quantities(data, period = base, set = id))
  corr_start <-
  stats::cor(prices(data, period = start, set = id),
  quantities(data, period = base, set = id))
  corr_end <-
  stats::cor(prices(data, period = end, set = id),
  quantities(data, period = base, set = id))
  if ((abs(corr_base) + abs(corr_start) +
  abs(corr_end)) == 0)
  {
  corr_base <- 1 / 3
  corr_start <- 1 / 3
  corr_end <- 1 / 3
  }
  weight <- c()
  for (i in 1:length(id))
  weight <-
  c(
  weight,
  abs(corr_base) / (abs(corr_base) + abs(corr_start) + abs(corr_end)) * sale_base[i] /
  vol_base + abs(corr_start) / (abs(corr_base) + abs(corr_start) + abs(corr_end)) *
  sale_start[i] / vol_start + abs(corr_end) / (abs(corr_base) + abs(corr_start) +
  abs(corr_end)) * sale_end[i] / vol_end
  )
  return(prod((price_end / price_start) ^
  weight))
  }
  }


#' @title  Calculating theoretical (expected) values of the unweighted price index
#'
#' @description This function calculates the theoretical value of the unweighted price index for lognormally distributed prices.
#' @param pmi A numeric vector indicating \code{mi} parameters for lognormally distributed prices from the subsequent months.
#' @param psigma A numeric vector indicating \code{sigma} parameters for lognormally distributed prices from the subsequent months.
#' @param start The first period in the generated data frame (as character) limited to the year and month, e.g. '2019-12'.
#' @param ratio A logical parameter indicating how we define the theoretical unweighted price index. If it is set to TRUE, then the resulting value is a ratio of expected price values from compared months; otherwise the resulting value is the expected value of the ratio of prices from compared months.
#' @rdname tindex
#' @return This function calculates the theoretical value of the unweighted price index for lognormally distributed prices (the month defined by \code{start} parameter plays a role of the fixed base period). The characteristics for these lognormal distributions are set by \code{pmi} and \code{sigma} parameters. The \code{ratio} parameter allows to control the definition of resulting theoretical price index values. The function provides a data frame consisting of dates and corresponding expected values of the theoretical unweighted price index. The generated dataset is ready for further price index calculations.
#'
#' @examples 
#' tindex(pmi=c(1,1.2,1.3),psigma=c(0.1,0.2,0.15),start="2020-01")
#' \donttest{tindex(pmi=c(1,1.2,1.3),psigma=c(0.1,0.2,0.15),start="2020-01",ratio=FALSE)}
#' @export

tindex<-function(pmi=c(),psigma=c(),start, ratio=TRUE)
{
 if ((length(pmi)<=1)|(length(psigma)<=1)) stop("Lengths of parameters: pmi and psigma must be 2 or more!")
 if (!(length(pmi)==length(psigma))) stop("Lengths of parameters: pmi and psigma must be identical!")
 date<-c(start)
 tindex<-c(1)
 start<-paste(start,"-01",sep="")
 start<-as.Date(start)
 #values of parameters for base period
 mi0<-replicate(length(pmi)-1,pmi[1])
 sigma0<-replicate(length(psigma)-1,psigma[1])
 mi<-c()
 sigma<-c()
 #values of parameters for other periods
 for (k in 2:length(pmi))
 { 
 lubridate::month(start)<-lubridate::month(start)+1
 date<-c(date,substr(start,0,7))
 mi<-c(mi,pmi[k])
 sigma<-c(sigma,psigma[k])
 }
 #calculating the expected value of the theoretical price index
 if (ratio==TRUE) {
 tindexx<-mi-mi0+0.5*(sigma0^2+sigma^2)
 tindexx<-exp(tindexx)
                 }
 else {
 tindexx1<-mi0+0.5*sigma0^2 
 tindexx1<-exp(tindexx1)
 tindexx2<-mi+0.5*sigma^2
 tindexx2<-exp(tindexx2)
 tindexx<-tindexx2/tindexx1
      }
 
 tindex<-c(tindex, tindexx)
 #the resulting data frame
 DT<-data.frame(date, tindex)
 return (DT)  
}

#' @title  Calculating the quadratic mean of order r quantity index
#'
#' @description This function returns a value (or vector of values) of the quadratic mean of order r quantity index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter.  
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname QMq
#' @return The function returns a value (or vector of values) of the quadratic mean of order r quantity index - see CPI Manual (2004), Section 17.35, formula 17.30 (page 321).
#' @references
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' QMq(sugar, start="2019-01", end="2020-01")
#' \donttest{QMq(sugar, start="2019-01", end="2020-01", r=1.3, interval=TRUE)}
#' @export

QMq <-
    function(data,
    start,
    end,
    r = 2,
    interval = FALSE)  {
    if (start == end)
    return (1)
    if (nrow(data) == 0)
    stop("A data frame is empty")
    if (r == 0)
    stop("A specification of the parameter 'r' is wrong")
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    #returning vector of values
    if (interval == TRUE) {
    result <- c(1)
    end2 <- end
    end <- start
    lubridate::month(end) <-
    lubridate::month(end) + 1
    while (end <= end2)
    {
    t <- substr(end, 0, 7)
    date <- c(date, t)
    data2 <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    )
    )
    id <- matched(data2, start, end)
    price_end <-
    prices(data2, period = end, set = id)
    price_start <-
    prices(data2, period = start, set = id)
    quantity_end <-
    quantities(data2, period = end, set = id)
    quantity_start <-
    quantities(data2, period = start, set = id)
    v_start <-
    sum(price_start * quantity_start)
    v_end <-
    sum(price_end * quantity_end)
    #index calculation
    sum1<-sum(price_start * quantity_start*(quantity_end/quantity_start)^(r/2))/v_start
    sum2<-sum(price_end * quantity_end*(quantity_end/quantity_start)^(-r/2))/v_end
    #returning results
    result <- c(result, (sum1/sum2)^(1/r))
    lubridate::month(end) <-
    lubridate::month(end) + 1
    }
    return(result)
    }
    #returning one value
    else {
    data <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    )
    )
    id <- matched(data, start, end)
    price_end <-
    prices(data, period = end, set = id)
    price_start <-
    prices(data, period = start, set = id)
    quantity_end <-
    quantities(data, period = end, set = id)
    quantity_start <-
    quantities(data, period = start, set = id)
    v_start <-
    sum(price_start * quantity_start)
    v_end <-
    sum(price_end * quantity_end)
    #index calculation
    sum1<-sum(price_start * quantity_start*(quantity_end/quantity_start)^(r/2))/v_start
    sum2<-sum(price_end * quantity_end*(quantity_end/quantity_start)^(-r/2))/v_end
    #returning results
    return((sum1/sum2)^(1/r))
    }
    }

#' @title  Calculating the quadratic mean of order r price index
#'
#' @description This function returns a value (or vector of values) of the quadratic mean of order r price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter.  
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname QMp
#' @return The function returns a value (or vector of values) of the quadratic mean of order r price index - see CPI Manual (2004), Section 17.40, formula 17.35 (page 321).
#' @references
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' QMp(sugar, start="2019-01", end="2020-01")
#' \donttest{QMp(sugar, start="2019-01", end="2020-01", r=1.3, interval=TRUE)}
#' @export


QMp <-
    function(data,
    start,
    end,
    r = 2,
    interval = FALSE)  {
    if (start == end)
    return (1)
    if (nrow(data) == 0)
    stop("A data frame is empty")
    if (r == 0)
    stop("A specification of the parameter 'r' is wrong")
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    #returning vector of values
    if (interval == TRUE) {
    result <- c(1)
    end2 <- end
    end <- start
    lubridate::month(end) <-
    lubridate::month(end) + 1
    while (end <= end2)
    {
    t <- substr(end, 0, 7)
    date <- c(date, t)
    data2 <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    )
    )
    id <- matched(data2, start, end)
    price_end <-
    prices(data2, period = end, set = id)
    price_start <-
    prices(data2, period = start, set = id)
    quantity_end <-
    quantities(data2, period = end, set = id)
    quantity_start <-
    quantities(data2, period = start, set = id)
    v_start <-
    sum(price_start * quantity_start)
    v_end <-
    sum(price_end * quantity_end)
    #index calculation
    sum1<-sum(price_start * quantity_start*(price_end/price_start)^(r/2))/v_start
    sum2<-sum(price_end * quantity_end*(price_end/price_start)^(-r/2))/v_end
    #returning results
    result <- c(result, (sum1/sum2)^(1/r))
    lubridate::month(end) <-
    lubridate::month(end) + 1
    }
    return(result)
    }
    #returning one value
    else {
    data <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    )
    )
    id <- matched(data, start, end)
    price_end <-
    prices(data, period = end, set = id)
    price_start <-
    prices(data, period = start, set = id)
    quantity_end <-
    quantities(data, period = end, set = id)
    quantity_start <-
    quantities(data, period = start, set = id)
    v_start <-
    sum(price_start * quantity_start)
    v_end <-
    sum(price_end * quantity_end)
    #index calculation
    sum1<-sum(price_start * quantity_start*(price_end/price_start)^(r/2))/v_start
    sum2<-sum(price_end * quantity_end*(price_end/price_start)^(-r/2))/v_end
    #returning results
    return((sum1/sum2)^(1/r))
    }
    }

#' @title  Calculating the implicit quadratic mean of order r price index
#'
#' @description This function returns a value (or vector of values) of the implicit quadratic mean of order r price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter.  
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname IQMp
#' @return The function returns a value (or vector of values) of the implicit quadratic mean of order r price index - see CPI Manual (2004), Section 17.37, formula 17.32 (page 321).
#' @references
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' IQMp(sugar, start="2019-01", end="2020-01")
#' \donttest{IQMp(sugar, start="2019-01", end="2020-01", r=1.3, interval=TRUE)}
#' @export

IQMp <-
    function(data,
    start,
    end,
    r = 2,
    interval = FALSE)  {
    if (start == end)
    return (1)
    if (nrow(data) == 0)
    stop("A data frame is empty")
    if (r == 0)
    stop("A specification of the parameter 'r' is wrong")
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    #returning vector of values
    if (interval == TRUE) {
    result <- c(1)
    end2 <- end
    end <- start
    lubridate::month(end) <-
    lubridate::month(end) + 1
    while (end <= end2)
    {
    t <- substr(end, 0, 7)
    date <- c(date, t)
    data2 <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    )
    )
    id <- matched(data2, start, end)
    price_end <-
    prices(data2, period = end, set = id)
    price_start <-
    prices(data2, period = start, set = id)
    quantity_end <-
    quantities(data2, period = end, set = id)
    quantity_start <-
    quantities(data2, period = start, set = id)
    v_start <-
    sum(price_start * quantity_start)
    v_end <-
    sum(price_end * quantity_end)
    #index calculation
    sum1<-sum(price_start * quantity_start*(quantity_end/quantity_start)^(r/2))/v_start
    sum1<-sum1^(1/r)
    sum2<-sum(price_end * quantity_end*(quantity_end/quantity_start)^(-r/2))/v_end
    sum2<-sum2^(1/r)
    #returning results
    result <- c(result, v_end*sum2/(v_start*sum1))
    lubridate::month(end) <-
    lubridate::month(end) + 1
    }
    return(result)
    }
    #returning one value
    else {
    data <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    )
    )
    id <- matched(data, start, end)
    price_end <-
    prices(data, period = end, set = id)
    price_start <-
    prices(data, period = start, set = id)
    quantity_end <-
    quantities(data, period = end, set = id)
    quantity_start <-
    quantities(data, period = start, set = id)
    v_start <-
    sum(price_start * quantity_start)
    v_end <-
    sum(price_end * quantity_end)
    #index calculation
    sum1<-sum(price_start * quantity_start*(quantity_end/quantity_start)^(r/2))/v_start
    sum1<-sum1^(1/r)
    sum2<-sum(price_end * quantity_end*(quantity_end/quantity_start)^(-r/2))/v_end
    sum2<-sum2^(1/r)
    #returning results
    return(v_end*sum2/(v_start*sum1))
    }
    }

#' @title  Calculating the value index
#'
#' @description This function returns a value (or vector of values) of the value index
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname value_index
#' @return The function returns a value (or vector of values) of the value index. The value index is calculated as sum of expenditures from period \code{end} divided by sum of expenditures from period \code{start}.
#' @references
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' value_index(sugar, start="2019-01", end="2020-01")
#' \donttest{value_index(sugar, start="2019-01", end="2020-01", interval=TRUE)}
#' @export

value_index <-
    function(data,
    start,
    end,
    interval = FALSE)  {
    if (start == end)
    return (1)
    if (nrow(data) == 0)
    stop("A data frame is empty")
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    #returning vector of values
    if (interval == TRUE) {
    result <- c(1)
    end2 <- end
    end <- start
    lubridate::month(end) <-
    lubridate::month(end) + 1
    while (end <= end2)
    {
    t <- substr(end, 0, 7)
    date <- c(date, t)
    data2 <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    )
    )
    id <- matched(data, start, end)
    expenditures_end <-
    expenditures(data, period = end, set = id)
    expenditures_start <-
    expenditures(data, period = start, set = id)
    #returning results
    result <- c(result, sum(expenditures_end)/sum(expenditures_start))
    lubridate::month(end) <-
    lubridate::month(end) + 1
    }
    return(result)
    }
    #returning one value
    else {
    data <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    )
    )
    id <- matched(data, start, end)
    expenditures_end <-
    expenditures(data, period = end, set = id)
    expenditures_start <-
    expenditures(data, period = start, set = id)
    #returning results
    return(sum(expenditures_end)/sum(expenditures_start))
    }
    }

#' @title  Calculating the unit value index
#'
#' @description This function returns a value (or vector of values) of the unit value index
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname unit_value_index
#' @return The function returns a value (or vector of values) of the unit value index. The value index is calculated as the unit value at time \code{start} divided by the unit value at time \code{start}.
#' @references
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' unit_value_index(sugar, start="2019-01", end="2020-01")
#' \donttest{unit_value_index(sugar, start="2019-01", end="2020-01", interval=TRUE)}
#' @export

unit_value_index <-
    function(data,
    start,
    end,
    interval = FALSE)  {
    if (start == end)
    return (1)
    if (nrow(data) == 0)
    stop("A data frame is empty")
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    #returning vector of values
    if (interval == TRUE) {
    result <- c(1)
    end2 <- end
    end <- start
    lubridate::month(end) <-
    lubridate::month(end) + 1
    while (end <= end2)
    {
    t <- substr(end, 0, 7)
    date <- c(date, t)
    data2 <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    )
    )
    id <- matched(data, start, end)
    expenditures_end <-
    expenditures(data, period = end, set = id)
    expenditures_start <-
    expenditures(data, period = start, set = id)
    quantities_end <-
    quantities(data, period = end, set = id)
    quantities_start <-
    quantities(data, period = start, set = id)
    #returning results
    result <- c(result, (sum(expenditures_end)/sum(quantities_end)) /(sum(expenditures_start)/sum(quantities_start)))
    lubridate::month(end) <-
    lubridate::month(end) + 1
    }
    return(result)
    }
    #returning one value
    else {
    data <-
    dplyr::filter(
    data,
    (
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    ) |
    (
    lubridate::year(data$time) == lubridate::year(end) &
    lubridate::month(data$time) == lubridate::month(end)
    )
    )
    id <- matched(data, start, end)
    expenditures_end <-
    expenditures(data, period = end, set = id)
    expenditures_start <-
    expenditures(data, period = start, set = id)
    quantities_end <-
    quantities(data, period = end, set = id)
    quantities_start <-
    quantities(data, period = start, set = id)
    #returning results
    result <- (sum(expenditures_end)/sum(quantities_end)) /(sum(expenditures_start)/sum(quantities_start))
    return (result)
    }
    }

