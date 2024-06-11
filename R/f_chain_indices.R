#' @title  Calculating the monthly chained Jevons price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Jevons price index
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this  function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chjevons
#' @return The function returns a value (or vector of values) of the monthly chained Jevons price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).    
#' @references
#' {Jevons, W. S., (1865). \emph{The variation of prices and the value of the currency since 1782}. J. Statist. Soc. Lond., 28, 294-320.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chjevons(sugar, start="2018-12", end="2019-04")
#' \donttest{chjevons(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

  chjevons <-
    function(data, start, end, interval = FALSE)  {
    if (start == end)
    return (1)
    if (nrow(data) == 0)
    stop("A data frame is empty")
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    date <- c()
    while (start <= end)
    {
    t <- substr(start, 0, 7)
    date <- c(date, t)
    lubridate::month(start) <-
    lubridate::month(start) + 1
    }
    f <-
    function (i)
    return (jevons(data, start = date[i], end = date[i + 1]))
    ind <- seq(1:(length(date) - 1))
    chained1 <- sapply(ind, f)
    chained <- prod(chained1)
    if (interval == TRUE) {
    #optional returning all fixed base chain indices
    chained <- c(1)
    for (i in 1:length(chained1))
    chained <- c(chained, prod(chained1[seq(1, i)]))
    }
    return(chained)
    }


#' @title  Calculating the monthly chained Carli price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Carli price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chcarli
#' @return The function returns a value (or vector of values) of the monthly chained Carli price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Carli, G. (1804). \emph{Del valore e della proporzione de'metalli monetati}. Scrittori Classici Italiani di Economia Politica, 13, 297-336.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chcarli(sugar, start="2018-12", end="2019-04")
#' \donttest{chcarli(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chcarli <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (carli(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }
  
#' @title  Calculating the monthly chained Dutot price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Dutot price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chdutot
#' @return The function returns a value (or vector of values) of the monthly chained Dutot price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).     
#' @references
#' {Dutot, C. F., (1738). \emph{Reflexions Politiques sur les Finances et le Commerce}. The Hague: Les Freres Vaillant et Nicolas Prevost, Vol. 1.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chdutot(sugar, start="2018-12", end="2019-04")
#' \donttest{chdutot(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chdutot <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (dutot(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained CSWD price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Carruthers-Sellwood-Ward-Dalen (CSWD) price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chcswd
#' @return The function returns a value (or vector of values) of the monthly chained CSWD price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Carruthers, A.G., Sellwood, D. J, Ward, P. W. (1980). \emph{Recent developments in the retail price index}. Journal of the Royal Statistical Society. Series D (The Statisticain), 29(1), 1-32.}
#'
#' {Dalen, J. (1992). \emph{Recent developments in the retail price index}. The Statistician, 29(1),  1-32.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chcswd(sugar, start="2018-12", end="2019-04")
#' \donttest{chcswd(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chcswd <-
  function(data, start, end, interval = FALSE) {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (cswd(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained harmonic price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained "unnamed" harmonic price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chharmonic
#' @return The function returns a value (or vector of values) of the monthly chained harmonic price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chharmonic(sugar, start="2018-12", end="2019-04")
#' \donttest{chharmonic(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chharmonic <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (harmonic(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained BMW price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Balk-Mehrhoff-Walsh (BMW)  price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chbmw
#' @return The function returns a value (or vector of values) of the monthly chained BMW price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Mehrhoff, J.(2007). \emph{A linear approximation to the Jevons index}. In: Von der Lippe (2007): Index Theory and Price Statistics, Peter Lang: Berlin, Germany.}
#'
#' {(2018). \emph{Harmonised Index of Consumer Prices (HICP). Methodological Manual}. Publication Office of the European union, Luxembourg.}
#' @examples 
#' chbmw(sugar, start="2018-12", end="2019-04")
#' \donttest{chbmw(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chbmw <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (bmw(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained Dikhanov price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Dikhanov price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chdikhanov
#' @return The function returns a value (or vector of values) of the monthly chained Dikhanov price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).     
#' @references
#' {Dikhanov, Y., (2024). \emph{A New Elementary Index Number}. Paper presented at the 18th Meeting of the Ottawa Group on Price Indices, Ottawa, Canada.}
#' @examples 
#' chdikhanov(sugar, start="2018-12", end="2019-04")
#' \donttest{chdikhanov(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chdikhanov <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (dikhanov(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained Laspeyres price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Laspeyres price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chlaspeyres
#' @return The function returns a value (or vector of values) of the monthly chained Laspeyres price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Laspeyres, E. (1871). \emph{Die Berechnung einer mittleren Waarenpreissteigerung}. Jahrbucher fur Nationalokonomie und Statistik 16, 296-314.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chlaspeyres(sugar, start="2018-12", end="2019-04")
#' \donttest{chlaspeyres(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chlaspeyres <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (laspeyres(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained Paasche price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Paasche price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chpaasche
#' @return The function returns a value (or vector of values) of the monthly chained Paasche price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Paasche, H. (1874). \emph{Uber die Preisentwicklung der letzten Jahre nach den Hamburger Borsennotirungen}. Jahrbucher fur Nationalokonomie und Statistik, 12, 168-178.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chpaasche(sugar, start="2018-12", end="2019-04")
#' \donttest{chpaasche(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chpaasche <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (paasche(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }
  
#' @title  Calculating the monthly chained Fisher price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Fisher price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chfisher
#' @return The function returns a value (or vector of values) of the monthly chained Fisher price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).  
#' @references
#' {Fisher, I. (1922). \emph{The Making of Index Numbers}. Boston: Houghton Mifflin.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chfisher(sugar, start="2018-12", end="2019-04")
#' \donttest{chfisher(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chfisher <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (fisher(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained Tornqvist price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Tornqvist price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chtornqvist
#' @return The function returns a value (or vector of values) of the monthly chained Tornqvist price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Tornqvist, L. (1936). \emph{The Bank of Finland's Consumption Price Index}. Bank of Finland Monthly Bulletin 10, 1-8.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chtornqvist(sugar, start="2018-12", end="2019-04")
#' \donttest{chtornqvist(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chtornqvist <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (tornqvist(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained geo-logarithmic Laspeyres price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained geo-logarithmic Laspeyres price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chgeolaspeyres
#' @return The function returns a value (or vector of values) of the monthly chained geo-logarithmic Laspeyres price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chgeolaspeyres(sugar, start="2018-12", end="2019-04")
#' \donttest{chgeolaspeyres(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chgeolaspeyres <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (geolaspeyres(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained geo-logarithmic Paasche price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained geo-logarithmic Paasche price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chgeopaasche
#' @return The function returns a value (or vector of values) of the monthly chained geo-logarithmic Paasche price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chgeopaasche(sugar, start="2018-12", end="2019-04")
#' \donttest{chgeopaasche(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chgeopaasche <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (geopaasche(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained Drobisch price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Drobisch price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chdrobisch
#' @return The function returns a value (or vector of values) of the monthly chained Drobisch price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
 #' {Drobisch, M. W. (1871). \emph{Ueber einige Einwurfe gegen die in diesen Jahrbuchern veroffentlichte neue Methode, die Veranderungen der Waarenpreise und des Geldwerths zu berechten}.Jahrbucher fur Nationalokonomie und Statistik, Vol. 16, s. 416-427.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' \donttest{chdrobisch(sugar, start="2018-12", end="2019-04")}
#' \donttest{chdrobisch(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chdrobisch <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (drobisch(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained Marshall-Edgeworth price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Marshall-Edgeworth price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chmarshall_edgeworth
#' @return The function returns a value (or vector of values) of the monthly chained Marshall-Edgeworth price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
 #' {Marshall, A. (1887). \emph{Remedies for Fluctuations of General Prices}. Contemporary Review, 51, 355-375.}
#'
#' {Edgeworth, F. Y. (1887). \emph{Measurement of Change in Value of Money I}. The first Memorandum presented to the British Association for the Advancement of Science; reprinted in Papers Relating to Political Economy, Vol. 1, New York, Burt Franklin, s. 1925.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' chmarshall_edgeworth(sugar, start="2018-12", end="2019-04")
#' \donttest{chmarshall_edgeworth(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export 

chmarshall_edgeworth <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (marshall_edgeworth(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }            

#' @title  Calculating the monthly chained Walsh price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Walsh price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chwalsh
#' @return The function returns a value (or vector of values) of the monthly chained Walsh price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Walsh, C. M. (1901). \emph{The Measurement of General Exchange Value}. The MacMillan Company, New York.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' chwalsh(sugar, start="2018-12", end="2019-04")
#' \donttest{chwalsh(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chwalsh <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (walsh(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained Bialek price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Bialek price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chbialek
#' @return The function returns a value (or vector of values) of the monthly chained Bialek price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Von der Lippe, P. (2012). \emph{Some short notes on the price index of Jacek Bialek}. Econometrics (Ekonometria). 1(35), 76-83.}
#'
#' {Bialek, J. (2013). \emph{Some Remarks on the Original Price Index Inspired by the Notes of Peter von der Lippe}. Econometrics (Ekonometria), 3(41), 40-54.}
#'
#' {Bialek, J. (2014). \emph{Simulation Study of an Original Price Index Formula}. Communications in Statistics - Simulation and Computation, 43(2), 285-297}
#' @examples 
#' chbialek(sugar, start="2018-12", end="2019-04")
#' \donttest{chbialek(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chbialek <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (bialek(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }
  
#' @title  Calculating the monthly chained Banajree price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Banajree price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chbanajree
#' @return The function returns a value (or vector of values) of the monthly chained Banajree price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Banajree, K. S. (1977). \emph{On the factorial approach providing the true index of cost of living.} Gottingen : Vandenhoeck und Ruprecht.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' chbanajree(sugar, start="2018-12", end="2019-04")
#' \donttest{chbanajree(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chbanajree <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (banajree(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained Davies price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Davies price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chdavies
#' @return The function returns a value (or vector of values) of the monthly chained Davies price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Davies, G. R. (1924). \emph{The Problem of a Standard Index Number Formula.} Journal of the American Statistical Association, 19 (146), 180-188.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' chdavies(sugar, start="2018-12", end="2019-04")
#' \donttest{chdavies(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chdavies <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (davies(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained Stuvel price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Stuvel price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chstuvel
#' @return The function returns a value (or vector of values) of the monthly chained Stuvel price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Stuvel, G. (1957). \emph{A New Index Number Formula.} Econometrica, 25, 123-131.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' chstuvel(sugar, start="2018-12", end="2019-04")
#' \donttest{chstuvel(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chstuvel <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (stuvel(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained Palgrave price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Palgrave price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chpalgrave
#' @return The function returns a value (or vector of values) of the monthly chained Palgrave price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Palgrave, R. H. I. (1886). \emph{Currency and Standard of Value in England, France and India and the Rates of Exchange Between these Countries.} Memorandum submitted to the Royal Commission on Depression of trade and Industry, Third Report, Appendix B, 312-390.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' chpalgrave(sugar, start="2018-12", end="2019-04")
#' \donttest{chpalgrave(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chpalgrave <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (palgrave(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained Geary-Khamis price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Geary-Khamis price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chgeary_khamis
#' @return The function returns a value (or vector of values) of the monthly chained Geary-Khamis price index depending on the \code{interval} parameter (please use \code{\link{gk}} function to calculate the multilateral Geary-Khamis price index). If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (see the \code{final_index} function).    
#' @references
#' {Geary, R. G. (1958). \emph{A Note on Comparisons of Exchange Rates and Purchasing Power between Countries.} Journal of the Royal Statistical Society, Series A, 121, 97-99.}
#'
#' {Khamis, S. H. (1970). \emph{Properties and Conditions for the Existence of a new Type of Index Number.} Sankhya Series B32, 81-98.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' chgeary_khamis(sugar, start="2018-12", end="2019-04")
#' \donttest{chgeary_khamis(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chgeary_khamis <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (geary_khamis(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }
  
#' @title  Calculating the monthly chained Lehr price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Lehr price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chlehr
#' @return The function returns a value (or vector of values) of the monthly chained Lehr price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).  
#' @references
#' {Lehr, J. (1885). \emph{Beitrage zur Statistik der Preise, insbesondere des Geldes und des Holzes.} J. D. Sauerlander, Frankfurt am Main.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chlehr(sugar, start="2018-12", end="2019-04")
#' \donttest{chlehr(milk, start="2018-12", end="2020-01", TRUE)}
#' @export

chlehr <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (lehr(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }
  

#' @title  Calculating the monthly chained Vartia-I price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Vartia-I price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chvartia
#' @return The function returns a value (or vector of values) of the monthly chained Vartia-I price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Vartia, Y. 0. (1976). \emph{Ideal Log-Change Index Numbers .}  Scandinavian Journal of Statistics 3(3), 121-126.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' chvartia(sugar, start="2018-12", end="2019-04")
#' \donttest{chvartia(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chvartia <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (vartia(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained Vartia-II (Sato-Vartia) price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Vartia-II (Sato-Vartia) price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chsato_vartia
#' @return The function returns a value (or vector of values) of the monthly chained Vartia-II (Sato-Vartia) price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Sato, K. (1976). \emph{The Ideal Log-Change Index Number.}  The Review of Economics and Statistics, 58(2), 223-228.}
#'
#' {Vartia, Y. 0. (1976). \emph{Ideal Log-Change Index Numbers .}  Scandinavian Journal of Statistics 3(3), 121-126.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' chsato_vartia(sugar, start="2018-12", end="2019-04")
#' \donttest{chsato_vartia(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chsato_vartia <-
  function(data, start, end, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (sato_vartia(data, start = dates[i], end = dates[i + 1]))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained Lloyd-Moulton price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Lloyd-Moulton price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param sigma The elasticity of substitution parameter (as numeric).
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chlloyd_moulton
#' @return The function returns a value (or vector of values) of the monthly chained Lloyd-Moulton price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Lloyd, P. J. (1975). \emph{Substitution Effects and Biases in Nontrue Price Indices.}  The American Economic Review, 65, 301-313.}
#'
#' {Moulton,  B.  R.  (1996). \emph{Constant  Elasticity  Cost-of-Living  Index  in  Share-Relative  Form.}  Washington DC: U. S. Bureau of Labor Statistics, mimeograph}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' chlloyd_moulton(sugar, start="2018-12", end="2019-04",sigma=0.9)
#' \donttest{chlloyd_moulton(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chlloyd_moulton <-
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
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (lloyd_moulton(data, start = dates[i], end = dates[i + 1], sigma))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained AG Mean price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained AG Mean price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param sigma The elasticity of substitution parameter (as numeric).
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chagmean
#' @return The function returns a value (or vector of values) of the monthly chained AG Mean price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Lent J., & Dorfman,A. H. (2009). \emph{Using a Weighted Average of Base Period Price Indexes to Approximate a Superlative Index.} Journal of Official Statistics, 25(1), 139-149.}
#' @examples 
#' chagmean(sugar, start="2019-01", end="2019-04",sigma=0.5)
#' \donttest{chagmean(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chagmean <-
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
  dates <- c()
  dates <- seq.Date(from = start, to = end, by = "month")
  dates <- format(dates, format = "%Y-%m")
  f <-
  function (i)
  return (agmean(data, start = dates[i], end = dates[i + 1], sigma))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained Young price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Young price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the Young price index formula (as character) limited to the year and month, e.g. "2020-01".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chyoung
#' @return The function returns a value (or vector of values) of the monthly chained Young price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Young, A. H. (1992). \emph{Alternative Measures of Change in Real Output and Prices.}  Survey of Current Business, 72, 32-48.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chyoung(sugar, start="2019-01", end="2019-04",base="2018-12")
#' \donttest{chyoung(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chyoung <-
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
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  dates <- seq.Date(from = start, to = end, by = "month")
  dates <- format(dates, format = "%Y-%m")
  f <-
  function (i)
  return (young(data, start = dates[i], end = dates[i + 1], base))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained geometric Young price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained geometric Young price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the geometric Young price index formula (as character) limited to the year and month, e.g. "2020-01".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chgeoyoung
#' @return The function returns a value (or vector of values) of the monthly chained geometric Young price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Young, A. H. (1992). \emph{Alternative Measures of Change in Real Output and Prices.}  Survey of Current Business, 72, 32-48.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chgeoyoung(sugar, start="2019-01", end="2019-04",base="2018-12")
#' \donttest{chgeoyoung(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chgeoyoung <-
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
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  dates <- seq.Date(from = start, to = end, by = "month")
  dates <- format(dates, format = "%Y-%m")
  f <-
  function (i)
  return (geoyoung(data, start = dates[i], end = dates[i + 1], base))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained Lowe price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Lowe price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the Lowe price index formula (as character) limited to the year and month, e.g. "2020-01".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chlowe
#' @return The function returns a value (or vector of values) of the monthly chained Lowe price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).  
#' @references
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chlowe(sugar, start="2019-01", end="2019-04",base="2018-12")
#' \donttest{chlowe(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chlowe <-
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
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  dates <- seq.Date(from = start, to = end, by = "month")
  dates <- format(dates, format = "%Y-%m")
  f <-
  function (i)
  return (lowe(data, start = dates[i], end = dates[i + 1], base))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained geometric Lowe price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained geometric Lowe price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the geometric Lowe price index formula (as character) limited to the year and month, e.g. "2020-01"
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chgeolowe
#' @return The function returns a value (or vector of values) of the monthly chained geometric Lowe price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).  
#' @references
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chgeolowe(sugar, start="2019-01", end="2019-04",base="2018-12")
#' \donttest{chgeolowe(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chgeolowe <-
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
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  dates <- seq.Date(from = start, to = end, by = "month")
  dates <- format(dates, format = "%Y-%m")
  f <-
  function (i)
  return (geolowe(data, start = dates[i], end = dates[i + 1], base))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the the monthly chained hybrid price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained hybrid price index. The hybrid index was proposed by Bialek (2020) and it uses correlation coefficients between prices and quantities.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the hybrid price index formula (as character) limited to the year and month, e.g. "2020-01"
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chhybrid
#' @return The function returns a value (or vector of values) of the monthly chained hybrid price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Bialek, J. (2020). \emph{Proposition of a Hybrid Price Index Formula for the Consumer Price Index Measurement}. Equilibrium. Quarterly Journal of Economics and Economic Policy, 15(4), 697-716.}
#'
#' @examples 
#' \donttest{chhybrid(sugar, start="2019-12", end="2020-05", base="2018-12")}
#' \donttest{chhybrid(milk, start="2019-12", end="2020-08", base="2018-12", interval=TRUE)}
#' @export

chhybrid <-
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
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  dates <- seq.Date(from = start, to = end, by = "month")
  dates <- format(dates, format = "%Y-%m")
  f <-
  function (i)
  return (hybrid(data, start = dates[i], end = dates[i + 1], base))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }
  
#' @title  Calculating the the monthly chained geohybrid price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained geohybrid price index. The geohybrid index was proposed by Bialek (2020) and it uses correlation coefficients between prices and quantities.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the geohybrid price index formula (as character) limited to the year and month, e.g. "2020-01"
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chgeohybrid
#' @return The function returns a value (or vector of values) of the monthly chained geohybrid price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Bialek, J. (2020). \emph{Proposition of a Hybrid Price Index Formula for the Consumer Price Index Measurement}. Equilibrium. Quarterly Journal of Economics and Economic Policy, 15(4), 697-716.}
#'
#' @examples 
#' \donttest{chgeohybrid(sugar, start="2019-12", end="2020-05", base="2018-12")}
#' \donttest{chgeohybrid(milk, start="2019-12", end="2020-08", base="2018-12", interval=TRUE)}
#' @export
chgeohybrid <-
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
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  dates <- seq.Date(from = start, to = end, by = "month")
  dates <- format(dates, format = "%Y-%m")
  f <-
  function (i)
  return (geohybrid(data, start = dates[i], end = dates[i + 1], base))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained quadratic mean of order r quantity index
#'
#' @description This function returns a value (or vector of values) of the monthly chained quadratic mean of order r quantity index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter.  
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chQMq
#' @return The function returns a value (or vector of values) of the monthly chained quadratic mean of order r quantity index - see CPI Manual (2004), Section 17.35, formula 17.30 (page 321).
#' @references
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' \donttest{chQMq(sugar, start="2019-01", end="2020-01")}
#' \donttest{chQMq(sugar, start="2019-01", end="2020-01", r=1.3, interval=TRUE)}
#' @export

chQMq <-
  function(data, start, end, r = 2, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (QMq(data, start = dates[i], end = dates[i + 1], r = r))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained quadratic mean of order r price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained quadratic mean of order r price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter.  
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chQMp
#' @return The function returns a value (or vector of values) of the monthly chained quadratic mean of order r price index - see CPI Manual (2004), Section 17.40, formula 17.35 (page 321).
#' @references
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' \donttest{chQMp(sugar, start="2019-01", end="2020-01")}
#' \donttest{chQMp(sugar, start="2019-01", end="2020-01", r=1.3, interval=TRUE)}
#' @export

chQMp <-
  function(data, start, end, r = 2, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (QMp(data, start = dates[i], end = dates[i + 1], r = r))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }

#' @title  Calculating the monthly chained implicit quadratic mean of order r price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained implicit quadratic mean of order r price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter.  
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chIQMp
#' @return The function returns a value (or vector of values) of the monthly chained implicit quadratic mean of order r price index - see CPI Manual (2004), Section 17.37, formula 17.32 (page 321).
#' @references
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' \donttest{chIQMp(sugar, start="2019-01", end="2020-01")}
#' \donttest{chIQMp(sugar, start="2019-01", end="2020-01", r=1.3, interval=TRUE)}
#' @export

chIQMp <-
  function(data, start, end, r = 2, interval = FALSE)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- c()
  while (start <= end)
  {
  t <- substr(start, 0, 7)
  dates <- c(dates, t)
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  f <-
  function (i)
  return (IQMp(data, start = dates[i], end = dates[i + 1], r = r))
  ind <- seq(1:(length(dates) - 1))
  chained1 <- sapply(ind, f)
  chained <- prod(chained1)
  if (interval == TRUE) {
  #optional returning all fixed base chain indices
  chained <- c(1)
  for (i in 1:length(chained1))
  chained <- c(chained, prod(chained1[seq(1, i)]))
  }
  return(chained)
  }