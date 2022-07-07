

#' Filtering where only two months are compared
#' This function returns a filtered data set, i.e. a reduced user's data frame with the same columns and rows limited by a criterion defined by filters
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param filters A vector of filter names (options are: \code{extremeprices}, \code{dumpprices} and/or \code{lowsales}). 
#' @param plimits A two-dimensional vector of thresholds  for minimum and maximum price change (it works if one of the chosen filters is \code{extremeprices} filter). 
#' @param pquantiles A two-dimensional vector of quantile levels for minimum and maximum price change (it works if one of the chosen filters is \code{extremeprices} filter).
#' @param dplimits A two-dimensional vector of thresholds for maximum price drop and maximum ependiture drop (it works if one of the chosen filters is \code{dumpprices} filter). 
#' @param lambda The lambda parameter for \code{lowsales} filter (see \code{References} below).
#' @noRd

filtering <-
  function(data,
  start,
  end,
  filters = c(),
  plimits = c(),
  pquantiles = c(),
  dplimits = c(),
  lambda = 1.25)
  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  start <- as.Date(start)
  start2 <- start
  end <- paste(end, "-01", sep = "")
  end <- as.Date(end)
  end2 <- end
  lubridate::day(start2) <- lubridate::days_in_month(start2)
  lubridate::day(end2) <- lubridate::days_in_month(end2)
  data <-
  dplyr::filter(data,
  (data$time >= start &
  data$time <= start2) | (data$time >= end & data$time <= end2))
  filter1 <- "extremeprices"
  filter2 <- "lowsales"
  filter3 <- "dumpprices"
  afilters <- c(filter1, filter2, filter3)
  if ((start == end) | length(filters) == 0)
  return (data)
  else if (length(base::intersect(filters, afilters)) == 0)
  stop("there are no such filters")
  if (length(base::setdiff(filters, base::intersect(filters, afilters))) >
  0)
  stop("At least one filter's name is wrong")
  data1 <- data[0:0, ]
  data2 <- data[0:0, ]
  data3 <- data[0:0, ]
  data4 <- data[0:0, ]
  if (filter1 %in% filters) {
  if ((length(pquantiles) + length(plimits)) == 0)
  data1 <- data
  else {
  id <- matched(data, start, end)
  priceshares <-
  c()
  for (i in 1:length(id))
  priceshares <-
  c(
  priceshares,
  price(data, period = end, ID = id[i]) / price(data, period = start, ID =
  id[i])
  )
  }
  if (length(pquantiles) >
  0)
  {
  tresh <- c(0, 1)
  if ((pquantiles[1] ==
  tresh[1]) & (pquantiles[2] == tresh[2])) {
  data1 <- data
  }
  else {
  qq <- stats::quantile(priceshares, probs = pquantiles, names = FALSE)
  #selecting the sample by checking condition
  id1 <- c()
  for (i in 1:length(id))
  if ((priceshares[i] >=
  qq[1]) & (priceshares[i] <= qq[2]))
  id1 <- c(id1, id[i])
  data1 <-
  dplyr::filter(data, data$prodID %in% id1)
  }
  } else
  data1 <- data
  if (length(plimits) >
  0)
  {
  #selecting the sample by chacking condition
  id2 <- c()
  for (i in 1:length(id))
  if ((priceshares[i] >= plimits[1]) &
  (priceshares[i] <= plimits[2]))
  id2 <- c(id2, id[i])
  data2 <-
  dplyr::filter(data, data$prodID %in% id2)
  } else
  data2 <- data
  } else
  {
  data1 <- data
  data2 <- data
  }
  if (filter2 %in% filters) {
  if (lambda <= 0)
  data3 <- data
  id <-
  matched(data, start, end)
  expenditures_start <-
  sales(data, period = start, set = id)
  expenditures_end <-
  sales(data, period = end, set = id)
  sum_start <-
  sum(expenditures_start)
  sum_end <-
  sum(expenditures_end)
  id3 <- c()
  for (i in 1:length(id))
  if (0.5 * ((expenditures_start[i] /
  sum_start) + (expenditures_end[i] / sum_end)) > (1 / (length(id) * lambda)))
  id3 <-
  c(id3, id[i])
  data3 <-
  dplyr::filter(data, data$prodID %in% id3)
  } else
  data3 <- data
  
  if (filter3 %in% filters) {
  if (!(length(dplimits) == 2))
  data4 <- data
  else {
  id <- matched(data, start, end)
  expenditures_start <-
  sales(data, period = start, set = id)
  expenditures_end <-
  sales(data, period = end, set = id)
  priceshares <-
  c()
  for (i in 1:length(id))
  priceshares <-
  c(
  priceshares,
  price(data, period = end, ID = id[i]) / price(data, period = start, ID =
  id[i])
  )
  id4 <- c()
  for (i in 1:length(id))
  if ((priceshares[i] >=
  dplimits[1]) |
  ((expenditures_end[i] / expenditures_start[i]) >= dplimits[2]))
  id4 <-
  c(id4, id[i])
  data4 <-
  dplyr::filter(data, data$prodID %in% id4)
  }
  } else
  data4 <- data
  data_final <- dplyr::intersect(data1, data2)
  data_final <- dplyr::intersect(data_final, data3)
  data_final <- dplyr::intersect(data_final, data4)
  return (data_final)
  }
  
#' Filtering where each subsequent months from the considered time interval are compared
#' This function returns a filtered data set, i.e. a reduced user's data frame with the same columns and rows limited by a criterion defined by filters
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param filters A vector of filter names (options are: \code{extremeprices}, \code{dumpprices} and/or \code{lowsales}). 
#' @param plimits A two-dimensional vector of thresholds  for minimum and maximum price change (it works if one of the chosen filters is \code{extremeprices} filter). 
#' @param pquantiles A two-dimensional vector of quantile levels for minimum and maximum price change (it works if one of the chosen filters is \code{extremeprices} filter).
#' @param dplimits A two-dimensional vector of thresholds for maximum price drop and maximum ependiture drop (it works if one of the chosen filters is \code{dumpprices} filter). 
#' @param lambda The lambda parameter for \code{lowsales} filter (see \code{References} below).
#' @noRd

filtering_interval <-
  function(data,
  start,
  end,
  filters = c(),
  plimits = c(),
  pquantiles = c(),
  dplimits = c(),
  lambda = 1.25)
  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  start <- as.Date(start)
  end <- paste(end, "-01", sep = "")
  end <- as.Date(end)
  start2 <- as.Date(start)
  if (start == end) {
  data <-
  dplyr::filter(
  data,
  (lubridate::year(data$time) == lubridate::year(start)) &
  (lubridate::month(data$time) == lubridate::month(start))
  )
  return (data)
  }
  lubridate::month(start2) <- lubridate::month(start2) + 1
  data_set <- data[0:0, ]
  while (start < end)
  {
  d <-
  filtering(data,
  start,
  start2,
  filters,
  plimits,
  pquantiles,
  dplimits,
  lambda)
  data_set <- dplyr::union(data_set, d)
  lubridate::month(start) <- lubridate::month(start) + 1
  lubridate::month(start2) <- lubridate::month(start2) + 1
  }
  return (data_set)
  }
  
#' The function returns the quantity of a given product which was sold in a given period. 
#' @param data The user's data frame. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{quantities} (as positive numeric) and \code{prodID} (as numeric or character) with unique product IDs. 
#' @param period The time period (as character) limited to the year and month, e.g. "2019-03".
#' @param ID The ID of unique product which is used for determining the quantity 
#' @noRd

quantity <- function(data, period, ID)
  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  period <- paste(period, "-01", sep = "")
  period <- as.Date(period)
  lubridate::day(period) <- 1
  period2 <- period
  lubridate::day(period2) <-
  lubridate::days_in_month(period2)
  data <- dplyr::filter(data, data$prodID == ID)
  data <-
  dplyr::filter(data, data$time >= period & data$time <= period2)
  if (nrow(data) == 0)
  stop("There are no data in selected period")
  return(sum(data$quantities))
  }
  
#' The function returns the price of a given product which was sold in a given period. 
#' @param data The user's data frame. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{quantities} (as positive numeric) and \code{prodID} (as numeric or character) with unique product IDs. 
#' @param period The time period (as character) limited to the year and month, e.g. "2019-03".
#' @param ID The ID of unique product which is used for determining the price 
#' @noRd

price <- function(data, period, ID)
  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  period <- paste(period, "-01", sep = "")
  period <- as.Date(period)
  lubridate::day(period) <- 1
  period2 <- period
  lubridate::day(period2) <-
  lubridate::days_in_month(period2)
  data <- dplyr::filter(data, data$prodID == ID)
  data <-
  dplyr::filter(data, data$time >= period & data$time <= period2)
  if (nrow(data) == 0)
  stop("There are no data in selected period")
  data$sales <- data$prices * data$quantities
  return(sum(data$sales) / sum(data$quantities))
  }
  
#' The function returns the logarithmic mean of two numbers. 
#' @param x A real positive number
#' @param y A real positive number
#' @noRd
  
#logarithmic means
  L <- function (x, y) {
  if (x == y)
  return (x)
  else
  return ((y - x) / log(y / x))
  }
  
  LL <- function (x) {
  if (x[1] == x[2])
  return (x[1])
  else
  return ((x[1] - x[2]) / log(x[1] / x[2]))
  }
  
#' An additional function used in the 'geks_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd
  
geks_fbmw2 <- function(data, start, end)  {
if (start == end)
return (1)
if (nrow(data) == 0)
stop("A data frame is empty")
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
wstart <- end
lubridate::year(wstart) <-
lubridate::year(wstart) - 1
#checking conditions
if (start > end)
stop("parameters must satisfy: start<=end")
if (lubridate::month(start) < 12)
stop("a month of the 'start' parameter must be December")
if (start == end)
return (1)
else
return (geks(
data,
substr(start, 0, 7),
substr(end, 0, 7),
substr(wstart, 0, 7),
window = 13
))
}

#' An additional function used in the 'wgeks_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd
  
wgeks_fbmw2 <- function(data, start, end)  {
if (start == end)
return (1)
if (nrow(data) == 0)
stop("A data frame is empty")
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
wstart <- end
lubridate::year(wstart) <-
lubridate::year(wstart) - 1
#checking conditions
if (start > end)
stop("parameters must satisfy: start<=end")
if (lubridate::month(start) < 12)
stop("a month of the 'start' parameter must be December")
if (start == end)
return (1)
else
return (wgeks(
data,
substr(start, 0, 7),
substr(end, 0, 7),
substr(wstart, 0, 7),
window = 13
))
}

#' An additional function used in the 'geksw_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

geksw_fbmw2 <- function(data, start, end)  {
if (start == end)
return (1)
if (nrow(data) == 0)
stop("A data frame is empty")
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
wstart <- end
lubridate::year(wstart) <-
lubridate::year(wstart) - 1
#checking conditions
if (start > end)
stop("parameters must satisfy: start<=end")
if (lubridate::month(start) < 12)
stop("a month of the 'start' parameter must be December")
if (start == end)
return (1)
else
return (geksw(
data,
substr(start, 0, 7),
substr(end, 0, 7),
substr(wstart, 0, 7),
window = 13
))
}  

#' An additional function used in the 'geksj_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

geksj_fbmw2 <- function(data, start, end)  {
if (start == end)
return (1)
if (nrow(data) == 0)
stop("A data frame is empty")
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
wstart <- end
lubridate::year(wstart) <-
lubridate::year(wstart) - 1
#checking conditions
if (start > end)
stop("parameters must satisfy: start<=end")
if (lubridate::month(start) < 12)
stop("a month of the 'start' parameter must be December")
if (start == end)
return (1)
else
return (geksj(
data,
substr(start, 0, 7),
substr(end, 0, 7),
substr(wstart, 0, 7),
window = 13
))
}

#' An additional function used in the 'ccdi_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

ccdi_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (ccdi(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}

#' An additional function used in the 'gk' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

gkreal <- function(data, start, end)  {
if (start == end)
return (1)
if (nrow(data) == 0)
stop("A data frame is empty")
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
lubridate::day(end)<-lubridate::days_in_month(end)
#checking conditions
if (start > end)
stop("parameters must satisfy: start<=end")
if (lubridate::month(start) < 12)
stop("a month of the 'start' parameter must be December")
#data filtration
d <-
dplyr::filter(data, data$time >= start & data$time <= end)
prodID <- unique(d$prodID)
#main body
#initial values of indices
index1 <- c()
index2 <- c()
#set of dates
dates <- c()
dates <- seq.Date(from = start, to = end, by = "month")
dates <- format(dates, format = "%Y-%m")
index1<-replicate(length(dates),1)
index2<-replicate(length(dates),2)
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
0.01)
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
return (QU(d, substr(start, 0, 7), tt, v))
ind <- sapply(dates, indd)
index2 <- index1
index1 <- ind
}
result <-
index1[which(dates == substr(end, 0, 7))]
result <- result[[1]]
return (result)
}

#' An additional function used in the 'geksl_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

geksl_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (geksl(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}

#' An additional function used in the 'geksgl_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

geksgl_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (geksgl(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}


#' An additional function used in the 'gk_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

gk_fbmw2 <- function(data, start, end) {
if (start == end)
return (1)
if (nrow(data) == 0)
stop("A data frame is empty")
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
wstart <- end
lubridate::year(wstart) <-
lubridate::year(wstart) - 1
#checking conditions
if (start > end)
stop("parameters must satisfy: start<=end")
if (lubridate::month(start) < 12)
stop("a month of the 'start' parameter must be December")
if (start == end)
return (1)
else
return (gk(
data,
substr(start, 0, 7),
substr(end, 0, 7),
substr(wstart, 0, 7),
window = 13
))
}

#' The function returns a distance between two dates (in months) - it is not exported
#' @param data1 The first date (as Date type) written in the format with a year, month and day, e.g. "2020-04-03".
#' @param data2 The second date (as Date type) written in the format with a year, month and day, e.g. "2020-04-03".
#' @noRd

dist <- function(data1, data2)
{
n <- 0
while (data1 <= data2)
{
n <- n + 1
lubridate::month(data1) <- lubridate::month(data1) + 1
}
return (n - 1)
}

#' An additional function used in the 'tpd_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

tpd_fbmw2 <- function(data, start, end)  {
if (start == end)
return (1)
if (nrow(data) == 0)
stop("A data frame is empty")
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
wstart <- end
lubridate::year(wstart) <-
lubridate::year(wstart) - 1
#checking conditions
if (start > end)
stop("parameters must satisfy: start<=end")
if (lubridate::month(start) < 12)
stop("A month of the 'start' parameter must be December")
if (start == end)
return (1)
else
return (tpd(
data,
substr(start, 0, 7),
substr(end, 0, 7),
substr(wstart, 0, 7),
window = 13
))
}

#' An additional function used in the 'wgeksl_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

wgeksl_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (wgeksl(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}

#' An additional function used in the 'wgeksgl_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

wgeksgl_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (wgeksgl(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}


#' An additional function used in the 'unit' function
#' @param string A string which contains the grammage of the product and its unit
#' @noRd

numextract <- function(string) {
  unlist(regmatches(string, gregexpr(
  "[[:digit:]]+\\.*[[:digit:]]*", string
  )))
} 

#' An additional function used in the 'data_unit' function
#' @param string A string which contains the grammage of the product and its unit
#' @param units Units of products which are to be detected
#' @param multiplication A sign of the multiplication used in product descriptions
#' @param space A maximum space between the product grammage and its unit 
#' @noRd

unit <-
  function (string,
  units = c("g", "ml", "kg", "l"),
  multiplication = "x",
  space = 1)
  {
  detect <- FALSE
  string <- tolower(stringr::str_replace_all(string, ',', '.'))
  units <- tolower(units)
  numbers <- n <- pattern <- text <- sizes <- unit <- grammage <- NULL
  numbers <- as.numeric(numextract(string))
  if (length(numbers) == 0)
  return (list("1", "item"))
  #recalculating expressions with a sign of the product
  n <- length(numbers)
  if (stringr::str_detect(string, multiplication) &
  length(numbers) > 1)
  {
  nn <- n - 1
  for (i in 1:nn)  {
  patt <-
  paste(as.character(numbers[i]),
  multiplication,
  as.character(numbers[i + 1]),
  sep = "")
  if (stringr::str_detect(string, patt)) {
  string <-
  stringr::str_replace(string, patt,  as.character(as.numeric(numbers[i]) * as.numeric(numbers[i +
  1])))
  detect <- TRUE
  }
  if (detect == TRUE) {
  numbers <- numextract(string)
  n <- length(numbers)
  }
  }
  }
  #main body
  #initial values (no unit detected)
  unit <- "item"
  grammage <- 1
  #checking for units
  unit_list <- c()
  
  for (i in 1:n) {
  text <-
  strex::str_after_first(string, pattern = as.character(numbers[i]))
  string<-text
  for (k in 1:length(units)) {
  sizes <- nchar(units[k]) + space
  text2 <- substr(text, 0, sizes)
  if (!(is.na(stringr::str_detect(text2, units[k])))) 
    if (stringr::str_detect(text2, units[k])) {
  unit_list <- c(unit_list, units[k])
  grammage <- numbers[i]
  }
  }
  }
  if (length(unit_list) > 0)
  unit <-
  unit_list[max(which(nchar(unit_list) == max(nchar(unit_list))))]
  return (list(grammage, unit))
  }

#' An additional function used in the 'geksl' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

nl <-
  function(data, start, end)  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
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
  price_start <-
  prices(data, period = start, set = id)
  price_end <-
  prices(data, period = end, set = id)
  quantity_start <-
  quantities(data, period = start, set = id)
  return(sum(quantity_start * price_end)/sum(quantity_start * price_start))
  }

#' An additional function used in the 'geksaqu' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param v The data frame which contains quality adjusted factors.
#' @noRd

aqu <-
  function(data, start, end, v=data.frame())  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
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
  quantity_start <-
  quantities(data, period = start, set = id)
  factors<-c()
  for (ID in id) factors<-c(factors,v[v$prodID==ID,]$value)
  return(sum(quantity_start * price_end) / sum(factors*quantity_start))
  }

#' An additional function used in the 'geksaqu_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

geksaqu_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (geksaqu(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}

#' An additional function used in the 'wgeksaqu_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

wgeksaqu_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (wgeksaqu(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}

#' An additional function used in the 'geksaqi' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param v The data frame which contains quality adjusted factors.
#' @noRd

aqi <-
  function(data, start, end, v=data.frame())  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
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
  price_start <-
  prices(data, period = start, set = id)
  price_end <-
  prices(data, period = end, set = id)
  quantity_start <-
  quantities(data, period = start, set = id)
  factors<-c()
  for (ID in id) factors<-c(factors,v[v$prodID==ID,]$value)
  return(sum(factors*quantity_start * price_end/price_start) / sum(factors*quantity_start))
  }

#' An additional function used in the 'geksaqi_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

geksaqi_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (geksaqi(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}

#' An additional function used in the 'wgeksaqi_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

wgeksaqi_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (wgeksaqi(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}


#' An additional function used in the 'geksgaqi' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param v The data frame which contains quality adjusted factors.
#' @noRd

gaqi <-
  function(data, start, end, v=data.frame())  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
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
  price_start <-
  prices(data, period = start, set = id)
  price_end <-
  prices(data, period = end, set = id)
  quantity_start <-
  quantities(data, period = start, set = id)
  factors<-c()
  for (ID in id) factors<-c(factors,v[v$prodID==ID,]$value)
  coef<-c()
  coef<-factors*quantity_start/sum(factors*quantity_start)
  return(prod((price_end/price_start)^coef))
  }

#' An additional function used in the 'geksgaqi_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

geksgaqi_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (geksgaqi(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}

#' An additional function used in the 'wgeksgaqi_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

wgeksgaqi_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (wgeksgaqi(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}

#' An additional function used in the 'model_classification' function
#' @param str A character string which is transformed into unique integer number.
#' @noRd

conversion<-function (str)
{
  utf<-utf8ToInt(str)
  int<-seq(1:nchar(str))
  return (sum(utf*int))
}

