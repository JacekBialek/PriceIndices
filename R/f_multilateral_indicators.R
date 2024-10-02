#' @title  Calculating the multilateral Bennet price and quantity indicators 
#'
#' @description This function returns the multilateral Bennet price and quantity indicators and optionally also the price and quantity contributions of individual products.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The first period of the time window (as character) limited to the year and month, e.g. "2019-12".
#' @param interval A logical parameter indicating whether calculations are to be made for the whole time interval (TRUE) or no (FALSE).
#' @param matched A logical parameter indicating whether the matched sample approach is to be used (if yes, the parameter has the value TRUE).
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param interval A logical parameter indicating whether calculations are to be made for the whole time interval (TRUE) or no (FALSE).
#' @param contributions A logical parameter indicating whether contributions of individual products are to be displayed. If it is \code{TRUE}, then contributions are calculated for the the base period \code{start} and the current period \code{end}.
#' @param prec A numeric vector indicating precision, i.e. the number of decimal places for presenting results.
#' @rdname mbennet
#' @return This function returns the multilateral Bennet price and quantity indicators and optionally also the price and quantity contributions of individual products.    
#' @references
#' {Bennet, T. L., (1920). \emph{The Theory of Measurement of Changes in Cost of Living}. Journal of the Royal Statistical Society, 83, 455-462.}
#'
#' {Fox, K.J., (2006). \emph{A Method for Transitive and Additive Multilateral Comparisons: A Transitive Bennet Indicator}. Journal of Economics, 87(1), 73-87.}
#'
#' {Białek, J. (2024). \emph{The use of the Bennet indicators and their transitive versions for scanner data analysis}. Statistics in Transition new series, 25(3), 155-173.}
#' @examples 
#' \donttest{mbennet(milk, "2018-12", "2019-12", matched=TRUE, contributions=TRUE)}
#' \donttest{mbennet(coffee, start="2018-12", end="2019-03", interval=TRUE)}
#' @export

mbennet <-
  function(data,
  start,
  end,
  wstart=start,
  matched=FALSE,
  window=13,
  interval=FALSE,
  contributions=FALSE,
  prec=2)  {
  if (interval==FALSE) {df<-mbennet_internal(data=data, 
                                              start=start,
                                              end=end,
                                              wstart=wstart,
                                              matched=matched,
                                              window=window,
                                              contributions=contributions)
                      if (contributions==FALSE)
                      {
                      df$Value_difference<-round(df$Value_difference,prec)  
                      df$Price_indicator<-round(df$Price_indicator,prec)
                      df$Quantity_indicator<-round(df$Quantity_indicator,prec)
                      }
                      else
                      {
                      df$value_differences<-round(df$value_differences,prec)
                      df$price_contributions<-round(df$price_contributions,prec)
                      df$quantity_contributions<-round(df$quantity_contributions,prec)
                      }
                      return (df) 
                      }
  else { if (contributions==TRUE) {df<-mbennet_internal(data=data, 
                                              start=start,
                                              end=end,
                                              wstart=wstart,
                                              matched=matched,
                                              window=window,
                                              contributions=TRUE)
                                   df$value_differences<-round(df$value_differences,prec)
                                   df$price_contributions<-round(df$price_contributions,prec)
                                   df$quantity_contributions<-round(df$quantity_contributions,prec)
                                   return (df)
                                  }
    else
    {
    start. <- paste(start, "-01", sep = "")
    end. <- paste(end, "-01", sep = "")
    start. <- as.Date(start.)
    end. <- as.Date(end.)
    dates. <- seq.Date(from = start., to = end., by = "month")
    dates.<-dates.[2:length(dates.)]
    dates.<-substr(dates., 0, 7)
    Value_difference.<-c()
    Price_indicator.<-c()
    Quantity_indicator.<-c()
    for (times in dates.) {
      mb<-mbennet_internal(data=data, 
                          start=start,
                          end=times,
                          wstart=wstart,
                          matched=matched,
                          window=window,
                          contributions=FALSE)
    Value_difference.<-c(Value_difference.,round(mb[1,1], prec))
    Price_indicator.<-c(Price_indicator.,round(mb[1,2],prec))
    Quantity_indicator.<-c(Quantity_indicator.,round(mb[1,3],prec))
    }
    return (data.frame(time=dates.,
                       Value_difference=Value_difference.,
                       Price_indicator=Price_indicator.,
                       Quantity_indicator=Quantity_indicator.))
    }
  } 
  }

#' @title  Calculating the multilateral Montgomery price and quantity indicators 
#'
#' @description This function returns the multilateral Montgomery price and quantity indicators and optionally also the price and quantity contributions of individual products.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The first period of the time window (as character) limited to the year and month, e.g. "2019-12".
#' @param interval A logical parameter indicating whether calculations are to be made for the whole time interval (TRUE) or no (FALSE).
#' @param matched A logical parameter indicating whether the matched sample approach is to be used (if yes, the parameter has the value TRUE).
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param interval A logical parameter indicating whether calculations are to be made for the whole time interval (TRUE) or no (FALSE).
#' @param contributions A logical parameter indicating whether contributions of individual products are to be displayed. If it is \code{TRUE}, then contributions are calculated for the the base period \code{start} and the current period \code{end}.
#' @param prec A numeric vector indicating precision, i.e. the number of decimal places for presenting results.
#' @rdname mmontgomery
#' @return This function returns the multilateral Montgomery price and quantity indicators and optionally also the price and quantity contributions of individual products.    
#' @references
#' {Montgomery, J. K., (1929). \emph{Is There a Theoretically Correct Price Index of a Group of Commodities?} Rome, International Institute of Agriculture}
#'
#' {Fox, K.J., (2006). \emph{A Method for Transitive and Additive Multilateral Comparisons: A Transitive Bennet Indicator}. Journal of Economics, 87(1), 73-87.}
#'
#' {Białek, J., Pawelec, N. (2024). \emph{The use of transitive Montgomery Indicators for scanner data analysis}. Argumenta Oeconomica,  2(53).}
#' @examples 
#' \donttest{mmontgomery(milk, "2018-12", "2019-12", matched=TRUE, contributions=TRUE)}
#' \donttest{mmontgomery(coffee, start="2018-12", end="2019-03", interval=TRUE)}
#' @export

mmontgomery <-
  function(data,
  start,
  end,
  wstart=start,
  matched=FALSE,
  window=13,
  interval=FALSE,
  contributions=FALSE,
  prec=2)  {
  if (interval==FALSE) {df<-mmontgomery_internal(data=data, 
                                              start=start,
                                              end=end,
                                              wstart=wstart,
                                              matched=matched,
                                              window=window,
                                              contributions=contributions)
                      if (contributions==FALSE)
                      {
                      df$Value_difference<-round(df$Value_difference,prec)  
                      df$Price_indicator<-round(df$Price_indicator,prec)
                      df$Quantity_indicator<-round(df$Quantity_indicator,prec)
                      }
                      else
                      {
                      df$value_differences<-round(df$value_differences,prec)
                      df$price_contributions<-round(df$price_contributions,prec)
                      df$quantity_contributions<-round(df$quantity_contributions,prec)
                      }
                      return (df) 
                      }
  else { if (contributions==TRUE) {df<-mmontgomery_internal(data=data, 
                                              start=start,
                                              end=end,
                                              wstart=wstart,
                                              matched=matched,
                                              window=window,
                                              contributions=TRUE)
                                   df$value_differences<-round(df$value_differences,prec)
                                   df$price_contributions<-round(df$price_contributions,prec)
                                   df$quantity_contributions<-round(df$quantity_contributions,prec)
                                   return (df)
                                  }
    else
    {
    start. <- paste(start, "-01", sep = "")
    end. <- paste(end, "-01", sep = "")
    start. <- as.Date(start.)
    end. <- as.Date(end.)
    dates. <- seq.Date(from = start., to = end., by = "month")
    dates.<-dates.[2:length(dates.)]
    dates.<-substr(dates., 0, 7)
    Value_difference.<-c()
    Price_indicator.<-c()
    Quantity_indicator.<-c()
    for (times in dates.) {
      mb<-mmontgomery_internal(data=data, 
                          start=start,
                          end=times,
                          wstart=wstart,
                          matched=matched,
                          window=window,
                          contributions=FALSE)
    Value_difference.<-c(Value_difference.,round(mb[1,1], prec))
    Price_indicator.<-c(Price_indicator.,round(mb[1,2],prec))
    Quantity_indicator.<-c(Quantity_indicator.,round(mb[1,3],prec))
    }
    return (data.frame(time=dates.,
                       Value_difference=Value_difference.,                       
                       Price_indicator=Price_indicator.,
                       Quantity_indicator=Quantity_indicator.))
    }
  } 
  }