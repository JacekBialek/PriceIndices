#' @title  Calculating the Bennet price and quantity indicators
#'
#' @description This function returns the Bennet price and quantity indicators and optionally also the price and quantity contributions of individual products.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical parameter indicating whether calculations are to be made for the whole time interval (TRUE) or no (FALSE).
#' @param matched A logical parameter indicating whether the matched sample approach is to be used (if yes, the parameter has the value TRUE).
#' @param contributions A logical parameter indicating whether contributions of individual products are to be displayed. If it is \code{TRUE}, then contributions are calculated for the the base period \code{start} and the current period \code{end}.
#' @param prec A numeric vector indicating precision, i.e. the number of decimal places for presenting results.
#' @rdname bennet
#' @return This function returns the Bennet price and quantity indicators and optionally also the price and quantity contributions of individual products.
#' @references
#' {Bennet, T. L., (1920). \emph{The Theory of Measurement of Changes in Cost of Living}. Journal of the Royal Statistical Society, 83, 455-462.}
#' @examples
#' bennet(milk, "2018-12", "2019-12", matched = TRUE, contributions = TRUE)
#' \donttest{
#' bennet(coffee, start = "2018-12", end = "2019-03", interval = TRUE)
#' }
#' @export

bennet <-
  function(
      data,
      start,
      end,
      interval = FALSE,
      matched = FALSE,
      contributions = FALSE,
      prec = 2) {
    if (matched == FALSE) {
      df <- bennet_internal(
        data = data,
        start = start,
        end = end,
        interval = interval,
        contributions = contributions
      )
      if (contributions == FALSE) {
        df$Value_difference <- round(df$Value_difference, prec)
        df$Price_indicator <- round(df$Price_indicator, prec)
        df$Quantity_indicator <- round(df$Quantity_indicator, prec)
      } else {
        df$value_differences <- round(df$value_differences, prec)
        df$price_contributions <- round(df$price_contributions, prec)
        df$quantity_contributions <- round(df$quantity_contributions, prec)
      }
    } else {
      df <- bennet_matched_internal(
        data = data,
        start = start,
        end = end,
        interval = interval,
        contributions = contributions
      )
      if (contributions == FALSE) {
        df$Value_difference <- round(df$Value_difference, prec)
        df$Price_indicator <- round(df$Price_indicator, prec)
        df$Quantity_indicator <- round(df$Quantity_indicator, prec)
      } else {
        df$value_differences <- round(df$value_differences, prec)
        df$price_contributions <- round(df$price_contributions, prec)
        df$quantity_contributions <- round(df$quantity_contributions, prec)
      }
    }
    return(df)
  }


#' @title  Calculating the Montgomery price and quantity indicators
#'
#' @description This function returns the Montgomery price and quantity indicators and optionally also the price and quantity contributions of individual products.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical parameter indicating whether calculations are to be made for the whole time interval (TRUE) or no (FALSE).
#' @param matched A logical parameter indicating whether the matched sample approach is to be used (if yes, the parameter has the value TRUE).
#' @param contributions A logical parameter indicating whether contributions of individual products are to be displayed. If it is \code{TRUE}, then contributions are calculated for the the base period \code{start} and the current period \code{end}.
#' @param prec A numeric vector indicating precision, i.e. the number of decimal places for presenting results.
#' @rdname montgomery
#' @return This function returns the Montgomery price and quantity indicators and optionally also the price and quantity contributions of individual products.
#' @references
#' {Montgomery, J. K., (1929). \emph{Is There a Theoretically Correct Price Index of a Group of Commodities?} Rome, International Institute of Agriculture}
#' @examples
#' montgomery(milk, "2018-12", "2019-12", matched = TRUE, contributions = TRUE)
#' \donttest{
#' montgomery(coffee, start = "2018-12", end = "2019-03", interval = TRUE)
#' }
#' @export

montgomery <-
  function(
      data,
      start,
      end,
      interval = FALSE,
      matched = FALSE,
      contributions = FALSE,
      prec = 2) {
    if (matched == FALSE) {
      df <- montgomery_internal(
        data = data,
        start = start,
        end = end,
        interval = interval,
        contributions = contributions
      )
      if (contributions == FALSE) {
        df$Value_difference <- round(df$Value_difference, prec)
        df$Price_indicator <- round(df$Price_indicator, prec)
        df$Quantity_indicator <- round(df$Quantity_indicator, prec)
      } else {
        df$value_differences <- round(df$value_differences, prec)
        df$price_contributions <- round(df$price_contributions, prec)
        df$quantity_contributions <- round(df$quantity_contributions, prec)
      }
    } else {
      df <- montgomery_matched_internal(
        data = data,
        start = start,
        end = end,
        interval = interval,
        contributions = contributions
      )
      if (contributions == FALSE) {
        df$Value_difference <- round(df$Value_difference, prec)
        df$Price_indicator <- round(df$Price_indicator, prec)
        df$Quantity_indicator <- round(df$Quantity_indicator, prec)
      } else {
        df$value_differences <- round(df$value_differences, prec)
        df$price_contributions <- round(df$price_contributions, prec)
        df$quantity_contributions <- round(df$quantity_contributions, prec)
      }
    }
    return(df)
  }
