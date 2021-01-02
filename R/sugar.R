#' Real data set on sold sugar
#'
#' A collection of scanner data on the sale of sugar in one of Polish supermarkets in the period from December 2017 to October 2020
#' @format A data frame with 6 columns and 7666 rows. The used variables are as follows:
#'
#' \code{time} - Dates of transactions (Year-Month-Day)
#'
#' \code{prices} - Prices of sold products [PLN]
#'
#' \code{quantities} - Quantities of sold products [kg]
#'
#' \code{prodID} - Unique product codes (data set contains 11 different prodIDs)
#'
#' \code{retID} - Unique codes identifying outlets/retailer sale points (data set contains 20 different retIDs)
#'
#' \code{description} Descriptions of sold sugar products (data set contains 3 different product descriptions)
#' @docType data
#'
"sugar"