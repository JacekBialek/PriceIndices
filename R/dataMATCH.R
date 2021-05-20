#' An artificial scanner data set for product matching
#'
#' A collection of scanner data on the sale of sample artificial products.  
#' @format A data frame with 7 columns and 30 rows. The used variables are as follows:
#'
#' \code{time} - Dates of transactions (Year-Month-Day)
#'
#' \code{prices} - Prices of sold products [PLN]
#'
#' \code{quantities} - Quantities of sold products [liters]
#'
#' \code{codeIN} - Unique internal (retailer) product codes (data set contains 5 different codeINs)
#'
#' \code{codeOUT} - Unique external product codes (data set contains 5 different codeOUTs)
#'
#' \code{retID} - Unique codes identifying outlets/retailer sale points (data set contains 2 different retIDs)
#'
#' \code{description} Descriptions of sold products (data set contains 3 different product descriptions)
#' @docType data
#'
"dataMATCH"