#' An artificial scanner data set containing 10 elementary product groups
#'
#' A collection of artificial scanner data on the sale of tomatoes, fruit juices, low fat milk, full fat milk, sugar, chocolate, yoghurt, coffee, eggs and salt in the period from December 2018 to October 2020
#' @format A data frame with 8 columns and 96600 rows (some rows are not complete). The used variables are as follows:
#'
#' \code{time} - Dates of transactions (Year-Month-Day)
#'
#' \code{prices} - Prices of sold products [EUR]
#'
#' \code{quantities} - Quantities of sold products [unit defined in the 'unit' column]
#'
#' \code{prodID} - Retailer product codes
#'
#' \code{retID} - Unique codes identifying outlets/retailer sale points (10 retIDs)
#'
#' \code{description} Descriptions of sold products 
#'
#' \code{unit} Sales units, e.g.: kg, ml, etc.
#'
#' \code{coicop} Identifiers of COICOP groups (10 groups)
#'
#' @docType data
#'
"dataCOICOP"