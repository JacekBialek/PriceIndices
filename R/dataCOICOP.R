#' A real scanner data set for the product classification 
#'
#' A collection of real scanner data on the sale of milk products sold in a period: Dec, 2020 - Feb, 2022.
#' @format A data frame with 10 columns and 139600 rows. The used variables are as follows:
#'
#' \code{time} - Dates of transactions (Year-Month-Day)
#'
#' \code{prices} - Prices of sold products [PLN]
#'
#' \code{quantities} - Quantities of sold products 
#'
#' \code{description} - Descriptions of sold products (original: in Polish)
#'
#' \code{codeIN} - Retailer product codes
#'
#' \code{retID} - Unique codes identifying outlets/retailer sale points
#'
#' \code{grammage} - Product grammages
#'
#' \code{unit} - Sales units, e.g.: kg, ml, etc.
#'
#' \code{category} - Product categories (in English) corresponding to COICOP 6 levels
#'
#' \code{coicop6} - Identifiers of local COICOP 6 groups (6 groups)
#'
#' @docType data
#'
"dataCOICOP"