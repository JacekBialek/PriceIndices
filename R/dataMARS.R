#' An artificial scanner data set for testing the MARS method
#'
#' An artificial scanner data set of shirt sales.  
#' @format A data frame with 9 columns and 44 rows. The used variables are as follows:
#'
#' \code{time} - Dates of transactions (Year-Month-Day)
#'
#' \code{prices} - Prices of sold products [PLN]
#'
#' \code{quantities} - Quantities of sold products [liters]
#'
#' \code{prodID} - Unique product identifiers (data set contains 28 different prodIDs)
#'
#' \code{description} - Descriptions (labels) of sold shirts (data set contains 12 different descriptions)
#'
#' \code{brand} - Brand of sold shirts (data set contains 2 different brands: X and Y)
#'
#' \code{gender} - Gender of the person for whom the shirt is dedicated (M or F)
#'
#' \code{size} - Size of shirts (M, L, and XL)
#'
#' \code{fabric} - Fabric of shirts (cotton, polyester, blend)
#'
#' @docType data
#'
"dataMARS"