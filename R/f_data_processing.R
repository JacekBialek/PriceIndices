#' @title  Preparing a data set for further data processing or price index calculations
#'
#' @description This function returns a prepared data frame based on the user's data set. The resulting data frame is ready for further data processing (such as data selecting, matching or filtering) and it is also ready for price index calculations (if only it contains required columns).
#'
#' @param data The user's data frame to be prepared. The user must indicate columns: \code{time} (as Date or character type, allowed formats are, eg.: `2020-03` or `2020-12-28`), \code{prices} and \code{quantities} (as numeric). Optionally, the user may also indicate columns: \code{prodID}, \code{codeIN}, \code{codeOUT}, \code{retID} (as numeric, factor or character), \code{description} (as character), \code{grammage} (as numeric or character), \code{unit} (as character) and other columns specified by the \code{additional} parameter.
#' @param time A character name of the column which provides transaction dates.
#' @param prices A character name of the column which provides product prices. 
#' @param quantities A character name of the column which provides product quantities.
#' @param prodID  A character name of the column which provides product IDs. The \code{prodID} column should include unique product IDs used for product matching (as numeric or character). It is not obligatory to consider this column while data preparing but it is required while price index calculating (to obtain it, please see \code{\link{data_matching}}). 
#' @param retID A character name of the column which provides outlet IDs (retailer sale points). The \code{retID} column should include unique outlet IDs used for aggregating subindices over outlets. It is not obligatory to consider this column while data preparing but it is required while final price index calculating (to obtain it, please see the \code{\link{final_index}} function).
#' @param description A character name of the column which provides product descriptions. It is not obligatory to consider this column while data preparing but it is required while product selecting (please see the \code{\link{data_selecting}} function).
#' @param codeIN A character name of the column which provides internal product codes (from the retailer). It is not obligatory to consider this column while data preparing but it may be required while product matching (please see the \code{\link{data_matching}} function).
#' @param codeOUT A character name of the column which provides external product codes (e.g. GTIN or SKU). It is not obligatory to consider this column while data preparing but it may be required while product matching (please see the \code{\link{data_matching}} function).
#' @param grammage A character name of the numeric column which provides the grammage of products
#' @param unit A character name of the column which provides the unit of the grammage of products
#' @param additional A character vector of names of additional columns to be considered while data preparing (records with missing values are deleted).
#' @rdname data_preparing
#' @return The resulting data frame is free from missing values, zero or negative prices and quantities. As a result, column \code{time} is set to be Date type (in format: `Year-Month-01`), columns \code{prices} and \code{quantities} are set to be numeric. If the column \code{description} is selected, then it is set to be character type. If columns: \code{prodID}, \code{retID}, \code{codeIN} or  \code{codeOUT} are selected, then they are set to be factor type.
#'
#' @examples 
#' \donttest{data_preparing(milk, time="time",prices="prices",quantities="quantities")}
#' \donttest{data_preparing(dataCOICOP, time="time",
#' prices="prices",quantities="quantities",additional="coicop")}
#' @export

data_preparing <-
  function(data,
  time = NULL,
  prices = NULL,
  quantities = NULL,
  prodID = NULL,
  retID = NULL,
  description = NULL,
  codeIN = NULL,
  codeOUT = NULL,
  grammage = NULL,
  unit = NULL,
  additional = c())
  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  variables <- c()
  cn <- colnames(data)
  #checking obligatory columns
  if ((length(time) == 0) |
  (length(prices) == 0) |
  (length(quantities) == 0))
  stop ("Columns: time, prices and quantities must be specified!")
  if (!(time %in% cn))
  stop ("Bad specification of the 'time' column!")
  colnames(data)[which(names(data) == time)] <- "time"
  data$time <- as.character(data$time)
  #checking if there is a format "Year-Month". If yes it is transformed to "Year-Month-01" (with 'Day')
  if (nchar(data$time[1]) == 7)
  data$time <- paste(data$time, "-01", sep = "")
  data$time <- as.Date(data$time)
  variables <- c(variables, "time")
  
  if (!(prices %in% cn))
  stop ("Bad specification of the 'prices' column!")
  colnames(data)[which(names(data) == prices)] <- "prices"
  if (!(is.numeric(data$prices)))
  data$prices <- as.numeric(data$prices)
  variables <- c(variables, "prices")
  
  if (!(quantities %in% cn))
  stop ("Bad specification of the 'quantities' column!")
  colnames(data)[which(names(data) == quantities)] <- "quantities"
  if (!(is.numeric(data$quantities)))
  data$quantities <- as.numeric(data$quantities)
  variables <- c(variables, "quantities")
  
  #checking additional columns
  if (length(prodID) > 0) {
  if (!(prodID %in% cn))
  stop ("Bad specification of the 'prodID' column!")
  colnames(data)[which(names(data) == prodID)] <- "prodID"
  if (!(is.factor(data$prodID)))
  data$prodID <- as.factor(data$prodID)
  variables <- c(variables, "prodID")
  }
  if (length(retID) > 0) {
  if (!(retID %in% cn))
  stop ("Bad specification of the 'retID' column!")
  colnames(data)[which(names(data) == retID)] <- "retID"
  if (!(is.factor(data$retID)))
  data$retID <- as.factor(data$retID)
  variables <- c(variables, "retID")
  }
  if (length(description) > 0) {
  if (!(description %in% cn))
  stop ("Bad specification of the 'description' column!")
  colnames(data)[which(names(data) == description)] <-
  "description"
  if (!(is.character(data$description)))
  data$description <- as.character(data$description)
  variables <- c(variables, "description")
  }
  if (length(codeIN) > 0) {
  if (!(codeIN %in% cn))
  stop ("Bad specification of the 'codeIN' column!")
  colnames(data)[which(names(data) == codeIN)] <- "codeIN"
  if (!(is.factor(data$codeIN)))
  data$codeIN <- as.factor(data$codeIN)
  variables <- c(variables, "codeIN")
  }
  if (length(codeOUT) > 0) {
  if (!(codeOUT %in% cn))
  stop ("Bad specification of the 'codeOUT' column!")
  colnames(data)[which(names(data) == codeOUT)] <- "codeOUT"
  if (!(is.factor(data$codeOUT)))
  data$codeOUT <- as.factor(data$codeOUT)
  variables <- c(variables, "codeOUT")
  }
  if (length(grammage) > 0) {
  if (!(grammage %in% cn))
  stop ("Bad specification of the 'grammage' column!")
  colnames(data)[which(names(data) == grammage)] <- "grammage"
  if (!(is.character(data$grammage)))
  data$grammage <- as.character(data$grammage)
  variables <- c(variables, "grammage")
  }
  if (length(unit) > 0) {
  if (!(unit %in% cn))
  stop ("Bad specification of the 'unit' column!")
  colnames(data)[which(names(data) == unit)] <- "unit"
  if (!(is.character(data$unit)))
  data$unit <- as.character(data$unit)
  variables <- c(variables, "unit")
  }
  variables <- c(variables, additional)
  data <- dplyr::select(data, variables)
  #filtering
  data <- stats::na.omit(data)
  data <- dplyr::filter(data, data$prices > 0 & data$quantities > 0)
  return(data)
  }
  

#' @title  Matching products 
#'
#' @description This function returns a data set defined in the first parameter (\code{data}) with an additional column (\code{prodID}). Two products are treated as being matched if they have the same \code{prodID} value.  
#' @param data The user's data frame with information about products to be matched. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01') and at least one of the following columns: \code{codeIN} (as numeric, factor or character), \code{codeOUT} (as numeric, factor or character) and \code{description} (as character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the matching process concerns only two periods defined by \code{start} and \code{end} parameters (then the \code{interval} is set to FALSE) or whether that function is to match products sold during the whole time interval <start, end>. 
#' @param variables The optional parameter describing the vector of additional column names. Values of these additional columns must be identical for matched products. 
#' @param codeIN A logical value, e.g. if there are retailer (internal) product codes (as numeric or character) written in \code{codeIN} column and there is a need to use that column while data matching, then that parameter should be set to TRUE. Otherwise it is set to FALSE.
#' @param codeOUT A logical value, e.g. if there are external product codes, such as GTIN or SKU (as numeric or character) written in \code{codeOUT} column and there is a need to use that column while data preparing then, that parameter should be set to TRUE. Otherwise it is set to FALSE.
#' @param  description A logical value, e.g. if there are product labels (as character) written in \code{description} column and there is a need to use that column while data preparing, then that parameter should be set to TRUE. Otherwise it is set to FALSE.
#' @param  onlydescription A logical value indicating whether products with identical labels (described in the \code{description}) are to be matched.
#' @param precision A threshold value for the Jaro-Winkler similarity measure when comparing labels (its value must belong to the interval [0,1]). Two labels are treated as similar enough if their Jaro-Winkler similarity exceeds the \code{precision} value. 
#' @rdname data_matching
#' @return This function returns a data set defined in the first parameter (\code{data}) with an additional column (\code{prodID}). Two products are treated as being matched if they have the same \code{prodID} value. The procedure of generating the above-mentioned additional column depends on the set of chosen columns for matching. In most extreme case, when the \code{onlydescription} parameter value is TRUE, two products are also matched if they have identical descriptions. Other cases are as follows: \code{Case 1}: Parameters \code{codeIN}, \code{codeOUT} and \code{description} are set to TRUE. Products with two identical codes or one of the codes identical and an identical \code{description} are automatically matched. Products are also matched if they have identical one of codes and the Jaro-Winkler similarity of their descriptions is bigger than the \code{precision} value.\code{Case 2}: Only one of the parameters: \code{codeIN} or \code{codeOUT} are set to TRUE and also the \code{description} parameter is set to TRUE. Products with an identical chosen code and an identical description are automatically matched. In the second stage, products are also matched if they have an identical chosen code and the Jaro-Winkler similarity of their descriptions is bigger than the \code{precision} value. \code{Case 3}: Parameters \code{codeIN} and \code{codeOUT} are set to TRUE and the parameter \code{description} is set to FALSE. In this case, products are matched if they have both codes identical. \code{Case 4}: Only the parameter \code{description} is set to TRUE. This case requires the \code{onlydescription} parameter to be TRUE and then the matching process is based only on product labels (two products are matched if they have identical descriptions). \code{Case 5}:  Only one of the parameters: \code{codeIN} or \code{codeOUT} are set to TRUE and the \code{description} parameter is set to FALSE. In this case, the only reasonable option is to return the \code{prodID} column which is identical with the chosen code column. Please note that if the set of column names defined in the \code{variables} parameter is not empty, then the values of these additional columns must be identical while product matching.
#' @examples 
#' data_matching(dataMATCH, start="2018-12",end="2019-02",onlydescription=TRUE,interval=TRUE)
#' \donttest{data_matching(dataMATCH, start="2018-12",end="2019-02",precision=0.98, interval=TRUE)}
#' 
#' @export

data_matching <-
  function(data,
  start,
  end,
  interval = FALSE,
  variables = c(),
  codeIN = TRUE,
  codeOUT = TRUE,
  description = TRUE,
  onlydescription = FALSE,
  precision = 0.95)
  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  #checking condition for 'precision'
  if ((precision < 0) |
  (precision > 1))
  stop("parametr 'precision' must belong to [0,1]")
  prodID<-NULL
  #preparing data set
  columns <- c()
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  lubridate::day(end) <- lubridate::days_in_month(end)
  if (interval == TRUE)
  data <- dplyr::filter(data, data$time >= start & data$time <= end)
  else
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
  data <- stats::na.omit(data)
  #original dataset
  data_oryginal <- data
  if (description == TRUE) {
  data$description <- as.character(data$description)
  data$descriptionID <- data$description
  columns <- c(columns, "descriptionID")
  }
  #reducing a dataset
  if (codeIN == TRUE)
  columns <- c(columns, "codeIN")
  if (codeOUT == TRUE)
  columns <- c(columns, "codeOUT")
  if (description == TRUE)
  columns <- c(columns, "description")
  if (length(variables) > 0)
  columns <- c(columns, variables)
  if (length(columns) == 0)
  stop("At least one column for matching must be selected!")
  data <- dplyr::select(data, columns)
  data <- dplyr::distinct(data)
  #main body
  if (codeIN == TRUE & codeOUT == TRUE & description == TRUE)
  {
  if (length(variables)>0) pairs <- reclin2::pair_blocking(data, on = variables,  deduplication = TRUE)
  else pairs <- reclin2::pair(data, deduplication = TRUE)
  pairs <- reclin2::compare_pairs(pairs, on = "descriptionID")
  pairs <-
  reclin2::compare_pairs(pairs,
  on = "description",
  default_comparator = reclin2::jaro_winkler())
  pairs <- reclin2::compare_pairs(pairs, on = "codeOUT")
  pairs <- reclin2::compare_pairs(pairs, on = "codeIN")
  pairs$simsum <-
  pairs$descriptionID * pairs$codeOUT + pairs$descriptionID * pairs$codeIN +
  pairs$codeOUT * pairs$codeIN + pairs$description * pairs$codeOUT + pairs$description *
  pairs$codeIN + onlydescription * pairs$descriptionID
  pairs <-
  reclin2::select_threshold(pairs, threshold=precision, score = "simsum", variable = "select")
  pairs <-
  reclin2::deduplicate_equivalence(pairs, selection = "select", variable = "prodID")
  pairs$descriptionID <- NULL
  }
  else if (codeIN == TRUE & codeOUT == FALSE & description == TRUE)
  {
  if (length(variables)>0) pairs <- reclin2::pair_blocking(data, on = variables, deduplication = TRUE)
  else pairs <- reclin2::pair(data, deduplication = TRUE)
  pairs <- reclin2::compare_pairs(pairs, on = "descriptionID")
  pairs <-
  reclin2::compare_pairs(pairs,
  on = "description",
  default_comparator = reclin2::jaro_winkler())
  pairs <- reclin2::compare_pairs(pairs, on = "codeIN")
  pairs$simsum <-
  pairs$descriptionID * pairs$codeIN + pairs$description * pairs$codeIN +
  onlydescription * pairs$descriptionID
  pairs <-
  reclin2::select_threshold(pairs, threshold=precision, score = "simsum", variable = "select")
  pairs <-
  reclin2::deduplicate_equivalence(pairs, selection = "select", variable = "prodID")
  pairs$descriptionID <- NULL
  }
  else if (codeIN == FALSE & codeOUT == TRUE & description == TRUE)
  {
  if (length(variables)>0) pairs <- reclin2::pair_blocking(data, on = variables, deduplication = TRUE)
  else pairs <- reclin2::pair(data, deduplication = TRUE)
  pairs <- reclin2::compare_pairs(pairs, on = "descriptionID")
  pairs <-
  reclin2::compare_pairs(pairs,
  on = "description",
  default_comparator = reclin2::jaro_winkler())
  pairs <- reclin2::compare_pairs(pairs, on = "codeOUT")
  pairs$simsum <-
  pairs$descriptionID * pairs$codeOUT + pairs$description * pairs$codeOUT +
  onlydescription * pairs$descriptionID
  pairs <-
  reclin2::select_threshold(pairs, threshold=precision, score = "simsum", variable = "select")
  pairs <-
  reclin2::deduplicate_equivalence(pairs, selection = "select", variable = "prodID")
  pairs$descriptionID <- NULL
  }
  else if (codeIN == TRUE & codeOUT == TRUE & description == FALSE)
  {
  if (length(variables)>0) pairs <- reclin2::pair_blocking(data, on = variables, deduplication = TRUE)
  else pairs <- reclin2::pair(data, deduplication = TRUE)
  pairs <- reclin2::compare_pairs(pairs, on = "codeIN")
  pairs <- reclin2::compare_pairs(pairs, on = "codeOUT")
  pairs$simsum <- (pairs$codeIN * pairs$codeOUT)
  pairs <-
  reclin2::select_threshold(pairs, 0.5, score = "simsum", variable = "select")
  pairs <-
  reclin2::deduplicate_equivalence(pairs, selection = "select", variable = "prodID")
  }
  else if (codeIN == FALSE & codeOUT == FALSE & description == TRUE)
  {
  if (onlydescription == TRUE)
  {
  if (length(variables)>0) pairs <- reclin2::pair_blocking(data, on = variables, deduplication = TRUE)
  else pairs <- reclin2::pair(data, deduplication = TRUE)
  pairs <- reclin2::compare_pairs(pairs, on = "descriptionID")
  pairs$simsum <- pairs$descriptionID
  pairs <-
  reclin2::select_threshold(pairs, 0.5, score = "simsum", variable = "select")
  pairs <-
  reclin2::deduplicate_equivalence(pairs, selection = "select", variable = "prodID")
  pairs$descriptionID <- NULL
  }
  else
  stop("Parametr 'onlydescription' must be TRUE to start matching process")
  }
  else if (codeIN == TRUE &
  codeOUT == FALSE & description == FALSE) {
  pairs <- data
  pairs$prodID <-
  pairs$codeIN
  }
  else if (codeIN == FALSE &
  codeOUT == TRUE & description == FALSE) {
  pairs <- data
  pairs$prodID <-
  pairs$codeOUT
  }
  else if (codeIN == FALSE &
  codeOUT == FALSE &
  description == FALSE)
  stop("at least one of parameters: codeIN, codeOUT or description must be TRUE")
  #pairs - new dataframe with reduced dataframe with matched products (additional column:   prodID)
  #now, let us back to the oryginal dataset, i.e. 'data_oryginal'
  #names of columns which are considered in matching process
  pairs<-data.frame(pairs)
  columns <- colnames(dplyr::select(pairs,-prodID))
  #setting a pattern
  value_pattern<-pairs[,"prodID"]
  vector_pattern<-as.character(pairs[,columns[1]])
  if (length(columns)>1) for (i in 1:length(columns)) vector_pattern<-paste(vector_pattern,  as.character(pairs[,columns[i]]),sep="")
  #matching
  vector_test<-as.character(data_oryginal[,columns[1]])
  if (length(columns)>1) for (i in 1:length(columns)) vector_test<-paste(vector_test,as.character(data_oryginal[,columns[i]]),sep="")
  f<-function (word) value_pattern[which(vector_pattern==word)]
  data_oryginal$prodID<-sapply(vector_test,f)
  return (data_oryginal)
  }

#' @title  Filtering a data set for further price index calculations
#'
#' @description This function returns a filtered data set, i.e. a reduced user's data frame with the same columns and rows limited by a criterion defined by \code{filters}.
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param filters A vector of filter names (options are: \code{extremeprices}, \code{dumpprices} and/or \code{lowsales}). 
#' @param plimits A two-dimensional vector of thresholds  for minimum and maximum price change (it works if one of the chosen filters is \code{extremeprices} filter). 
#' @param pquantiles A two-dimensional vector of quantile levels for minimum and maximum price change (it works if one of the chosen filters is \code{extremeprices} filter).
#' @param dplimits A two-dimensional vector of thresholds for maximum price drop and maximum ependiture drop (it works if one of the chosen filters is \code{dumpprices} filter). 
#' @param lambda The lambda parameter for \code{lowsales} filter (see \code{References} below).
#' @param interval A logical value indicating whether the filtering process concerns only two periods defined by \code{start} and \code{end} parameters (then the \code{interval} is set to FALSE) or whether that function is to filter products sold during the whole time interval <start, end>, i.e. any subsequent months are compared. 
#' @param retailers A logical parameter indicating whether filtering should be done for each outlet (\code{retID}) separately. If it is set to FALSE, then there is no need to consider the \code{retID} column.
#' @rdname data_filtering
#' @return This function returns a filtered data set (a reduced user's data frame). If the set of \code{filters} is empty, then the function returns the original data frame (defined by the \code{data} parameter) limited to considered months. On the other hand, if all filters are chosen, i.e. \code{filters=c(extremeprices,dumpprices,lowsales)}, then these filters work independently and a summary result is returned. Please note that both variants of \code{extremeprices} filter can be chosen at the same time, i.e. \code{plimits} and \code{pquantiles}, and they work also independently.
#' @references
#' {Van Loon, K., Roels, D. (2018) \emph{Integrating big data in Belgian CPI}. Meeting of the Group of Experts on Consumer Price Indices, Geneva.}
#' @examples 
#' \donttest{data_filtering(milk,start="2018-12",end="2019-03",
#' filters=c("extremeprices"),pquantiles=c(0.01,0.99),interval=TRUE)}
#' \donttest{data_filtering(milk,start="2018-12",end="2019-03",
#' filters=c("extremeprices","lowsales"), plimits=c(0.25,2))}
#' @export

 data_filtering <-
    function(data,
    start,
    end,
    filters = c(),
    plimits = c(),
    pquantiles = c(),
    dplimits = c(),
    lambda = 1.25,
    interval = FALSE,
    retailers = FALSE)
    {
    if (nrow(data) == 0)
    stop("A data frame is empty")
    if (retailers == FALSE) {
    if (interval == FALSE)
    return (filtering(
    data,
    start,
    end,
    filters,
    plimits,
    pquantiles,
    dplimits,
    lambda
    ))
    else
    return (
    filtering_interval(
    data,
    start,
    end,
    filters,
    plimits,
    pquantiles,
    dplimits,
    lambda
    )
    )
    }
    else {
    if (interval == FALSE) {
    ret <-
    matched(
    data,
    period1 = start,
    period2 = end,
    type = "retID",
    interval = FALSE
    )
    data_set <- data[0:0, ]
    for (i in (1:length(ret))) {
    rs <- dplyr::filter(data, data$retID == ret[i])
    d <-
    filtering(rs,
    start,
    end,
    filters,
    plimits,
    pquantiles,
    dplimits,
    lambda)
    data_set <-
    dplyr::union(data_set, d)
    }
    }
    else {
    ret <-
    matched(
    data,
    period1 = start,
    period2 = end,
    type = "retID",
    interval = TRUE
    )
    data_set <- data[0:0, ]
    for (i in (1:length(ret))) {
    rs <- dplyr::filter(data, data$retID == ret[i])
    d <-
    filtering_interval(rs,
    start,
    end,
    filters,
    plimits,
    pquantiles,
    dplimits,
    lambda)
    data_set <-
    dplyr::union(data_set, d)
    }
    }
    return (data_set)
    }
    }

#' @title  Selecting products from the user's data set for further price index calculations
#'
#' @description The function returns a subset of the user's data set obtained by selection based on keywords and phrases.
#' @param data The user's data frame. It must contain a column \code{description} (as character).
#' @param include A vector consisting of words and phrases. The function reduces the data set to one in which the \code{description} column contains any of these values.
#' @param must A vector consisting of words and phrases. The function reduces the data set to one in which the \code{description} column contains each of these values.
#' @param exclude A vector consisting of words and phrases. The function reduces the data set to one in which the \code{description} column does not contain any of these values.
#' @param sensitivity A logical parameter indicating whether sensitivity to lowercase and uppercase letters is taken into consideration (if yes, its value is TRUE). 
#' @param coicop An optional parameter indicating a value for an additional column \code{coicop} which is added to the resulting data frame
#' @rdname data_selecting
#' @return The function returns a subset of the user's data set obtained by selection based on keywords and phrases defined by parameters: \code{include}, \code{must} and \code{exclude} (an additional column \code{coicop} is optional). Providing values of these parameters, please remember that the procedure distinguishes between uppercase and lowercase letters only when \code{sensitivity} is set to TRUE.
#' @examples 
#' data_selecting(milk, include=c("milk"), must=c("UHT"))
#' data_selecting(milk, must=c("milk"), exclude=c("paust"))
#' @export

 data_selecting <-
    function(data,
    include = c(),
    must = c(),
    exclude = c(),
    sensitivity = FALSE,
    coicop = NULL)
    {
    if (nrow(data) == 0)
    stop("A data frame is empty")
    if (sensitivity == FALSE)
    data$description <- tolower(data$description)
    if (length(must) == 0)
    set3 <- data
    else
    {
    if (sensitivity == FALSE)
    must <- tolower(must)
    set3 <-
    dplyr::filter(data, stringr::str_detect(data$description, must[1]))
    if (length(must) > 1)
    for (i in 2:length(must))
    set3 <-
    dplyr::intersect(set3, dplyr::filter(data,     stringr::str_detect(data$description, must[i])))
    }
    if (length(include) == 0)
    set1 <- data
    else
    {
    if (sensitivity == FALSE)
    include <- tolower(include)
    set1 <-
    dplyr::filter(data, stringr::str_detect(data$description, include[1]))
    if (length(include) > 1)
    for (i in 2:length(include))
    set1 <-
    dplyr::union(set1, dplyr::filter(data, stringr::str_detect(data$description, include[i])))
    }
    if (length(exclude) == 0)
    set <- set1
    else
    {
    if (sensitivity == FALSE)
    exclude <- tolower(exclude)
    set2 <-
    dplyr::filter(data, stringr::str_detect(data$description, exclude[1]))
    if (length(exclude) > 1)
    for (i in 2:length(exclude))
    set2 <-
    dplyr::union(set2, dplyr::filter(data, stringr::str_detect(data$description, exclude[i])))
    set <- dplyr::setdiff(set1, set2)
    }
    new_set <- dplyr::intersect(set, set3)
    if (length(coicop) > 0)
    new_set$coicop <- coicop
    return (new_set)
    }
    

#' @title  Providing values from the indicated column that occur simultaneously in the compared periods or in a given time interval. 
#'
#' @description The function returns all values from the indicated column (defined by the \code{type} parameter) which occur simultaneously in the compared periods or in a given time interval.
#' @param data The user's data frame. It must contain a column \code{time} (as Date in format: year-month-day, e.g. '2020-12-01') and also a column indicated by the \code{type} parameter.  
#' @param period1 The first period (as character) limited to the year and month, e.g. "2019-03".
#' @param period2 The second period (as character) limited to the year and month, e.g. "2019-04".
#' @param type This parameters defines the column which is used in the procedure. Possible values of the \code{type} parameter are: \code{retID}, \code{prodID}, \code{codeIN}, \code{codeOUT} or \code{description}.
#' @param interval A logical parameter indicating whether the procedure is to work for the whole time period between \code{period1} and \code{period2} (then it is TRUE).
#' @rdname matched
#' @return The function returns all values from the indicated column (defined by the \code{type} parameter) which occur simultaneously in the compared periods or in a given time interval. Possible values of the \code{type} parameter are: \code{retID}, \code{prodID}, \code{codeIN}, \code{codeOUT} or \code{description}. If the \code{interval} parameter is set to FALSE, then the function compares only periods defined by \code{period1} and \code{period2}. Otherwise the whole time period between \code{period1} and \code{period2} is considered.
#' @examples 
#' matched(milk, period1="2018-12", period2="2019-12", interval=TRUE)
#' matched(milk, period1="2018-12", period2="2019-12", type="description")
#' @export

  matched <-
    function(data,
    period1,
    period2,
    type = "prodID",
    interval = FALSE) {
    atype <-
    c("retID", "prodID", "codeIN", "codeOUT", "description") #allowed values for 'type' parameter
    if (!(type %in% atype))
    stop ("The 'type' parameter has a wrong value")
    if (nrow(data) == 0)
    stop("A data frame is empty")
    period1 <- paste(period1, "-01", sep = "")
    period1 <- as.Date(period1)
    period2 <- paste(period2, "-01", sep = "")
    period2 <- as.Date(period2)
    #main body
    if (type == "prodID")
    {
    if (interval == FALSE)
    set <-
    base::intersect(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$prodID,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$prodID
    )
    else
    {
    set <-
    base::intersect(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$prodID,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$prodID
    )
    start <- min(period1, period2)
    end <- max(period1, period2)
    while (start < end)
    {
    start2 <- start
    lubridate::month(start2) <-
    lubridate::month(start2) + 1
    set <-
    base::intersect(set,
    base::intersect(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    )$prodID,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start2) &
    lubridate::month(data$time) == lubridate::month(start2)
    )$prodID
    ))
    lubridate::month(start) <-
    lubridate::month(start) + 1
    }
    }
    }
    if (type == "retID")
    {
    if (interval == FALSE)
    set <-
    base::intersect(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$retID,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$retID
    )
    else
    {
    set <-
    base::intersect(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$retID,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$retID
    )
    start <- min(period1, period2)
    end <- max(period1, period2)
    while (start < end)
    {
    start2 <- start
    lubridate::month(start2) <-
    lubridate::month(start2) + 1
    set <-
    base::intersect(set,
    base::intersect(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    )$retID,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start2) &
    lubridate::month(data$time) == lubridate::month(start2)
    )$retID
    ))
    lubridate::month(start) <-
    lubridate::month(start) + 1
    }
    }
    }
    if (type == "codeIN")
    {
    if (interval == FALSE)
    set <-
    base::intersect(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$codeIN,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$codeIN
    )
    else
    {
    set <-
    base::intersect(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$codeIN,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$codeIN
    )
    start <- min(period1, period2)
    end <- max(period1, period2)
    while (start < end)
    {
    start2 <- start
    lubridate::month(start2) <-
    lubridate::month(start2) + 1
    set <-
    base::intersect(set,
    base::intersect(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    )$codeIN,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start2) &
    lubridate::month(data$time) == lubridate::month(start2)
    )$codeIN
    ))
    lubridate::month(start) <-
    lubridate::month(start) + 1
    }
    }
    }
    if (type == "codeOUT")
    {
    if (interval == FALSE)
    set <-
    base::intersect(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$codeOUT,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$codeOUT
    )
    else
    {
    set <-
    base::intersect(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$codeOUT,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$codeOUT
    )
    start <- min(period1, period2)
    end <- max(period1, period2)
    while (start < end)
    {
    start2 <- start
    lubridate::month(start2) <-
    lubridate::month(start2) + 1
    set <-
    base::intersect(set,
    base::intersect(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    )$codeOUT,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start2) &
    lubridate::month(data$time) == lubridate::month(start2)
    )$codeOUT
    ))
    lubridate::month(start) <-
    lubridate::month(start) + 1
    }
    }
    }
    if (type == "description")
    {
    if (interval == FALSE)
    set <-
    base::intersect(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$description,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$description
    )
    else
    {
    set <-
    base::intersect(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$description,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$description
    )
    start <- min(period1, period2)
    end <- max(period1, period2)
    while (start < end)
    {
    start2 <- start
    lubridate::month(start2) <-
    lubridate::month(start2) + 1
    set <-
    base::intersect(
    set,
    base::intersect(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    )$description,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start2) &
    lubridate::month(data$time) == lubridate::month(start2)
    )$description
    )
    )
    lubridate::month(start) <-
    lubridate::month(start) + 1
    }
    }
    }
    return(set)
    }
    
#' @title  Providing values from the indicated column that occur at least once in one of the compared periods or in a given time interval 
#'
#' @description The function returns all values from the indicated column (defined by the \code{type} parameter) which occur at least once in one of the compared periods or in a given time interval.
#' @param data The user's data frame. It must contain a column \code{time} (as Date in format: year-month-day,e.g. '2020-12-01') and also a column indicated by the \code{type} parameter.  
#' @param period1 The first period (as character) limited to the year and month, e.g. "2019-03".
#' @param period2 The second period (as character) limited to the year and month, e.g. "2019-04".
#' @param type This parameters defines the column which is used in the procedure. Possible values of the \code{type} parameter are: \code{retID}, \code{prodID}, \code{codeIN}, \code{codeOUT} or \code{description}.
#' @param interval A logical parameter indicating whether the procedure is to work for the whole time period between \code{period1} and \code{period2} (then it is TRUE).
#' @rdname available
#' @return The function returns all values from the indicated column (defined by the \code{type} parameter) which occur at least once in one of the compared periods or in a given time interval. Possible values of the \code{type} parameter are: \code{retID}, \code{prodID}, \code{codeIN}, \code{codeOUT} or \code{description}. If the \code{interval} parameter is set to FALSE, then the function compares only periods defined by \code{period1} and \code{period2}. Otherwise the whole time period between \code{period1} and \code{period2} is considered.
#' @examples 
#' available(milk, period1="2018-12", period2="2019-12", interval=TRUE)
#' available(milk, period1="2018-12", period2="2019-12", type="description")
#' @export

  available <-
    function(data,
    period1,
    period2,
    type = "prodID",
    interval = FALSE) {
    atype <-
    c("retID", "prodID", "codeIN", "codeOUT", "description") #allowed values for 'type' parameter
    if (!(type %in% atype))
    stop ("The 'type' parameter has a wrong value")
    if (nrow(data) == 0)
    stop("A data frame is empty")
    period1 <- paste(period1, "-01", sep = "")
    period1 <- as.Date(period1)
    period2 <- paste(period2, "-01", sep = "")
    period2 <- as.Date(period2)
    #main body
    if (type == "prodID")
    {
    if (interval == FALSE)
    set <-
    base::union(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$prodID,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$prodID
    )
    else
    {
    set <-
    base::union(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$prodID,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$prodID
    )
    start <- min(period1, period2)
    end <- max(period1, period2)
    while (start < end)
    {
    start2 <- start
    lubridate::month(start2) <-
    lubridate::month(start2) + 1
    set <-
    base::union(set,
    base::union(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    )$prodID,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start2) &
    lubridate::month(data$time) == lubridate::month(start2)
    )$prodID
    ))
    lubridate::month(start) <-
    lubridate::month(start) + 1
    }
    }
    }
    if (type == "retID")
    {
    if (interval == FALSE)
    set <-
    base::union(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$retID,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$retID
    )
    else
    {
    set <-
    base::union(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$retID,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$retID
    )
    start <- min(period1, period2)
    end <- max(period1, period2)
    while (start < end)
    {
    start2 <- start
    lubridate::month(start2) <-
    lubridate::month(start2) + 1
    set <-
    base::union(set,
    base::union(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    )$retID,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start2) &
    lubridate::month(data$time) == lubridate::month(start2)
    )$retID
    ))
    lubridate::month(start) <-
    lubridate::month(start) + 1
    }
    }
    }
    if (type == "codeIN")
    {
    if (interval == FALSE)
    set <-
    base::union(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$codeIN,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$codeIN
    )
    else
    {
    set <-
    base::union(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$codeIN,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$codeIN
    )
    start <- min(period1, period2)
    end <- max(period1, period2)
    while (start < end)
    {
    start2 <- start
    lubridate::month(start2) <-
    lubridate::month(start2) + 1
    set <-
    base::union(set,
    base::union(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    )$codeIN,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start2) &
    lubridate::month(data$time) == lubridate::month(start2)
    )$codeIN
    ))
    lubridate::month(start) <-
    lubridate::month(start) + 1
    }
    }
    }
    if (type == "codeOUT")
    {
    if (interval == FALSE)
    set <-
    base::union(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$codeOUT,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$codeOUT
    )
    else
    {
    set <-
    base::union(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$codeOUT,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$codeOUT
    )
    start <- min(period1, period2)
    end <- max(period1, period2)
    while (start < end)
    {
    start2 <- start
    lubridate::month(start2) <-
    lubridate::month(start2) + 1
    set <-
    base::union(set,
    base::union(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    )$codeOUT,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start2) &
    lubridate::month(data$time) == lubridate::month(start2)
    )$codeOUT
    ))
    lubridate::month(start) <-
    lubridate::month(start) + 1
    }
    }
    }
    if (type == "description")
    {
    if (interval == FALSE)
    set <-
    base::union(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$description,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$description
    )
    else
    {
    set <-
    base::union(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period1) &
    lubridate::month(data$time) == lubridate::month(period1)
    )$description,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(period2) &
    lubridate::month(data$time) == lubridate::month(period2)
    )$description
    )
    start <- min(period1, period2)
    end <- max(period1, period2)
    while (start < end)
    {
    start2 <- start
    lubridate::month(start2) <-
    lubridate::month(start2) + 1
    set <-
    base::union(set,
    base::union(
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start) &
    lubridate::month(data$time) == lubridate::month(start)
    )$description,
    dplyr::filter(
    data,
    lubridate::year(data$time) == lubridate::year(start2) &
    lubridate::month(data$time) == lubridate::month(start2)
    )$description
    ))
    lubridate::month(start) <-
    lubridate::month(start) + 1
    }
    }
    }
    return(set)
    }
    
#' @title Providing the ratio of number of matched values from the indicated column to the number of all available values from this column   
#' 
#' @description The function returns a ratio of number of values from the indicated column that occur simultaneously in the compared periods or in a given time interval to the number of all available values from the above-mentioned column (defined by the \code{type} parameter) at the same time.
#' @param data The user's data frame. It must contain a column \code{time} (as Date in format: year-month-day,e.g. '2020-12-01') and also a column indicated by the \code{type} parameter.  
#' @param period1 The first period (as character) limited to the year and month, e.g. "2019-03".
#' @param period2 The second period (as character) limited to the year and month, e.g. "2019-04".
#' @param type This parameter defines the column which is used in the procedure. Possible values of the \code{type} parameter are: \code{retID}, \code{prodID}, \code{codeIN}, \code{codeOUT} or \code{description}.
#' @param interval A logical parameter indicating whether the procedure is to work for the whole time period between \code{period1} and \code{period2} (then it is TRUE).
#' @rdname matched_index
#' @return The function returns a ratio of number of values from the indicated column that occur simultaneously in the compared periods or in a given time interval to the number of all available values from the above-mentioned column (defined by the \code{type} parameter) at the same time. Possible values of the \code{type} parameter are: \code{retID}, \code{prodID} or \code{description}. If the \code{interval} parameter is set to FALSE, then the function compares only periods defined by \code{period1} and \code{period2}. Otherwise the whole time period between \code{period1} and \code{period2} is considered. The returned value belongs to [0,1].
#' @examples 
#' \donttest{matched_index(milk, period1="2018-12", period2="2019-12", interval=TRUE)}
#' matched_index(milk, period1="2018-12", period2="2019-12", type="retID")
#' @export

  matched_index <-
    function(data,
    period1,
    period2,
    type = "prodID",
    interval = FALSE) {
    atype <-
    c("retID", "prodID", "codeIN", "codeOUT", "description") #allowed values for 'type' parameter
    if (!(type %in% atype))
    stop ("The 'type' parameter has a wrong value")
    if (nrow(data) == 0)
    stop("A data frame is empty")
    a <-
    length(matched(data, period1, period2, type, interval))
    b <-
    length(available(data, period1, period2, type, interval))
    return (a / b)
    }

#' @title Providing a time dependent matched_index() function 
#' 
#' @description The function provides a data frame or a figure presenting the \code{\link{matched_index}} function calculated for the column defined by the \code{type} parameter and for each month from the considered time interval
#' @param data The user's data frame. It must contain a column \code{time} (as Date in format: year-month-day,e.g. '2020-12-01') and also a column indicated by the \code{type} parameter.  
#' @param start The beginning of a time interval (as character) limited to the year and month, e.g. "2019-03".
#' @param end The end of a time interval (as character) limited to the year and month, e.g. "2019-04".
#' @param base The base period (as character) for product comparisons. Its possible values are: "start" and "end".
#' @param type This parameter defines the column which is used in the procedure. Possible values of the \code{type} parameter are: \code{retID}, \code{prodID}, \code{codeIN}, \code{codeOUT} or \code{description}.
#' @param fixedbase A logical parameter indicating whether the procedure is to work for subsequent months from the considered time interval (\code{fixedbase}=FALSE). Otherwise the period defined by \code{base} plays a role of fixed base month (\code{fixedbase}=TRUE)
#' @param figure A logical parameter indicating whether the function returns a figure (TRUE) or a data frame (FALSE) with \code{\link{matched_index}} values.
#' @param date_breaks A string giving the distance between breaks on the X axis like "1 month" (default value) or "4 months".
#' @rdname matched_fig
#' @return The function returns a data frame or a figure presenting the \code{\link{matched_index}} function calculated for the column defined by the \code{type} parameter and for each month from the considered time interval. The interval is set by \code{start} and \code{end} parameters. The returned object (data frame or figure) depends on the value of \code{figure} parameter. The returned values belong to [0,1].
#' @examples 
#' \donttest{matched_fig(milk, start="2018-12", end="2019-12")}
#' \donttest{matched_fig(milk, start="2018-12", end="2019-12", figure=FALSE)}
#' @export

  matched_fig <-
    function (data,
    start,
    end,
    base="start",
    type = "prodID",
    fixedbase = TRUE,
    figure = TRUE,
    date_breaks = "1 month")
    {
    date <- fraction <- NULL
    atype <-
    c("retID", "prodID", "codeIN", "codeOUT", "description") #allowed values for 'type' parameter
    if (!(type %in% atype))
    stop ("The 'type' parameter has a wrong value")
    abase<-c("start","end")
    if (!(base %in% abase))
    stop ("The 'base' parameter has a wrong value")
    if (nrow(data) == 0)
    stop("A data frame is empty")
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    times <- c()
    if (base=="start") t0 <- substr(start, 0, 7)
    else t0 <- substr(end, 0, 7)
    if (fixedbase == TRUE) 
    {
    times <- c(substr(start, 0, 7))
    if (base=="start") values <- c(1)
    else values<-values<-c(matched_index(
    data,
    period1 = substr(start, 0, 7),
    period2 = substr(end, 0, 7),
    type,
    interval = TRUE
    ))
    }
    else
    {
    times<-c()
    values<-c()
    }
    while (start < end)
    {
    t1 <- substr(start, 0, 7)
    lubridate::month(start) <- lubridate::month(start) + 1
    t2 <- substr(start, 0, 7)
    times <- c(times, t2)
    if (fixedbase == FALSE)
    values <-
    c(values,
    matched_index(
    data,
    period1 = t1,
    period2 = t2,
    type,
    interval = FALSE
    ))
    else
    values <-
    c(values,
    matched_index(
    data,
    period1 = t0,
    period2 = t2,
    type,
    interval = TRUE
    ))
    }
    tab <- data.frame(c(times), c(values))
    colnames(tab) <- c("date", "fraction")
    if (figure == FALSE)
    return (tab)
    #returning a figure which is based on 'tab'
    else
    {
    tab$date <- as.Date(paste(tab$date, "01", sep = "-"))
    ggplot2::ggplot(tab, ggplot2::aes(x = date, y = fraction)) + ggplot2::geom_point() +
    ggplot2::geom_line() + ggplot2::labs(x = "date", y = "fraction") + ggplot2::scale_x_date(date_labels =
    "%Y %m", date_breaks  = date_breaks) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle =
    45, hjust = 1))
    }
    }

#' @title  Providing prices (unit values) of sold products
#'
#' @description The function returns prices (unit values) of sold products with given IDs. 
#' @param data The user's data frame. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities} (as positive numeric) and \code{prodID} (as numeric, factor or character) with unique product IDs. 
#' @param period The time period (as character) limited to the year and month, e.g. "2019-03".
#' @param set The set of unique product IDs to be used for determining prices of sold products (see also \code{\link{data_matching}}). If the \code{set} is empty, the function returns prices of all products being available in \code{period}.
#' @param ID A logical parameter indicating whether a data frame with prodIDs and prices (unit values) should be returned.
#' @rdname prices
#' @return The function analyzes the user's data frame and returns prices (unit value) of products with given \code{ID} and being sold in the time period indicated by the \code{period} parameter. Please note, that the function returns the price values for sorted prodIDs and in the absence of a given prodID in the data set, the function returns nothing (it does not return zero). If the ID parameter is set to TRUE then the function returns a data frame with columns: \code{by} (IDs of products) and \code{uv} (unit values of products).
#' @examples 
#' \donttest{prices(milk, period="2019-06")}
#' prices(milk, period="2019-12", set=c(400032, 82919), ID=TRUE)
#' @export

  prices <- function(data, period, set = c(), ID = FALSE)
  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  prodID<-NULL
  period <-
  paste(period, "-01", sep = "")
  period <- as.Date(period)
  data<-dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(period) &
  lubridate::month(data$time) == lubridate::month(period)
  ))
  if (nrow(data) == 0)
  stop("There are no data in selected period")
  if (length(set) > 0) {data<-dplyr::filter(data, prodID %in% set)
  if (nrow(data) == 0)
  stop("There are no data in selected period")
  }
  data<-dplyr::summarise(dplyr::group_by(data, by=prodID), uv=sum(prices*quantities)/sum(quantities), .groups = 'drop')
  if (ID==FALSE) return (data$uv)
  else return(data)
  }
  
#' @title  Providing quantities of sold products
#'
#' @description The function returns quantities of sold products with given IDs. 
#' @param data The user's data frame. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{quantities} (as positive numeric) and \code{prodID} (as numeric, factor or character) with unique product IDs. 
#' @param period The time period (as character) limited to the year and month, e.g. "2019-03".
#' @param set The set of unique product IDs to be used for determining quantities of sold products (see also \code{\link{data_matching}}). If the \code{set} is empty, the function returns quantities of all products being available in \code{period}.
#' @param ID A logical parameter indicating whether a data frame with prodIDs and quantities should be returned.
#' @rdname quantities
#' @return The function analyzes the user's data frame and returns quantities of products with given \code{ID} and being sold in the time period indicated by the \code{period} parameter. Please note that the function returns the quantity values for sorted prodIDs and in the absence of a given prodID in the data set, the function returns nothing (it does not return zero). If the ID parameter is set to TRUE then the function returns a data frame with columns: \code{by} (IDs of products) and \code{q} (quantities of products).
#' @examples 
#' \donttest{quantities(milk, period="2019-06")}
#' quantities(milk, period="2019-12", set=c(400032, 82919), ID=TRUE)
#' @export

quantities <- function(data, period, set = c(), ID = FALSE)
  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  prodID<-NULL
  period <-
  paste(period, "-01", sep = "")
  period <- as.Date(period)
  data<-dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(period) &
  lubridate::month(data$time) == lubridate::month(period)
  ))
  if (nrow(data) == 0)
  stop("There are no data in selected period")
  if (length(set) > 0) {data<-dplyr::filter(data, prodID %in% set)
  if (nrow(data) == 0)
  stop("There are no data in selected period")
  }
  data<-dplyr::summarise(dplyr::group_by(data, by=prodID), q=sum(quantities), .groups = 'drop')
  if (ID==FALSE) return (data$q)
  else return(data)
}

#' @title  Providing expenditures of sold products
#'
#' @description The function returns expenditures of sold products with given IDs. 
#' @param data The user's data frame. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{quantities} (as positive numeric) and \code{prodID} (as numeric, factor or character) with unique product IDs. 
#' @param period The time period (as character) limited to the year and month, e.g. "2019-03".
#' @param set The set of unique product IDs to be used for determining expenditures of sold products (see also \code{\link{data_matching}}). If the \code{set} is empty, the function returns quantities of all products being available in \code{period}.
#' @param ID A logical parameter indicating whether a data frame with prodIDs and quantities should be returned.
#' @rdname expenditures
#' @return The function analyzes the user's data frame and returns expenditures of products with given \code{ID} and being sold in the time period indicated by the \code{period} parameter. Please note that the function returns the expenditure values for sorted prodIDs and in the absence of a given prodID in the data set, the function returns nothing (it does not return zero). If the ID parameter is set to TRUE then the function returns a data frame with columns: \code{by} (IDs of products) and \code{expend} (expenditures of products).
#' @examples 
#' \donttest{expenditures(milk, period="2019-06")}
#' expenditures(milk, period="2019-12", set=c(400032, 82919), ID=TRUE)
#' @export

expenditures <- function(data, period, set = c(), ID = FALSE) 
  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  prodID<-NULL
  period <-
  paste(period, "-01", sep = "")
  period <- as.Date(period)
  data<-dplyr::filter(
  data,
  (
  lubridate::year(data$time) == lubridate::year(period) &
  lubridate::month(data$time) == lubridate::month(period)
  ))
  if (nrow(data) == 0)
  stop("There are no data in selected period")
  if (length(set) > 0) {data<-dplyr::filter(data, prodID %in% set)
  if (nrow(data) == 0)
  stop("There are no data in selected period")
  }
  data<-dplyr::summarise(dplyr::group_by(data, by=prodID), expend=sum(prices*quantities), .groups = 'drop')
  if (ID==FALSE) return (data$expend)
  else return(data)
}


#' @title  Providing a correlation coefficient for price and quantity of sold products
#'
#' @description The function returns correlation between price and quantity of sold products with given IDs. 
#' @param data The user's data frame. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities} (as positive numeric) and \code{prodID} (as numeric, factor or character) with unique product IDs. 
#' @param period The time period (as character) limited to the year and month, e.g. "2019-03".
#' @param set The set of unique product IDs to be used for determining correlation between price and quantity of sold products (see also \code{\link{data_matching}}). If the \code{set} is empty, the function works for all products being available in \code{period}.
#' @param figure A logical parameter indicating whether the function returns a figure (TRUE) or a data frame (FALSE) with correlations between price and quantity of sold products.
#' @rdname pqcor
#' @return The function returns Pearson's correlation coefficient between price and quantity of products with given IDs and sold in \code{period}.
#' @examples 
#' pqcor(milk, period="2019-03")
#' \donttest{pqcor(milk, period="2019-03",figure=TRUE)}
#' @export

pqcor <- function(data,
period,
set = c(),
figure = FALSE)
{
if (nrow(data) == 0)
stop("A data frame is empty")
prices <- prices(data, period, set)
quantities <- quantities(data, period, set)
coeff <- stats::cor(prices, quantities)
coeff <- signif(coeff, 4)
if (figure == TRUE) {
df <- data.frame(prices, quantities)
title <-
paste("Pearson's correlation coefficient = ", as.character(coeff))
ggplot2::ggplot(df, ggplot2::aes(x = prices, y = quantities)) + ggplot2::geom_point() +
ggplot2::ggtitle(title) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}
else
return (coeff)
}

#' @title  Providing correlations between price and quantity of sold products
#'
#' @description The function returns Pearson's correlation coefficients between price and quantity of sold products with given IDs.
#' @param data The user's data frame. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities} (as positive numeric) and \code{prodID} (as numeric, factor or character) with unique product IDs. 
#' @param start The beginning of the considered time interval (as character) limited to the year and month, e.g. "2020-03".
#' @param end The end of the considered time interval (as character) limited to the year and month, e.g. "2020-04".
#' @param figure A logical parameter indicating whether the function returns a figure (TRUE) or a data frame (FALSE) with price-quantity correlations.
#' @param date_breaks A string giving the distance between breaks on the X axis like "1 month" (default value) or "4 months".
#' @param set The set of unique product IDs to be used for determining correlation between prices and quantities of sold products (see also \code{\link{data_matching}}). If the \code{set} is empty, the function works for all products being available in \code{period}.
#' @rdname pqcor_fig
#' @return The function returns Pearson's correlation coefficients between price and quantity of products with given IDs and sold in the time interval: \code{<start, end>}. Correlation coefficients are calculated for each month separately. Results are presented in tabular or graphical form depending on the \code{figure} parameter.
#' @examples 
#' \donttest{pqcor_fig(milk, start="2018-12", end="2019-12", figure=FALSE)}
#' \donttest{pqcor_fig(milk, start="2018-12", end="2019-12", figure=TRUE)}
#' @export

pqcor_fig <- function (data,
                       start,
                       end,
                       figure = TRUE,
                       date_breaks = "1 month",
                       set = c())
                       {
                       if (nrow(data) == 0)
                       stop("A data frame is empty")
                       date <- correlation <- NULL
                       start <- paste(start, "-01", sep = "")
                       end <- paste(end, "-01", sep = "")
                       start <- as.Date(start)
                       end <- as.Date(end)
                       times <- c()
                       values <- c()
                       while (start <= end)
                       {
                       t <- substr(start, 0, 7)
                       times <- c(times, t)
                       values <-
                       c(values, pqcor(data, period = t, set))
                       lubridate::month(start) <-
                       lubridate::month(start) + 1
                       }
                       tab <- data.frame(c(times), c(values))
                       colnames(tab) <- c("date", "correlation")
                       if (figure == FALSE)
                       return (tab)
                       #returning a figure which is based on 'tab'
                       else
                       {
tab$date <- as.Date(paste(tab$date, "01", sep = "-"))
ggplot2::ggplot(tab, ggplot2::aes(x = date, y = correlation)) + ggplot2::geom_point() +
ggplot2::geom_line() + ggplot2::labs(x = "date", y = "correlation") + ggplot2::scale_x_date(date_labels ="%Y %m", date_breaks  = date_breaks) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle =45, hjust = 1))
                       }
                       }
                       
#' @title  Providing values of product sales
#'
#' @description The function returns values of sales of products with given IDs. 
#' @param data The user's data frame. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities} (as positive numeric) and \code{prodID} (as numeric, factor or character) with unique product IDs. 
#' @param period The time period (as character) limited to the year and month, e.g. "2019-03".
#' @param set The set of unique product IDs to be used for determining product sales values (see also \code{\link{data_matching}}). If the \code{set} is empty, then the function returns sale values of all products being available in \code{period}.
#' @param shares A logical parameter indicating whether the function is to return shares of product sales.
#' @param hist A logical parameter indicating whether the function is to return histogram of product sales.
#' @rdname sales
#' @return The function analyzes the user's data frame and returns values of sales of products with given IDs and being sold in time period indicated by the \code{period} parameter (see also \code{expenditures} function which returns the expenditure values for sorted prodIDs).
#' @examples 
#' \donttest{sales(milk, period="2019-06", shares=TRUE, hist=TRUE)}
#' sales(milk, period="2019-12",set=unique(milk$prodID)[1])
#' @export

sales <- function(data,
                  period,
                  set = c(),
                  shares = FALSE,
                  hist = FALSE)
                  {
                  if (nrow(data) == 0)
                  stop("A data frame is empty")
                  period <-
                  paste(period, "-01", sep = "")
                  period <- as.Date(period)
                  data <-
                  dplyr::filter(data, lubridate::year(data$time) == lubridate::year(period) &
                  lubridate::month(data$time) == lubridate::month(period))
                  if (nrow(data) == 0)
                  stop("There are no data in selected period")
                  if (length(set) == 0) set<-unique(data$prodID)
                  vec <- numeric(length(set))
                  for (i in 1:length(set)) {
                  d <- dplyr::filter(data, data$prodID == set[i])
                  if (nrow(d) == 0)
                  vec[i] <- 0
                  else
                  vec[i] <- sum(d$prices * d$quantities)
                  }
                  if (hist == FALSE) {
                  if (shares == FALSE)
                  return(vec)
                  else
                  return (vec / sum(vec))
                  }
                  else             {
                  if (shares == FALSE)
                  return (
                  graphics::hist(
                  vec,
                  main = "",
                  xlab = " value of sale",
                  ylab = "number of obs.",
                  col = "grey"
                  )
                  )
                  else
                  return (
                  graphics::hist(
                  vec / sum(vec),
                  main = "",
                  xlab = " share in sale",
                  ylab = "number of obs.",
                  col = "grey"
                  )
                  )
                  }
                  }

#' @title  Providing information about sales of products from one or more datasets
#'
#' @description The function returns values of sales of products from one or more datasets or the corresponding barplot for these sales. 
#' @param datasets A list of user's data frames. Each data frame must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities} (as positive numeric).
#' @param start The beginning of the considered time interval (as character) limited to the year and month, e.g. "2020-03".
#' @param end The end of the considered time interval (as character) limited to the year and month, e.g. "2020-04".
#' @param shares A logical parameter indicating whether the function is to calculate shares of product sales
#' @param barplot A logical parameter indicating whether the function is to return barplot for product sales. 
#' @param names A vector of characters describing product groups defined by \code{datasets}.
#' @rdname sales_groups
#' @return The function returns values of sales of products from one or more datasets or the corresponding barplot for these sales (if \code{barplot} is TRUE). Alternatively, it calculates the sale shares (if \code{shares} is TRUE).
#' @examples 
#' ## Creating 3 subgroups of milk:
#' \donttest{ctg<-unique(milk$description)}
#' \donttest{categories<-c(ctg[1],ctg[2],ctg[3])}
#' \donttest{milk1<-dplyr::filter(milk, milk$description==categories[1])}
#' \donttest{milk2<-dplyr::filter(milk, milk$description==categories[2])}
#' \donttest{milk3<-dplyr::filter(milk, milk$description==categories[3])}
#' ## Sample use of this function:
#' \donttest{sales_groups(datasets=list(milk1,milk2,milk3),start="2019-04",end="2019-04",shares=TRUE)}
#' \donttest{sales_groups(datasets=list(milk1,milk2,milk3),start="2019-04",end="2019-07", 
#' barplot=TRUE, names=categories)}
#' @export

sales_groups <-
  function(datasets = list(),
  start,
  end,
  shares = FALSE,
  barplot = FALSE,
  names = c())
  {
  groups <- value <- NULL
  start <- paste(start, "-01", sep = "")
  start <- as.Date(start)
  end <- paste(end, "-01", sep = "")
  end <- as.Date(end)
  lubridate::day(start) <- 1
  lubridate::day(end) <-
  lubridate::days_in_month(end)
  nm <- c()
  sales <- c()
  for (m in 1:length(datasets))  {
  set <- data.frame(datasets[[m]])
  if (nrow(set) == 0)
  print("At least one data frame is empty")
  nm <-
  c(nm, paste("group ", as.character(m)))
  set <-
  dplyr::filter(set, set$time >= start & set$time <= end)
  sales <-
  c(sales, sum(set$prices * set$quantities))
  }
  if (shares == TRUE)
  sales <- sales / sum(sales)
  if (length(names) == 0)
  names <- nm
  if (barplot == FALSE)
  return (sales)
  else {
  if (shares == FALSE)
  {
  df <- data.frame(groups = names, value = sales)
  ggplot2::ggplot(data = df, ggplot2::aes(x =
  groups, y = value)) +
  ggplot2::geom_bar(stat = "identity",
  fill = "grey",
  color = "black") + ggplot2::labs(y = "value of sales")
  }
  else
  {
  df <- data.frame(groups = names, value = sales)
  ggplot2::ggplot(data = df, ggplot2::aes(x =
  groups, y = value)) +
  ggplot2::geom_bar(stat = "identity",
  fill = "grey",
  color = "black") + ggplot2::labs(y = "share in sales")
  }
  }
  }
  

#' @title  Generating an artificial scanner dataset
#'
#' @description This function provides artificial scanner datasets where prices and quantities are lognormally distributed.
#' @param pmi A numeric vector indicating \code{mi} parameters for lognormally distributed prices from the subsequent months.
#' @param psigma A numeric vector indicating \code{sigma} parameters for lognormally distributed prices from the subsequent months.
#' @param qmi A numeric vector indicating \code{mi} parameters for lognormally distributed quantities from the subsequent months.
#' @param qsigma A numeric vector indicating \code{sigma} parameters for lognormally distributed quantities from the subsequent months.
#' @param prec A two-dimensional numeric vector indicating precision, i.e. the number of decimal places, for presenting prices and quantities.
#' @param n An integer parameter indicating the number of products which are to be generated.
#' @param n0 An integer parameter indicating the first (the smallest) prodID.
#' @param r An integer parameter indicating the number of outlets (retailer sale points) for which prices and quantities are to be generated.
#' @param r0 n0 An integer parameter indicating the first (the smallest) retID.
#' @param start The first period in the generated data frame (as character) limited to the year and month, e.g. '2019-12'.
#' @param days A logical parameter indicating whether the trading day in a given month is to be randomised. The default value of \code{days} is FALSE, which means that each transaction for a given month takes place on the first day of the month.
#' @rdname generate
#' @return This function returns an artificial scanner dataset where prices and quantities are lognormally distributed. The characteristics for these lognormal distributions are set by \code{pmi}, \code{psigma}, \code{qmi} and \code{qsigma} parameters. This function works for a fixed number of products and outlets (see \code{n} and \code{r} parameters). The generated dataset is ready for further price index calculations.   
#'
#' @examples 
#' generate(pmi=c(1.02,1.03,1.04),psigma=c(0.05,0.09,0.02),qmi=c(3,4,4),
#' qsigma=c(0.1,0.1,0.15),start="2020-01",days=TRUE)
#' \donttest{generate(pmi=c(1.02,1.03,1.04),psigma=c(0.05,0.09,0.02),qmi=c(6,6,7),
#' qsigma=c(0.1,0.1,0.15),start="2020-01",n=1000,n0=132578,r=10)}
#' @export

generate <-
  function(pmi = c(),
  psigma = c(),
  qmi = c(),
  qsigma = c(),
  prec = c(2, 0),
  n = 100,
  n0 = 1,
  r = 1,
  r0 = 1,
  start,
  days = FALSE)
  {
  if ((length(pmi) <= 1) |
  (length(psigma) <= 1) |
  (length(qmi) <= 1) |
  (length(qsigma) <= 1))
  stop("Lengths of parameters: pmi, psigma, qmi and qsigma must be 2 or more!")
  if (!((length(pmi) == length(psigma)) &
  (length(pmi) == length(qmi)) &
  (length(qmi) == length(qsigma))))
  stop("Lengths of parameters: pmi, psigma, qmi and qsigma must be identical!")
  if (!(length(prec) == 2))
  stop("A length of 'prec' parameter must be 2!")
  start <- paste(start, "-01", sep = "")
  start <- as.Date(start)
  #rand data frames for all periods
  DT <- data.frame()
  for (k in 1:length(pmi))
  {
  #time
  time <- c()
  for (i in 1:n) {
  if (days == TRUE) {
  nd <- 28
  lubridate::day(start) <- sample(nd, 1)
  }
  time <- c(time, as.character(start))
  }
  time <- as.Date(time)
  #prodID
  prodID <- seq(n0, n0 + n - 1)
  ret <- r0 + r - 1
  for (i in r0:ret) {
  #retID
  retID <- replicate(n, i)
  #prices
  prices <- stats::rlnorm(n, pmi[k], psigma[k])
  #quantities
  quantities <- stats::rlnorm(n, qmi[k], qsigma[k])
  DT <- rbind(DT, data.frame(time, prices, quantities, prodID, retID))
  }
  lubridate::month(start) <- lubridate::month(start) + 1
  }
  DT$prices <- round(DT$prices, prec[1])
  DT$quantities <- round(DT$quantities, prec[2])
  return (DT)
  }
  
#' @title  Generating an artificial scanner dataset in the CES model
#'
#' @description This function provides artificial scanner datasets where prices are lognormally distributed and quantities are obtained under a CES utility.
#' @param pmi A numeric vector indicating \code{mi} parameters for lognormally distributed prices from the subsequent months.
#' @param psigma A numeric vector indicating \code{sigma} parameters for lognormally distributed prices from the subsequent months.
#' @param prec A numeric value indicating precision, i.e. the number of decimal places, for generating prices.
#' @param elasticity The elasticity of substitution. The default value is 0.7.
#' @param S Sum of spending. The default value is 1000. 
#' @param alfa A numeric vector indicating positive weights that reflect the consumer preferences.By default, this vector is randomized based on a uniform distribution. 
#' @param n An integer parameter indicating the number of products which are to be generated.
#' @param n0 An integer parameter indicating the first (the smallest) prodID.
#' @param r An integer parameter indicating the number of outlets (retailer sale points) for which prices and quantities are to be generated.
#' @param r0 n0 An integer parameter indicating the first (the smallest) retID.
#' @param start The first period in the generated data frame (as character) limited to the year and month, e.g. '2019-12'.
#' @param days A logical parameter indicating whether the trading day in a given month is to be randomised. The default value of \code{days} is FALSE, which means that each transaction for a given month takes place on the first day of the month.
#' @rdname generate_CES
#' @return This function returns an artificial scanner dataset where prices are lognormally distributed, quantities are calculated under the assumption that consumers have CES (Constant Elasticity of Substitution) preferences and their spending on all products is \code{S}. The characteristics for the lognormal price distribution are set by \code{pmi} and \code{psigma} parameters. This function works for a fixed number of products and outlets (see \code{n} and \code{r} parameters). The generated dataset is ready for further price index calculations.   
#' @references
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' #Generating an artificial dataset (the elasticity of substitution is 1.25)
#' \donttest{df<-generate_CES(pmi=c(1.02,1.03),psigma=c(0.04,0.03),
#' elasticity=1.25,start="2020-01",n=100,days=TRUE)}
#' #Verifying the elasticity of substitution
#' \donttest{elasticity(df, start="2020-01",end="2020-02")}
#' @export

generate_CES <-
  function(pmi = c(),
  psigma = c(),
  prec = 2,
  elasticity=0.7,
  S=1000,
  alfa = c(),
  n = 100,
  n0 = 1,
  r = 1,
  r0 = 1,
  start,
  days = FALSE)
  {
  if ((length(pmi) <= 1) |
  (length(psigma) <= 1))
  stop("Lengths of parameters pmi and psigma must be 2 or more!")
  if (!(length(pmi) == length(psigma)))
  stop("Lengths of parameters pmi and psigma must be identical!")
  if (S<=0) stop("The S parameter must be positive!")
  if (length(alfa)>0) {
  if (!(length(alfa)==n)) stop("Length of parameter alfa and a value of n must be identical!")  
  if (!(sum(alfa)==1)) stop("Sum of elements of the alfa vector must be one!")
  }
  else
  {
  alfa<-stats::runif(n,0,1)
  alfa<-alfa/sum(alfa)
  }  
  start <- paste(start, "-01", sep = "")
  start <- as.Date(start)
  #rand data frames for all periods
  DT <- data.frame()
  for (k in 1:length(pmi))
  {
  #time
  time <- c()
  for (i in 1:n) {
  if (days == TRUE) {
  nd <- 28
  lubridate::day(start) <- sample(nd, 1)
  }
  time <- c(time, as.character(start))
  }
  time <- as.Date(time)
  #prodID
  prodID <- seq(n0, n0 + n - 1)
  ret <- r0 + r - 1
  for (i in r0:ret) {
  #retID
  retID <- replicate(n, i)
  #prices
  prices <- stats::rlnorm(n, pmi[k], psigma[k])
  prices <- round(prices, prec)
  #quantities
  denom<-sum(alfa*(prices/alfa)^(1-elasticity))
  quantities <-((S/prices)*alfa*(prices/alfa)^(1-elasticity))/denom 
  DT <- rbind(DT, data.frame(time, prices, quantities, prodID, retID))
  }
  lubridate::month(start) <- lubridate::month(start) + 1
  }
  return (DT)
  }

#' @title  Calculating the relative price and/or quantity dissimilarity measure between periods
#'
#' @description This function returns a value of the relative price and/or quantity dissimilarity measure.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param period1 The first period (as character) limited to the year and month, e.g. '2019-03'.
#' @param period2 The second period (as character) limited to the year and month, e.g. '2019-04'.
#' @param type The parameter indicates what type of dissimilarity measure is to be calculated. Possible values of the \code{type} parameter are: \code{p} (for the price dissimilarity measure calculation), \code{q} (for the quantity dissimilarity measure calculation) or \code{pq} (for the dSPQ measure calculation, i.e. the measure of relative price and quantity dissimilarity - see \code{References}).
#' @rdname dissimilarity
#' @return This function returns a value of the relative price (dSP) and/or quantity (dSQ) dissimilarity measure. In a special case, when the \code{type} parameter is set to \code{pq}, the function provides the value of dSPQ measure (the relative price and quantity dissimilarity measure calculated as min(dSP,dSQ). 
#' @references
#' {Diewert, E. (2020). \emph{The Chain Drift Problem and Multilateral Indexes.} Chapter 6 in: Consumer Price Index Theory (draft)}
#'
#' @examples 
#' dissimilarity(milk, period1="2018-12",period2="2019-12",type="q")
#' \donttest{dissimilarity(milk, period1="2018-12",period2="2019-12",type="pq")}
#' @export

dissimilarity <- function (data, period1, period2, type = "p")
{
if (nrow(data) == 0)
stop("A data frame is empty")
allowed <- c("p", "q", "pq")
if (length(base::intersect(type, allowed)) == 0)
stop("there are no such types of dissimilarity measures")
r <- paste(period1, "-01", sep = "")
t <- paste(period2, "-01", sep = "")
r <- as.Date(r)
t <- as.Date(t)
data <-
dplyr::filter(
data,
(
lubridate::year(data$time) == lubridate::year(r) &
lubridate::month(data$time) == lubridate::month(r)
) |
(
lubridate::year(data$time) == lubridate::year(t) &
lubridate::month(data$time) == lubridate::month(t)
)
)
id <- matched(data, r, t)
price_r <- prices(data, period = r, set = id)
price_t <- prices(data, period = t, set = id)
quantity_r <- quantities(data, period = r, set = id)
quantity_t <- quantities(data, period = t, set = id)
sales_r <- expenditures(data, period = r, set = id)
sales_t <- expenditures(data, period = t, set = id)
sum_r <- sum(sales_r)
sum_t <- sum(sales_t)
prqt <- sum(price_r * quantity_t)
ptqr <- sum(price_t * quantity_r)
if (type == "p") {
sum1 <-
0.5 * sum(((sales_t / sum_t) + (sales_r / sum_r)) * ((sales_t / sum_t) -
price_r * quantity_t / prqt) ^ 2)
sum2 <-
0.5 * sum(((sales_t / sum_t) + (sales_r / sum_r)) * ((sales_r / sum_r) -
price_t * quantity_r / ptqr) ^ 2)
return (sum1 + sum2)
}
if (type == "q") {
sum1 <-
0.5 * sum(((sales_t / sum_t) + (sales_r / sum_r)) * ((sales_t / sum_t) -
price_t * quantity_r / ptqr) ^ 2)
sum2 <-
0.5 * sum(((sales_t / sum_t) + (sales_r / sum_r)) * ((sales_r / sum_r) -
price_r * quantity_t / prqt) ^ 2)
return (sum1 + sum2)

}
if (type == "pq") {
sum1 <-
0.5 * sum(((sales_t / sum_t) + (sales_r / sum_r)) * ((sales_t / sum_t) -
price_r * quantity_t / prqt) ^ 2)
sum2 <-
0.5 * sum(((sales_t / sum_t) + (sales_r / sum_r)) * ((sales_r / sum_r) -
price_t * quantity_r / ptqr) ^ 2)
sum3 <-
0.5 * sum(((sales_t / sum_t) + (sales_r / sum_r)) * ((sales_t / sum_t) -
price_t * quantity_r / ptqr) ^ 2)
sum4 <-
0.5 * sum(((sales_t / sum_t) + (sales_r / sum_r)) * ((sales_r / sum_r) -
price_r * quantity_t / prqt) ^ 2)
return (min(sum1 + sum2, sum3 + sum4))
}
}

#' @title  Presenting the relative price and/or quantity dissimilarity measure over time
#'
#' @description This function presents values of the relative price and/or quantity dissimilarity measure over time.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. '2019-03'.
#' @param end The research period (as character) limited to the year and month, e.g. '2019-07'.
#' @param type The parameter indicates what type of dissimilarity measure is to be calculated. Possible values of the \code{type} parameter are: \code{p} (for the price dissimilarity measure calculation), \code{q} (for the quantity dissimilarity measure calculation) or \code{pq} (for the dSPQ measure calculation, i.e. the measure of relative price and quantity dissimilarity - see \code{References}).
#' @param benchmark The benchmark period (as character) limited to the year and month, e.g. '2019-07'.
#' @param figure A logical parameter indicating the resulting object. If it is TRUE, the function presents the above-mentioned dissimilarities over time via a figure. Otherwise, the function returns a dataframe.  
#' @param date_breaks A string giving the distance between breaks on the X axis like "1 month" (default value) or "4 months".
#' @rdname dissimilarity_fig
#' @return This function presents values of the relative price and/or quantity dissimilarity measure over time. The user can choose a benchmark period (defined by \code{benchmark}) and the type of dissimilarity measure is to be calculated (defined by \code{type}). The obtained results of dissimilarities over time can be presented in a dataframe form or via a figure (the default value of \code{figure} is TRUE, which results in a figure). 
#' @references
#' {Diewert, E. (2020). \emph{The Chain Drift Problem and Multilateral Indexes.} Chapter 6 in: Consumer Price Index Theory (draft)}
#'
#' @examples 
#' \donttest{dissimilarity_fig(milk, start="2018-12",end="2019-12",type="q",figure=FALSE)}
#' \donttest{dissimilarity_fig(milk, start="2018-12",end="2019-12",type="pq",benchmark="start")}
#' @export

dissimilarity_fig <-
  function (data,
  start,
  end,
  type = "p",
  benchmark = "end",
  figure = TRUE,
  date_breaks = "1 month")
  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  allowed_type <- c("p", "q", "pq")
  if (length(base::intersect(type, allowed_type)) == 0)
  stop("there are no such types of dissimilarity measures")
  allowed_benchmark <- c("start", "end")
  if (length(base::intersect(benchmark, allowed_benchmark)) == 0)
  stop("bad specification of the 'benchmark' parameter")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  if (start >= end)
  stop("parameters must satisfy: start<end")
  times <- c()
  values <- c()
  if (benchmark == "end")
  {
  {
  t2 <- substr(end, 0, 7)
  while (start < end)
  
  {
  t1 <- substr(start, 0, 7)
  times <- c(times, t1)
  values <-
  c(values,
  dissimilarity(data, period1 = t1, period2 = t2, type))
  lubridate::month(start) <- lubridate::month(start) + 1
  }
  times <- c(times, t2)
  values <- c(values, 0)
  tab <- data.frame(c(times), c(values))
  colnames(tab) <- c("date", "dissimilarity")
  if (figure == FALSE)
  return (tab)
  #returning a figure
  else
  {
  tab$date <- as.Date(paste(tab$date, "01", sep = "-"))
  ggplot2::ggplot(tab, ggplot2::aes(x = date, y = dissimilarity)) + ggplot2::geom_point() +
  ggplot2::geom_line() + ggplot2::labs(x = "date", y = "dissimilarity") +
  ggplot2::scale_x_date(date_labels = "%Y %m", date_breaks  = date_breaks) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
  }
  }
  else
  {
  values <- c(values, 0)
  t1 <- substr(start, 0, 7)
  times <- c(times, t1)
  while (start < end)
  
  {
  lubridate::month(start) <- lubridate::month(start) + 1
  t2 <- substr(start, 0, 7)
  times <- c(times, t2)
  values <-
  c(values,
  dissimilarity(data, period1 = t1, period2 = t2, type))
  }
  tab <- data.frame(c(times), c(values))
  colnames(tab) <- c("date", "dissimilarity")
  if (figure == FALSE)
  return (tab)
  #returning a figure
  else
  {
  tab$date <- as.Date(paste(tab$date, "01", sep = "-"))
  ggplot2::ggplot(tab, ggplot2::aes(x = date, y = dissimilarity)) + ggplot2::geom_point() +
  ggplot2::geom_line() + ggplot2::labs(x = "date", y = "dissimilarity") +
  ggplot2::scale_x_date(date_labels = "%Y %m", date_breaks  = date_breaks) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
  }
  }


#' @title  Providing information about sales of products 
#'
#' @description The function returns values of sales of products or the corresponding barplot for these sales. 
#' @param data The user's data frame with subgroups of sold products (see \code{by} parameter). The data frame must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities} (as positive numeric). An additional column indicated via \code{by} parameter is also needed.
#' @param by The column name indicating grouping variable, i.e. this column is used for creating subgroups of products.
#' @param start The beginning of the considered time interval (as character) limited to the year and month, e.g. "2020-03".
#' @param end The end of the considered time interval (as character) limited to the year and month, e.g. "2020-04".
#' @param shares A logical parameter indicating whether the function is to calculate shares of product sales
#' @param barplot A logical parameter indicating whether the function is to return barplot for product sales. 
#' @param names A vector of characters describing product groups defined by \code{datasets}.
#' @rdname sales_groups2
#' @return The function returns values of sales of products or the corresponding barplot for these sales (if \code{barplot} is TRUE). Alternatively, it calculates the sale shares (if \code{shares} is TRUE).
#' @examples 
#' \donttest{outlets<-as.character(unique(milk$retID))}
#' \donttest{sales_groups2(milk,by="retID",start="2019-04",end="2019-04",
#' shares=TRUE,barplot=TRUE,names=outlets)}
#' @export

sales_groups2 <-
  function(data = data.frame(),
  by,
  start,
  end,
  shares = FALSE,
  barplot = FALSE,
  names = c())
  {
  if (nrow(data) == 0)
  stop("A data set is empty!")
  ns <- colnames(data)
  if (!(by %in% ns))
  stop ("There is no column specified via 'by' parameter!")
  group <- as.character(unique(data[, by]))
  datasets <- list()
  for (i in 1:length(group))
  datasets[[i]] <- dplyr::filter(data, data[, by] == group[i])
  return (sales_groups(datasets, start, end, shares, barplot, names))
  }
  
#' @title  Providing information about the grammage and unit of products
#'
#' @description The function returns the grammage and unit of products as two additional columns. 
#' @param data The user's data frame. The data frame must contain the \code{description} column (as character). 
#' @param units Units of products which are to be detected
#' @param multiplication A sign of the multiplication used in product descriptions
#' @param space A maximum space between the product grammage and its unit
#' @rdname data_unit
#' @return The function returns the user's data frame with two additional columns: \code{grammage} and \code{unit} (both are character type). The values of these columns are extracted from product descriptions on the basis of provided \code{units}. Please note, that the function takes into consideration a sign of the multiplication, e.g. if the product description contains: '2x50 g', we obtain: \code{grammage: 100} and \code{unit: g} for that product (for \code{multiplication} set to 'x'). 
#' @examples 
#' data_unit(dataU,units=c("g","ml","kg","l"),multiplication="x")
#' @export

data_unit <-
  function (data = data.frame(),
  units = c("g", "ml", "kg", "l"),
  multiplication = "x",
  space = 1)
  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  columns <- colnames(data)
  if (!("description" %in% columns))
  stop("Your data frame must contain a 'description' column!")
  if (length(units) == 0)
  stop("You must set at least one unit")
  descriptions <- data$description
  grammage <- c()
  unit <- c()
  for (i in 1:length(descriptions)) {
  result <- unit(descriptions[i], units, multiplication, space)
  grammage <- c(grammage, result[[1]])
  unit <- c(unit, result[[2]])
  }
  data$grammage <- grammage
  data$unit <- unit
  return (data)
  }

#' @title  Normalization of grammage units and recalculation of prices and quantities with respect to these units
#'
#' @description The function normalizes grammage units of products and recalculates product prices and quantities with respect to these normalized grammage units. 
#' @param data The user's data frame. The data frame must contain the following columns: \code{prices} (as positive numeric), \code{quantities} (as positive numeric), \code{grammage} (as numeric or character) and \code{unit} (as character). 
#' @param rules User rules for transforming \code{grammage}, \code{unit}, \code{prices} and \code{quantities} of products. For instance, a rule \code{("ml","l",1000)} changes the 'old' grammage unit: \code{ml} into the new one: \code{l} on the basis of the provided relation: \code{1000ml=1l}. As a consequence, for each product which is sold in liters \code{l} , the unit price and quantity are calculated. 
#' @param all A logical value indicating whether the resulting data frame is to be limited to products with detected  grammage. Its default value is \code{TRUE} which means that not transformed rows (products) are also returned.
#' @rdname data_norm
#' @return The function returns the user's data frame with two transformed columns: \code{grammage} and \code{unit}, and two rescaled columns: \code{prices} and \code{quantities}. The above-mentioned transformation and rescaling take into consideration the user \code{rules}. Recalculated prices and quantities concern grammage units defined as the second parameter in the given rule.   
#' @examples 
#' # Preparing a data set
#' data<-data_unit(dataU,units=c("g","ml","kg","l"),multiplication="x")
#' # Normalization of grammage units
#' data_norm(data, rules=list(c("ml","l",1000),c("g","kg",1000)))
#' @export

data_norm <-
  function(data = data.frame(),
  rules = list(c("ml", "l", 1000), c("g", "kg", 1000)),
  all = TRUE)
  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  if (length(rules) == 0)
  stop("Bad specification of 'rules'")
  for (i in 1:length(rules))
  if (!(length(rules[[i]]) == 3))
  stop("Bad specification of 'rules'")
  columns <- colnames(data)
  if (!("prices" %in% columns))
  stop("Your data frame must contain a 'prices' column!")
  if (!("quantities" %in% columns))
  stop("Your data frame must contain a 'quantities' column!")
  if (!("grammage" %in% columns))
  stop("Your data frame must contain a 'grammage' column!")
  if (!("unit" %in% columns))
  stop("Your data frame must contain a 'unit' column!")
  #checking rules
  data_return <- data.frame()
  #corrections
  options(scipen = 999)
  for (i in 1:length(rules))
  {
  data_rules1 <- dplyr::filter(data, data$unit == rules[[i]][1])
  data_rules2 <- dplyr::filter(data, data$unit == rules[[i]][2])
  if (nrow(data_rules2) > 0)
  data_rules2$grammage <- as.numeric(data_rules2$grammage)
  if (nrow(data_rules1) > 0) {
  data_rules1$grammage <- as.numeric(data_rules1$grammage)
  data_rules1$grammage <- data_rules1$grammage / as.numeric(rules[[i]][3])
  data_rules1$unit <- rules[[i]][2]
  }
  #unit price
  data_rules <- rbind(data_rules1, data_rules2)
  if ((nrow(data_rules1) + nrow(data_rules2) > 0)) 
  {
  data_rules$prices <- data_rules$prices / data_rules$grammage
  data_rules$quantities <- data_rules$quantities * data_rules$grammage
  }
  data_return <- rbind(data_return, data_rules)
  }
  #should we take the rest of products? 'all=TRUE' means: 'Yes'
  if (all == TRUE) {
  units_all <- c()
  for (i in 1:length(rules))
  units_all <- c(units_all, rules[[i]][1])
  for (i in 1:length(rules))
  units_all <- c(units_all, rules[[i]][2])
  data <- dplyr::filter(data, (!(data$unit %in% units_all)))
  data_return <- rbind(data_return, data)
  }
  data_return$grammage <- as.character(data_return$grammage)
  return (data_return)
  }

#' @title  Checking the user's data frame
#'
#' @description The function checks if the argument \code{data} points to a data frame which is suitable for further price index calculation. In particular, the function checks whether the indicated data frame contains the required columns and whether they are of the appropriate type (if not, the function returns FALSE and an appropriate comment).
#' @param data Any R object but ultimately it is a data frame.
#' @rdname data_check
#' @return The function returns TRUE if the data frame indicated by the \code{data} parameter is suitable for the calculation of price indices and returns FALSE otherwise.
#' @examples 
#' data_check(milk)
#' data_check(iris)
#' @export

data_check <- function (data)
{
if (!(is.data.frame(data))) {
message("Argument 'data' is not a data frame!")
return (FALSE)
}
if (nrow(data) == 0) {
message("A data frame is empty")
return (FALSE)
}
mustbe <- c("time", "prices", "quantities", "prodID")
col_names <- colnames(data)
if (prod(as.numeric(mustbe %in% col_names)) == 0) {
message("Columns: 'time', 'prices', 'quantities' and 'prodID' are obligatory!")
return (FALSE)
}
if (!lubridate::is.instant(data$time)) {
message("The 'time' column must be as Date type!")
return (FALSE)
}
if (!is.numeric(data$prices)) {
message("The 'prices' column must be numeric!")
return (FALSE)
}
if (!is.numeric(data$quantities)) {
message("The 'quantities' column must be numeric!")
return (FALSE)
}
if ((!(is.numeric(data$prodID))) &
(!(is.factor(data$prodID))) &
(!(is.character(data$prodID)))) {
message("The 'prodID' columns must be as numeric, factor or character type!")
return (FALSE)
}
return (TRUE)
}

#' @title  Aggregating the user's data frame
#'
#' @description The function aggregates the user's data frame over time and optionally over outlets.
#' @param data The user's data frame.
#' @param join_outlets A logical value indicating whether the data aggregation over outlets should be also done.
#' @rdname data_aggregating
#' @return The function aggregates the user's data frame over time and/or over outlets. Consequently, we obtain monthly data, where the unit value is calculated instead of a price for each \code{prodID} observed in each month (the \code{time} column gets the Date format: "Year-Month-01"). If the parameter \code{join_outlets} is TRUE, then the function also performs aggregation over outlets (retIDs) and the \code{retID} column is removed from the data frame. The main advantage of using this function is the ability to reduce the size of the data frame and the time needed to calculate the price index. Please note, that unnecessary columns are removed (e.g. \code{description}).
#' @examples 
#' #Example 1
#' data_aggregating(dataAGGR,join_outlets = FALSE)
#' data_aggregating(dataAGGR,join_outlets = TRUE)
#' #Example 2 (data frame reduction)
#' nrow(milk)
#' nrow(data_aggregating(milk))
#' @export

data_aggregating<-function (data, join_outlets = TRUE)
{
time<-prodID<-retID<-prices2<-quantities2<-NULL
#checking columns
cols<-colnames(data)
if (!("time" %in% cols) | !("prodID" %in% cols)) stop("A data frame must contain columns: time, prodID")
if ((join_outlets==FALSE) & !("retID" %in% cols)) stop("A date frame must contain the 'retID' column")
#main body
data$time<-as.character(data$time)
data$time<-substr(data$time,0,7)
if (join_outlets==TRUE) data_aggr<-dplyr::summarise(dplyr::group_by(data, time, prodID), prices2=sum(prices*quantities)/sum(quantities),quantities2=sum(quantities),.groups="drop")
else data_aggr<-dplyr::summarise(dplyr::group_by(data, time, prodID, retID), prices2=sum(prices*quantities)/sum(quantities),quantities2=sum(quantities),.groups="drop")
data_aggr$time<-paste(data_aggr$time,"-01",sep="")
data_aggr$time<-as.Date(data_aggr$time)
data_aggr<-dplyr::rename(data_aggr, prices=prices2, quantities=quantities2)
return (data_aggr)
}

#' @title  Calculating the elasticity of substitution 
#'
#' @description This function returns a value of the elasticity of substitution 
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities} (as positive numeric) and \code{prodID} (as numeric, factor or character). 
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param method The index formula for which the CES index will be equated to calculate the elasticity. Acceptable options are \code{lm}, \code{f} and \code{sv}.
#' @param left The beginning of an interval for estimation of the elasticity of substitution (its default value is -10).
#' @param right The end of an interval for estimation of the elasticity of substitution (its default value is 10).
#' @param precision The precision of estimation (a 'stop' condition for the procedure). A default value of this parameter is 0.000001.
#' @rdname elasticity
#' @return This function returns a value of the elasticity of substitution. If the \code{method} parameter is set to \code{lm}, the procedure of estimation solves the equation: LM(sigma)-CW(sigma)=0 numerically, where LM denotes the Lloyd-Moulton price index, the CW denotes a current weight counterpart of the Lloyd-Moulton price index, and sigma is the elasticity of substitution parameter, which is estimated. If the \code{method} parameter is set to \code{f}, the Fisher price index formula is used instead of the CW price index. If the \code{method} parameter is set to \code{sv}, the Sato-Vartia price index formula is used instead of the CW price index.The procedure continues until the absolute value of this difference is greater than the value of the 'precision' parameter.    
#' @references
#' {de Haan, J., Balk, B.M., Hansen, C.B. (2010). \emph{Retrospective Approximations of Superlative Price Indexes for Years Where Expenditure Data Is Unavailable.} In: Biggeri, L., Ferrari, G. (eds) Price Indexes in Time and Space. Contributions to Statistics. Physica-Verlag HD.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' \donttest{elasticity(coffee, start = "2018-12", end = "2019-01")}
#' \donttest{elasticity(coffee, start = "2018-12", end = "2019-01", method = "f")}
#' \donttest{elasticity(coffee, start = "2018-12", end = "2019-01", method = "sv")}
#' @export

elasticity<-function (data, start, end, method = "lm", left = -10, right = 10, precision = 0.000001)
{
  if (nrow(data)==0) stop("A data frame is empty!")
  if (right<=left) stop("Bad specification of 'left' and 'right' parameters!")
  if (precision<=0 | precision>0.5) stop("'precision' should be a small, positive number!")
  av_methods<-c("lm","f","sv")
  if (!(method %in% av_methods)) stop("Available options for the 'method' parameter are: 'lm', 'f' or 'sv'.")
  superlative<-function (sigma) {
  if (method=="lm") return (lm(data, start, end, sigma)-cw(data, start, end, sigma))
  if (method=="f") return (lm(data, start, end, sigma)-fisher(data, start, end))
  if (method=="sv") return (lm(data, start, end, sigma)-sato_vartia(data, start, end))
  }
  if (superlative(left)*superlative(right)>0) stop("There is no solution in the given interval!")
  ll=left
  pp=right
  x0=(ll+pp)/2
  while (abs(superlative(x0))>precision) {
                                      if (superlative(ll)*superlative(x0)>0) ll=x0
                                      else pp=x0
                                      x0=(ll+pp)/2
                                      }
  return (x0)
}


#' @title  Presenting elasticities of substitution for time interval
#'
#' @description The function provides a data frame or a figure presenting elasticities of substitution calculated for time interval.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities} (as positive numeric) and \code{prodID} (as numeric, factor or character). 
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param method A vector indicating index formulas for which the CES index will be equated to calculate the elasticity. Acceptable options are \code{lm}, \code{f} and \code{sv} or their combinations.
#' @param fixedbase A logical parameter indicating whether the procedure is to work for subsequent months from the considered time interval (\code{fixedbase}=FALSE). Otherwise the period defined by \code{start} plays a role of fixed base month (\code{fixedbase}=TRUE)
#' @param figure A logical parameter indicating whether the function returns a figure (TRUE) or a data frame (FALSE) with values of elasticity of substitution.
#' @param date_breaks A string giving the distance between breaks on the X axis like "1 month" (default value) or "4 months".
#' @param names A character string indicating names of indices used for elasticity approximation (see the \code{method} parameter).
#' @param left The beginning of an interval for estimation of each elasticity of substitution (its default value is -10)
#' @param right The end of an interval for estimation of each elasticity of substitution (its default value is 10)
#' @param precision The precision of estimation (a 'stop' condition for the procedure). A default value of this parameter is 0.000001.
#' @rdname elasticity_fig
#' @return The function provides a data frame or a figure presenting elasticities of substitution calculated for time interval (see the \code{figure} parameter). The elasticities of substitution can be calculated for subsequent months or for a fixed base month (see the \code{start} parameter) and rest of months from the given time interval (it depends on the \code{fixedbase} parameter). The above-mentioned parameters for compared months are calculated by using the \code{elasticity} function.    
#' @references
#' {de Haan, J., Balk, B.M., Hansen, C.B. (2010). \emph{Retrospective Approximations of Superlative Price Indexes for Years Where Expenditure Data Is Unavailable.} In: Biggeri, L., Ferrari, G. (eds) Price Indexes in Time and Space. Contributions to Statistics. Physica-Verlag HD.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' \donttest{elasticity_fig (milk,start="2018-12",end="2019-04",figure=TRUE, 
#' method=c("lm","f","sv"),names=c("LM","Fisher", "SV"))}
#' \donttest{elasticity_fig (milk,start="2018-12",end="2019-12",figure=FALSE)}
#' @export

elasticity_fig<-function(data, start, end, method = c("lm"), fixedbase = TRUE, figure = TRUE, date_breaks = "1 month", names=c(), left = -10, right = 10, precision = 0.000001)
{
value<-NULL
formula<-NULL
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
if (end<=start) stop ("Bad specification of dates!")
if (length(names)>0) if (!(length(names)==length(method))) stop ("Parameters 'method' and 'names' must have identical length!")
#vector of elasticities  
el<-c()
#vector of dates
date <- seq.Date(from = start, to = end, by = "month")
date <- format(date, format = "%Y-%m")
df<-data.frame(date=date[2:length(date)])
nm<-length(method) #number of methods
if (fixedbase == TRUE) {for (k in 1:nm)
  {el<-c()
  for (i in 2:length(date)) el<-c(el,elasticity(data, start = date[1], end = date[i], method = method[k], left = left, right = right, precision = precision))
  df[,k+1]<-el
  }}
else {for (k in 1:nm)
  {el<-c()
  for (i in 2:length(date)) el<-c(el,elasticity(data, start = date[i-1], end = date[i], method = method[k], left = left, right = right, precision = precision))
  df[,k+1]<-el 
}}
if (length(names)==0) colnames(df)<-c("date",method)
else colnames(df)<-c("date",names)
if (figure == FALSE) return (df)
else {
df$date<-as.Date(paste(df$date,"-01",sep = ""))
df<-reshape::melt(df, id.var = 'date') 
colnames(df)<-c("date","formula","value")
fig<-ggplot2::ggplot(df, ggplot2::aes(x = date, y = value, col = formula)) + ggplot2::geom_point()+ggplot2::geom_line()+ggplot2::labs(x = "date",y = "elasticity of substitution")+ggplot2::scale_x_date(date_labels = "%Y %m",date_breaks = date_breaks)+ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))   
return (fig)  
     }
}


