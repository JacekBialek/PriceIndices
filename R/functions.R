

#' @title  Preparing a data set for further data processing or price index calculations
#'
#' @description This function returns a prepared data frame based on the user's data set. The resulting data frame is ready for further data processing (such as data selecting, matching or filtering) and it is also ready for price index calculations (if only it contains required columns).
#'
#' @param data The user's data frame to be prepared. The user must indicate columns: \code{time} (as Date or character type, allowed formats are, eg.: `2020-03` or `2020-12-28`), \code{prices} and \code{quantities} (as numeric). Optionally, the user may also indicate columns: \code{prodID}, \code{codeIN}, \code{codeOUT}, \code{retID} (as numeric, factor or character), \code{description} (as character) and other columns specified by the \code{additional} parameter.
#' @param time A character name of the column which provides transaction dates.
#' @param prices A character name of the column which provides product prices. 
#' @param quantities A character name of the column which provides product quantities.
#' @param prodID  A character name of the column which provides product IDs. The \code{prodID} column should include unique product IDs used for product matching (as numeric or character). It is not obligatory to consider this column while data preparing but it is required while price index calculating (to obtain it, please see \code{\link{data_matching}}). 
 #' @param retID A character name of the column which provides outlet IDs (retailer sale points). The \code{retID} column should include unique outlet IDs used for aggregating subindices over outlets. It is not obligatory to consider this column while data preparing but it is required while final price index calculating (to obtain it, please see the \code{\link{final_index}} or \code{\link{final_index2}} function).
#' @param description A character name of the column which provides product descriptions. It is not obligatory to consider this column while data preparing but it is required while product selecting (please see the \code{\link{data_selecting}} function).
#' @param codeIN A character name of the column which provides internal product codes (from the retailer). It is not obligatory to consider this column while data preparing but it may be required while product matching (please see the \code{\link{data_matching}} function).
#' @param codeOUT A character name of the column which provides external product codes (e.g. GTIN or SKU). It is not obligatory to consider this column while data preparing but it may be required while product matching (please see the \code{\link{data_matching}} function).
#' @param additional A character vector of names of additional columns to be considered while data preparing (records with missing values are deleted).
#' @rdname data_preparing
#' @return The resulting data frame is free from missing values, zero or negative prices and quantities. As a result, column \code{time} is set to be Date type (in format: `Year-Month-01`), columns \code{prices} and \code{quantities} are set to be numeric. If the column \code{description} is selected, then it is set to be character type. If columns: \code{prodID}, \code{retID}, \code{codeIN} or  \code{codeOUT} are selected, then they are set to be factor type.
#'
#' @examples 
#' data_preparing(milk, time="time",prices="prices",quantities="quantities")
#' data_preparing(dataCOICOP, time="time",
#' prices="prices",quantities="quantities",additional="coicop")
#' @export

data_preparing<-function(data, time=NULL, prices=NULL, quantities=NULL, prodID=NULL, retID=NULL, description=NULL, codeIN=NULL, codeOUT=NULL, additional=c())
{
if (nrow(data)==0) stop("A data frame is empty")
variables<-c()
cn<-colnames(data)

#checking obligatory columns
if ((length(time)==0) | (length(prices)==0) | (length(quantities)==0)) stop ("Columns: time, prices and quantities must be specified!")
if (!(time %in% cn)) stop ("Bad specification of the 'time' column!")
colnames(data)[which(names(data) == time)] <- "time" 
data$time<-as.character(data$time)
#checking if there is a format "Year-Month". If yes it is transformed to "Year-Month-01" (with 'Day')
if  (nchar(data$time[1])==7) data$time<-paste(data$time,"-01",sep="")
data$time<-as.Date(data$time)
variables<-c(variables, "time")

if (!(prices %in% cn)) stop ("Bad specification of the 'prices' column!")
colnames(data)[which(names(data) == prices)] <- "prices" 
if (!(is.numeric(data$prices))) data$prices<-as.numeric(data$prices)
variables<-c(variables, "prices")

if (!(quantities %in% cn)) stop ("Bad specification of the 'quantities' column!")
colnames(data)[which(names(data) == quantities)] <- "quantities" 
if (!(is.numeric(data$quantities))) data$quantities<-as.numeric(data$quantities)
variables<-c(variables, "quantities")
  
#checking additional columns
if (length(prodID)>0) {
  if (!(prodID %in% cn)) stop ("Bad specification of the 'prodID' column!")
  colnames(data)[which(names(data) == prodID)] <- "prodID" 
  if (!(is.factor(data$prodID))) data$prodID<-as.factor(data$prodID)
  variables<-c(variables, "prodID")
                    }
if (length(retID)>0) {
  if (!(retID %in% cn)) stop ("Bad specification of the 'retID' column!")
  colnames(data)[which(names(data) == retID)] <- "retID" 
  if (!(is.factor(data$retID))) data$retID<-as.factor(data$retID)
  variables<-c(variables, "retID")
                    }
if (length(description)>0) {
  if (!(description %in% cn)) stop ("Bad specification of the 'description' column!")
  colnames(data)[which(names(data) == description)] <- "description" 
  if (!(is.character(data$description))) data$description<-as.character(data$description)
  variables<-c(variables, "description")
                    }
if (length(codeIN)>0) {
  if (!(codeIN %in% cn)) stop ("Bad specification of the 'codeIN' column!")
  colnames(data)[which(names(data) == codeIN)] <- "codeIN" 
  if (!(is.factor(data$codeIN))) data$codeIN<-as.factor(data$codeIN)
  variables<-c(variables, "codeIN")
                    }
if (length(codeOUT)>0) {
  if (!(codeOUT %in% cn)) stop ("Bad specification of the 'codeOUT' column!")
  colnames(data)[which(names(data) == codeOUT)] <- "codeOUT" 
  if (!(is.factor(data$codeOUT))) data$codeOUT<-as.factor(data$codeOUT)
  variables<-c(variables, "codeOUT")
                       }
variables<-c(variables, additional)
data<-dplyr::select(data, variables)

#filtering
data<-stats::na.omit(data)
data<-dplyr::filter(data,data$prices>0 & data$quantities>0)
return(data)
}


#' @title  Matching products 
#'
#' @description This function returns a data set defined in the first parameter (\code{data}) with an additional column (\code{prodID}). Two products are treated as being matched if they have the same \code{prodID} value.  
#' @param data The user's data frame with information about products to be matched. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01') and at least one of the following columns: \code{codeIN} (as numeric or character), \code{codeOUT} (as numeric or character) and \code{description} (as character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the matching process concerns only two periods defined by \code{start} and \code{end} parameters (then the \code{interval} is set to FALSE) or whether that function is to match products sold during the whole time interval <start, end>. 
#' @param variables The optional parameter describing the vector of additional column names. Values of these additional columns must be identical for matched products. 
#' @param codeIN A logical value, e.g. if there are retailer (internal) product codes (as numeric or character) written in \code{codeIN} column and there is a need to use that column while data matching, then that parameter should be set to TRUE. Otherwise it is set to FALSE.
#' @param codeOUT A logical value, e.g. if there are external product codes, such as GTIN or SKU (as numeric or character) written in \code{codeOUT} column and there is a need to use that column while data preparing then, that parameter should be set to TRUE. Otherwise it is set to FALSE.
#' @param  description A logical value, e.g. if there are product labels (as character) written in \code{description} column and there is a need to use that column while data preparing, then that parameter should be set to TRUE. Otherwise it is set to FALSE.
#' @param  onlydescription A logical value indicating whether products with identical labels (described in the \code{description}) are to be matched.
#' @param precision A threshold value for the Jaro-Winkler distance measure when comparing labels (its value must belong to the interval [0,1]). Two labels are treated as similar enough if their Jaro-Winkler distance exceeds the \code{precision} value. 
#' @rdname data_matching
#' @return This function returns a data set defined in the first parameter (\code{data}) with an additional column (\code{prodID}). Two products are treated as being matched if they have the same \code{prodID} value. The procedure of generating the above-mentioned additional column depends on the set of chosen columns for matching. In most extreme case, when the \code{onlydescription} parameter value is TRUE, two products are also matched if they have identical descriptions. Other cases are as follows: \code{Case 1}: Parameters \code{codeIN}, \code{codeOUT} and \code{description} are set to TRUE. Products with two identical codes or one of the codes identical and an identical \code{description} are automatically matched. Products are also matched if they have identical one of codes and the Jaro-Winkler distance of their descriptions is bigger than the \code{precision} value.\code{Case 2}: Only one of the parameters: \code{codeIN} or \code{codeOUT} are set to TRUE and also the \code{description} parameter is set to TRUE. Products with an identical chosen code and an identical description are automatically matched. In the second stage, products are also matched if they have an identical chosen code and the Jaro-Winkler distance of their descriptions is bigger than the \code{precision} value. \code{Case 3}: Parameters \code{codeIN} and \code{codeOUT} are set to TRUE and the parameter \code{description} is set to FALSE. In this case, products are matched if they have both codes identical. \code{Case 4}: Only the parameter \code{description} is set to TRUE. This case requires the \code{onlydescription} parameter to be TRUE and then the matching process is based only on product labels (two products are matched if they have identical descriptions). \code{Case 5}:  Only one of the parameters: \code{codeIN} or \code{codeOUT} are set to TRUE and the \code{description} parameter is set to FALSE. In this case, the only reasonable option is to return the \code{prodID} column which is identical with the chosen code column. Please note that if the set of column names defined in the \code{variables} parameter is not empty, then the values of these additional columns must be identical while product matching.
#' @examples 
#' data_matching(dataMATCH, start="2018-12",end="2019-02",onlydescription=TRUE,interval=TRUE)
#' data_matching(dataMATCH, start="2018-12",end="2019-02",precision=0.98, interval=TRUE)
#' 
#' @export

data_matching<-function(data, start, end, interval=FALSE, variables=c(),codeIN=TRUE, codeOUT=TRUE, description=TRUE, onlydescription=FALSE, precision=0.95)
{
if (nrow(data)==0) stop("A data frame is empty")
#checking condition for 'precision'  
if ((precision<0) | (precision>1)) stop("parametr 'precision' must belong to [0,1]")
 

#preparing data set 
columns<-c()
start<-paste(start,"-01",sep="")
end<-paste(end,"-01",sep="")
start<-as.Date(start)
end<-as.Date(end)
lubridate::day(end)<-lubridate::days_in_month(end)
if (interval==TRUE) data<-dplyr::filter(data, data$time>=start & data$time<=end)
else data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))
data<-stats::na.omit(data) 
#original dataset
data_oryginal<-data
if (description==TRUE) {data$description<-as.character(data$description)
data$descriptionID<-data$description
columns<-c(columns, "descriptionID")
                       }
if (length(columns)==0) stop("At least one column for matching must be selected!")
#reducing a dataset
if (codeIN==TRUE) columns<-c(columns, "codeIN")
if (codeOUT==TRUE) columns<-c(columns, "codeOUT")
if (description==TRUE) columns<-c(columns, "description")
if (length(variables)>0) columns<-c(columns, variables)

data<-dplyr::select(data,columns)
data<-dplyr::distinct(data)

#main body
if (codeIN==TRUE & codeOUT==TRUE & description==TRUE)
{
pairs<-reclin::pair_blocking(data,data,blocking_var = variables)
pairs<-reclin::filter_pairs_for_deduplication(pairs)
pairs<-reclin::compare_pairs(pairs, by="descriptionID")
pairs<-reclin::compare_pairs(pairs, by="description", default_comparator = reclin::jaro_winkler())
pairs<-reclin::compare_pairs(pairs, by="codeOUT")
pairs<-reclin::compare_pairs(pairs, by="codeIN")
pairs$simsum<-pairs$descriptionID*pairs$codeOUT+pairs$descriptionID*pairs$codeIN+pairs$codeOUT*pairs$codeIN+pairs$description*pairs$codeOUT+pairs$description*pairs$codeIN+onlydescription*pairs$descriptionID
pairs<-reclin::select_threshold(pairs, precision, weight="simsum",var="select")
pairs<-reclin::deduplicate_equivalence(pairs, selection="select",var="prodID")
pairs$descriptionID<-NULL
}
else if (codeIN==TRUE & codeOUT==FALSE & description==TRUE)
{
pairs<-reclin::pair_blocking(data,data,blocking_var = variables)
pairs<-reclin::filter_pairs_for_deduplication(pairs)
pairs<-reclin::compare_pairs(pairs, by="descriptionID")
pairs<-reclin::compare_pairs(pairs, by="description", default_comparator = reclin::jaro_winkler())
pairs<-reclin::compare_pairs(pairs, by="codeIN")
pairs$simsum<-pairs$descriptionID*pairs$codeIN+pairs$description*pairs$codeIN+onlydescription*pairs$descriptionID
pairs<-reclin::select_threshold(pairs, precision, weight="simsum",var="select")
pairs<-reclin::deduplicate_equivalence(pairs, selection="select",var="prodID")
pairs$descriptionID<-NULL
}
else if (codeIN==FALSE & codeOUT==TRUE & description==TRUE)
{
pairs<-reclin::pair_blocking(data,data,blocking_var = variables)
pairs<-reclin::filter_pairs_for_deduplication(pairs)
pairs<-reclin::compare_pairs(pairs, by="descriptionID")
pairs<-reclin::compare_pairs(pairs, by="description", default_comparator = reclin::jaro_winkler())
pairs<-reclin::compare_pairs(pairs, by="codeOUT")
pairs$simsum<-pairs$descriptionID*pairs$codeOUT+pairs$description*pairs$codeOUT+onlydescription*pairs$descriptionID
pairs<-reclin::select_threshold(pairs, precision, weight="simsum",var="select")
pairs<-reclin::deduplicate_equivalence(pairs, selection="select",var="prodID")
pairs$descriptionID<-NULL
}
else if (codeIN==TRUE & codeOUT==TRUE & description==FALSE)
{
pairs<-reclin::pair_blocking(data,data,blocking_var = variables)
pairs<-reclin::filter_pairs_for_deduplication(pairs)
pairs<-reclin::compare_pairs(pairs, by="codeIN")
pairs<-reclin::compare_pairs(pairs, by="codeOUT")
pairs$simsum<-(pairs$codeIN*pairs$codeOUT)
pairs<-reclin::select_threshold(pairs, 0.5, weight="simsum",var="select")
pairs<-reclin::deduplicate_equivalence(pairs, selection="select",var="prodID")
}
else if (codeIN==FALSE & codeOUT==FALSE & description==TRUE)
{
  if (onlydescription==TRUE) 
  {
pairs<-reclin::pair_blocking(data,data,blocking_var = variables)
pairs<-reclin::filter_pairs_for_deduplication(pairs)
pairs<-reclin::compare_pairs(pairs, by="descriptionID")
pairs$simsum<-pairs$descriptionID
pairs<-reclin::select_threshold(pairs, 0.5, weight="simsum",var="select")
pairs<-reclin::deduplicate_equivalence(pairs, selection="select",var="prodID")
pairs$descriptionID<-NULL 
  }
  else stop("Parametr 'onlydescription' must be TRUE to start matching process")
}
else if (codeIN==TRUE & codeOUT==FALSE & description==FALSE) {pairs<-data
                                                              pairs$prodID<-pairs$codeIN
                                                             }
else if (codeIN==FALSE & codeOUT==TRUE & description==FALSE) {pairs<-data
                                                              pairs$prodID<-pairs$codeOUT
                                                             }
else if (codeIN==FALSE & codeOUT==FALSE & description==FALSE) stop("at least one of parameters: codeIN, codeOUT or description must be TRUE")

#pairs - new dataframe with reduced dataframe with matched products (additional column: prodID)
#now, let us back to the oryginal dataset, i.e. 'data_oryginal'

#names of columns which are considered in matching process
columns<-colnames(dplyr::select(pairs, -prodID))

match<-function (i) {df<-data_oryginal[i,]
                     df2<-pairs
                     for (k in 1:length(columns)) df2<-dplyr::filter(df2,df2[,columns[k]]==df[,columns[k]])
                     return (df2$prodID)
                    }
prodID<-sapply(seq(1,nrow(data_oryginal)),match)
data_oryginal$prodID<-prodID

return (data_oryginal)  
}


#filtering where only two months are compared
filtering<-function(data, start, end, filters=c(), plimits=c(),pquantiles=c(), dplimits=c(),lambda=1.25)
{
 if (nrow(data)==0) stop("A data frame is empty")
 
 start<-paste(start,"-01",sep="") 
 start<-as.Date(start)
 start2<-start
 end<-paste(end,"-01",sep="") 
 end<-as.Date(end)
 end2<-end
 lubridate::day(start2)<-lubridate::days_in_month(start2)
 lubridate::day(end2)<-lubridate::days_in_month(end2)
 data<-dplyr::filter(data, (data$time>=start & data$time<=start2) | (data$time>=end & data$time<=end2))
 filter1<-"extremeprices"
 filter2<-"lowsales"
 filter3<-"dumpprices"
 afilters<-c(filter1, filter2,filter3)
 if ((start==end) | length(filters)==0) return (data)
 else if (length(base::intersect(filters,afilters))==0) stop("there are no such filters")
 if (length(base::setdiff(filters, base::intersect(filters, afilters)))>0)  stop("At least one filter's name is wrong")
 data1<-data[0:0,] 
 data2<-data[0:0,]
 data3<-data[0:0,]
 data4<-data[0:0,]
 if (filter1 %in% filters) {  if ((length(pquantiles)+length(plimits))==0) data1<-data
                                                     else {id<-matched(data,start,end)
                                                     priceshares<-c()
                                                     for (i in 1:length(id))        priceshares<-c(priceshares,price(data,period=end,ID=id[i])/price(data,period=start,ID=id[i]))
                                                          }
                                                       if (length(pquantiles)>0) 
                                                        {
                                                       tresh<-c(0,1)
                                                       if ((pquantiles[1]==tresh[1]) & (pquantiles[2]==tresh[2])) {
                                                       data1<-data
                                                        }
                                                        else {
                                                        qq<-stats::quantile(priceshares,probs=pquantiles,names=FALSE)
                                                        #selecting the sample by checking condition
                                                        id1<-c()
                                                        for (i in 1:length(id)) 
                                                        if ((priceshares[i]>=qq[1]) & (priceshares[i]<=qq[2]))  id1<-c(id1,id[i])  
                                                        data1<-dplyr::filter(data, data$prodID %in% id1)
                                                        }
                                                        } else data1<-data
                                                       if (length(plimits)>0)
                                                       {
                                                        #selecting the sample by chacking condition
                                                        id2<-c()
                                                        for (i in 1:length(id)) 
                                if ((priceshares[i]>=plimits[1]) & (priceshares[i]<=plimits[2])) id2<-c(id2,id[i])  
                                                        data2<-dplyr::filter(data, data$prodID %in% id2)
                                                        } else data2<-data
                                                   } else
                                                   {data1<-data
                                                    data2<-data 
                                                   }
if (filter2 %in% filters) { if (lambda<=0) data3<-data
                                                     id<-matched(data,start,end)
                                                     expenditures_start<-sales(data, period=start, set=id)
                                                     expenditures_end<-sales(data, period=end, set=id)
                                                     sum_start<-sum(expenditures_start)
                                                     sum_end<-sum(expenditures_end)
                                                     id3<-c()
                                                     for (i in 1:length(id)) 
                                                     if (0.5*((expenditures_start[i]/sum_start)+(expenditures_end[i]/sum_end))>(1/(length(id)*lambda)))                                                          id3<-c(id3,id[i])
                                                     data3<-dplyr::filter(data, data$prodID %in% id3)
                                                   } else data3<-data

if (filter3 %in% filters) { if (!(length(dplimits)==2)) data4<-data
                            else {
                                                     id<-matched(data,start,end)
                                                     expenditures_start<-sales(data, period=start, set=id)
                                                     expenditures_end<-sales(data, period=end, set=id)
                                                     priceshares<-c()
                                                     for (i in 1:length(id))        priceshares<-c(priceshares,price(data,period=end,ID=id[i])/price(data,period=start,ID=id[i]))
                                                     id4<-c()
                                                     for (i in 1:length(id)) 
                                                     if
((priceshares[i]>=dplimits[1]) | ((expenditures_end[i]/expenditures_start[i])>=dplimits[2]))                                                                                 id4<-c(id4,id[i])
                                                     data4<-dplyr::filter(data, data$prodID %in% id4)
                                 }
                                                   } else data4<-data 
 
 
data_final<-dplyr::intersect(data1,data2)
data_final<-dplyr::intersect(data_final,data3)
data_final<-dplyr::intersect(data_final,data4)
return (data_final)   
   }

#filtering where each subsequent months from the considered time interval are compared
filtering_interval<-function(data, start, end, filters=c(), plimits=c(),pquantiles=c(), dplimits=c(),lambda=1.25)
{
 if (nrow(data)==0) stop("A data frame is empty")
 start<-paste(start,"-01",sep="") 
 start<-as.Date(start)
 end<-paste(end,"-01",sep="")
 end<-as.Date(end)
 start2<-as.Date(start)
 if (start==end) {data<-dplyr::filter(data, (lubridate::year(data$time)==lubridate::year(start)) & (lubridate::month(data$time)==lubridate::month(start)))
                  return (data)
                 }
 lubridate::month(start2)<-lubridate::month(start2)+1 
 data_set<-data[0:0,]
 while (start<end)
 {
   d<-filtering(data,start,start2,filters,plimits,pquantiles,dplimits,lambda)
   data_set<-dplyr::union(data_set,d)
   lubridate::month(start)<-lubridate::month(start)+1
   lubridate::month(start2)<-lubridate::month(start2)+1
 }
 return (data_set)  
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
#' data_filtering(milk,start="2018-12",end="2019-03",
#' filters=c("extremeprices"),pquantiles=c(0.01,0.99),interval=TRUE)
#' data_filtering(milk,start="2018-12",end="2019-03",
#' filters=c("extremeprices","lowsales"), plimits=c(0.25,2))
#' @export

data_filtering<-function(data, start, end, filters=c(), plimits=c(),pquantiles=c(), dplimits=c(),lambda=1.25, interval=FALSE, retailers=FALSE)
{ 
  if (nrow(data)==0) stop("A data frame is empty") 
  if (retailers==FALSE) { if (interval==FALSE) return (filtering(data,start,end,filters,plimits, pquantiles,dplimits, lambda))
                         else return (filtering_interval(data,start,end,filters,plimits, pquantiles,dplimits, lambda))  
                        }
  else { if (interval==FALSE) {ret<-matched(data, period1=start,period2=end, type="retID", interval=FALSE)
                               data_set<-data[0:0,]
                               for (i in (1:length(ret))) {rs<-dplyr::filter(data, data$retID==ret[i])
                                                           d<-filtering(rs,start,end,filters,plimits, pquantiles,dplimits,lambda)
                                                           data_set<-dplyr::union(data_set,d)
                                                          }
                              }
         else {                ret<-matched(data, period1=start,period2=end, type="retID", interval=TRUE)
                               data_set<-data[0:0,]
                               for (i in (1:length(ret))) {rs<-dplyr::filter(data, data$retID==ret[i])
                                                          d<-filtering_interval(rs,start,end,filters,plimits, pquantiles,dplimits,lambda)
                                                           data_set<-dplyr::union(data_set,d)
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

data_selecting<-function(data, include=c(), must=c(), exclude=c(), sensitivity=TRUE,coicop=NULL)
{
 if (nrow(data)==0) stop("A data frame is empty")
 if (sensitivity==TRUE) data$description<-tolower(data$description)
 if (length(must)==0) set3<-data 
 else
 {if (sensitivity==TRUE) must<-tolower(must)
 set3<-dplyr::filter(data, stringr::str_detect(data$description, must[1]))
 if (length(must)>1) for (i in 2: length(must)) set3<-dplyr::intersect(set3,dplyr::filter(data,     stringr::str_detect(data$description, must[i])))
 }
 if (length(include)==0) set1<-data
 else
 { if (sensitivity==TRUE) include<-tolower(include) 
 set1<-dplyr::filter(data, stringr::str_detect(data$description, include[1]))
  if (length(include)>1) for (i in 2: length(include)) set1<-dplyr::union(set1,dplyr::filter(data, stringr::str_detect(data$description, include[i])))
 }
 if (length(exclude)==0) set<-set1
 else
   {if (sensitivity==TRUE) exclude<-tolower(exclude)
   set2<-dplyr::filter(data, stringr::str_detect(data$description, exclude[1]))
   if (length(exclude)>1) for (i in 2: length(exclude)) set2<-dplyr::union(set2,dplyr::filter(data, stringr::str_detect(data$description, exclude[i])))
   set<-dplyr::setdiff(set1,set2)
   }
 new_set<-dplyr::intersect(set,set3)
 if (length(coicop)>0) new_set$coicop<-coicop
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


matched<-function(data,period1,period2, type="prodID", interval=FALSE) {
 atype<-c("retID","prodID","codeIN", "codeOUT", "description") #allowed values for 'type' parameter
 if (!(type %in% atype)) stop ("The 'type' parameter has a wrong value")
 if (nrow(data)==0) stop("A data frame is empty")
 period1<-paste(period1,"-01",sep="") 
 period1<-as.Date(period1)
 period2<-paste(period2,"-01",sep="")                        
 period2<-as.Date(period2)  
 #main body
 if (type=="prodID") 
 {  
if (interval==FALSE) set<-base::intersect(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$prodID, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$prodID)
else
{
set<-base::intersect(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$prodID, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$prodID)
                                  start<-min(period1,period2)
                                  end<-max(period1,period2)
                                  while (start<end)
                                       {start2<-start
                                       lubridate::month(start2)<-lubridate::month(start2)+1
set<-base::intersect(set,base::intersect(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start))$prodID,dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start2) & lubridate::month(data$time)==lubridate::month(start2))$prodID))
                                        lubridate::month(start)<-lubridate::month(start)+1
                                       }   
}
}  
if (type=="retID") 
 {  
if (interval==FALSE) set<-base::intersect(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$retID, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$retID)
else
{
set<-base::intersect(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$retID, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$retID)
                                  start<-min(period1,period2)
                                  end<-max(period1,period2)
                                  while (start<end)
                                       {start2<-start
                                       lubridate::month(start2)<-lubridate::month(start2)+1
set<-base::intersect(set,base::intersect(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start))$retID,dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start2) & lubridate::month(data$time)==lubridate::month(start2))$retID))
                                       lubridate::month(start)<-lubridate::month(start)+1
                                       }   
}
}
if (type=="codeIN") 
 {  
if (interval==FALSE) set<-base::intersect(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$codeIN, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$codeIN)
else
{
set<-base::intersect(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$codeIN, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$codeIN)
                                  start<-min(period1,period2)
                                  end<-max(period1,period2)
                                  while (start<end)
                                       {start2<-start
                                       lubridate::month(start2)<-lubridate::month(start2)+1
 set<-base::intersect(set,base::intersect(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start))$codeIN,dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start2) & lubridate::month(data$time)==lubridate::month(start2))$codeIN))
                                       lubridate::month(start)<-lubridate::month(start)+1
                                       }   
}
}
if (type=="codeOUT") 
 {  
if (interval==FALSE) set<-base::intersect(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$codeOUT, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$codeOUT)
else
{
set<-base::intersect(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$codeOUT, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$codeOUT)
                                  start<-min(period1,period2)
                                  end<-max(period1,period2)
                                  while (start<end)
                                       {start2<-start
                                       lubridate::month(start2)<-lubridate::month(start2)+1
set<-base::intersect(set,base::intersect(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start))$codeOUT,dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start2) & lubridate::month(data$time)==lubridate::month(start2))$codeOUT))
                                       lubridate::month(start)<-lubridate::month(start)+1
                                       }   
}
} 
if (type=="description") 
 {  
if (interval==FALSE) set<-base::intersect(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$description, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$description)
else
{
set<-base::intersect(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$description, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$description)
                                  start<-min(period1,period2)
                                  end<-max(period1,period2)
                                  while (start<end)
                                       {start2<-start
                                       lubridate::month(start2)<-lubridate::month(start2)+1
set<-base::intersect(set,base::intersect(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start))$description,dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start2) & lubridate::month(data$time)==lubridate::month(start2))$description))
                                       lubridate::month(start)<-lubridate::month(start)+1
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


 available<-function(data,period1,period2, type="prodID", interval=FALSE) {
 atype<-c("retID","prodID","codeIN", "codeOUT", "description") #allowed values for 'type' parameter
 if (!(type %in% atype)) stop ("The 'type' parameter has a wrong value")
 if (nrow(data)==0) stop("A data frame is empty")
 period1<-paste(period1,"-01",sep="") 
 period1<-as.Date(period1)
 period2<-paste(period2,"-01",sep="")                        
 period2<-as.Date(period2)  
 #main body
 if (type=="prodID") 
 {  
if (interval==FALSE) set<-base::union(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$prodID, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$prodID)
else
{
set<-base::union(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$prodID, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$prodID)
                                  start<-min(period1,period2)
                                  end<-max(period1,period2)
                                  while (start<end)
                                       {start2<-start
                                       lubridate::month(start2)<-lubridate::month(start2)+1
set<-base::union(set,base::union(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start))$prodID,dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start2) & lubridate::month(data$time)==lubridate::month(start2))$prodID))
                                       lubridate::month(start)<-lubridate::month(start)+1
                                       }   
}
}  
if (type=="retID") 
 {  
if (interval==FALSE) set<-base::union(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$retID, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$retID)
else
{
set<-base::union(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$retID, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$retID)
                                  start<-min(period1,period2)
                                  end<-max(period1,period2)
                                  while (start<end)
                                       {start2<-start
                                       lubridate::month(start2)<-lubridate::month(start2)+1
set<-base::union(set,base::union(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start))$retID,dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start2) & lubridate::month(data$time)==lubridate::month(start2))$retID))
                                        lubridate::month(start)<-lubridate::month(start)+1
                                       }   
}
}
if (type=="codeIN") 
 {  
if (interval==FALSE) set<-base::union(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$codeIN, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$codeIN)
else
{
set<-base::union(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$codeIN, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$codeIN)
                                  start<-min(period1,period2)
                                  end<-max(period1,period2)
                                  while (start<end)
                                      {start2<-start
                                       lubridate::month(start2)<-lubridate::month(start2)+1
set<-base::union(set,base::union(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start))$codeIN,dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start2) & lubridate::month(data$time)==lubridate::month(start2))$codeIN))
                                       lubridate::month(start)<-lubridate::month(start)+1
                                       }   
}
} 
if (type=="codeOUT") 
 {  
if (interval==FALSE) set<-base::union(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$codeOUT, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$codeOUT)
else
{
set<-base::union(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$codeOUT, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$codeOUT)
                                  start<-min(period1,period2)
                                  end<-max(period1,period2)
                                  while (start<end)
                                       {start2<-start
                                       lubridate::month(start2)<-lubridate::month(start2)+1
set<-base::union(set,base::union(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start))$codeOUT,dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start2) & lubridate::month(data$time)==lubridate::month(start2))$codeOUT))
                                        lubridate::month(start)<-lubridate::month(start)+1
                                       }   
}
}   
if (type=="description") 
 {  
if (interval==FALSE) set<-base::union(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$description, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$description)
else
{
set<-base::union(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period1) & lubridate::month(data$time)==lubridate::month(period1))$description, dplyr::filter(data, lubridate::year(data$time)==lubridate::year(period2) & lubridate::month(data$time)==lubridate::month(period2))$description)
                                  start<-min(period1,period2)
                                  end<-max(period1,period2)
                                  while (start<end)
                                       {start2<-start
                                       lubridate::month(start2)<-lubridate::month(start2)+1
set<-base::union(set,base::union(dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start))$description,dplyr::filter(data, lubridate::year(data$time)==lubridate::year(start2) & lubridate::month(data$time)==lubridate::month(start2))$description))
                                       lubridate::month(start)<-lubridate::month(start)+1
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
#' matched_index(milk, period1="2018-12", period2="2019-12", interval=TRUE)
#' matched_index(milk, period1="2018-12", period2="2019-12", type="retID")
#' @export

matched_index<-function(data,period1,period2, type="prodID", interval=FALSE) {
 atype<-c("retID","prodID","codeIN", "codeOUT", "description") #allowed values for 'type' parameter
 if (!(type %in% atype)) stop ("The 'type' parameter has a wrong value")
                                      if (nrow(data)==0) stop("A data frame is empty")
                                      a<-length(matched(data, period1, period2, type, interval))
                                      b<-length(available(data, period1, period2,type, interval))
                                      return (a/b)
                                                                              }

#' @title Providing a matched_index() function dependant on time
#' 
#' @description The function provides a data frame or a figure presenting the \code{\link{matched_index}} function calculated for the column defined by the \code{type} parameter and for each month from the considered time interval
#' @param data The user's data frame. It must contain a column \code{time} (as Date in format: year-month-day,e.g. '2020-12-01') and also a column indicated by the \code{type} parameter.  
#' @param start The base period (as character) limited to the year and month, e.g. "2019-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2019-04".
#' @param type This parameter defines the column which is used in the procedure. Possible values of the \code{type} parameter are: \code{retID}, \code{prodID}, \code{codeIN}, \code{codeOUT} or \code{description}.
#' @param fixedbase A logical parameter indicating whether the procedure is to work for subsequent months from the considered time interval (\code{fixedbase}=FALSE). Otherwise the month defined by \code{start} plays a role of fixed base month (\code{fixedbase}=TRUE)
#' @param figure A logical parameter indicating whether the function returns a figure (TRUE) or a data frame (FALSE) with \code{\link{matched_index}} values.
#' @rdname matched_fig
#' @return The function returns a data frame or a figure presenting the \code{\link{matched_index}} function calculated for the column defined by the \code{type} parameter and for each month from the considered time interval. The interval is set by \code{start} and \code{end} parameters. The returned object (data frame or figure) depends on the value of \code{figure} parameter. The returned values belong to [0,1].
#' @examples 
#' matched_fig(milk, start="2018-12", end="2019-12")
#' matched_fig(milk, start="2018-12", end="2019-12", figure=FALSE)
#' @export

matched_fig<-function (data, start, end, type="prodID", fixedbase=TRUE, figure=TRUE)
{
date<-fraction<-NULL
atype<-c("retID","prodID","codeIN", "codeOUT", "description") #allowed values for 'type' parameter
if (!(type %in% atype)) stop ("The 'type' parameter has a wrong value")
if (nrow(data)==0) stop("A data frame is empty")
start<-paste(start,"-01",sep="")
end<-paste(end,"-01",sep="")
start<-as.Date(start)
end<-as.Date(end)  
times<-c()
t0<-substr(start,0,7)
times<-c()
values<-c()
while (start<end)
                                    
              {t1<-substr(start,0,7)  
               lubridate::month(start)<-lubridate::month(start)+1 
               t2<-substr(start,0,7)
               times<-c(times,t2)
               if (fixedbase==FALSE) values<-c(values, matched_index(data, period1=t1,period2=t2,type,interval=FALSE))
               else values<-c(values, matched_index(data, period1=t0,period2=t2,type,interval=TRUE))
              }
tab<-data.frame(c(times), c(values))
colnames(tab)<-c("date", "fraction")
if (figure==FALSE) return (tab)  
#returning a figure which is based on 'tab'
else
 {
tab$date<-as.Date(paste(tab$date,"01",sep="-"))
ggplot2::ggplot(tab, ggplot2::aes(x=date, y=fraction)) + ggplot2::geom_point()+ggplot2::geom_line()+ggplot2::labs(x="date",y="fraction")+ggplot2::scale_x_date(date_labels="%Y %m",date_breaks  ="1 month")+ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust = 1))  
 }
}

quantity<-function(data, period, ID)
                                   {
                        if (nrow(data)==0) stop("A data frame is empty")
                        period<-paste(period,"-01",sep="") 
                        period<-as.Date(period)
                        lubridate::day(period)<-1
                        period2<-period
                        lubridate::day(period2)<-lubridate::days_in_month(period2)
                        data<-dplyr::filter(data, data$prodID==ID)
                        data<-dplyr::filter(data, data$time>=period & data$time<=period2)
                        if (nrow(data)==0) stop("There are no data in selected period")
                        return(sum(data$quantities))
                                    }

price<-function(data, period, ID)
                                   {
                        if (nrow(data)==0) stop("A data frame is empty")         
                        period<-paste(period,"-01",sep="") 
                        period<-as.Date(period)
                        lubridate::day(period)<-1
                        period2<-period
                        lubridate::day(period2)<-lubridate::days_in_month(period2)
                        data<-dplyr::filter(data, data$prodID==ID)
                        data<-dplyr::filter(data, data$time>=period & data$time<=period2)
                        if (nrow(data)==0) stop("There are no data in selected period")
                        data$sales<-data$prices*data$quantities
                        return(sum(data$sales)/sum(data$quantities))
                                    }



#' @title  Providing prices (unit values) of sold products
#'
#' @description The function returns prices (unit values) of sold products with given IDs. 
#' @param data The user's data frame. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities} (as positive numeric) and \code{prodID} (as numeric or character) with unique product IDs. 
#' @param period The time period (as character) limited to the year and month, e.g. "2019-03".
#' @param set The set of unique product IDs to be used for determining prices of sold products (see also \code{\link{data_matching}}). If the \code{set} is empty, the function returns prices of all products being available in \code{period}.
#' @rdname prices
#' @return The function analyzes the user's data frame and returns prices (unit value) of products with given \code{ID} and being sold in the time period indicated by the \code{period} parameter.
#' @examples 
#' prices(milk, period="2019-06")
#' prices(milk, period="2019-12",set=c(400032, 71772, 82919))
#' @export

prices<-function(data, period, set=c())
                                  {
  if (nrow(data)==0) stop("A data frame is empty")
  if (length(set)==0)   set<-matched(data,period1=period, period2=period)                               
                                   period<-paste(period,"-01",sep="") 
                                   period<-as.Date(period)
                                   lubridate::day(period)<-1
                                   period2<-period
                                   lubridate::day(period2)<-lubridate::days_in_month(period2)
                                   data<-dplyr::filter(data, data$time>=period & data$time<=period2)
                                   if (nrow(data)==0) stop("There are no data in selected period")
                                   vec<-numeric(length(set))
                                   for (i in 1:length(set)) {
                                   d<-dplyr::filter(data, data$prodID==set[i])
                                   if (nrow(data)==0) vec[i]<-0
                                   #returning the unit value
                                   else vec[i]<-sum(d$prices*d$quantities)/sum(d$quantities)
                                                             }  
                                   return (vec)
                                       }


#' @title  Providing quantities of sold products
#'
#' @description The function returns quantities of sold products with given IDs. 
#' @param data The user's data frame. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{quantities} (as positive numeric) and \code{prodID} (as numeric or character) with unique product IDs. 
#' @param period The time period (as character) limited to the year and month, e.g. "2019-03".
#' @param set The set of unique product IDs to be used for determining quantities of sold products (see also \code{\link{data_matching}}). If the \code{set} is empty, the function returns quantities of all products being available in \code{period}.
#' @rdname quantities
#' @return The function analyzes the user's data frame and returns quantities of products with given \code{ID} and being sold in the time period indicated by the \code{period} parameter.
#' @examples 
#' quantities(milk, period="2019-06")
#' quantities(milk, period="2019-12",set=c(400032, 71772, 82919))
#' @export


quantities<-function(data, period, set=c())
                                   {
  if (nrow(data)==0) stop("A data frame is empty")
  if (length(set)==0)   set<-matched(data,period1=period, period2=period)
                                   period<-paste(period,"-01",sep="") 
                                   period<-as.Date(period)
                                   lubridate::day(period)<-1
                                   period2<-period
                                   lubridate::day(period2)<-lubridate::days_in_month(period2)
                                   data<-dplyr::filter(data, data$time>=period & data$time<=period2)
                                   if (nrow(data)==0) stop("There are no data in selected period")
                                   vec<-numeric(length(set))
                                   for (i in 1:length(set)) {
                                   d<-dplyr::filter(data, data$prodID==set[i])
                                   if (nrow(data)==0) vec[i]<-0
                                   else vec[i]<-sum(d$quantities)
                                                             }
                                   return(vec)
                                   }
#' @title  Providing a correlation coefficient for price and quantity of sold products
#'
#' @description The function returns correlation between price and quantity of sold products with given IDs. 
#' @param data The user's data frame. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities} (as positive numeric) and \code{prodID} (as numeric or character) with unique product IDs. 
#' @param period The time period (as character) limited to the year and month, e.g. "2019-03".
#' @param set The set of unique product IDs to be used for determining correlation between price and quantity of sold products (see also \code{\link{data_matching}}). If the \code{set} is empty, the function works for all products being available in \code{period}.
#' @param figure A logical parameter indicating whether the function returns a figure (TRUE) or a data frame (FALSE) with correlations between price and quantity of sold products.
#' @rdname pqcor
#' @return The function returns Pearson's correlation coefficient between price and quantity of products with given IDs and sold in \code{period}.
#' @examples 
#' pqcor(milk, period="2019-03")
#' pqcor(milk, period="2019-03",figure=TRUE)
#' @export

pqcor<-function(data,period,set=c(),figure=FALSE)
{ if (nrow(data)==0) stop("A data frame is empty")
  prices<-prices(data, period,set)
  quantities<-quantities(data, period,set)
  coeff<-stats::cor(prices, quantities)
  coeff<-signif(coeff,4)
  if (figure==TRUE) {
    df<-data.frame(prices, quantities)
    title<-paste("Pearson's correlation coefficient = ",as.character(coeff))
    ggplot2::ggplot(df,ggplot2::aes(x=prices, y=quantities))+ggplot2::geom_point()+ggplot2::ggtitle(title)+ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
                    }
else return (coeff)
}

#' @title  Providing correlations between price and quantity of sold products
#'
#' @description The function returns Pearson's correlation coefficients between price and quantity of sold products with given IDs.
#' @param data The user's data frame. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities} (as positive numeric) and \code{prodID} (as numeric or character) with unique product IDs. 
#' @param start The beginning of the considered time interval (as character) limited to the year and month, e.g. "2020-03".
#' @param end The end of the considered time interval (as character) limited to the year and month, e.g. "2020-04".
#' @param figure A logical parameter indicating whether the function returns a figure (TRUE) or a data frame (FALSE) with price-quantity correlations.
#' @param set The set of unique product IDs to be used for determining correlation between prices and quantities of sold products (see also \code{\link{data_matching}}). If the \code{set} is empty, the function works for all products being available in \code{period}.
#' @rdname pqcor_fig
#' @return The function returns Pearson's correlation coefficients between price and quantity of products with given IDs and sold in the time interval: \code{<start, end>}. Correlation coefficients are calculated for each month separately. Results are presented in tabular or graphical form depending on the \code{figure} parameter.
#' @examples 
#' pqcor_fig(milk, start="2018-12", end="2019-12", figure=FALSE)
#' pqcor_fig(milk, start="2018-12", end="2019-12", figure=TRUE)
#' @export

pqcor_fig<-function (data, start, end, figure=TRUE, set=c())
{
if (nrow(data)==0) stop("A data frame is empty")
date<-correlation<-NULL
start<-paste(start,"-01",sep="")
end<-paste(end,"-01",sep="")
start<-as.Date(start)
end<-as.Date(end)   
times<-c()
values<-c()  
while (start<=end)
              {t<-substr(start,0,7)  
               times<-c(times,t)
               values<-c(values, pqcor(data,period=t,set))
               lubridate::month(start)<-lubridate::month(start)+1 
              }
tab<-data.frame(c(times), c(values))
colnames(tab)<-c("date", "correlation")  
if (figure==FALSE) return (tab)  
#returning a figure which is based on 'tab'
else
 {
tab$date<-as.Date(paste(tab$date,"01",sep="-"))
ggplot2::ggplot(tab, ggplot2::aes(x=date, y=correlation)) + ggplot2::geom_point()+ggplot2::geom_line()+ggplot2::labs(x="date",y="correlation")+ggplot2::scale_x_date(date_labels="%Y %m",date_breaks  ="1 month")+ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust = 1))  
 }
}


#' @title  Providing values of product sales
#'
#' @description The function returns values of sales of products with given IDs. 
#' @param data The user's data frame. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities} (as positive numeric) and \code{prodID} (as numeric or character) with unique product IDs. 
#' @param period The time period (as character) limited to the year and month, e.g. "2019-03".
#' @param set The set of unique product IDs to be used for determining product sales values (see also \code{\link{data_matching}}). If the \code{set} is empty, then the function returns sale values of all products being available in \code{period}.
#' @param shares A logical parameter indicating whether the function is to return shares of product sales.
#' @param hist A logical parameter indicating whether the function is to return histogram of product sales.
#' @rdname sales
#' @return The function analyzes the user's data frame and returns values of sales of products with given IDs and being sold in time period indicated by the \code{period} parameter.
#' @examples 
#' sales(milk, period="2019-06", shares=TRUE, hist=TRUE)
#' sales(milk, period="2019-12",set=unique(milk$prodID)[1])
#' @export

sales<-function(data, period, set=c(), shares=FALSE, hist=FALSE)
                                   {
  if (nrow(data)==0) stop("A data frame is empty")
  if (length(set)==0)   set<-matched(data,period1=period, period2=period)
                                   period<-paste(period,"-01",sep="") 
                                   period<-as.Date(period)
                                   lubridate::day(period)<-1
                                   period2<-period
                                   lubridate::day(period2)<-lubridate::days_in_month(period2)
                                   data<-dplyr::filter(data, data$time>=period & data$time<=period2)
                                   if (nrow(data)==0) stop("There are no data in selected period")
                                   vec<-numeric(length(set))
                                   for (i in 1:length(set)) {
                                   
                                   d<-dplyr::filter(data, data$prodID==set[i])
                                   if (nrow(data)==0) vec[i]<-0
                                   
                                   else vec[i]<-sum(d$prices*d$quantities)
                                                             }
                                   if (hist==FALSE) {
                                   if (shares==FALSE) return(vec)
                                   else return (vec/sum(vec))
                                                    }
                                   else             {
                                   if (shares==FALSE) return (graphics::hist(vec, main="", xlab=" value of sale", ylab="number of obs.", col="grey"))
                                   else return (graphics::hist(vec/sum(vec), main="", xlab=" share in sale", ylab="number of obs.", col="grey"))
                                      
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
#' ctg<-unique(milk$description)
#' categories<-c(ctg[1],ctg[2],ctg[3])
#' milk1<-dplyr::filter(milk, milk$description==categories[1])
#' milk2<-dplyr::filter(milk, milk$description==categories[2])
#' milk3<-dplyr::filter(milk, milk$description==categories[3])
#' ## Sample use of this function:
#' sales_groups(datasets=list(milk1,milk2,milk3),start="2019-04",end="2019-04",shares=TRUE)
#' sales_groups(datasets=list(milk1,milk2,milk3),start="2019-04",end="2019-07", 
#' barplot=TRUE, names=categories)
#' @export

sales_groups<-function(datasets=list(), start, end, shares=FALSE, barplot=FALSE, names=c())
                                  {
                                   groups<-value<-NULL
                                   start<-paste(start,"-01",sep="") 
                                   start<-as.Date(start)
                                   end<-paste(end,"-01",sep="") 
                                   end<-as.Date(end)
                                   lubridate::day(start)<-1
                                   lubridate::day(end)<-lubridate::days_in_month(end)
                                   nm<-c()
                                   sales<-c()
                                   for (m in 1:length(datasets))  {set<-data.frame(datasets[[m]])
                                   if (nrow(set)==0) print("At least one data frame is empty")
                                   nm<-c(nm,paste("group ",as.character(m)))
                                   set<-dplyr::filter(set, set$time>=start & set$time<=end)
                                   sales<-c(sales,sum(set$prices*set$quantities))
                                                                  }
                                   if (shares==TRUE) sales<-sales/sum(sales)
                                   if (length(names)==0) names<-nm
                                   if (barplot==FALSE) return (sales)
                                   else {if (shares==FALSE) 
                                   {
                                     df <- data.frame(groups=names,value=sales)
                                     ggplot2::ggplot(data=df, ggplot2::aes(x=groups, y=value)) +
  ggplot2::geom_bar(stat="identity", fill="grey",color="black")+ggplot2::labs(y="value of sales")
                                   }
                                     else 
                                     { df <- data.frame(groups=names,value=sales)
                                       ggplot2::ggplot(data=df, ggplot2::aes(x=groups, y=value)) +
  ggplot2::geom_bar(stat="identity", fill="grey",color="black")+ggplot2::labs(y="share in sales")
                                     }
}
}


#unweighted bilateral indices
#' @title  Calculating the unweighted Jevons price index
#'
#' @description This function returns a value (or vector of values) of the unweighted bilateral Jevons price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname jevons
#' @return The function returns a value (or vector of values) of the unweighted bilateral Jevons price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).      
#' @references
#' {Jevons, W. S., (1865). \emph{The variation of prices and the value of the currency since 1782}. J. Statist. Soc. Lond., 28, 294-320.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' jevons(milk, start="2018-12", end="2020-01")
#' \donttest{jevons(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

jevons<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   date<-c(start) 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))                                                                         
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   jev<-prod(price_end/price_start)
                                   result<-c(result, jev^(1/length(id)))    
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                  return(result)      
                                                       }
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data,start,end)
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   jev<-prod(price_end/price_start)
                                   return(jev^(1/length(id)))
                                        }
                                  }
#' @title  Calculating the unweighted Dutot price index
#'
#' @description This function returns a value (or vector of values) of the unweighted bilateral Dutot price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname dutot
#' @return The function returns a value (or vector of values) of the unweighted bilateral Dutot price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).      
#' @references
#' {Dutot, C. F., (1738). \emph{Reflexions Politiques sur les Finances et le Commerce}. The Hague: Les Freres Vaillant et Nicolas Prevost, Vol. 1.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' dutot(milk, start="2018-12", end="2020-01")
#' \donttest{dutot(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
dutot<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                      {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))       
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   result<-c(result,sum(price_end)/sum(price_start))     
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                  return(result)      
                                                       }
                                   #returning one value
                                   else {
                                  data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))                                               
                                   id<-matched(data,start,end)
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   return(sum(price_end)/sum(price_start)) 
                                   }
                                  }

#' @title  Calculating the unweighted Carli price index
#'
#' @description This function returns a value (or vector of values) of the unweighted bilateral Carli price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname carli
#' @return The function returns a value (or vector of values) of the unweighted bilateral Carli price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Carli, G. (1804). \emph{Del valore e della proporzione de'metalli monetati}. Scrittori Classici Italiani di Economia Politica, 13, 297-336.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' carli(milk, start="2018-12", end="2020-01")
#' \donttest{carli(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
carli<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                      {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   result<-c(result,sum(price_end/price_start)/length(id))     
                                   lubridate::month(end)<-lubridate::month(end)+1
                                      }
                                   return(result)      
                                                       }
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))       
                                  id<-matched(data,start,end)
                                  price_end<-prices(data,period=end,set=id)
                                  price_start<-prices(data,period=start,set=id)
                                  return(sum(price_end/price_start)/length(id))  
                                   }
                                  }

#' @title  Calculating the unweighted CSWD price index
#'
#' @description This function returns a value (or vector of values) of the unweighted Carruthers-Sellwood-Ward-Dalen (CSWD) price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname cswd
#' @return The function returns a value (or vector of values) of the unweighted bilateral CSWD price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Carruthers, A.G., Sellwood, D. J, Ward, P. W. (1980). \emph{Recent developments in the retail price index}. Journal of the Royal Statistical Society. Series D (The Statisticain), 29(1), 1-32.}
#'
#' {Dalen, J. (1992). \emph{Recent developments in the retail price index}. The Statistician, 29(1),  1-32.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' cswd(milk, start="2018-12", end="2020-01")
#' \donttest{cswd(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
cswd<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                  if (nrow(data)==0) stop("A data frame is empty") 
                                  start<-paste(start,"-01",sep="")
                                  end<-paste(end,"-01",sep="")
                                  start<-as.Date(start)
                                  end<-as.Date(end)
                                  #returning vector of values
                                  if (interval==TRUE) { result<-c(1)
                                  end2<-end
                                  end<-start
                                  lubridate::month(end)<-lubridate::month(end)+1
                                  while (end<=end2)
                                       {
                                  t<-substr(end,0,7)
                                  date<-c(date,t)
data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                 id<-matched(data2,start,end)
                                 price_end<-prices(data2,period=end,set=id)
                                 price_start<-prices(data2,period=start,set=id)
                                 a<-sum(price_end/price_start)
                                 b<-sum(price_start/price_end)
                                 result<-c(result,(a/b)^(1/2))    
                                 lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                 return(result)      
                                                       }
                                 #returning one value
                                 else {
                                 data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                 id<-matched(data,start,end)
                                 price_end<-prices(data,period=end,set=id)
                                 price_start<-prices(data,period=start,set=id)
                                 a<-sum(price_end/price_start)
                                 b<-sum(price_start/price_end)
                                 return((a/b)^(1/2))  
                                   }
                                  }

#' @title  Calculating the unweighted harmonic price index
#'
#' @description This function returns a value (or vector of values) of the unweighted "unnamed" harmonic price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname harmonic
#' @return The function returns a value (or vector of values) of the unweighted bilateral harmonic price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' harmonic(milk, start="2018-12", end="2020-01")
#' \donttest{harmonic(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
harmonic<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")  
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))       
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   result<-c(result,length(id)/sum(price_start/price_end))
                                   lubridate::month(end)<-lubridate::month(end)+1
                                      }
                                   return(result)      
                                   }
                                   
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                  id<-matched(data,start,end)
                                  price_end<-prices(data,period=end,set=id)
                                  price_start<-prices(data,period=start,set=id)
                                  return(length(id)/sum(price_start/price_end))      
                                   }
                                  }

#' @title  Calculating the unweighted BMW price index
#'
#' @description This function returns a value (or vector of values) of the unweighted Balk-Mehrhoff-Walsh (BMW)  price index.
#' @param data User's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname bmw
#' @return The function returns a value (or vector of values) of the unweighted bilateral BMW price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).  
#' @references
#' {Mehrhoff, J.(2007). \emph{A linear approximation to the Jevons index}. In: Von der Lippe (2007): Index Theory and Price Statistics, Peter Lang: Berlin, Germany.}
#'
#' {(2018). \emph{Harmonised Index of Consumer Prices (HICP). Methodological Manual}. Publication Office of the European union, Luxembourg.}
#' @examples 
#' bmw(milk, start="2018-12", end="2020-01")
#' \donttest{bmw(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
bmw<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))      
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   a<-sum((price_start/price_end)^0.5)
                                   b<-sum((price_end/price_start)*((price_start/price_end)^0.5))
                                   result<-c(result,b/a) 
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                   return(result)      
                                   } 
                                   #returning one value
                                   else
                                   {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data,start,end)
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   a<-sum((price_start/price_end)^0.5)
                                   b<-sum((price_end/price_start)*((price_start/price_end)^0.5))
                                   return(b/a)      
                                   }
                                   }

#' @title  Calculating the bilateral Laspeyres price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Laspeyres price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname laspeyres
#' @return The function returns a value (or vector of values) of the bilateral Laspeyres price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Laspeyres, E. (1871). \emph{Die Berechnung einer mittleren Waarenpreissteigerung}. Jahrbucher fur Nationalokonomie und Statistik 16, 296-314.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' laspeyres(milk, start="2018-12", end="2020-01")
#' \donttest{laspeyres(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
laspeyres<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   quantity_start<-quantities(data2,period=start,set=id)
                                   result<-c(result,sum(quantity_start*price_end)/sum(quantity_start*price_start)) 
                                   lubridate::month(end)<-lubridate::month(end)+1
                                      }
                                   return(result)      
                                   } 
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                  id<-matched(data,start,end, type="prodID", interval=FALSE)
                                  price_end<-prices(data,period=end,set=id)
                                  price_start<-prices(data,period=start,set=id)
                                  quantity_start<-quantities(data,period=start,set=id)
                                  return(sum(quantity_start*price_end)/sum(quantity_start*price_start))      
                                  }
                                  }


#' @title  Calculating the bilateral Paasche price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Paasche price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname paasche
#' @return The function returns a value (or vector of values) of the bilateral Paasche price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Paasche, H. (1874). \emph{Uber die Preisentwicklung der letzten Jahre nach den Hamburger Borsennotirungen}. Jahrbucher fur Nationalokonomie und Statistik, 12, 168-178.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' paasche(milk, start="2018-12", end="2020-01")
#' \donttest{paasche(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
paasche<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   quantity_end<-quantities(data2,period=end,set=id)
                                   result<-c(result,sum(price_end*quantity_end)/sum(price_start*quantity_end))  
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                   return(result)      
                                   } 
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data,start,end, type="prodID", interval=FALSE)
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   quantity_end<-quantities(data,period=end,set=id)
                                   return(sum(price_end*quantity_end)/sum(price_start*quantity_end))      
                                      }
                                     }
#' @title  Calculating the bilateral Fisher price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Fisher price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname fisher
#' @return The function returns a value (or vector of values) of the bilateral Fisher price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating,please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Fisher, I. (1922). \emph{The Making of Index Numbers}. Boston: Houghton Mifflin.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' fisher(milk, start="2018-12", end="2020-01")
#' \donttest{fisher(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
fisher<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   quantity_end<-quantities(data2,period=end,set=id)
                                   quantity_start<-quantities(data2,period=start,set=id)
                                   result<-c(result,((sum(price_end*quantity_end)/sum(price_start*quantity_end))*(sum(price_end*quantity_start)/sum(price_start*quantity_start)))^(0.5))
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                   return(result)      
                                   }
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data,start,end, type="prodID", interval=FALSE)
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   quantity_end<-quantities(data,period=end,set=id)
                                   quantity_start<-quantities(data,period=start,set=id)
                                   return (((sum(price_end*quantity_end)/sum(price_start*quantity_end))*(sum(price_end*quantity_start)/sum(price_start*quantity_start)))^(0.5))      
                                      }
                                      }
#' @title  Calculating the bilateral Tornqvist price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Tornqvist price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname tornqvist
#' @return The function returns a value (or vector of values) of the bilateral Tornqvist price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Tornqvist, L. (1936). \emph{The Bank of Finland's Consumption Price Index}. Bank of Finland Monthly Bulletin 10, 1-8.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' tornqvist(milk, start="2018-12", end="2020-01")
#' \donttest{tornqvist(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
tornqvist<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   sales_end<-sales(data2,period=end,set=id)
                                   sales_start<-sales(data2,period=start,set=id)
                                   sum_start<-sum(sales_start)
                                   sum_end<-sum(sales_end)
                                   tornq<-prod((price_end/price_start)^(0.5*((sales_start/sum_start)+(sales_end/sum_end))))
                                   result<-c(result,tornq) 
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                   return(result)      
                                   }
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))      
                                  id<-matched(data,start,end)
                                  price_end<-prices(data,period=end,set=id)
                                  price_start<-prices(data,period=start,set=id)
                                  sales_end<-sales(data,period=end,set=id)
                                  sales_start<-sales(data,period=start,set=id)
                                  sum_start<-sum(sales_start)
                                  sum_end<-sum(sales_end)
                                  tornq<-prod((price_end/price_start)^(0.5*((sales_start/sum_start)+(sales_end/sum_end))))
                                  return(tornq)      
                                   }
                                   }
#' @title  Calculating the bilateral geo-logarithmic Laspeyres price index
#'
#' @description This function returns a value (or vector of values) of the bilateral geo-logarithmic Laspeyres price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname geolaspeyres
#' @return The function returns a value (or vector of values) of the bilateral geo-logarithmic Laspeyres price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' geolaspeyres(milk, start="2018-12", end="2020-01")
#' \donttest{geolaspeyres(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
geolaspeyres<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   sales_start<-sales(data2,period=start,set=id)
                                   sum_start<-sum(sales_start)
                                   result<-c(result,prod((price_end/price_start)^(sales_start/sum_start)))      
                                   lubridate::month(end)<-lubridate::month(end)+1
                                        }
                                   return(result)      
                                   }
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))       
                                   id<-matched(data,start,end)
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   sales_start<-sales(data,period=start,set=id)
                                   sum_start<-sum(sales_start)
                                   return(prod((price_end/price_start)^(sales_start/sum_start)))      
                                   }
                                   }

#' @title  Calculating the bilateral geo-logarithmic Paasche price index
#'
#' @description This function returns a value (or vector of values) of the bilateral geo-logarithmic Paasche price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname geopaasche
#' @return The function returns a value (or vector of values) of the bilateral geo-logarithmic Paasche price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' geopaasche(milk, start="2018-12", end="2020-01")
#' \donttest{geopaasche(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
geopaasche<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   sales_end<-sales(data2,period=end,set=id)
                                   sum_end<-sum(sales_end)
                                   result<-c(result,prod((price_end/price_start)^(sales_end/sum_end)))      
                                   lubridate::month(end)<-lubridate::month(end)+1
                                      }
                                   return(result)      
                                   }
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data,start,end)
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   sales_end<-sales(data,period=end,set=id)
                                   sum_end<-sum(sales_end)
                                   return(prod((price_end/price_start)^(sales_end/sum_end)))      
                                   }
                                   }
#' @title  Calculating the bilateral Drobisch price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Drobisch price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname drobisch
#' @return The function returns a value (or vector of values) of the bilateral Drobisch price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
 #' {Drobisch, M. W. (1871). \emph{Ueber einige Einwurfe gegen die in diesen Jahrbuchern veroffentlichte neue Methode, die Veranderungen der Waarenpreise und des Geldwerths zu berechten}.Jahrbucher fur Nationalokonomie und Statistik, Vol. 16, s. 416-427.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' drobisch(milk, start="2018-12", end="2020-01")
#' \donttest{drobisch(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

drobisch<-function(data,start,end,interval=FALSE) {if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   a<-laspeyres(data,substr(start,0,7),substr(end,0,7))
                                   b<-paasche(data,substr(start,0,7),substr(end,0,7))           
                                   result<-c(result,(a+b)/2)
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                   return(result)      
                                   }
                                   #returning one value
                                   else {
                                   a<-laspeyres(data,substr(start,0,7),substr(end,0,7))
                                   b<-paasche(data,substr(start,0,7),substr(end,0,7))
                                   return((a+b)/2)
                                   }
                                   }

#' @title  Calculating the bilateral Marshall-Edgeworth price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Marshall-Edgeworth price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname marshall_edgeworth
#' @return The function returns a value (or vector of values) of the bilateral Marshall-Edgeworth price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
 #' {Marshall, A. (1887). \emph{Remedies for Fluctuations of General Prices}. Contemporary Review, 51, 355-375.}
#'
#' {Edgeworth, F. Y. (1887). \emph{Measurement of Change in Value of Money I}. The first Memorandum presented to the British Association for the Advancement of Science; reprinted in Papers Relating to Political Economy, Vol. 1, New York, Burt Franklin, s. 1925.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' marshall_edgeworth(milk, start="2018-12", end="2020-01")
#' \donttest{marshall_edgeworth(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

marshall_edgeworth<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   quantity_end<-quantities(data2,period=end,set=id)
                                   quantity_start<-quantities(data2,period=start,set=id)
                                   result<-c(result,sum(price_end*(quantity_start+quantity_end)/sum(price_start*(quantity_start+quantity_end))))                                    
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                   return(result)      
                                   }
                                   #returning one value
                                   else 
                                   {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data,start,end)
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   quantity_end<-quantities(data,period=end,set=id)
                                   quantity_start<-quantities(data,period=start,set=id)
                        return(sum(price_end*(quantity_start+quantity_end)/sum(price_start*(quantity_start+quantity_end))))                                       }
                                  }



#' @title  Calculating the bilateral Walsh price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Walsh price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname walsh
#' @return The function returns a value (or vector of values) of the bilateral Walsh price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).  
#' @references
#' {Walsh, C. M. (1901). \emph{The Measurement of General Exchange Value}. The MacMillan Company, New York.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' walsh(milk, start="2018-12", end="2020-01")
#' \donttest{walsh(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

walsh<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                  if (nrow(data)==0) stop("A data frame is empty")
                                  start<-paste(start,"-01",sep="")
                                  end<-paste(end,"-01",sep="")
                                  start<-as.Date(start)
                                  end<-as.Date(end)
                                  #returning vector of values
                                  if (interval==TRUE) { result<-c(1)
                                  end2<-end
                                  end<-start
                                  lubridate::month(end)<-lubridate::month(end)+1
                                  while (end<=end2)
                                       {
                                  t<-substr(end,0,7)
                                  date<-c(date,t)
                                  data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                  id<-matched(data2,start,end)
                                  price_end<-prices(data2,period=end,set=id)
                                  price_start<-prices(data2,period=start,set=id)
                                  quantity_end<-quantities(data2,period=end,set=id)
                                  quantity_start<-quantities(data2,period=start,set=id)
                                  result<-c(result,sum(price_end*(quantity_start*quantity_end)^(0.5)/sum(price_start*(quantity_start*quantity_end)^(0.5))))      
                                  lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                  return(result)      
                                   }
                                  #returning one value
                                  else {
                                  data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))       
                                  id<-matched(data,start,end)
                                  price_end<-prices(data,period=end,set=id)
                                  price_start<-prices(data,period=start,set=id)
                                  quantity_end<-quantities(data,period=end,set=id)
                                  quantity_start<-quantities(data,period=start,set=id)
return(sum(price_end*(quantity_start*quantity_end)^(0.5)/sum(price_start*(quantity_start*quantity_end)^(0.5))))                                                   }
                                   }



#' @title  Calculating the bilateral Bialek price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Bialek price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname bialek
#' @return The function returns a value (or vector of values) of the bilateral Bialek price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Von der Lippe, P. (2012). \emph{Some short notes on the price index of Jacek Bialek}. Econometrics (Ekonometria). 1(35), 76-83.}
#'
#' {Bialek, J. (2013). \emph{Some Remarks on the Original Price Index Inspired by the Notes of Peter von der Lippe}. Econometrics (Ekonometria), 3(41), 40-54.}
#'
#' {Bialek, J. (2014). \emph{Simulation Study of an Original Price Index Formula}. Communications in Statistics - Simulation and Computation, 43(2), 285-297}
#' @examples 
#' bialek(milk, start="2018-12", end="2020-01")
#' \donttest{bialek(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

bialek<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   quantity_end<-quantities(data2,period=end,set=id)
                                   quantity_start<-quantities(data2,period=start,set=id)
                                   a1<-sum(price_end*pmin(quantity_start,quantity_end))
                                   a2<-sum(price_start*pmin(quantity_start,quantity_end))
                                   b1<-sum(price_end*pmax(quantity_start,quantity_end))
                                   b2<-sum(price_start*pmax(quantity_start,quantity_end))
                                   result<-c(result,((a1*b1)/(a2*b2))^(1/2))  
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                   return(result)      
                                   }
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data,start,end)
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   quantity_end<-quantities(data,period=end,set=id)
                                   quantity_start<-quantities(data,period=start,set=id)
                                   a1<-sum(price_end*pmin(quantity_start,quantity_end))
                                   a2<-sum(price_start*pmin(quantity_start,quantity_end))
                                   b1<-sum(price_end*pmax(quantity_start,quantity_end))
                                   b2<-sum(price_start*pmax(quantity_start,quantity_end))
                                   return(((a1*b1)/(a2*b2))^(1/2))   
                                   }
                                  }
#' @title  Calculating the bilateral Banajree price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Banajree price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname banajree
#' @return The function returns a value (or vector of values) of the bilateral Banajree price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function)..   
#' @references
#' {Banajree, K. S. (1977). \emph{On the factorial approach providing the true index of cost of living.} Gottingen : Vandenhoeck und Ruprecht.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' banajree(milk, start="2018-12", end="2020-01")
#' \donttest{banajree(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

banajree<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   quantity_end<-quantities(data2,period=end,set=id)
                                   quantity_start<-quantities(data2,period=start,set=id)
                                   vt<-sum(price_end*quantity_end)
                                   v0<-sum(price_start*quantity_start)
                                   vt0<-sum(price_end*quantity_start)
                                   v0t<-sum(price_start*quantity_end)
                                   result<-c(result,(vt/v0)*(v0+vt0)/(vt+v0t))  
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                   return(result)      
                                   }
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))         
                                   id<-matched(data,start,end)
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   quantity_end<-quantities(data,period=end,set=id)
                                   quantity_start<-quantities(data,period=start,set=id)
                                   vt<-sum(price_end*quantity_end)
                                   v0<-sum(price_start*quantity_start)
                                   vt0<-sum(price_end*quantity_start)
                                   v0t<-sum(price_start*quantity_end)
                                   return((vt/v0)*(v0+vt0)/(vt+v0t))
                                   }
                                  }
#' @title  Calculating the bilateral Davies price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Davies price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname davies
#' @return The function returns a value (or vector of values) of the bilateral Davies price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Davies, G. R. (1924). \emph{The Problem of a Standard Index Number Formula.} Journal of the American Statistical Association, 19 (146), 180-188.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' davies(milk, start="2018-12", end="2020-01")
#' \donttest{davies(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

davies<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                      {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   quantity_end<-quantities(data2,period=end,set=id)
                                   quantity_start<-quantities(data2,period=start,set=id)
                                   vt<-sum(price_end*quantity_end)
                                   v0<-sum(price_start*quantity_start)
                                   a<-sum(quantity_start*(price_end*price_start)^(0.5))
                                   b<-sum(quantity_end*(price_end*price_start)^(0.5))
                                   result<-c(result,(vt/v0)*(a/b)) 
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                   return(result)      
                                   }
                                   #returning one value
                                   else 
                                   {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))       
                                   id<-matched(data,start,end)
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   quantity_end<-quantities(data,period=end,set=id)
                                   quantity_start<-quantities(data,period=start,set=id)
                                   vt<-sum(price_end*quantity_end)
                                   v0<-sum(price_start*quantity_start)
                                   a<-sum(quantity_start*(price_end*price_start)^(0.5))
                                   b<-sum(quantity_end*(price_end*price_start)^(0.5))
                                   return((vt/v0)*(a/b))   
                                   }
                                   }

#' @title  Calculating the bilateral Stuvel price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Stuvel price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname stuvel
#' @return The function returns a value (or vector of values) of the bilateral Stuvel price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Stuvel, G. (1957). \emph{A New Index Number Formula.} Econometrica, 25, 123-131.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' stuvel(milk, start="2018-12", end="2020-01")
#' \donttest{stuvel(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

stuvel<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   quantity_end<-quantities(data2,period=end,set=id)
                                   quantity_start<-quantities(data2,period=start,set=id)
                                   vt<-sum(price_end*quantity_end)
                                   v0<-sum(price_start*quantity_start)
                                   v0t<-sum(price_start*quantity_end)
                                   vt0<-sum(price_end*quantity_start)
                                   result<-c(result,((vt0/v0)-(v0t/v0))/2 + ((((vt0/v0)-(v0t/v0))/2)^2  + (vt/v0))^(1/2))  
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                   return(result)      
                                   }
                                   #returning one value
                                   else { 
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))       
                                   id<-matched(data,start,end)
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   quantity_end<-quantities(data,period=end,set=id)
                                   quantity_start<-quantities(data,period=start,set=id)
                                   vt<-sum(price_end*quantity_end)
                                   v0<-sum(price_start*quantity_start)
                                   v0t<-sum(price_start*quantity_end)
                                   vt0<-sum(price_end*quantity_start)
                                   return(((vt0/v0)-(v0t/v0))/2 + ((((vt0/v0)-(v0t/v0))/2)^2  + (vt/v0))^(1/2))   
                                  }
                                  }

#' @title  Calculating the bilateral Palgrave price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Palgrave price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname palgrave
#' @return The function returns a value (or vector of values) of the bilateral Palgrave price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Palgrave, R. H. I. (1886). \emph{Currency and Standard of Value in England, France and India and the Rates of Exchange Between these Countries.} Memorandum submitted to the Royal Commission on Depression of trade and Industry, Third Report, Appendix B, 312-390.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' palgrave(milk, start="2018-12", end="2020-01")
#' \donttest{palgrave(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

palgrave<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   quantity_end<-quantities(data2,period=end,set=id)
                                   quantity_start<-quantities(data2,period=start,set=id)
                                   vt<-sum(price_end*quantity_end)
                                   a<-sum(price_end^2*quantity_end/price_start)
                                   result<-c(result,a/vt)    
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                   return(result)      
                                   }
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))
                                   id<-matched(data,start,end)
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   quantity_end<-quantities(data,period=end,set=id)
                                   quantity_start<-quantities(data,period=start,set=id)
                                   vt<-sum(price_end*quantity_end)
                                   a<-sum(price_end^2*quantity_end/price_start)
                                   return(a/vt)    
                                   }
                                   }

#' @title  Calculating the bilateral Geary-Khamis price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Geary-Khamis price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname geary_khamis
#' @return The function returns a value (or vector of values) of the bilateral Geary-Khamis price index depending on the \code{interval} parameter (please use \code{\link{gk}} function to calculate the multilateral Geary-Khamis price index). If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Geary, R. G. (1958). \emph{A Note on Comparisons of Exchange Rates and Purchasing Power between Countries.} Journal of the Royal Statistical Society, Series A, 121, 97-99.}
#'
#' {Khamis, S. H. (1970). \emph{Properties and Conditions for the Existence of a new Type of Index Number.} Sankhya Series B32, 81-98.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' geary_khamis(milk, start="2018-12", end="2020-01")
#' \donttest{geary_khamis(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
geary_khamis<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   quantity_end<-quantities(data2,period=end,set=id)
                                   quantity_start<-quantities(data2,period=start,set=id)
                                   a<-sum(price_end*quantity_start*quantity_end/(quantity_start+quantity_end))
                                   b<-sum(price_start*quantity_start*quantity_end/(quantity_start+quantity_end))
                                   result<-c(result,a/b)     
                                   lubridate::month(end)<-lubridate::month(end)+1
                                      }
                                   return(result)      
                                   }
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))
                                   id<-matched(data,start,end)
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   quantity_end<-quantities(data,period=end,set=id)
                                   quantity_start<-quantities(data,period=start,set=id)
                                   a<-sum(price_end*quantity_start*quantity_end/(quantity_start+quantity_end))
                                   b<-sum(price_start*quantity_start*quantity_end/(quantity_start+quantity_end))
                                   return(a/b) 
                                   }
                                   }

#' @title  Calculating the bilateral Lehr price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Lehr price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname lehr
#' @return The function returns a value (or vector of values) of the bilateral Lehr price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Lehr, J. (1885). \emph{Beitrage zur Statistik der Preise, insbesondere des Geldes und des Holzes.} J. D. Sauerlander, Frankfurt am Main.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' lehr(milk, start="2018-12", end="2020-01")
#' \donttest{lehr(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

lehr<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   quantity_end<-quantities(data2,period=end,set=id)
                                   quantity_start<-quantities(data2,period=start,set=id)
                                   a<-sum(price_end*quantity_end)
                                   b<-sum((price_start*quantity_start+price_end*quantity_end)*quantity_end/(quantity_start+quantity_end))
                                   c<-sum(price_start*quantity_start)
                                   d<-sum((price_start*quantity_start+price_end*quantity_end)*quantity_start/(quantity_start+quantity_end))
                                  result<-c(result,(a/b)/(c/d))     
                                  lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                  return(result)      
                                   }
                                  #returning one value
                                  else {
                                  data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))       
                                  id<-matched(data,start,end)
                                  price_end<-prices(data,period=end,set=id)
                                  price_start<-prices(data,period=start,set=id)
                                  quantity_end<-quantities(data,period=end,set=id)
                                  quantity_start<-quantities(data,period=start,set=id)
                                  a<-sum(price_end*quantity_end)
b<-sum((price_start*quantity_start+price_end*quantity_end)*quantity_end/(quantity_start+quantity_end))
                                  c<-sum(price_start*quantity_start)
d<-sum((price_start*quantity_start+price_end*quantity_end)*quantity_start/(quantity_start+quantity_end))
                                  return((a/b)/(c/d))
                                   }
                                   }
#logarithmic means
L<-function (x,y) {
  if (x==y) return (x)
  else return ((y-x)/log(y/x))
}

LL<-function (x) {
  if (x[1]==x[2]) return (x[1])
  else return ((x[1]-x[2])/log(x[1]/x[2]))
                  }

#' @title  Calculating the bilateral Vartia-I price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Vartia-I price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname vartia
#' @return The function returns a value (or vector of values) of the bilateral Vartia-I price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Vartia, Y. 0. (1976). \emph{Ideal Log-Change Index Numbers .}  Scandinavian Journal of Statistics 3(3), 121-126.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' vartia(milk, start="2018-12", end="2020-01")
#' \donttest{vartia(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export


vartia<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   quantity_end<-quantities(data2,period=end,set=id)
                                   quantity_start<-quantities(data2,period=start,set=id)
                                   vt<-sum(price_end*quantity_end)
                                   v0<-sum(price_start*quantity_start)
                                   sale_start<-price_start*quantity_start
                                   sale_end<-price_end*quantity_end
                                   sale<-data.frame(sale_start, sale_end)
                                   vecL<-apply(sale,1,LL)
                                   summ<-sum(vecL*log(price_end/price_start))
                                   vartia<-exp(summ/L(vt,v0))
                                   result<-c(result,vartia)     
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                   return(result)      
                                   }
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))       
                                   id<-matched(data,start,end)
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   quantity_end<-quantities(data,period=end,set=id)
                                   quantity_start<-quantities(data,period=start,set=id)
                                   vt<-sum(price_end*quantity_end)
                                   v0<-sum(price_start*quantity_start)
                                   sale_start<-price_start*quantity_start
                                   sale_end<-price_end*quantity_end
                                   sale<-data.frame(sale_start, sale_end)
                                   vecL<-apply(sale,1,LL)
                                   summ<-sum(vecL*log(price_end/price_start))
                                   vartia<-exp(summ/L(vt,v0))
                                   return(vartia)    
                                   }
                                   }

#' @title  Calculating the bilateral Vartia-II (Sato-Vartia) price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Vartia-II (Sato-Vartia) price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname sato_vartia
#' @return The function returns a value (or vector of values) of the bilateral Vartia-II (Sato-Vartia) price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Sato, K. (1976). \emph{The Ideal Log-Change Index Number.}  The Review of Economics and Statistics, 58(2), 223-228.}
#'
#' {Vartia, Y. 0. (1976). \emph{Ideal Log-Change Index Numbers .}  Scandinavian Journal of Statistics 3(3), 121-126.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' sato_vartia(milk, start="2018-12", end="2020-01")
#' \donttest{sato_vartia(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

sato_vartia<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   quantity_end<-quantities(data2,period=end,set=id)
                                   quantity_start<-quantities(data2,period=start,set=id)
                                   vt<-sum(price_end*quantity_end)
                                   v0<-sum(price_start*quantity_start)
                                   x<-price_end*quantity_end/vt
                                   y<-price_start*quantity_start/v0
                                   z<-data.frame(x,y)
                                   vecL<-apply(z,1,LL)
                                   sum1<-sum(vecL*log(price_end/price_start))
                                   sum2<-sum(vecL)
                                   result<-c(result,exp(sum1/sum2))     
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                   return(result)      
                                   }
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data,start,end)
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   quantity_end<-quantities(data,period=end,set=id)
                                   quantity_start<-quantities(data,period=start,set=id)
                                   vt<-sum(price_end*quantity_end)
                                   v0<-sum(price_start*quantity_start)
                                   x<-price_end*quantity_end/vt
                                   y<-price_start*quantity_start/v0
                                   z<-data.frame(x,y)
                                   vecL<-apply(z,1,LL)
                                   sum1<-sum(vecL*log(price_end/price_start))
                                   sum2<-sum(vecL)
                                   return(exp(sum1/sum2))
                                   }
                                  }

#' @title  Calculating the bilateral Lloyd-Moulton price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Lloyd-Moulton price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param sigma The elasticity of substitution parameter (as numeric).
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname lloyd_moulton
#' @return The function returns a value (or vector of values) of the bilateral Lloyd-Moulton price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Lloyd, P. J. (1975). \emph{Substitution Effects and Biases in Nontrue Price Indices.}  The American Economic Review, 65, 301-313.}
#'
#' {Moulton,  B.  R.  (1996). \emph{Constant  Elasticity  Cost-of-Living  Index  in  Share-Relative  Form.}  Washington DC: U. S. Bureau of Labor Statistics, mimeograph}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' lloyd_moulton(milk, start="2018-12", end="2020-01",sigma=0.9)
#' \donttest{lloyd_moulton(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

lloyd_moulton<-function(data,start,end,sigma=0.7,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   if (sigma==1) stop("A specification of the parameter 'sigma' is wrong")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))        
                                   id<-matched(data2,start,end)
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   quantity_start<-quantities(data2,period=start,set=id)
                                   v0<-sum(price_start*quantity_start)
                                   sum<-sum(price_start*quantity_start/v0*(price_end/price_start)^(1-sigma))
                                   sum<-sum^(1/(1-sigma))                
                                   result<-c(result,sum)     
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                   return(result)      
                                   }
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))      
                                  id<-matched(data,start,end)
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   quantity_start<-quantities(data,period=start,set=id)
                                   v0<-sum(price_start*quantity_start)
                                   sum<-sum(price_start*quantity_start/v0*(price_end/price_start)^(1-sigma))
                                   sum<-sum^(1/(1-sigma))                
                                   return(sum)  
                                   }
                                  }

#' @title  Calculating the bilateral Young price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Young price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the Young price index formula (as character) limited to the year and month, e.g. "2020-01"
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname young
#' @return The function returns a value (or vector of values) of the bilateral Young price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Young, A. H. (1992). \emph{Alternative Measures of Change in Real Output and Prices.}  Survey of Current Business, 72, 32-48.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' young(milk, start="2019-01", end="2020-01",base="2018-12")
#' \donttest{young(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

young<-function(data,start,end,base=start,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   base<-paste(base,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   base<-as.Date(base)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)) | (lubridate::year(data$time)==lubridate::year(base) & lubridate::month(data$time)==lubridate::month(base)))        
                                   id<-intersect(matched(data2,start,end),matched(data,base,end))
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   sale_base<-sales(data2,period=base,set=id)
                                   vold<-sum(sale_base)
                                   result<-c(result,sum(sale_base/vold*(price_end/price_start)))     
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                   return(result)      
                                   }
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)) | (lubridate::year(data$time)==lubridate::year(base) & lubridate::month(data$time)==lubridate::month(base)))                                                               
                                   id<-intersect(matched(data,start,end),matched(data,base,end))
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   sale_base<-sales(data,period=base,set=id)
                                   vold<-sum(sale_base)
                                   return(sum(sale_base/vold*(price_end/price_start)))
                                   }
                                  }

#' @title  Calculating the bilateral geometric Young price index
#'
#' @description This function returns a value (or vector of values) of the bilateral geometric Young price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the geometric Young price index formula (as character) limited to the year and month, e.g. "2020-01"
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname geoyoung
#' @return The function returns a value (or vector of values) of the bilateral geometric Young price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Young, A. H. (1992). \emph{Alternative Measures of Change in Real Output and Prices.}  Survey of Current Business, 72, 32-48.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' geoyoung(milk, start="2019-01", end="2020-01",base="2018-12")
#' \donttest{geoyoung(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

geoyoung<-function(data,start,end,base=start,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   base<-paste(base,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   base<-as.Date(base)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end))| (lubridate::year(data$time)==lubridate::year(base) & lubridate::month(data$time)==lubridate::month(base)))        
                                   id<-intersect(matched(data2,start,end),matched(data2,base,end))
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   sale_base<-sales(data2,period=base,set=id)
                                   sale_base<-sale_base/sum(sale_base)
                                   result<-c(result,prod((price_end/price_start)^(sale_base)))     
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                   return(result)      
                                   }
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end))| (lubridate::year(data$time)==lubridate::year(base) & lubridate::month(data$time)==lubridate::month(base))) 
                                   id<-intersect(matched(data,start,end),matched(data,base,end))
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   sale_base<-sales(data,period=base,set=id)
                                   sale_base<-sale_base/sum(sale_base)
                                   return (prod((price_end/price_start)^(sale_base)))
                                   }
                                  }
#' @title  Calculating the bilateral Lowe price index
#'
#' @description This function returns a value (or vector of values) of the bilateral Lowe price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the Lowe price index formula (as character) limited to the year and month, e.g. "2020-01".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname lowe
#' @return The function returns a value (or vector of values) of the bilateral Lowe price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' lowe(milk, start="2019-01", end="2020-01",base="2018-12")
#' \donttest{lowe(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

lowe<-function(data,start,end,base=start,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   base<-paste(base,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   base<-as.Date(base)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end))| (lubridate::year(data$time)==lubridate::year(base) & lubridate::month(data$time)==lubridate::month(base)))        
                                   id<-intersect(matched(data2,start,end),matched(data2,base,end))
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   quantity_base<-quantities(data2,period=base,set=id)
                                   sale_base<-price_start*quantity_base
                                   vold<-sum(sale_base)  
                                   result<-c(result,sum(sale_base/vold*(price_end/price_start)))  
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                   return(result)      
                                   }
                                   #returning one value
                                   else
                                   {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end))| (lubridate::year(data$time)==lubridate::year(base) & lubridate::month(data$time)==lubridate::month(base)))        
                                   id<-intersect(matched(data,start,end),matched(data,base,end))
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   quantity_base<-quantities(data,period=base,set=id)
                                   sale_base<-price_start*quantity_base
                                   vold<-sum(sale_base)  
                                   return(sum(sale_base/vold*(price_end/price_start)))
                                   }  
                                   }
#' @title  Calculating the bilateral geometric Lowe price index
#'
#' @description This function returns a value (or vector of values) of the bilateral geometric Lowe price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the geometric Lowe price index formula (as character) limited to the year and month, e.g. "2020-01"
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname geolowe
#' @return The function returns a value (or vector of values) of the bilateral geometric Lowe price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' geolowe(milk, start="2019-01", end="2020-01",base="2018-12")
#' \donttest{geolowe(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

geolowe<-function(data,start,end,base=start,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   base<-paste(base,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   base<-as.Date(base)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   data2<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end))| (lubridate::year(data$time)==lubridate::year(base) & lubridate::month(data$time)==lubridate::month(base)))        
                                   id<-intersect(matched(data2,start,end),matched(data2,base,end))
                                   price_end<-prices(data2,period=end,set=id)
                                   price_start<-prices(data2,period=start,set=id)
                                   quantity_base<-quantities(data2,period=base,set=id)
                                   sale_base<-price_start*quantity_base
                                   sale_base<-sale_base/sum(sale_base)
                                   result<-c(result,prod((price_end/price_start)^(sale_base)))
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                  
                                   return(result)      
                                   }
                                   #returning one value
                                   else {
                                   data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end))| (lubridate::year(data$time)==lubridate::year(base) & lubridate::month(data$time)==lubridate::month(base)))                                                              
                                   id<-intersect(matched(data,start,end),matched(data,base,end))
                                   price_end<-prices(data,period=end,set=id)
                                   price_start<-prices(data,period=start,set=id)
                                   quantity_base<-quantities(data,period=base,set=id)
                                   sale_base<-price_start*quantity_base
                                   sale_base<-sale_base/sum(sale_base)
                                   return (prod((price_end/price_start)^(sale_base)))
                                   }
                                   }

#' @title  Calculating the bilateral AG Mean price index
#'
#' @description This function returns a value (or vector of values) of the bilateral AG Mean price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param sigma The elasticity of substitution parameter (as numeric)
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname agmean
#' @return The function returns a value (or vector of values) of the bilateral AG Mean price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Lent J., & Dorfman,A. H. (2009). \emph{Using a Weighted Average of Base Period Price Indexes to Approximate a Superlative Index.} Journal of Official Statistics, 25(1), 139-149.}
#' @examples 
#' agmean(milk, start="2019-01", end="2020-01",sigma=0.5)
#' \donttest{agmean(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export


agmean<-function(data,start,end,sigma=0.7,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   if (sigma==1) stop("A specification of the parameter 'sigma' is wrong")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #returning vector of values
                                   if (interval==TRUE) { result<-c(1)
                                   end2<-end
                                   end<-start
                                   lubridate::month(end)<-lubridate::month(end)+1
                                   while (end<=end2)
                                       {
                                   t<-substr(end,0,7)
                                   date<-c(date,t)
                                   ag<-geolaspeyres(data, substr(start,0,7),substr(end,0,7))*sigma+laspeyres(data, substr(start,0,7),substr(end,0,7))*(1-sigma)           
                                   result<-c(result,ag)
                                   lubridate::month(end)<-lubridate::month(end)+1
                                       }
                                   return(result)      
                                   }
                                   #returning one value
                                   else {
                                   ag<-geolaspeyres(data, substr(start,0,7),substr(end,0,7))*sigma+laspeyres(data, substr(start,0,7),substr(end,0,7))*(1-sigma)
                                   return (ag)
                                   }
                                   }


#chain indices

#' @title  Calculating the monthly chained Jevons price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Jevons price index
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is also needed because this  function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chjevons
#' @return The function returns a value (or vector of values) of the monthly chained Jevons price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).    
#' @references
#' {Jevons, W. S., (1865). \emph{The variation of prices and the value of the currency since 1782}. J. Statist. Soc. Lond., 28, 294-320.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chjevons(milk, start="2018-12", end="2020-01")
#' \donttest{chjevons(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
chjevons<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   date<-c()
                                   while (start<=end)
                                   {
                                   t<-substr(start,0,7)
                                   date<-c(date,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                   }
                                   f<-function (i) return (jevons(data, start=date[i],end=date[i+1]))
                                   ind<-seq(1:(length(date)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                        }
                                   return(chained)    
                                   }


#' @title  Calculating the monthly chained Carli price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Carli price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chcarli
#' @return The function returns a value (or vector of values) of the monthly chained Carli price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Carli, G. (1804). \emph{Del valore e della proporzione de'metalli monetati}. Scrittori Classici Italiani di Economia Politica, 13, 297-336.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chcarli(milk, start="2018-12", end="2020-01")
#' \donttest{chcarli(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
chcarli<-function(data,start,end, interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                   {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                   }
                                   f<-function (i) return (carli(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained Dutot price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Dutot price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chdutot
#' @return The function returns a value (or vector of values) of the monthly chained Dutot price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).     
#' @references
#' {Dutot, C. F., (1738). \emph{Reflexions Politiques sur les Finances et le Commerce}. The Hague: Les Freres Vaillant et Nicolas Prevost, Vol. 1.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chdutot(milk, start="2018-12", end="2020-01")
#' \donttest{chdutot(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
chdutot<-function(data,start,end, interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                   {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                   }
                                   f<-function (i) return (dutot(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                         }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained CSWD price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Carruthers-Sellwood-Ward-Dalen (CSWD) price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chcswd
#' @return The function returns a value (or vector of values) of the monthly chained CSWD price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Carruthers, A.G., Sellwood, D. J, Ward, P. W. (1980). \emph{Recent developments in the retail price index}. Journal of the Royal Statistical Society. Series D (The Statisticain), 29(1), 1-32.}
#'
#' {Dalen, J. (1992). \emph{Recent developments in the retail price index}. The Statistician, 29(1),  1-32.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chcswd(milk, start="2018-12", end="2020-01")
#' \donttest{chcswd(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chcswd<-function(data,start,end, interval=FALSE) { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                   {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                   }
                                   f<-function (i) return (cswd(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                   }

#' @title  Calculating the monthly chained harmonic price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained "unnamed" harmonic price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chharmonic
#' @return The function returns a value (or vector of values) of the monthly chained harmonic price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chharmonic(milk, start="2018-12", end="2020-01")
#' \donttest{chharmonic(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
chharmonic<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                  if (nrow(data)==0) stop("A data frame is empty") 
                                  start<-paste(start,"-01",sep="")
                                  end<-paste(end,"-01",sep="")
                                  start<-as.Date(start)
                                  end<-as.Date(end)
                                  dates<-c()
                                  while (start<=end)
                                        {
                                  t<-substr(start,0,7)
                                  dates<-c(dates,t)
                                  lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                  f<-function (i) return (harmonic(data, start=dates[i],end=dates[i+1]))
                                  ind<-seq(1:(length(dates)-1))
                                  chained1<-sapply(ind, f)
                                  chained<-prod(chained1)
                                  if (interval==TRUE) {#optional returning all fixed base chain indices
                                  chained<-c(1)
                                  for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                        }
                                  return(chained)    
                                  }


#' @title  Calculating the monthly chained BMW price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Balk-Mehrhoff-Walsh (BMW)  price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chbmw
#' @return The function returns a value (or vector of values) of the monthly chained BMW price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Mehrhoff, J.(2007). \emph{A linear approximation to the Jevons index}. In: Von der Lippe (2007): Index Theory and Price Statistics, Peter Lang: Berlin, Germany.}
#'
#' {(2018). \emph{Harmonised Index of Consumer Prices (HICP). Methodological Manual}. Publication Office of the European union, Luxembourg.}
#' @examples 
#' chbmw(milk, start="2018-12", end="2020-01")
#' \donttest{chbmw(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
chbmw<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                  if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                       {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (bmw(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                  }


#' @title  Calculating the monthly chained Laspeyres price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Laspeyres price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chlaspeyres
#' @return The function returns a value (or vector of values) of the monthly chained Laspeyres price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Laspeyres, E. (1871). \emph{Die Berechnung einer mittleren Waarenpreissteigerung}. Jahrbucher fur Nationalokonomie und Statistik 16, 296-314.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chlaspeyres(milk, start="2018-12", end="2020-01")
#' \donttest{chlaspeyres(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
chlaspeyres<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                       {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (laspeyres(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained Paasche price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Paasche price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chpaasche
#' @return The function returns a value (or vector of values) of the monthly chained Paasche price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Paasche, H. (1874). \emph{Uber die Preisentwicklung der letzten Jahre nach den Hamburger Borsennotirungen}. Jahrbucher fur Nationalokonomie und Statistik, 12, 168-178.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chpaasche(milk, start="2018-12", end="2020-01")
#' \donttest{chpaasche(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chpaasche<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                    dates<-c()
                                   while (start<=end)
                                       {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (paasche(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained Fisher price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Fisher price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chfisher
#' @return The function returns a value (or vector of values) of the monthly chained Fisher price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).  
#' @references
#' {Fisher, I. (1922). \emph{The Making of Index Numbers}. Boston: Houghton Mifflin.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' \donttest{chfisher(milk, start="2018-12", end="2020-01")}
#' \donttest{chfisher(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chfisher<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                       {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (fisher(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained Tornqvist price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Tornqvist price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chtornqvist
#' @return The function returns a value (or vector of values) of the monthly chained Tornqvist price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Tornqvist, L. (1936). \emph{The Bank of Finland's Consumption Price Index}. Bank of Finland Monthly Bulletin 10, 1-8.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' \donttest{chtornqvist(milk, start="2018-12", end="2020-01")}
#' \donttest{chtornqvist(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chtornqvist<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                       {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (tornqvist(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                        }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained geo-logarithmic Laspeyres price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained geo-logarithmic Laspeyres price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chgeolaspeyres
#' @return The function returns a value (or vector of values) of the monthly chained geo-logarithmic Laspeyres price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chgeolaspeyres(milk, start="2018-12", end="2020-01")
#' \donttest{chgeolaspeyres(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export
chgeolaspeyres<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                        {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (geolaspeyres(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                        }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained geo-logarithmic Paasche price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained geo-logarithmic Paasche price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chgeopaasche
#' @return The function returns a value (or vector of values) of the monthly chained geo-logarithmic Paasche price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chgeopaasche(milk, start="2018-12", end="2020-01")
#' \donttest{chgeopaasche(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chgeopaasche<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                   {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (geopaasche(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                        }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained Drobisch price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Drobisch price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chdrobisch
#' @return The function returns a value (or vector of values) of the monthly chained Drobisch price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
 #' {Drobisch, M. W. (1871). \emph{Ueber einige Einwurfe gegen die in diesen Jahrbuchern veroffentlichte neue Methode, die Veranderungen der Waarenpreise und des Geldwerths zu berechten}.Jahrbucher fur Nationalokonomie und Statistik, Vol. 16, s. 416-427.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' \donttest{chdrobisch(milk, start="2018-12", end="2020-01")}
#' \donttest{chdrobisch(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chdrobisch<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                       {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (drobisch(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                  }
#' @title  Calculating the monthly chained Marshall-Edgeworth price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Marshall-Edgeworth price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chmarshall_edgeworth
#' @return The function returns a value (or vector of values) of the monthly chained Marshall-Edgeworth price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
 #' {Marshall, A. (1887). \emph{Remedies for Fluctuations of General Prices}. Contemporary Review, 51, 355-375.}
#'
#' {Edgeworth, F. Y. (1887). \emph{Measurement of Change in Value of Money I}. The first Memorandum presented to the British Association for the Advancement of Science; reprinted in Papers Relating to Political Economy, Vol. 1, New York, Burt Franklin, s. 1925.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' \donttest{chmarshall_edgeworth(milk, start="2018-12", end="2020-01")}
#' \donttest{chmarshall_edgeworth(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export 

chmarshall_edgeworth<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                      {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (marshall_edgeworth(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                  }            
#' @title  Calculating the monthly chained Walsh price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Walsh price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chwalsh
#' @return The function returns a value (or vector of values) of the monthly chained Walsh price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Walsh, C. M. (1901). \emph{The Measurement of General Exchange Value}. The MacMillan Company, New York.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' \donttest{chwalsh(milk, start="2018-12", end="2020-01")}
#' \donttest{chwalsh(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chwalsh<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                        {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (walsh(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained Bialek price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Bialek price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chbialek
#' @return The function returns a value (or vector of values) of the monthly chained Bialek price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Von der Lippe, P. (2012). \emph{Some short notes on the price index of Jacek Bialek}. Econometrics (Ekonometria). 1(35), 76-83.}
#'
#' {Bialek, J. (2013). \emph{Some Remarks on the Original Price Index Inspired by the Notes of Peter von der Lippe}. Econometrics (Ekonometria), 3(41), 40-54.}
#'
#' {Bialek, J. (2014). \emph{Simulation Study of an Original Price Index Formula}. Communications in Statistics - Simulation and Computation, 43(2), 285-297}
#' @examples 
#' \donttest{chbialek(milk, start="2018-12", end="2020-01")}
#' \donttest{chbialek(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chbialek<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                       {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (bialek(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained Banajree price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Banajree price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chbanajree
#' @return The function returns a value (or vector of values) of the monthly chained Banajree price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Banajree, K. S. (1977). \emph{On the factorial approach providing the true index of cost of living.} Gottingen : Vandenhoeck und Ruprecht.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' \donttest{chbanajree(milk, start="2018-12", end="2020-01")}
#' \donttest{chbanajree(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chbanajree<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                       {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (banajree(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained Davies price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Davies price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chdavies
#' @return The function returns a value (or vector of values) of the monthly chained Davies price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Davies, G. R. (1924). \emph{The Problem of a Standard Index Number Formula.} Journal of the American Statistical Association, 19 (146), 180-188.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' chdavies(milk, start="2018-12", end="2020-01")
#' \donttest{chdavies(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chdavies<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                       {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (davies(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained Stuvel price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Stuvel price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chstuvel
#' @return The function returns a value (or vector of values) of the monthly chained Stuvel price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Stuvel, G. (1957). \emph{A New Index Number Formula.} Econometrica, 25, 123-131.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' \donttest{chstuvel(milk, start="2018-12", end="2020-01")}
#' \donttest{chstuvel(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chstuvel<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                       {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (stuvel(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                        }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained Palgrave price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Palgrave price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chpalgrave
#' @return The function returns a value (or vector of values) of the monthly chained Palgrave price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Palgrave, R. H. I. (1886). \emph{Currency and Standard of Value in England, France and India and the Rates of Exchange Between these Countries.} Memorandum submitted to the Royal Commission on Depression of trade and Industry, Third Report, Appendix B, 312-390.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' \donttest{chpalgrave(milk, start="2018-12", end="2020-01")}
#' \donttest{chpalgrave(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chpalgrave<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                       {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (palgrave(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained Geary-Khamis price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Geary-Khamis price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chgeary_khamis
#' @return The function returns a value (or vector of values) of the monthly chained Geary-Khamis price index depending on the \code{interval} parameter (please use \code{\link{gk}} function to calculate the multilateral Geary-Khamis price index). If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Geary, R. G. (1958). \emph{A Note on Comparisons of Exchange Rates and Purchasing Power between Countries.} Journal of the Royal Statistical Society, Series A, 121, 97-99.}
#'
#' {Khamis, S. H. (1970). \emph{Properties and Conditions for the Existence of a new Type of Index Number.} Sankhya Series B32, 81-98.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' \donttest{chgeary_khamis(milk, start="2018-12", end="2020-01")}
#' \donttest{chgeary_khamis(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chgeary_khamis<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                       {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (geary_khamis(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                  }


#' @title  Calculating the monthly chained Lehr price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Lehr price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chlehr
#' @return The function returns a value (or vector of values) of the monthly chained Lehr price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).  
#' @references
#' {Lehr, J. (1885). \emph{Beitrage zur Statistik der Preise, insbesondere des Geldes und des Holzes.} J. D. Sauerlander, Frankfurt am Main.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' \donttest{chlehr(milk, start="2018-12", end="2020-01")}
#' \donttest{chlehr(milk, start="2018-12", end="2020-01", TRUE)}
#' @export

chlehr<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                  if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                       {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (lehr(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                  }


#' @title  Calculating the monthly chained Vartia-I price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Vartia-I price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chvartia
#' @return The function returns a value (or vector of values) of the monthly chained Vartia-I price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Vartia, Y. 0. (1976). \emph{Ideal Log-Change Index Numbers .}  Scandinavian Journal of Statistics 3(3), 121-126.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' \donttest{chvartia(milk, start="2018-12", end="2020-01")}
#' \donttest{chvartia(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chvartia<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                       {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (vartia(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained Vartia-II (Sato-Vartia) price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Vartia-II (Sato-Vartia) price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chsato_vartia
#' @return The function returns a value (or vector of values) of the monthly chained Vartia-II (Sato-Vartia) price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Sato, K. (1976). \emph{The Ideal Log-Change Index Number.}  The Review of Economics and Statistics, 58(2), 223-228.}
#'
#' {Vartia, Y. 0. (1976). \emph{Ideal Log-Change Index Numbers .}  Scandinavian Journal of Statistics 3(3), 121-126.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' \donttest{chsato_vartia(milk, start="2018-12", end="2020-01")}
#' \donttest{chsato_vartia(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chsato_vartia<-function(data,start,end,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                       {
                                       t<-substr(start,0,7)
                                       dates<-c(dates,t)
                                       lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (sato_vartia(data, start=dates[i],end=dates[i+1]))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained Lloyd-Moulton price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Lloyd-Moulton price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param sigma The elasticity of substitution parameter (as numeric).
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chlloyd_moulton
#' @return The function returns a value (or vector of values) of the monthly chained Lloyd-Moulton price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Lloyd, P. J. (1975). \emph{Substitution Effects and Biases in Nontrue Price Indices.}  The American Economic Review, 65, 301-313.}
#'
#' {Moulton,  B.  R.  (1996). \emph{Constant  Elasticity  Cost-of-Living  Index  in  Share-Relative  Form.}  Washington DC: U. S. Bureau of Labor Statistics, mimeograph}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#'
#' {Von der Lippe, P. (2007). \emph{Index Theory and Price Statistics}. Peter Lang: Berlin, Germany.}
#' @examples 
#' chlloyd_moulton(milk, start="2018-12", end="2020-01",sigma=0.9)
#' \donttest{chlloyd_moulton(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chlloyd_moulton<-function(data,start,end,sigma=0.7,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   if (sigma==1) stop("A specification of the parameter 'sigma' is wrong")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                       {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (lloyd_moulton(data, start=dates[i],end=dates[i+1],sigma))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained AG Mean price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained AG Mean price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param sigma The elasticity of substitution parameter (as numeric).
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chagmean
#' @return The function returns a value (or vector of values) of the monthly chained AG Mean price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Lent J., & Dorfman,A. H. (2009). \emph{Using a Weighted Average of Base Period Price Indexes to Approximate a Superlative Index.} Journal of Official Statistics, 25(1), 139-149.}
#' @examples 
#' \donttest{chagmean(milk, start="2019-01", end="2020-01",sigma=0.5)}
#' \donttest{chagmean(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chagmean<-function(data,start,end,sigma=0.7,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   if (sigma==1) stop("A specification of the parameter 'sigma' is wrong")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                        {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (agmean(data, start=dates[i],end=dates[i+1],sigma))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                        }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained Young price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Young price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the Young price index formula (as character) limited to the year and month, e.g. "2020-01".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chyoung
#' @return The function returns a value (or vector of values) of the monthly chained Young price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Young, A. H. (1992). \emph{Alternative Measures of Change in Real Output and Prices.}  Survey of Current Business, 72, 32-48.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chyoung(milk, start="2019-01", end="2020-01",base="2018-12")
#' \donttest{chyoung(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chyoung<-function(data,start,end, base=start,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                    
                                       {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (young(data, start=dates[i],end=dates[i+1],base))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                        }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained geometric Young price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained geometric Young price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the geometric Young price index formula (as character) limited to the year and month, e.g. "2020-01".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chgeoyoung
#' @return The function returns a value (or vector of values) of the monthly chained geometric Young price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Young, A. H. (1992). \emph{Alternative Measures of Change in Real Output and Prices.}  Survey of Current Business, 72, 32-48.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chgeoyoung(milk, start="2019-01", end="2020-01",base="2018-12")
#' \donttest{chgeoyoung(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chgeoyoung<-function(data,start,end, base=start,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")  
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                       {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (geoyoung(data, start=dates[i],end=dates[i+1],base))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained Lowe price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained Lowe price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the Lowe price index formula (as character) limited to the year and month, e.g. "2020-01".
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chlowe
#' @return The function returns a value (or vector of values) of the monthly chained Lowe price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).  
#' @references
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chlowe(milk, start="2019-01", end="2020-01",base="2018-12")
#' \donttest{chlowe(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chlowe<-function(data,start,end, base=start,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                       {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (lowe(data, start=dates[i],end=dates[i+1],base))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                        }
                                   return(chained)    
                                  }

#' @title  Calculating the monthly chained geometric Lowe price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained geometric Lowe price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the geometric Lowe price index formula (as character) limited to the year and month, e.g. "2020-01"
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chgeolowe
#' @return The function returns a value (or vector of values) of the monthly chained geometric Lowe price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).  
#' @references
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' chgeolowe(milk, start="2019-01", end="2020-01",base="2018-12")
#' \donttest{chgeolowe(milk, start="2018-12", end="2020-01", interval=TRUE)}
#' @export

chgeolowe<-function(data,start,end, base=start,interval=FALSE)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   dates<-c()
                                   while (start<=end)
                                        {
                                   t<-substr(start,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(start)<-lubridate::month(start)+1
                                       }
                                   f<-function (i) return (geolowe(data, start=dates[i],end=dates[i+1],base))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                       }
                                   return(chained)    
                                         }


#multilateral indices

#' @title  Calculating the multilateral GEKS price index
#'
#' @description This function returns a value of the multilateral GEKS price index (to be more precise: the GEKS index based on the Fisher formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geks
#' @return This function returns a value of the multilateral GEKS price index (to be more precise: the GEKS index based on the Fisher formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' \donttest{geks(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{geks(milk, start="2018-12", end="2019-12")}
#' @export

geks<-function(data,start,end, wstart=start,window=13)  { if (start==end) return (1)
      if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   wstart<-paste(wstart,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   wstart<-as.Date(wstart)
                                   #checking conditions
                                   if (window<2)  stop("window must be at least 2 months")
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (wstart>start) stop("parameters must satisfy: wstat<=start")
                                   wend<-wstart
                                   lubridate::month(wend)<-lubridate::month(wend)+window-1
                                   if (end>wend) stop("parameters must satisfy: end<wstart+window")
                                   start<-substr(start,0,7)
                                   end<-substr(end,0,7)
                                   dates<-c()
                                   while (wstart<=wend)
                                       {
                                       t<-substr(wstart,0,7)
                                       dates<-c(dates,t)
                                       lubridate::month(wstart)<-lubridate::month(wstart)+1
                                       }
                                   #main body
                                   gks<-function (tt) return (c(fisher(data,tt,end),fisher(data,tt,start)))
                                   vec<-sapply(dates, gks)
                                   geks<-prod(vec[1,]/vec[2,])
                                   geks<-geks^(1/window)
                                   return(geks)    
                                    }

#' @title  Extending the multilateral GEKS price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral GEKS price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geks_fbew
#' @return This function returns a value of the multilateral GEKS price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' \donttest{geks_fbew(milk, start="2018-12", end="2019-08")}
#' @export

geks_fbew<-function(data,start,end)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("a month of the 'start' parameter must be December")
                                   if (start==end) return (1)
                                   ind<-1
                                   last<-as.Date(start)
                                   years<-lubridate::year(end)-lubridate::year(start)
                                   #main body
                                   for (i in 1:years) {lubridate::year(last)<-lubridate::year(last)+1
                                                       new<-min(end,last)
                                                       ind<-ind*geks(data,substr(start,0,7),substr(new,0,7), window=dist(start, new)+1)
                                                       lubridate::year(start)<-lubridate::year(start)+1
                                                      }
                                   return (ind)
                                  
                                   }

#' @title  Extending the multilateral GEKS price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral GEKS price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geks_fbmw
#' @return This function returns a value of the multilateral GEKS price index extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#' @examples 
#' \donttest{geks_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

geks_fbmw<-function(data,start,end)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("a month of the 'start' parameter must be December")
                                   if (start==end) return (1)
                                   ind<-1
                                   last<-as.Date(start)
                                   years<-lubridate::year(end)-lubridate::year(start)
                                   #main body
                                   for (i in 1:years) {lubridate::year(last)<-lubridate::year(last)+1
                                                       new<-min(end,last)
                                                       ind<-ind*geks_fbmw2(data,substr(start,0,7),substr(new,0,7))
                                                       lubridate::year(start)<-lubridate::year(start)+1
                                                      }
                                   return (ind)
                                   }

#an additional function used in geks_fbmw
geks_fbmw2<-function(data,start,end)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   wstart<-end
                                   lubridate::year(wstart)<-lubridate::year(wstart)-1
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("a month of the 'start' parameter must be December")
                                   if (start==end) return (1)
                                   else return (geks(data, substr(start,0,7), substr(end,0,7), substr(wstart,0,7),window=13))
                                   }


#' @title  Calculating the multilateral GEKS price index based on the Walsh formula (GEKS-W)
#'
#' @description This function returns a value of the multilateral GEKS-W price index, i.e. the GEKS price index based on the superlative Walsh index formula. 
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksw
#' @return This function returns a value of the multilateral GEKS-W price index (to be more precise: the GEKS index based on the Walsh formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Walsh, C. M. (1901). \emph{The Measurement of General Exchange Value}. The MacMillan Company, New York.}
#'
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' \donttest{geksw(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{geksw(milk, start="2018-12", end="2019-12")}
#' @export


geksw<-function(data,start,end, wstart=start,window=13)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   wstart<-paste(wstart,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   wstart<-as.Date(wstart)
                                   #checking conditions
                                   if (window<2)  stop("window must be at least 2 months")
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (wstart>start) stop("parameters must satisfy: wstat<=start")
                                   wend<-wstart
                                   lubridate::month(wend)<-lubridate::month(wend)+window-1
                                   if (end>wend) stop("parameters must satisfy: end<wstart+window")
                                   start<-substr(start,0,7)
                                   end<-substr(end,0,7)
                                   dates<-c()
                                   while (wstart<=wend)
                                       {
                                   t<-substr(wstart,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(wstart)<-lubridate::month(wstart)+1
                                       }
                                   #main body
                                   gksw<-function (tt) return (c(walsh(data,tt,end),walsh(data,tt,start)))
                                   vec<-sapply(dates, gksw)
                                   geksw<-prod(vec[1,]/vec[2,])
                                   geksw<-geksw^(1/window)
                                   return(geksw)    
                                   }

#' @title  Extending the multilateral GEKS-W price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral GEKS-W price index (GEKS based on the Walsh formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksw_fbew
#' @return This function returns a value of the multilateral GEKS-W price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Walsh, C. M. (1901). \emph{The Measurement of General Exchange Value}. The MacMillan Company, New York.}
#'
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' geksw_fbew(milk, start="2018-12", end="2019-08")
#' @export
geksw_fbew<-function(data,start,end)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("a month of the 'start' parameter must be December")
                                   if (start==end) return (1)
                                   ind<-1
                                   last<-as.Date(start)
                                   years<-lubridate::year(end)-lubridate::year(start)
                                   #main body
                                   for (i in 1:years) {lubridate::year(last)<-lubridate::year(last)+1
                                                       new<-min(end,last)
                                                       ind<-ind*geksw(data,substr(start,0,7),substr(new,0,7), window=dist(start, new)+1)
                                                      lubridate::year(start)<-lubridate::year(start)+1
                                                      }
                                   return (ind)
                                  
                                   }

#' @title  Extending the multilateral GEKS-W price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral GEKS-W price index (GEKS based on the Walsh formula) extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksw_fbmw
#' @return This function returns a value of the multilateral GEKS-W price index extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Walsh, C. M. (1901). \emph{The Measurement of General Exchange Value}. The MacMillan Company, New York.}
#'
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#' @examples 
#' geksw_fbmw(milk, start="2019-12", end="2020-04")
#' @export

geksw_fbmw<-function(data,start,end)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("a month of the 'start' parameter must be December")
                                   if (start==end) return (1)
                                   ind<-1
                                   last<-as.Date(start)
                                   years<-lubridate::year(end)-lubridate::year(start)
                                   for (i in 1:years) {lubridate::year(last)<-lubridate::year(last)+1
                                                       new<-min(end,last)
                                                       ind<-ind*geksw_fbmw2(data,substr(start,0,7),substr(new,0,7))
                                                       lubridate::year(start)<-lubridate::year(start)+1
                                                      }
                                   return (ind)
                                   }

#an additional function used in geks_fbmw
geksw_fbmw2<-function(data,start,end)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   wstart<-end
                                   lubridate::year(wstart)<-lubridate::year(wstart)-1
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("a month of the 'start' parameter must be December")
                                   if (start==end) return (1)
                                   else return (geksw(data, substr(start,0,7), substr(end,0,7), substr(wstart,0,7),window=13))
                                   }



#' @title  Calculating the multilateral GEKS price index based on the Jevons formula (typical notation: GEKS-J) 
#'
#' @description This function returns a value of the multilateral GEKS-J price index (to be more precise: the GEKS index based on the Jevons formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksj
#' @return This function returns a value of the multilateral GEKS-J price index (to be more precise: the GEKS index based on the Jevons formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' geksj(milk, start="2019-01", end="2019-08",window=10)
#' geksj(milk, start="2018-12", end="2019-12")
#' @export


geksj<-function(data,start,end, wstart=start,window=13)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   wstart<-paste(wstart,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   wstart<-as.Date(wstart)
                                   #checking conditions
                                   if (window<2)  stop("window must be at least 2 months")
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (wstart>start) stop("parameters must satisfy: wstat<=start")
                                   wend<-wstart
                                   lubridate::month(wend)<-lubridate::month(wend)+window-1
                                   if (end>wend) stop("parameters must satisfy: end<wstart+window")
                                   start<-substr(start,0,7)
                                   end<-substr(end,0,7)
                                   dates<-c()
                                   while (wstart<=wend)
                                       {
                                   t<-substr(wstart,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(wstart)<-lubridate::month(wstart)+1
                                       }
                                   #main body
                                   gks<-function (tt) return (c(jevons(data,tt,end),jevons(data,tt,start)))
                                   vec<-sapply(dates, gks)
                                   geksj<-prod(vec[1,]/vec[2,])
                                   geksj<-geksj^(1/window)
                                   return(geksj)    
                                   }

#' @title  Extending the multilateral GEKS-J price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral GEKS-J price index (i.e. the GEKS price index based on the Jevons formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksj_fbew
#' @return This function returns a value of the multilateral GEKS-J price index (i.e. the GEKS price index based on the Jevons formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' geksj_fbew(milk, start="2018-12", end="2019-08")
#' @export
geksj_fbew<-function(data,start,end)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("a month of the 'start' parameter must be December")
                                   if (start==end) return (1)
                                   ind<-1
                                   last<-as.Date(start)
                                   years<-lubridate::year(end)-lubridate::year(start)
                                   #main body
                                   for (i in 1:years) {lubridate::year(last)<-lubridate::year(last)+1
                                                       new<-min(end,last)
                                                       ind<-ind*geksj(data,substr(start,0,7),substr(new,0,7), window=dist(start, new)+1)
                                                       lubridate::year(start)<-lubridate::year(start)+1
                                                      }
                                   return (ind)
                                   }

#' @title  Extending the multilateral GEKS-J price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral GEKS-J price index (i.e. the GEKS price index based on the Jevons formula) extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksj_fbmw
#' @return This function returns a value of the multilateral GEKS-J price index (i.e. the GEKS price index based on the Jevons formula) extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#' @examples 
#' geksj_fbmw(milk, start="2019-12", end="2020-04")
#' @export

geksj_fbmw<-function(data,start,end)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("a month of the 'start' parameter must be December")
                                   if (start==end) return (1)
                                   ind<-1
                                   last<-as.Date(start)
                                   years<-lubridate::year(end)-lubridate::year(start)
                                   #main body
                                   for (i in 1:years) {lubridate::year(last)<-lubridate::year(last)+1
                                                       new<-min(end,last)
                                                       ind<-ind*geksj_fbmw2(data,substr(start,0,7),substr(new,0,7))
                                                       lubridate::year(start)<-lubridate::year(start)+1
                                                      }
                                   return (ind)
                                  
                                   }

#an additional function used in geksj_fbmw
geksj_fbmw2<-function(data,start,end)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   wstart<-end
                                   lubridate::year(wstart)<-lubridate::year(wstart)-1
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("a month of the 'start' parameter must be December")
                                   if (start==end) return (1)
                                   else return (geksj(data, substr(start,0,7), substr(end,0,7), substr(wstart,0,7),window=13))
                                   }



#' @title  Calculating the multilateral GEKS price index based on the Tornqvist formula (typical notation: GEKS-T or CCDI)
#'
#' @description This function returns a value of the multilateral CCDI price index, i.e. the GEKS price index based on the superlative Tornqvist index formula. 
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname ccdi
#' @return This function returns a value of the multilateral CCDI price index (to be more precise: the GEKS index based on the Tornqvist formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Caves, D.W., Christensen, L.R. and Diewert, W.E. (1982). \emph{Multilateral comparisons of output, input, and productivity using superlative index numbers.} Economic Journal 92, 73-86.}
#' @examples 
#' \donttest{ccdi(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{ccdi(milk, start="2018-12", end="2019-12")}
#' @export
ccdi<-function(data,start,end, wstart=start,window=13)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   wstart<-paste(wstart,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   wstart<-as.Date(wstart)
                                   #checking conditions
                                   if (window<2)  stop("window must be at least 2 months")
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (wstart>start) stop("parameters must satisfy: wstat<=start")
                                   wend<-wstart
                                   lubridate::month(wend)<-lubridate::month(wend)+window-1
                                   if (end>wend) stop("parameters must satisfy: end<wstart+window")
                                   start<-substr(start,0,7)
                                   end<-substr(end,0,7)
                                   dates<-c()
                                   while (wstart<=wend)
                                       {
                                   t<-substr(wstart,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(wstart)<-lubridate::month(wstart)+1
                                       }
                                   #main body
                                   cc<-function (tt) return (c(tornqvist(data,tt,end),tornqvist(data,tt,start)))
                                   vec<-sapply(dates, cc)
                                   ccdi<-prod(vec[1,]/vec[2,])
                                   ccdi<-ccdi^(1/window)
                                   return(ccdi)    
                                   }

#' @title  Extending the multilateral CCDI price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral CCDI price index (GEKS based on the Tornqvist formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname ccdi_fbew
#' @return This function returns a value of the multilateral CCDI price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Caves, D.W., Christensen, L.R. and Diewert, W.E. (1982). \emph{Multilateral comparisons of output, input, and productivity using superlative index numbers.} Economic Journal 92, 73-86.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' ccdi_fbew(milk, start="2018-12", end="2019-08")
#' @export
ccdi_fbew<-function(data,start,end)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("a month of the 'start' parameter must be December")
                                   if (start==end) return (1)
                                   ind<-1
                                   last<-as.Date(start)
                                   years<-lubridate::year(end)-lubridate::year(start)
                                   #main body
                                   for (i in 1:years) {lubridate::year(last)<-lubridate::year(last)+1
                                   new<-min(end,last)
                                   ind<-ind*ccdi(data,substr(start,0,7),substr(new,0,7), window=dist(start, new)+1)
                                   lubridate::year(start)<-lubridate::year(start)+1
                                                      }
                                   return (ind)
                                   }

#' @title  Extending the multilateral CCDI price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral CCDI price index (GEKS based on the Tornqvist formula) extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname ccdi_fbmw
#' @return This function returns a value of the multilateral CCDI price index extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Caves, D.W., Christensen, L.R. and Diewert, W.E. (1982). \emph{Multilateral comparisons of output, input, and productivity using superlative index numbers.} Economic Journal 92, 73-86.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#' @examples 
#' \donttest{ccdi_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

ccdi_fbmw<-function(data,start,end)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("a month of the 'start' parameter must be December")
                                   if (start==end) return (1)
                                   ind<-1
                                   last<-as.Date(start)
                                   years<-lubridate::year(end)-lubridate::year(start)
                                   #main body
                                   for (i in 1:years) {lubridate::year(last)<-lubridate::year(last)+1
                                   new<-min(end,last)
                                   ind<-ind*ccdi_fbmw2(data,substr(start,0,7),substr(new,0,7))
                                   lubridate::year(start)<-lubridate::year(start)+1
                                                      }
                                   return (ind)
                                   }

#an additional function used in ccdi_fbmw
ccdi_fbmw2<-function(data,start,end)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   wstart<-end
                                   lubridate::year(wstart)<-lubridate::year(wstart)-1
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("a month of the 'start' parameter must be December")
                                   if (start==end) return (1)
                                   else return (ccdi(data, substr(start,0,7), substr(end,0,7), substr(wstart,0,7),window=13))
                                   }


#QU - index

#' @title  Calculating the quality adjusted unit value index (QU index)
#'
#' @description This function returns a value of the quality adjusted unit value index (QU index) for a given set of adjustment factors.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param v The data frame with adjustment factors for at least all matched prodIDs. It must contain two columns: \code{prodID} (as numeric or character) with unique product IDs and \code{values} (as positive numeric) with corresponding adjustment factors.
#' @rdname QU
#' @return This function returns a value of the quality adjusted unit value index (QU index) for a given set of adjustment factors (adjusted factors must be available for all matched prodIDs).  
#' @references
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' ## Creating a data frame with artificial adjustment factors 
#' ## (random numbers from uniform distribution U[1,2])
#' prodID<-unique(milk$prodID)
#' values<-stats::runif(length(prodID),1,2)
#' v<-data.frame(prodID,values)
#' ## Calculating the QU index for the created data frame 'v'
#' QU(milk, start="2018-12", end="2019-12", v)
#' @export


QU<-function(data, start, end, v)
{
if (start==end) return (1)  
if (nrow(data)==0) stop("A data frame is empty")
start<-paste(start,"-01",sep="")
end<-paste(end,"-01",sep="")
start<-as.Date(start)
end<-as.Date(end)
data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(start) & lubridate::month(data$time)==lubridate::month(start)) | (lubridate::year(data$time)==lubridate::year(end) & lubridate::month(data$time)==lubridate::month(end)))   
Gstart<-matched(data,start,start) 
Gend<-matched(data,end,end)
sale_end<-sales(data,period=end,set=Gend)
sale_start<-sales(data,period=start,set=Gstart)
quantity_end<-quantities(data,period=end,set=Gend)
quantity_start<-quantities(data,period=start,set=Gstart)
#main body
a<-sum(sale_end)
b<-sum(sale_start)
f<-function (prod) {vl<-dplyr::filter(v,v$prodID==prod)
vl<-sum(vl$values)}
val_end<-sapply(Gend,f)
val_start<-sapply(Gstart,f)
c<-sum(val_end*quantity_end)
d<-sum(val_start*quantity_start)
return ((a/b)/(c/d))
}


#' @title  Calculating the multilateral Geary-Khamis price index
#'
#' @description This function returns a value of the multilateral Geary-Khamis price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname gk
#' @return This function returns a value of the multilateral Geary-Khamis price index which considers the time window defined by \code{wstart} and \code{window} parameters. The Geary-Khamis price index is calculated by using a special iterative algorithm from \code{Chessa (2016)}. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Geary, R. G. (1958). \emph{A Note on Comparisons of Exchange Rates and Purchasing Power between Countries.} Journal of the Royal Statistical Society, Series A, 121, 97-99.}
#'
#' {Khamis, S. H. (1970). \emph{Properties and Conditions for the Existence of a new Type of Index Number.} Sankhya Series B32, 81-98.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' \donttest{gk(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{gk(milk, start="2018-12", end="2019-12")}
#' @export
gk<-function(data,start,end, wstart=start,window=13)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   wstart<-paste(wstart,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   wstart<-as.Date(wstart)
                                   #calculating end of the window
                                   wend<-wstart
                                   lubridate::month(wend)<-lubridate::month(wend)+window-1
                                   lubridate::day(wend)<-lubridate::days_in_month(wend)
                                   #checking conditions
                                   if (window<2)  stop("window must be at least 2 months")
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (wstart>start) stop("parameters must satisfy: wstart<=start")
                                   if (end>wend) stop("parameters must satisfy: end<wstart+window")
                                   #data filtration
                                   d<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
                                   prodID<-unique(d$prodID)
                                   if (length(prodID)<2) stop ("At least two prodIDs must be available during the considered time interval")
                                   #main body
                                   #initial values of indices
                                   index1<-rep(1,window)
                                   index2<-rep(2,window)
                                   #set of dates
                                   dates<-c()
                                   wst<-wstart
                                   while (wst<=wend)
                                       {
                                   t<-substr(wst,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(wst)<-lubridate::month(wst)+1
                                       }
                                   s<-function(tt) return (sales(d,period=tt,set=prodID))
                                   q<-function(tt) return (quantities(d,period=tt,set=prodID))
                                   expenditure<-sapply(dates,s)
                                   quantity<-sapply(dates,q)
                                   #quantity weights - quality adjusted factors vi
                                   while (sqrt(sum((index1 - index2)^2))>0.005)
                                   {
                                   val<-function (i)  {  
                                   xx<-function (tt) return (expenditure[i,tt]/index1[which(dates==tt)])
                                   yy<-function (tt) return (quantity[i,tt])
                                   x<-sum(sapply(dates,xx))
                                   y<-sum(sapply(dates,yy))
                                   return (x/y)
                                                      }
                                   num_prod<-seq(1:length(prodID))
                                   values<-sapply(num_prod, val)
                                   v<-data.frame(prodID,values)
                                   #series  of indices
                                   indd<-function(tt) return (QU(d,substr(wstart,0,7),tt,v))
                                   ind<-sapply(dates,indd)
                                   index2<-index1
                                   index1<-ind
                                   }
                                   result<-index1[which(dates==substr(end,0,7))]/index1[which(dates==substr(start,0,7))]
                                   result<-result[[1]]
                                   return (result)
                                    }

#an additional function used in gk_fbew
gkreal<-function(data,start,end)  {if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty") 
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("a month of the 'start' parameter must be December")
                                   #data filtration
                                   d<-dplyr::filter(data, data$time>=start & data$time<=end)
                                   prodID<-unique(d$prodID)
                                   #main body
                                   #initial values of indices
                                   index1<-c()
                                   index2<-c()
                                   #set of dates
                                   dates<-c()
                                   st<-start
                                   while (st<=end)
                                       {
                                   index1<-c(index1,1)
                                   index2<-c(index2,2)
                                   t<-substr(st,0,7)
                                   dates<-c(dates,t)
                                   lubridate::month(st)<-lubridate::month(st)+1
                                       }
                                   s<-function(tt) return (sales(d,period=tt,set=prodID))
                                   q<-function(tt) return (quantities(d,period=tt,set=prodID))
                                   expenditure<-sapply(dates,s)
                                   quantity<-sapply(dates,q)
                                   #quantity weights - quality adjusted factors vi
                                   while (sqrt(sum((index1 - index2)^2))>0.01)
                                   {
                                     
                                   val<-function (i)  {  
                                   xx<-function (tt) return (expenditure[i,tt]/index1[which(dates==tt)])
                                   yy<-function (tt) return (quantity[i,tt])
                                   x<-sum(sapply(dates,xx))
                                   y<-sum(sapply(dates,yy))
                                   return (x/y)
                                                      }
                                   num_prod<-seq(1:length(prodID))
                                   values<-sapply(num_prod, val)
                                   v<-data.frame(prodID,values)
                                   #series  of indices
                                   indd<-function(tt) return (QU(d,substr(start,0,7),tt,v))
                                   ind<-sapply(dates,indd)
                                   index2<-index1
                                   index1<-ind
                                   }
                                   result<-index1[which(dates==substr(end,0,7))]
                                   result<-result[[1]]
                                   return (result)
                                   }

#' @title  Extending the multilateral Geary-Khamis price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral Geary-Khamis price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname gk_fbew
#' @return This function returns a value of the multilateral Geary-Khamis price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Geary, R. G. (1958). \emph{A Note on Comparisons of Exchange Rates and Purchasing Power between Countries.} Journal of the Royal Statistical Society, Series A, 121, 97-99.}
#'
#' {Khamis, S. H. (1970). \emph{Properties and Conditions for the Existence of a new Type of Index Number.} Sankhya Series B32, 81-98.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' gk_fbew(milk, start="2018-12", end="2019-08")
#' @export

gk_fbew<-function(data,start,end)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("a month of the 'start' parameter must be December")
                                   if (start==end) return (1)
                                   ind<-1
                                   last<-as.Date(start)
                                   years<-lubridate::year(end)-lubridate::year(start)
                                   #main body
                                   for (i in 1:years) {lubridate::year(last)<-lubridate::year(last)+1
                                   new<-min(end,last)
                                   ind<-ind*gkreal(data,substr(start,0,7),substr(new,0,7))
                                   lubridate::year(start)<-lubridate::year(start)+1
                                                      }
                                   return (ind)
                                   }

#' @title  Extending the multilateral Geary-Khamis price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral Geary-Khamis price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname gk_fbmw
#' @return This function returns a value of the multilateral Geary-Khamis price index extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Geary, R. G. (1958). \emph{A Note on Comparisons of Exchange Rates and Purchasing Power between Countries.} Journal of the Royal Statistical Society, Series A, 121, 97-99.}
#'
#' {Khamis, S. H. (1970). \emph{Properties and Conditions for the Existence of a new Type of Index Number.} Sankhya Series B32, 81-98.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#' @examples 
#' \donttest{gk_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

gk_fbmw<-function(data,start,end)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("a month of the 'start' parameter must be December")
                                   if (start==end) return (1)
                                   ind<-1
                                   last<-as.Date(start)
                                   years<-lubridate::year(end)-lubridate::year(start)
                                   #main body
                                   for (i in 1:years) {lubridate::year(last)<-lubridate::year(last)+1
                                   new<-min(end,last)
                                   ind<-ind*gk_fbmw2(data,substr(start,0,7),substr(new,0,7))
                                   lubridate::year(start)<-lubridate::year(start)+1
                                                      }
                                   return (ind)
                                   }

#an additional function used in gk_fbmw
gk_fbmw2<-function(data,start,end) { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   wstart<-end
                                   lubridate::year(wstart)<-lubridate::year(wstart)-1
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("a month of the 'start' parameter must be December")
                                   if (start==end) return (1)
                                   else return (gk(data, substr(start,0,7), substr(end,0,7), substr(wstart,0,7),window=13))
                                   }


#distance between two dates (in months) - it is not exported
dist<-function(data1,data2)
{
  n<-0
  while (data1<=data2)
  {
  n<-n+1 
  lubridate::month(data1)<-lubridate::month(data1)+1  
  }
  return (n-1)
}

#' @title  Calculating the multilateral TPD price index
#'
#' @description This function returns a value of the multilateral TPD (Time Product Dummy) price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname tpd
#' @return This function returns a value of the multilateral TPD price index which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). Please note that a Weighted Least Squares (WLS) regression is run with the expenditure shares in each period serving as weights.To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).  
#' @references
#' {de  Haan,  J.  and  F.  Krsinich  (2014). \emph{Time  Dummy  Hedonic  and  Quality-Adjusted  Unit Value Indexes: Do They Really Differ?} Paper presented at the Society for Economic Measurement Conference, 18-20 August 2014, Chicago, U.S.}
#' @examples 
#' \donttest{tpd(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{tpd(milk, start="2018-12", end="2019-12")}
#' @export
tpd<-function(data,start,end, wstart=start,window=13)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   wstart<-paste(wstart,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   wstart<-as.Date(wstart)
                                   #calculating end of the window
                                   wend<-wstart
                                   lubridate::month(wend)<-lubridate::month(wend)+window-1
                                   lubridate::day(wend)<-lubridate::days_in_month(wend)
                                   #checking conditions
                                   if (window<2)  stop("window must be at least 2 months")
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (wstart>start) stop("parameters must satisfy: wstart<=start")
                                   if (end>wend) stop("parameters must satisfy: end<wstart+window")
                                   #data filtration,i.e. we obtan products which are available during the time window
                                   d<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
                                   products<-unique(d$prodID)
                                   if (length(products)<2) stop ("At least two prodIDs must be available during the considered time interval")
                                   #main body
                                   dates<-c()  #vector with all dates written as 7 signs from the time interval [1,T]
                                   wst<-wstart
                                   lubridate::month(wst)<-lubridate::month(wst)+1 
                                   while (wst<=wend)
                                   {
                                   dates<-c(dates,substr(wst,0,7))
                                   lubridate::month(wst)<-lubridate::month(wst)+1  
                                   }
                                   #dates of availability of products
                                   av<-list() #list of dates when the given i-th product is available
                                   for (i in (1:length(products)))
                                        {
                                   d0<-dplyr::filter(d,d$prodID==products[i])
                                   time<-unique(d0$time)
                                   time<-substr(time,0,7)
                                   av[[i]]<-time
                                        }
                                   av_dates<-c() #avaiable dates as one vector
                                   for (i in (1:length(products))) av_dates<-c(av_dates,av[[i]])
                                   #vector connected with the alfa parameter in TPD model
                                   alfa<-replicate(length(av_dates),1) #unit vector
                                   #unit vectors corresponding to products
                                   vec<-list()
                                   for (i in (1:length(products))) vec[[i]]<-replicate(length(av[[i]]),1)
                                   #gamma vectors connected with Di (i=1,2,...N-1)
                                   gm<-list()
                                   for (i in (1:(length(products)-1))) {  bin<-c()
                                      for (k in (1:length(products)))
                                      {
                                      if (i==k) bin<-c(bin,vec[[k]])
                                      else bin<-c(bin,vec[[k]]-1)
                                      }  
                                      gm[[i]]<-bin
                                                                       }
                                   #sigma vectors for time moments 1,2,...,T
                                   sigma<-list()
                                   for (i in (1:length(dates)))  {sg<-c()
                                      for (k in (1:length(av_dates))) {
                                      if (dates[i]==av_dates[k]) sg<-c(sg,1)
                                      else sg<-c(sg,0)
                                                                      }
                                      sigma[[i]]<-sg
                                                                 }
                                   #vector of log prices
                                   logprices<-c()
                                   for (i in (1:length(products))) {for (k in (1:length(av[[i]]))) 
                                                                   logprices<-c(logprices,log(price(d,period=av[[i]][k],ID=products[i])))
                                                                    }
                                   #vector of expenditures
                                   weights<-c()
                                   for (i in (1:length(products))) {for (k in (1:length(av[[i]]))) 
weights<-c(weights,price(d,period=av[[i]][k],ID=products[i])*quantity(d,period=av[[i]][k],ID=products[i]))
                                                                  }
                                   weights<-diag(weights)
                                   #creating matrix X
                                   x<-alfa
                                   for (i in (1:(length(products)-1))) x<-c(x,gm[[i]])
                                   for (i in (1:length(dates))) x<-c(x,sigma[[i]])
                                   x<-matrix(x,nrow=length(av_dates),ncol=length(products)+length(dates))
                                   #estimation of parameters
                                   b<-t(x) %*% weights
                                   b<-b %*% x
                                   b<-solve(b)
                                   b<-b %*% t(x)
                                   b<-b %*% weights
                                   b<-b %*% logprices
                                   #index calculation
                                   if (wstart==start) return (exp(b[length(products)+dist(wstart,end)]))
                                   else return (exp(b[length(products)+dist(wstart,end)])/exp(b[length(products)+dist(wstart,start)]))
                                   }

#' @title  Extending the multilateral TPD price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral TPD price index (Time Product Dummy index) extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname tpd_fbew
#' @return This function returns a value of the multilateral TPD price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).  
#' @references
#' {de  Haan,  J.  and  F.  Krsinich  (2014). \emph{Time  Dummy  Hedonic  and  Quality-Adjusted  Unit Value Indexes: Do They Really Differ?} Paper presented at the Society for Economic Measurement Conference, 18-20 August 2014, Chicago, U.S.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples 
#' \donttest{tpd_fbew(milk, start="2018-12", end="2019-08")}
#' @export

tpd_fbew<-function(data,start,end)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("a month of the 'start' parameter must be December")
                                   if (start==end) return (1)
                                   ind<-1
                                   last<-as.Date(start)
                                   years<-lubridate::year(end)-lubridate::year(start)
                                   #main body
                                   for (i in 1:years) {lubridate::year(last)<-lubridate::year(last)+1
                                   new<-min(end,last)
                                   ind<-ind*tpd(data,substr(start,0,7),substr(new,0,7), window=dist(start, new)+1)
                                   lubridate::year(start)<-lubridate::year(start)+1
                                                      }
                                   return (ind)
                                   }

#' @title  Extending the multilateral TPD price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral TPD price index (Time Product Dummy index) extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname tpd_fbmw
#' @return This function returns a value of the multilateral TPD price index extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {de  Haan,  J.  and  F.  Krsinich  (2014). \emph{Time  Dummy  Hedonic  and  Quality-Adjusted  Unit Value Indexes: Do They Really Differ?} Paper presented at the Society for Economic Measurement Conference, 18-20 August 2014, Chicago, U.S.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#' @examples 
#' \donttest{tpd_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

tpd_fbmw<-function(data,start,end)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("a month of the 'start' parameter must be December")
                                   if (start==end) return (1)
                                   ind<-1
                                   last<-as.Date(start)
                                   years<-lubridate::year(end)-lubridate::year(start)
                                   #main body
                                   for (i in 1:years) {lubridate::year(last)<-lubridate::year(last)+1
                                   new<-min(end,last)
                                   ind<-ind*tpd_fbmw2(data,substr(start,0,7),substr(new,0,7))
                                   lubridate::year(start)<-lubridate::year(start)+1
                                                      }
                                   return (ind)
                                   }

#an additional function used in tpd_fbmw
tpd_fbmw2<-function(data,start,end)  { if (start==end) return (1)
                                   if (nrow(data)==0) stop("A data frame is empty")
                                   start<-paste(start,"-01",sep="")
                                   end<-paste(end,"-01",sep="")
                                   start<-as.Date(start)
                                   end<-as.Date(end)
                                   wstart<-end
                                   lubridate::year(wstart)<-lubridate::year(wstart)-1
                                   #checking conditions
                                   if (start>end) stop("parameters must satisfy: start<=end")
                                   if (lubridate::month(start)<12) stop("A month of the 'start' parameter must be December")
                                   if (start==end) return (1)
                                   else return (tpd(data, substr(start,0,7), substr(end,0,7), substr(wstart,0,7),window=13))
                                   }


#' @title  Extending the multilateral TPD price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral TPD price index (Time Product Dummy index) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname tpd_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral TPD price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {de  Haan,  J.  and  F.  Krsinich  (2014). \emph{Time  Dummy  Hedonic  and  Quality-Adjusted  Unit Value Indexes: Do They Really Differ?} Paper presented at the Society for Economic Measurement Conference, 18-20 August 2014, Chicago, U.S.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#' @examples 
#' \donttest{tpd_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{tpd_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export
tpd_splice<-function (data,start,end, window=13, splice="movement",interval=FALSE)
{asplice<-c("movement","window","half","mean","window_published","half_published","mean_published") #allowed values for 'splice' parameter
 if (!(splice %in% asplice)) stop ("The 'splice' parameter has a wrong value")
 if (start==end) return (1)
 if (nrow(data)==0) stop("A data frame is empty")
  t0<-start
  start<-paste(start,"-01",sep="")
  end<-paste(end,"-01",sep="")
  start<-as.Date(start)
  end<-as.Date(end)
  wend<-start
  lubridate::month(wend)<-lubridate::month(wend)+window-1
  #checking conditions
  if (window<2)  stop("window must be at least 2 months")
  if (start>end) stop("parameters must satisfy: start<=end")
  set<-c(1)
  #main body
  while (start<end)
                                       {lubridate::month(start)<-lubridate::month(start)+1
                                        t<-substr(start,0,7)
                                       if (start<=wend) set<-c(set,tpd(data,t0,t,wstart=t0,window))
                                       else {
                                       t1<-start
                                       lubridate::month(t1)<-lubridate::month(t1)-1
                                       tT<-start
                                       lubridate::month(tT)<-lubridate::month(tT)-(window-1)
                                       tT1<-start
                                       lubridate::month(tT1)<-lubridate::month(tT1)-(window-1)-1
                                       th<-start
                                       lubridate::month(th)<-lubridate::month(th)-floor(window/2)
                                       t1<-substr(t1,0,7)
                                       tT<-substr(tT,0,7)
                                       tT1<-substr(tT1,0,7)
                                       th<-substr(th,0,7)
                                       if (splice=="movement") set<-c(set, set[length(set)]*tpd(data,t1,t,wstart=tT,window))
                                       if (splice=="window")   set<-c(set, set[length(set)]*tpd(data,tT,t,wstart=tT,window)/tpd(data,tT,t1,wstart=tT1,window))
                                       if (splice=="half")   set<-c(set, set[length(set)]*tpd(data,th,t,wstart=tT,window)/tpd(data,th,t1,wstart=tT1,window))
                                       if (splice=="mean") {var<-1
                                       for (m in 1:(window-1)) {tm<-start
                                       lubridate::month(tm)<-lubridate::month(tm)-m
                                       tm<-substr(tm,0,7)
var<-var*tpd(data,tm,t,wstart=tT,window)/tpd(data,tm,t1,wstart=tT1,window) 
                                       }
                                       var<-var^(1/(window-1))
                                       set<-c(set, set[length(set)]*var)
                                                              }
                                       if (splice=="window_published")
                                       set<-c(set, set[length(set)+1-(window-1)]*tpd(data,tT,t,wstart=tT,window))
                                       if (splice=="half_published")
                                       set<-c(set, set[length(set)+1-floor(window/2)]*tpd(data,th,t,wstart=tT,window))  
                                       if (splice=="mean_published") {var<-1
                                       for (m in 1:(window-1)) {tm<-start
                                       lubridate::month(tm)<-lubridate::month(tm)-m
                                       tm<-substr(tm,0,7)
var<-var*set[length(set)+1-m]*tpd(data,tm,t,wstart=tT,window) 
                                       }
                                       var<-var^(1/(window-1))
                                       set<-c(set, var)
                                                              }
                                       
                                       
                                             } 
                                       }
 if (interval==FALSE) return (set[length(set)])
 else return(set)  
}


#' @title  Extending the multilateral GEKS price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral GEKS price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname geks_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral GEKS price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#' @examples 
#' \donttest{geks_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{geks_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export
geks_splice<-function (data,start,end, window=13, splice="movement",interval=FALSE)
{ asplice<-c("movement","window","half","mean","window_published","half_published","mean_published") #allowed values for 'splice' parameter
  if (!(splice %in% asplice)) stop ("The 'splice' parameter has a wrong value")
  if (start==end) return (1)
  if (nrow(data)==0) stop("A data frame is empty")
  t0<-start
  start<-paste(start,"-01",sep="")
  end<-paste(end,"-01",sep="")
  start<-as.Date(start)
  end<-as.Date(end)
  wend<-start
  lubridate::month(wend)<-lubridate::month(wend)+window-1
  #checking conditions
  if (window<2)  stop("window must be at least 2 months")
  if (start>end) stop("parameters must satisfy: start<=end")
  set<-c(1)
  #main body
  while (start<end)
                                       {lubridate::month(start)<-lubridate::month(start)+1
                                       t<-substr(start,0,7)
                                       if (start<=wend) set<-c(set,geks(data,t0,t,wstart=t0,window))
                                       else {
                                       t1<-start
                                       lubridate::month(t1)<-lubridate::month(t1)-1
                                       tT<-start
                                       lubridate::month(tT)<-lubridate::month(tT)-(window-1)
                                       tT1<-start
                                       lubridate::month(tT1)<-lubridate::month(tT1)-(window-1)-1
                                       th<-start
                                       lubridate::month(th)<-lubridate::month(th)-floor(window/2)
                                       t1<-substr(t1,0,7)
                                       tT<-substr(tT,0,7)
                                       tT1<-substr(tT1,0,7)
                                       th<-substr(th,0,7)
                                       if (splice=="movement") set<-c(set, set[length(set)]*geks(data,t1,t,wstart=tT,window))
                                       if (splice=="window")   set<-c(set, set[length(set)]*geks(data,tT,t,wstart=tT,window)/geks(data,tT,t1,wstart=tT1,window))
                                       if (splice=="half")   set<-c(set, set[length(set)]*geks(data,th,t,wstart=tT,window)/geks(data,th,t1,wstart=tT1,window))
                                       if (splice=="mean") {var<-1
                                       for (m in 1:(window-1)) {tm<-start
                                       lubridate::month(tm)<-lubridate::month(tm)-m
                                       tm<-substr(tm,0,7)
var<-var*geks(data,tm,t,wstart=tT,window)/geks(data,tm,t1,wstart=tT1,window) 
                                                                }
                                       var<-var^(1/(window-1))
                                       set<-c(set, set[length(set)]*var)
                                                                }
                                       if (splice=="window_published")
                                       set<-c(set, set[length(set)+1-(window-1)]*geks(data,tT,t,wstart=tT,window))
                                       if (splice=="half_published")
                                       set<-c(set, set[length(set)+1-floor(window/2)]*geks(data,th,t,wstart=tT,window))  
                                       if (splice=="mean_published") {var<-1
                                       for (m in 1:(window-1)) {tm<-start
                                       lubridate::month(tm)<-lubridate::month(tm)-m
                                       tm<-substr(tm,0,7)
var<-var*set[length(set)+1-m]*geks(data,tm,t,wstart=tT,window) 
                                       }
                                       var<-var^(1/(window-1))
                                       set<-c(set, var)
                                                              }
                                             } 
                                       }
if (interval==FALSE) return (set[length(set)])
else return(set)  
}

#' @title  Extending the multilateral GEKS-J price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral GEKS-J price index (GEKS based on the Jevons formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname geksj_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral GEKS-J price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#' @examples 
#' \donttest{geksj_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{geksj_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

geksj_splice<-function (data,start,end, window=13, splice="movement",interval=FALSE)
{ asplice<-c("movement","window","half","mean","window_published","half_published","mean_published") #allowed values for 'splice' parameter
  if (!(splice %in% asplice)) stop ("The 'splice' parameter has a wrong value")
  if (start==end) return (1)
  if (nrow(data)==0) stop("A data frame is empty")
  t0<-start
  start<-paste(start,"-01",sep="")
  end<-paste(end,"-01",sep="")
  start<-as.Date(start)
  end<-as.Date(end)
  wend<-start
  lubridate::month(wend)<-lubridate::month(wend)+window-1
  #checking conditions
  if (window<2)  stop("window must be at least 2 months")
  if (start>end) stop("parameters must satisfy: start<=end")
  set<-c(1)
  #main body
  while (start<end)
                                       {lubridate::month(start)<-lubridate::month(start)+1
                                       t<-substr(start,0,7)
                                       if (start<=wend) set<-c(set,geksj(data,t0,t,wstart=t0,window))
                                       else {
                                       t1<-start
                                       lubridate::month(t1)<-lubridate::month(t1)-1
                                       tT<-start
                                       lubridate::month(tT)<-lubridate::month(tT)-(window-1)
                                       tT1<-start
                                       lubridate::month(tT1)<-lubridate::month(tT1)-(window-1)-1
                                       th<-start
                                       lubridate::month(th)<-lubridate::month(th)-floor(window/2)
                                       t1<-substr(t1,0,7)
                                       tT<-substr(tT,0,7)
                                       tT1<-substr(tT1,0,7)
                                       th<-substr(th,0,7)
                                       if (splice=="movement") set<-c(set, set[length(set)]*geksj(data,t1,t,wstart=tT,window))
                                       if (splice=="window")   set<-c(set, set[length(set)]*geksj(data,tT,t,wstart=tT,window)/geksj(data,tT,t1,wstart=tT1,window))
                                       if (splice=="half")   set<-c(set, set[length(set)]*geksj(data,th,t,wstart=tT,window)/geksj(data,th,t1,wstart=tT1,window))
                                       if (splice=="mean") {var<-1
                                       for (m in 1:(window-1)) {tm<-start
                                       lubridate::month(tm)<-lubridate::month(tm)-m
                                       tm<-substr(tm,0,7)
var<-var*geksj(data,tm,t,wstart=tT,window)/geksj(data,tm,t1,wstart=tT1,window) 
                                                               }
                                       var<-var^(1/(window-1))
                                       set<-c(set, set[length(set)]*var)
                                                            }
                                       if (splice=="window_published")
                                       set<-c(set, set[length(set)+1-(window-1)]*geksj(data,tT,t,wstart=tT,window))
                                       if (splice=="half_published")
                                       set<-c(set, set[length(set)+1-floor(window/2)]*geksj(data,th,t,wstart=tT,window))  
                                       if (splice=="mean_published") {var<-1
                                       for (m in 1:(window-1)) {tm<-start
                                       lubridate::month(tm)<-lubridate::month(tm)-m
                                       tm<-substr(tm,0,7)
var<-var*set[length(set)+1-m]*geksj(data,tm,t,wstart=tT,window) 
                                       }
                                       var<-var^(1/(window-1))
                                       set<-c(set, var)
                                                              }
                                             } 
                                      }
if (interval==FALSE) return (set[length(set)])
else return(set)  
  }


#' @title  Extending the multilateral GEKS-W price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral GEKS-W price index (GEKS based on the Walsh formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname geksw_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral GEKS-W price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#' @examples 
#' \donttest{geksw_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{geksw_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export
geksw_splice<-function (data,start,end, window=13, splice="movement",interval=FALSE)
{ asplice<-c("movement","window","half","mean","window_published","half_published","mean_published") #allowed values for 'splice' parameter
  if (!(splice %in% asplice)) stop ("The 'splice' parameter has a wrong value")
  if (start==end) return (1)
  if (nrow(data)==0) stop("A data frame is empty")
  t0<-start
  start<-paste(start,"-01",sep="")
  end<-paste(end,"-01",sep="")
  start<-as.Date(start)
  end<-as.Date(end)
  wend<-start
  lubridate::month(wend)<-lubridate::month(wend)+window-1
  #checking conditions
  if (window<2)  stop("window must be at least 2 months")
  if (start>end) stop("parameters must satisfy: start<=end")
  set<-c(1)
  #main body
  while (start<end)
                                       {lubridate::month(start)<-lubridate::month(start)+1
                                       t<-substr(start,0,7)
                                       if (start<=wend) set<-c(set,geksw(data,t0,t,wstart=t0,window))
                                       else {
                                       t1<-start
                                       lubridate::month(t1)<-lubridate::month(t1)-1
                                       tT<-start
                                       lubridate::month(tT)<-lubridate::month(tT)-(window-1)
                                       tT1<-start
                                       lubridate::month(tT1)<-lubridate::month(tT1)-(window-1)-1
                                       th<-start
                                       lubridate::month(th)<-lubridate::month(th)-floor(window/2)
                                       t1<-substr(t1,0,7)
                                       tT<-substr(tT,0,7)
                                       tT1<-substr(tT1,0,7)
                                       th<-substr(th,0,7)
                                       if (splice=="movement") set<-c(set, set[length(set)]*geksw(data,t1,t,wstart=tT,window))
                                       if (splice=="window")   set<-c(set, set[length(set)]*geksw(data,tT,t,wstart=tT,window)/geksw(data,tT,t1,wstart=tT1,window))
                                       if (splice=="half")   set<-c(set, set[length(set)]*geksw(data,th,t,wstart=tT,window)/geksw(data,th,t1,wstart=tT1,window))
                                       if (splice=="mean") {var<-1
                                       for (m in 1:(window-1)) {tm<-start
lubridate::month(tm)<-lubridate::month(tm)-m
                                       tm<-substr(tm,0,7)
var<-var*geksw(data,tm,t,wstart=tT,window)/geksw(data,tm,t1,wstart=tT1,window) 
                                                               }
                                       var<-var^(1/(window-1))
                                       set<-c(set, set[length(set)]*var)
                                                               }
                                       if (splice=="window_published")
                                       set<-c(set, set[length(set)+1-(window-1)]*geksw(data,tT,t,wstart=tT,window))
                                       if (splice=="half_published")
                                       set<-c(set, set[length(set)+1-floor(window/2)]*geksw(data,th,t,wstart=tT,window))  
                                       if (splice=="mean_published") {var<-1
                                       for (m in 1:(window-1)) {tm<-start
                                       lubridate::month(tm)<-lubridate::month(tm)-m
                                       tm<-substr(tm,0,7)
var<-var*set[length(set)+1-m]*geksw(data,tm,t,wstart=tT,window) 
                                       }
                                       var<-var^(1/(window-1))
                                       set<-c(set, var)
                                                              }
                                             } 
                                       }
if (interval==FALSE) return (set[length(set)])
else return(set)  
  }


#' @title  Extending the multilateral CCDI price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral CCDI price index (GEKS based on the Tornqvist formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname ccdi_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral CCDI price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Caves, D.W., Christensen, L.R. and Diewert, W.E. (1982). \emph{Multilateral comparisons of output, input, and productivity using superlative index numbers.} Economic Journal 92, 73-86.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#' @examples 
#' \donttest{ccdi_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{ccdi_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export
ccdi_splice<-function (data,start,end, window=13, splice="movement",interval=FALSE)
{ asplice<-c("movement","window","half","mean","window_published","half_published","mean_published") #allowed values for 'splice' parameter
  if (!(splice %in% asplice)) stop ("The 'splice' parameter has a wrong value")
  if (start==end) return (1)
  if (nrow(data)==0) stop("A data frame is empty")
  t0<-start
  start<-paste(start,"-01",sep="")
  end<-paste(end,"-01",sep="")
  start<-as.Date(start)
  end<-as.Date(end)
  wend<-start
  lubridate::month(wend)<-lubridate::month(wend)+window-1
  #checking conditions
  if (window<2)  stop("window must be at least 2 months")
  if (start>end) stop("parameters must satisfy: start<=end")
  set<-c(1)
  #main body
  while (start<end)
                                       {lubridate::month(start)<-lubridate::month(start)+1
                                       t<-substr(start,0,7)
                                       if (start<=wend) set<-c(set,ccdi(data,t0,t,wstart=t0,window))
                                       else {
                                       t1<-start
                                       lubridate::month(t1)<-lubridate::month(t1)-1
                                       tT<-start
                                       lubridate::month(tT)<-lubridate::month(tT)-(window-1)
                                       tT1<-start
                                       lubridate::month(tT1)<-lubridate::month(tT1)-(window-1)-1
                                       th<-start
                                       lubridate::month(th)<-lubridate::month(th)-floor(window/2)
                                       t1<-substr(t1,0,7)
                                       tT<-substr(tT,0,7)
                                       tT1<-substr(tT1,0,7)
                                       th<-substr(th,0,7)
                                       if (splice=="movement") set<-c(set, set[length(set)]*ccdi(data,t1,t,wstart=tT,window))
                                       if (splice=="window")   set<-c(set, set[length(set)]*ccdi(data,tT,t,wstart=tT,window)/ccdi(data,tT,t1,wstart=tT1,window))
                                       if (splice=="half")   set<-c(set, set[length(set)]*ccdi(data,th,t,wstart=tT,window)/ccdi(data,th,t1,wstart=tT1,window))
                                       if (splice=="mean") {var<-1
                                       for (m in 1:(window-1)) {tm<-start
lubridate::month(tm)<-lubridate::month(tm)-m
                                       tm<-substr(tm,0,7)
var<-var*ccdi(data,tm,t,wstart=tT,window)/ccdi(data,tm,t1,wstart=tT1,window) 
                                                               }
                                       var<-var^(1/(window-1))
                                       set<-c(set, set[length(set)]*var)
                                                           }
                                       if (splice=="window_published")
                                       set<-c(set, set[length(set)+1-(window-1)]*ccdi(data,tT,t,wstart=tT,window))
                                       if (splice=="half_published")
                                       set<-c(set, set[length(set)+1-floor(window/2)]*ccdi(data,th,t,wstart=tT,window))  
                                       if (splice=="mean_published") {var<-1
                                       for (m in 1:(window-1)) {tm<-start
                                       lubridate::month(tm)<-lubridate::month(tm)-m
                                       tm<-substr(tm,0,7)
var<-var*set[length(set)+1-m]*ccdi(data,tm,t,wstart=tT,window) 
                                       }
                                       var<-var^(1/(window-1))
                                       set<-c(set, var)
                                                              }
                                             } 
                                       }
 if (interval==FALSE) return (set[length(set)])
 else return(set)  
}

#' @title  Extending the multilateral Geary-Khamis price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral Geary-Khamis price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname gk_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral Geary-Khamis price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Chessa, A. G. (2019). \emph{A Comparison of Index Extension Methods for Multilateral Methods.} Paper presented at the 16th Meeting of the Ottawa Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.}
#'
#' {de Haan, J., van der Grient, H.A. (2011). \emph{Eliminating chain drift in price indexes based on scanner data.} Journal of Econometrics, 161, 36-46.}
#'
#' {Krsinich, F. (2014). \emph{The FEWS Index: Fixed Effects with a Window Splice? Non-Revisable Quality-Adjusted Price Indices with No Characteristic Information.} Paper presented at the UNECE-ILO Meeting of the Group of Experts on Consumer Price Indices, 2-4 May 2016, Geneva, Switzerland.}
#'
#' {de Haan, J.(2015). \emph{A Framework for Large Scale Use of Scanner Data in the Dutch CPI.} Paper presented at the 14th Ottawa Group meeting, Tokyo, Japan.}
#'
#' {Diewert, W.E., and Fox, K.J. (2017). \emph{Substitution Bias in Multilateral Methods for CPI Construction using Scanner Data.} Discussion paper 17-02, Vancouver School of Economics, The University of British Columbia, Vancouver, Canada.}
#' @examples 
#' \donttest{gk_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{gk_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export
gk_splice<-function (data,start,end, window=13, splice="movement",interval=FALSE)
{ asplice<-c("movement","window","half","mean","window_published","half_published","mean_published") #allowed values for 'splice' parameter
  if (!(splice %in% asplice)) stop ("The 'splice' parameter has a wrong value")
  if (start==end) return (1)
  if (nrow(data)==0) stop("A data frame is empty")
  t0<-start
  start<-paste(start,"-01",sep="")
  end<-paste(end,"-01",sep="")
  start<-as.Date(start)
  end<-as.Date(end)
  wend<-start
  lubridate::month(wend)<-lubridate::month(wend)+window-1
  #checking conditions
  if (window<2)  stop("window must be at least 2 months")
  if (start>end) stop("parameters must satisfy: start<=end")
   set<-c(1)
  #main body
  while (start<end)
                                       {lubridate::month(start)<-lubridate::month(start)+1
                                       t<-substr(start,0,7)
                                       if (start<=wend) set<-c(set,gk(data,t0,t,wstart=t0,window))
                                       else {
                                       t1<-start
                                       lubridate::month(t1)<-lubridate::month(t1)-1
                                       tT<-start
                                       lubridate::month(tT)<-lubridate::month(tT)-(window-1)
                                       tT1<-start
                                       lubridate::month(tT1)<-lubridate::month(tT1)-(window-1)-1
                                       th<-start
                                       lubridate::month(th)<-lubridate::month(th)-floor(window/2)
                                       t1<-substr(t1,0,7)
                                       tT<-substr(tT,0,7)
                                       tT1<-substr(tT1,0,7)
                                       th<-substr(th,0,7)
                                       if (splice=="movement") set<-c(set, set[length(set)]*gk(data,t1,t,wstart=tT,window))
                                       if (splice=="window")   set<-c(set, set[length(set)]*gk(data,tT,t,wstart=tT,window)/gk(data,tT,t1,wstart=tT1,window))
                                       if (splice=="half")   set<-c(set, set[length(set)]*gk(data,th,t,wstart=tT,window)/gk(data,th,t1,wstart=tT1,window))
                                       if (splice=="mean") {var<-1
                                       for (m in 1:(window-1)) {tm<-start
lubridate::month(tm)<-lubridate::month(tm)-m
                                       tm<-substr(tm,0,7)
var<-var*gk(data,tm,t,wstart=tT,window)/gk(data,tm,t1,wstart=tT1,window) 
                                                               }
                                       var<-var^(1/(window-1))
                                       set<-c(set, set[length(set)]*var)
                                                                }
                                       if (splice=="window_published")
                                       set<-c(set, set[length(set)+1-(window-1)]*gk(data,tT,t,wstart=tT,window))
                                       if (splice=="half_published")
                                       set<-c(set, set[length(set)+1-floor(window/2)]*gk(data,th,t,wstart=tT,window))  
                                       if (splice=="mean_published") {var<-1
                                       for (m in 1:(window-1)) {tm<-start
                                       lubridate::month(tm)<-lubridate::month(tm)-m
                                       tm<-substr(tm,0,7)
var<-var*set[length(set)+1-m]*gk(data,tm,t,wstart=tT,window) 
                                       }
                                       var<-var^(1/(window-1))
                                       set<-c(set, var)
                                                              }
                                             } 
                                       }
if (interval==FALSE) return (set[length(set)])
else return(set)  
}


#' @title  A general function to compute a price index
#'
#' @description This function returns a value or values of the selected price index. 
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is also essential if the selected index is a weighted formula (as positive numeric). 
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param formula The character string indicating the price index formula is to be calculated. To see available options please use the link: \code{\link{PriceIndices}}.
#' @param window The length of the time window if the multilateral index is selected (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method (if the multilateral splicing index is selected). Available options are: "movement", "window","half","mean" and their additional variants: "window_published", "half_published" and "mean_published". 
#' @param base The prior period used in the Young- or Lowe-type price indices (as character) limited to the year and month, e.g. "2020-01".
#' @param sigma The elasticity of substitution parameter used in the Lloyed-Moulton and AG Mean indices (as numeric).
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname price_index
#' @return This function returns a value or values of the selected price index. If the \code{interval} parameter is set to TRUE then it returns a data frame with two columns: dates and index values. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @examples 
#' \donttest{price_index(milk, start="2018-12", end="2020-02",formula="walsh",interval=FALSE)}
#' \donttest{price_index(milk, start="2018-12",end="2020-02",formula="tpd_splice",
#' splice="half",interval=TRUE)}
#' @export

price_index<-function(data, start, end, formula="fisher", window=13, splice="movement", base=start, sigma=0.7, interval=FALSE) 
{ asplice<-c("movement","window","half","mean") #allowed values for 'splice' parameter
  if (!(splice %in% asplice)) stop ("The 'splice' parameter has a wrong value")
  aformula<-c("jevons","dutot","carli","cswd","harmonic","bmw","laspeyres","paasche","fisher","tornqvist","geolaspeyres","geopaasche","drobisch","marshall_edgeworth","walsh","bialek","banajree","davies","stuvel","palgrave","geary_khamis","lehr","vartia","sato_vartia","lloyd_moulton","agmean","young","geoyoung","lowe","geolowe","chjevons","chdutot","chcarli","chcswd","chharmonic","chlaspeyres","chpaasche","chfisher","chtornqvist","chgeolaspeyres","chgeopaasche","chdrobisch","chmarshall_edgeworth","chwalsh","chbialek","chbanajree","chdavies","chstuvel","chpalgrave","chgeary_khamis","chlehr","chvartia","chsato_vartia","chlloyd_moulton","chagmean","chyoung","chgeoyoung","chlowe","chgeolowe","chbmw","geks","geksj","geksw","ccdi","gk","tpd","geks_splice","geksj_splice","geksw_splice","ccdi_splice","gk_splice","tpd_splice","geks_fbew","geks_fbmw","geksj_fbew","geksj_fbmw","geksw_fbew","geksw_fbmw","ccdi_fbew","ccdi_fbmw","gk_fbew","gk_fbmw","tpd_fbew","tpd_fbmw","hybrid", "geohybrid", "chhybrid", "chgeohybrid","SPQ","geksl","wgeksl","geksl_splice","wgeksl_splice","geksl_fbew","wgeksl_fbew","geksl_fbmw","wgeksl_fbmw")
  if (!(formula %in% aformula)) stop ("There is a typo in the index name")
  if (start==end) return (1)
  if (nrow(data)==0) stop("A data frame is empty")
  if (sigma==1) stop("A specification of the parameter 'sigma' is wrong")
  if (interval==FALSE) {
                       #unweighted formulas
                       if (formula=="jevons") set<-jevons(data, start,end)
                       if (formula=="dutot") set<-dutot(data, start,end)
                       if (formula=="carli") set<-carli(data, start,end)
                       if (formula=="cswd") set<-cswd(data, start,end)
                       if (formula=="harmonic") set<-harmonic(data, start,end)
                       if (formula=="bmw") set<-bmw(data, start,end)
                       #weighted formulas
                       if (formula=="laspeyres") set<-laspeyres(data, start,end)
                       if (formula=="paasche") set<-paasche(data, start,end)
                       if (formula=="fisher") set<-fisher(data, start,end)
                       if (formula=="tornqvist") set<-tornqvist(data, start,end)
                       if (formula=="geolaspeyres") set<-geolaspeyres(data, start,end)
                       if (formula=="geopaasche") set<-geopaasche(data, start,end)
                       if (formula=="drobisch") set<-drobisch(data, start,end)
                       if (formula=="marshall_edgeworth") set<-marshall_edgeworth(data, start,end)
                       if (formula=="walsh") set<-walsh(data, start,end)
                       if (formula=="bialek") set<-bialek(data, start,end)
                       if (formula=="banajree") set<-banajree(data, start,end)
                       if (formula=="davies") set<-davies(data, start,end)
                       if (formula=="stuvel") set<-stuvel(data, start,end)
                       if (formula=="palgrave") set<-palgrave(data, start,end)
                       if (formula=="geary_khamis") set<-geary_khamis(data, start,end)
                       if (formula=="lehr") set<-lehr(data, start,end)
                       if (formula=="vartia") set<-vartia(data, start,end)
                       if (formula=="sato_vartia") set<-sato_vartia(data, start,end)
                       if (formula=="lloyd_moulton") set<-lloyd_moulton(data, start,end,sigma)
                       if (formula=="agmean") set<-agmean(data, start, end, sigma)
                       if (formula=="young") set<-young(data, start, end, base)
                       if (formula=="geoyoung") set<-geoyoung(data, start, end, base)
                       if (formula=="lowe") set<-lowe(data, start, end, base)
                       if (formula=="geolowe") set<-geolowe(data, start, end, base)
                       if (formula=="hybrid") set<-hybrid(data, start, end, base)
                       if (formula=="geohybrid") set<-geohybrid(data, start, end, base)
                       #chain indices
                       if (formula=="chjevons") set<-chjevons(data, start,end)
                       if (formula=="chdutot") set<-chdutot(data, start,end)
                       if (formula=="chcarli") set<-chcarli(data, start,end)
                       if (formula=="chcswd") set<-chcswd(data, start,end)
                       if (formula=="chbmw") set<-chbmw(data, start, end)
                       if (formula=="chharmonic") set<-chharmonic(data, start,end)
                       if (formula=="chlaspeyres") set<-chlaspeyres(data, start,end)
                       if (formula=="chpaasche") set<-chpaasche(data, start,end)
                       if (formula=="chfisher") set<-chfisher(data, start,end)
                       if (formula=="chtornqvist") set<-chtornqvist(data, start,end)
                       if (formula=="chgeolaspeyres") set<-chgeolaspeyres(data, start,end)
                       if (formula=="chgeopaasche") set<-chgeopaasche(data, start,end)
                       if (formula=="chdrobisch") set<-chdrobisch(data, start,end)
                       if (formula=="chmarshall_edgeworth") set<-chmarshall_edgeworth(data, start,end)
                       if (formula=="chwalsh") set<-chwalsh(data, start,end)
                       if (formula=="chbialek") set<-chbialek(data, start,end)
                       if (formula=="chbanajree") set<-chbanajree(data, start,end)
                       if (formula=="chdavies") set<-chdavies(data, start,end)
                       if (formula=="chstuvel") set<-chstuvel(data, start,end)
                       if (formula=="chpalgrave") set<-chpalgrave(data, start,end)
                       if (formula=="chgeary_khamis") set<-chgeary_khamis(data, start,end)
                       if (formula=="chlehr") set<-chlehr(data, start,end)
                       if (formula=="chvartia") set<-chvartia(data, start,end)
                       if (formula=="chsato_vartia") set<-chsato_vartia(data, start,end)
                       if (formula=="chlloyd_moulton") set<-chlloyd_moulton(data, start,end,sigma)
                       if (formula=="chagmean") set<-chagmean(data, start, end, sigma)
                       if (formula=="chyoung") set<-chyoung(data, start, end, base)
                       if (formula=="chgeoyoung") set<-chgeoyoung(data, start, end, base)
                       if (formula=="chlowe") set<-chlowe(data, start, end, base)
                       if (formula=="chgeolowe") set<-chgeolowe(data, start, end, base)
                       if (formula=="chhybrid") set<-chhybrid(data, start, end, base)
                       if (formula=="chgeohybrid") set<-chgeohybrid(data, start, end, base)
                       #multilateral indices
                       if (formula=="geks") set<-geks(data, start, end, start, window)
                       if (formula=="geksj") set<-geksj(data, start, end, start, window)
                       if (formula=="geksw") set<-geksw(data, start, end, start, window)
                       if (formula=="ccdi") set<-ccdi(data, start, end, start, window)
                       if (formula=="gk") set<-gk(data, start, end, start, window)
                       if (formula=="tpd") set<-tpd(data, start, end, start, window)
                       if (formula=="SPQ") set<-SPQ(data, start, end, interval)
                       if (formula=="geksl") set<-geksl(data, start, end, start, window)
                       if (formula=="wgeksl") set<-wgeksl(data, start, end, start, window)
                       #extended multilateral indices
                       if (formula=="geks_splice") set<-geks_splice(data, start, end, window, splice, interval)
                       if (formula=="geksl_splice") set<-geksl_splice(data, start, end, window, splice, interval)
                       if (formula=="wgeksl_splice") set<-wgeksl_splice(data, start, end, window, splice, interval)
                       if (formula=="geksj_splice") set<-geksj_splice(data, start, end, window, splice, interval)
                       if (formula=="geksw_splice") set<-geksw_splice(data, start, end, window, splice, interval)
                       if (formula=="ccdi_splice") set<-ccdi_splice(data, start, end, window, splice, interval)
                       if (formula=="gk_splice") set<-gk_splice(data, start, end, window, splice, interval)
                       if (formula=="tpd_splice") set<-tpd_splice(data, start, end, window, splice, interval)
                       if (formula=="geks_fbew") set<-geks_fbew(data, start, end)
                       if (formula=="geksl_fbew") set<-geksl_fbew(data, start, end)
                       if (formula=="wgeksl_fbew") set<-wgeksl_fbew(data, start, end)
                       if (formula=="geks_fbmw") set<-geks_fbmw(data, start, end)
                       if (formula=="geksl_fbmw") set<-geksl_fbmw(data, start, end)
                       if (formula=="wgeksl_fbmw") set<-wgeksl_fbmw(data, start, end)
                       if (formula=="geksj_fbew") set<-geksj_fbew(data, start, end)
                       if (formula=="geksj_fbmw") set<-geksj_fbmw(data, start, end)
                       if (formula=="geksw_fbew") set<-geksw_fbew(data, start, end)
                       if (formula=="geksw_fbmw") set<-geksw_fbmw(data, start, end)
                       if (formula=="ccdi_fbew") set<-ccdi_fbew(data, start, end)
                       if (formula=="ccdi_fbmw") set<-ccdi_fbmw(data, start, end)
                       if (formula=="gk_fbew") set<-gk_fbew(data, start, end)
                       if (formula=="gk_fbmw") set<-gk_fbmw(data, start, end)
                       if (formula=="tpd_fbew") set<-tpd_fbew(data, start, end)
                       if (formula=="tpd_fbmw") set<-tpd_fbmw(data, start, end)
                       return (set)
                       } 
 
 else                 {
                       set<-c(1)
                      #unweighted formulas
                       if (formula=="jevons") set<-jevons(data, start,end,interval)
                       if (formula=="dutot") set<-dutot(data, start,end,interval)
                       if (formula=="carli") set<-carli(data, start,end,interval)
                       if (formula=="cswd") set<-cswd(data, start,end,interval)
                       if (formula=="harmonic") set<-harmonic(data, start,end,interval)
                       if (formula=="bmw") set<-bmw(data, start,end,interval)                
                       #weighted formulas
                       if (formula=="laspeyres") set<-laspeyres(data, start,end,interval)
                       if (formula=="paasche") set<-paasche(data, start,end,interval)
                       if (formula=="fisher") set<-fisher(data, start,end,interval)
                       if (formula=="tornqvist") set<-tornqvist(data, start,end,interval)
                       if (formula=="geolaspeyres") set<-geolaspeyres(data, start,end,interval)
                       if (formula=="geopaasche") set<-geopaasche(data, start,end,interval)
                       if (formula=="drobisch") set<-drobisch(data, start,end,interval)
                       if (formula=="marshall_edgeworth") set<-marshall_edgeworth(data, start,end,interval)
                       if (formula=="walsh") set<-walsh(data, start,end,interval)
                       if (formula=="bialek") set<-bialek(data, start,end,interval)
                       if (formula=="banajree") set<-banajree(data, start,end,interval)
                       if (formula=="davies") set<-davies(data, start,end,interval)
                       if (formula=="stuvel") set<-stuvel(data, start,end,interval)
                       if (formula=="palgrave") set<-palgrave(data, start,end,interval)
                       if (formula=="geary_khamis") set<-geary_khamis(data, start,end,interval)
                       if (formula=="lehr") set<-lehr(data, start,end,interval)
                       if (formula=="vartia") set<-vartia(data, start,end,interval)
                       if (formula=="sato_vartia") set<-sato_vartia(data, start,end,interval)
                       if (formula=="lloyd-moulton") set<-lloyd_moulton(data, start,end,sigma,interval)
                       if (formula=="agmean") set<-agmean(data, start,end,sigma,interval)
                       if (formula=="young") set<-young(data, start,end,base,interval)
                       if (formula=="geoyoung") set<-geoyoung(data, start,end,base,interval)
                       if (formula=="lowe") set<-lowe(data, start,end,base,interval)
                       if (formula=="geolowe") set<-geolowe(data, start,end,base,interval)
                       if (formula=="hybrid") set<-hybrid(data, start,end,base,interval)
                       if (formula=="geohybrid") set<-geohybrid(data, start,end,base,interval)
                       #chain indices
                       if (formula=="chjevons") set<-chjevons(data, start,end,interval)
                       if (formula=="chdutot") set<-chdutot(data, start,end,interval)
                       if (formula=="chcarli") set<-chcarli(data, start,end,interval)
                       if (formula=="chcswd") set<-chcswd(data, start,end,interval)
                       if (formula=="chbmw") set<-chbmw(data, start,end,interval)
                       if (formula=="chharmonic") set<-chharmonic(data, start,end,interval)
                       if (formula=="chlaspeyres") set<-chlaspeyres(data, start,end,interval)
                       if (formula=="chpaasche") set<-chpaasche(data, start,end,interval)
                       if (formula=="chfisher") set<-chfisher(data, start,end,interval)
                       if (formula=="chtornqvist") set<-chtornqvist(data, start,end,interval)
                       if (formula=="chgeolaspeyres") set<-chgeolaspeyres(data, start,end,interval)
                       if (formula=="chgeopaasche") set<-chgeopaasche(data, start,end,interval)
                       if (formula=="chdrobisch") set<-chdrobisch(data, start,end,interval)
                       if (formula=="chmarshall_edgeworth") set<-chmarshall_edgeworth(data, start,end,interval)
                       if (formula=="chwalsh") set<-chwalsh(data, start,end,interval)
                       if (formula=="chbialek") set<-chbialek(data, start,end,interval)
                       if (formula=="chbanajree") set<-chbanajree(data, start,end,interval)
                       if (formula=="chdavies") set<-chdavies(data, start,end,interval)
                       if (formula=="chstuvel") set<-chstuvel(data, start,end,interval)
                       if (formula=="chpalgrave") set<-chpalgrave(data, start,end,interval)
                       if (formula=="chgeary_khamis") set<-chgeary_khamis(data, start,end,interval)
                       if (formula=="chlehr") set<-chlehr(data, start,end,interval)
                       if (formula=="chvartia") set<-chvartia(data, start,end,interval)
                       if (formula=="chsato_vartia") set<-chsato_vartia(data, start,end,interval)
                       if (formula=="chlloyd-moulton") set<-chlloyd_moulton(data, start,end,sigma,interval)
                       if (formula=="chagmean") set<-chagmean(data, start,end,sigma,interval)
                       if (formula=="chyoung") set<-chyoung(data, start,end,base,interval)
                       if (formula=="chgeoyoung") set<-chgeoyoung(data, start,end,base,interval)
                       if (formula=="chlowe") set<-chlowe(data, start,end,base,interval)
                       if (formula=="chgeolowe") set<-chgeolowe(data, start,end,base,interval)
                       if (formula=="chhybrid") set<-chhybrid(data, start,end,base,interval)
                       if (formula=="chgeohybrid") set<-chgeohybrid(data, start,end,base,interval)
                       #SPQ multilateral
                       if (formula=="SPQ") set<-SPQ(data, start, end, interval)
                       #extended multilateral indices
                       if (formula=="geks_splice") set<-geks_splice(data, start, end, window, splice, interval)
                       if (formula=="geksl_splice") set<-geksl_splice(data, start, end, window, splice, interval)
                       if (formula=="wgeksl_splice") set<-wgeksl_splice(data, start, end, window, splice, interval)
                       if (formula=="geksj_splice") set<-geksj_splice(data, start, end, window, splice, interval)
                       if (formula=="geksw_splice") set<-geksw_splice(data, start, end, window, splice, interval)
                       if (formula=="ccdi_splice") set<-ccdi_splice(data, start, end, window, splice, interval) 
                       if (formula=="gk_splice") set<-gk_splice(data, start, end, window, splice, interval)
                       if (formula=="tpd_splice") set<-tpd_splice(data, start, end, window, splice, interval)
                       start<-paste(start,"-01",sep="")
                       end<-paste(end,"-01",sep="")
                       start<-as.Date(start)
                       end<-as.Date(end)
                       t0<-c(substr(start,0,7))
                       times<-t0  #dates for writing
                       while (start<end)
                       {start2<-start
                       lubridate::month(start2)<-lubridate::month(start2)+1
                       t<-substr(start2,0,7)
                       #multilateral indices
                       if (formula=="geks") set<-c(set,geks(data, t0, t, t0, window))
                       if (formula=="geksl") set<-c(set,geksl(data, t0, t, t0, window))
                       if (formula=="wgeksl") set<-c(set,wgeksl(data, t0, t, t0, window))
                       if (formula=="geksj") set<-c(set,geksj(data, t0, t, t0, window))
                       if (formula=="geksw") set<-c(set,geksw(data, t0, t, t0, window))
                       if (formula=="ccdi") set<-c(set,ccdi(data, t0, t, t0, window))
                       if (formula=="gk") set<-c(set,gk(data, t0, t, t0, window))
                       if (formula=="tpd") set<-c(set,tpd(data, t0, t, t0, window))
                       if (formula=="geks_fbew") set<-c(set,geks_fbew(data, t0, t))
                       if (formula=="geksl_fbew") set<-c(set,geksl_fbew(data, t0, t))
                       if (formula=="wgeksl_fbew") set<-c(set,wgeksl_fbew(data, t0, t))
                       if (formula=="geks_fbmw") set<-c(set,geks_fbmw(data, t0, t))
                       if (formula=="geksl_fbmw") set<-c(set,geksl_fbmw(data, t0, t))
                       if (formula=="wgeksl_fbmw") set<-c(set,wgeksl_fbmw(data, t0, t))
                       if (formula=="geksj_fbew") set<-c(set,geksj_fbew(data, t0, t))
                       if (formula=="geksj_fbmw") set<-c(set,geksj_fbmw(data, t0, t))
                       if (formula=="geksw_fbew") set<-c(set,geksw_fbew(data, t0, t))
                       if (formula=="geksw_fbmw") set<-c(set,geksw_fbmw(data, t0, t))
                       if (formula=="ccdi_fbew") set<-c(set,ccdi_fbew(data, t0, t))
                       if (formula=="ccdi_fbmw") set<-c(set,ccdi_fbmw(data, t0, t))
                       if (formula=="gk_fbew") set<-c(set,gk_fbew(data, t0, t))
                       if (formula=="gk_fbmw") set<-c(set,gk_fbmw(data, t0, t))
                       if (formula=="tpd_fbew") set<-c(set,tpd_fbew(data, t0, t))
                       if (formula=="tpd_fbmw") set<-c(set,tpd_fbmw(data, t0, t))
                       times<-c(times,substr(start2,0,7))  
                       lubridate::month(start)<-lubridate::month(start)+1
                                       }
                       datfr<-data.frame(c(times),c(set)) 
                       colnames(datfr)<-c("date",formula)
                       return (datfr)
                      }
  }

#' @title  A very general function to compute one or more price indices
#'
#' @description This function returns a value or values of the selected price indices. 
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is also essential if the selected index is a weighted formula (as positive numeric). 
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param bilateral A vector of character strings indicating bilateral price index formulas that are to be calculated. To see available options please use the link: \code{\link{PriceIndices}}.
#' @param bindex A vector of character strings indicating Lowe- or Young-type price index formulas that are to be calculated. Available options are: \code{young},\code{geoyoung},\code{lowe} and \code{geolowe}. 
#' @param cesindex A vector of character strings indicating CES price index formulas that are to be calculated. To see available options, please use the link: \code{\link{PriceIndices}}.
#' @param simindex A vector of character strings indicating multilateral price index formulas based on relative price and quantity similarity that are to be calculated. To see available options, please use the link: \code{\link{PriceIndices}}.
#' @param fbmulti A vector of character strings indicating multilateral price index formulas that are to be calculated. The available set of indices includes full-window multilateral indices or their FBEW and FBMW extensions.To see available options, please use the link: \code{\link{PriceIndices}}.
#' @param splicemulti A vector of character strings indicating multilateral price index formulas that are to be extended by using splicing methods. To see available options. please use the link: \code{\link{PriceIndices}}.
#' @param base The vector of prior periods used in the Young- or Lowe-type price indices. Each element of the vector (as character) must be limited to the year and month, e.g. "2020-01".
#' @param sigma The vector of elasticity of substitution parameters used in the Lloyed-Moulton and AG Mean indices.
#' @param fbwindow A vector of integers. Each element of the vector defines the length of the time window of the corresponding multilateral index (if it is selected by \code{fbmulti}).
#' @param splicewindow A vector of integers. Each element of the vector defines the length of the time window of the corresponding multilateral index (if it is selected by \code{splicemulti}).
#' @param splice A vector of character strings. Each element of the vector indicates the splicing method is to be used for the corresponding multilateral index (if it is selected by \code{splicemulti} ). Available values of vector elements are: "movement", "window","half","mean" and their additional variants: "window_published", "half_published" and "mean_published".
#' @param namebilateral A vector of character strings describing names of bilateral price indices that are to be displayed. If this vector is empty, then default names are used.
#' @param namebindex A vector of character strings describing names of Young- and/or Lowe-type price indices are to be displayed. If this vector is empty, then default names are used.
#' @param namecesindex A vector of character strings describing names of CES price indices that are to be displayed. If this vector is empty, then default names are used.
#' @param namesimindex A vector of character strings describing names of multilateral price index formulas based on relative price and quantity similarity that are to be displayed. If this vector is empty, then default names are used.
#' @param namefbmulti A vector of character strings describing names of full-window multilateralindices or their FBEW and FBMW extensions that are to be displayed. If this vector is empty, then default names are used.
#' @param namesplicemulti  A vector of character strings describing names of multilateral splice indices that are to be displayed. If this vector is empty, then default names are used.
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname price_indices
#' @return This general function returns a value or values of the selected price indices. If the \code{interval} parameter is set to TRUE, then it returns a data frame where its first column indicates dates and the remaining columns show corresponding values of all selected price indices. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @examples 
#' \donttest{price_indices(milk, start="2018-12",end="2019-04",bilateral=c("jevons"),
#' fbmulti=c("tpd"),fbwindow=c(6),interval=TRUE)}
#' \donttest{price_indices(milk, start="2018-12", end="2019-05",
#' fbmulti=c("tpd","geks"),fbwindow=c(10,12),interval=TRUE)}
#' @export
price_indices<-function(data, start, end, bilateral=c(), bindex=c(), base=c(), cesindex=c(), sigma=c(), simindex=c(),fbmulti=c(), fbwindow=c(), splicemulti=c(), splicewindow=c(), splice=c(), namebilateral=bilateral, namebindex=bindex, namecesindex=cesindex, namesimindex=simindex,namefbmulti=fbmulti, namesplicemulti=splicemulti,interval=FALSE) 
{ 
  if (nrow(data)==0) stop("A data frame is empty") 
  if (!(length(bindex)==length(base))) stop("Length of 'bindex' must be the same as length of 'base'")
  if (!(length(cesindex)==length(sigma))) stop("Length of 'cesindex' must be the same as length of 'sigma'")
  if (!(length(fbmulti)==length(fbwindow))) stop("Length of 'fbmulti' must be the same as length of 'fbwindow'")
  if (!(length(splicemulti)==length(splicewindow))) stop("Length of 'splicemulti' must be the same as length of 'splicewindow'")
  if (!(length(splicemulti)==length(splice))) stop("Length of 'splicemulti' must be the same as length of 'splice'")
  if (length(bilateral)+length(bindex)+length(cesindex)+length(fbmulti)+length(splicemulti)+length(simindex)==0) stop("at least one price index formula must be chosen")
   if (interval==FALSE) {
                      results1<-NULL
                      results2<-NULL
                      results3<-NULL
                      results4<-NULL
                      results5<-NULL
                      results6<-NULL
                      bil<-c()
                      b<-c()
                      ces<-c()
                      full<-c()
                      ex<-c()
                      sind<-c()
                      if (length(bilateral)>0) {for (i in 1:length(bilateral)) bil<-c(bil, price_index(data, start, end, formula=bilateral[i], interval=FALSE))
                      results1<-data.frame(namebilateral, bil)
                      colnames(results1)<-c("index formula","value") }
                      if (length(bindex)>0) {for (i in 1:length(bindex)) b<-c(b, price_index(data, start, end, formula=bindex[i], base=base[i], interval=FALSE))
                      results2<-data.frame(namebindex, b)
                      colnames(results2)<-c("index formula","value") }
                      if (length(cesindex)>0) {for (i in 1:length(cesindex)) ces<-c(ces, price_index(data, start, end, formula=cesindex[i], sigma=sigma[i], interval=FALSE))
                      results3<-data.frame(namecesindex, ces)
                      colnames(results3)<-c("index formula","value") }
                      if (length(fbmulti)>0) {for (i in 1:length(fbmulti)) full<-c(full, price_index(data, start, end, formula=fbmulti[i], window=fbwindow[i], interval=FALSE))
                      results4<-data.frame(namefbmulti, full)
                      colnames(results4)<-c("index formula","value") }
                      if (length(splicemulti)>0) {for (i in 1:length(splicemulti)) ex<-c(ex, price_index(data, start, end, formula=splicemulti[i], window=splicewindow[i], splice=splice[i], interval=FALSE))
                      results5<-data.frame(namesplicemulti, ex)
                      colnames(results5)<-c("index formula","value") }
                      if (length(simindex)>0) {for (i in 1:length(simindex)) sind<-c(sind, price_index(data, start, end, formula=simindex[i], interval=FALSE))
                      results6<-data.frame(namesimindex, sind)
                      colnames(results6)<-c("index formula","value") }
                      results<-base::rbind(results1, results2, results3,results4,results5,results6)
                      colnames(results)<-c("index formula","value")
                       }  
   else {              
                      start<-paste(start,"-01",sep="")
                      end<-paste(end,"-01",sep="")
                      start<-as.Date(start)
                      st<-start
                      end<-as.Date(end)
                      dates<-c()
                      while (st<=end) {dates<-c(dates, substr(st,0,7))
                      lubridate::month(st)<-lubridate::month(st)+1
                                      }
                      results<-data.frame(c(dates))
                      colnames(results)[1]="date"
                      if (length(bilateral)>0) {for (i in 2:(length(bilateral)+1)) {results$i<-price_index(data, substr(start,0,7), substr(end,0,7), formula=bilateral[i-1], interval=TRUE)[[2]]
                      colnames(results)[i]=namebilateral[i-1]
                                                                                    }
                                                }
                      if (length(bindex)>0) {
                      for (i in 1:length(bindex)) {k<-length(bilateral)+1+i
                      results$k<-price_index(data, substr(start,0,7), substr(end,0,7), formula=bindex[i], base=base[i], interval=TRUE)[[2]]
                      colnames(results)[k]=namebindex[i]
                                                                                    }
                                             }
                      if (length(cesindex)>0) {for (i in 1:length(cesindex)) {k<-length(bilateral)+1+length(bindex)+i
                      results$k<-price_index(data, substr(start,0,7), substr(end,0,7), formula=cesindex[i], sigma=sigma[i], interval=TRUE)[[2]]
                      colnames(results)[k]=namecesindex[i]
                                                                                    }
                                               }
                      if (length(fbmulti)>0) {for (i in 1:length(fbmulti)) {k<-length(bilateral)+1+length(bindex)+length(cesindex)+i
                       results$k<-price_index(data, substr(start,0,7), substr(end,0,7), formula=fbmulti[i], window=fbwindow[i], interval=TRUE)[[2]]
                      colnames(results)[k]=namefbmulti[i]
                                                                                    }
                                                }
                      if (length(splicemulti)>0) {
                      for (i in 1:length(splicemulti)) {k<-length(bilateral)+1+length(bindex)+length(cesindex)+length(fbmulti)+i
                      results$k<-price_index(data, substr(start,0,7), substr(end,0,7), formula=splicemulti[i], window=splicewindow[i], splice=splice[i],interval=TRUE)[[2]]
colnames(results)[k]=namesplicemulti[i]
                                                                                    }
                                              }

                      if (length(simindex)>0) {
                      for (i in 1:length(simindex)) {k<-length(bilateral)+1+length(bindex)+length(cesindex)+length(fbmulti)+length(splicemulti)+i
                      results$k<-price_index(data, substr(start,0,7), substr(end,0,7), formula=simindex[i], interval=TRUE)[[2]]
colnames(results)[k]=namesimindex[i]
                                                                                    }
                                              }
                      
                              }
return(results)
}

#' @title  A function for graphical comparison of price indices
#'
#' @description This function returns a figure with plots of selected price indices. 
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric or character). A column \code{quantities} is also essential if at least one selected index is a weighted formula (as positive numeric). 
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param bilateral A vector of character strings indicating bilateral price index formulas that are to be calculated. To see available options please use the link: \code{\link{PriceIndices}}.
#' @param bindex A vector of character strings indicating Lowe- or Young-type price index formulas that are to be calculated. Available options are: \code{young}, \code{geoyoung}, \code{lowe} and \code{geolowe}. 
#' @param cesindex A vector of character strings indicating CES price index formulas that are to be calculated. To see available options, please use the link: \code{\link{PriceIndices}}.
#' @param simindex A vector of character strings indicating multilateral price index formulas based on relative price and quantity similarity that are to be calculated. To see available options, please use the link: \code{\link{PriceIndices}}.
#' @param fbmulti A vector of character strings indicating multilateral price index formulas that are to be calculated. The available set of indices includes full-window multilateral indices or their FBEW and FBMW extensions.To see available options, please use the link: \code{\link{PriceIndices}}.
#' @param splicemulti The vector of character strings indicating multilateral price index formulas are to be extended by using splicing methods. To see available options please use the link: \code{\link{PriceIndices}}.
#' @param base The vector of prior periods used in the Young- or Lowe-type price indices. Each element of the vector (as character) must be limited to the year and month, e.g. "2020-01".
#' @param sigma The vector of elasticity of substitution parameters used in the Lloyed-Moulton and AG Mean indices.
#' @param fbwindow A vector of integers. Each element of the vector defines the length of the time window of the corresponding multilateral index (if it is selected by \code{fbmulti}).
#' @param splicewindow A vector of integers. Each element of the vector defines the length of the time window of the corresponding multilateral index (if it is selected by \code{splicemulti}).
#' @param splice A vector of character strings. Each element of the vector indicates the splicing method is to be used for the corresponding multilateral index (if it is selected by \code{splicemulti} ). Available values of vector elements are: "movement", "window","half","mean".
#' @param namebilateral A vector of character strings describing names of bilateral price indices that are to be displayed. If this vector is empty, then default names are used.
#' @param namebindex A vector of character strings describing names of Young- and/or Lowe-type price indices are to be displayed. If this vector is empty then default names are used.
#' @param namecesindex A vector of character strings describing names of CES price indices that are to be displayed. If this vector is empty, then default names are used.
#' @param namesimindex A vector of character strings describing names of multilateral price index formulas based on relative price and quantity similarity that are to be displayed. If this vector is empty, then default names are used.
#' @param namefbmulti A vector of character strings describing names of full-window multilateralindices or their FBEW and FBMW extensions that are to be displayed. If this vector is empty, then default names are used.
#' @param namesplicemulti  A vector of character strings describing names of multilateral splice indices that are to be displayed. If this vector is empty, then default names are used.
#' @rdname compare_indices
#' @return This function calculates selected bilateral or/and multilateral price indices and returns a figure with plots of these indices (together with dates on X-axis and a corresponding legend). The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use functions: \code{\link{final_index}} and \code{\link{compare_final_indices}}).   
#' @examples 
#' \donttest{compare_indices(milk, start="2018-12", end="2019-04",
#' bilateral=c("jevons"),fbmulti=c("tpd"),fbwindow=c(6))}
#' \donttest{compare_indices(milk, start="2018-12", end="2019-05",
#' fbmulti=c("tpd","geks"),fbwindow=c(10,12))}
#' @export


compare_indices<-function(data, start, end, bilateral=c(), bindex=c(), base=c(), cesindex=c(), sigma=c(), simindex=c(),fbmulti=c(), fbwindow=c(), splicemulti=c(), splicewindow=c(), splice=c(), namebilateral=bilateral, namebindex=bindex, namecesindex=cesindex, namesimindex=simindex,namefbmulti=fbmulti, namesplicemulti=splicemulti)
{
date<-value<-formula<-NULL
if (nrow(data)==0) stop("A data frame is empty")
if (length(bilateral)+length(bindex)+length(cesindex)+length(fbmulti)+length(splicemulti)+length(simindex)==0) stop("at least one price index formula must be chosen")
#main body
graph<-price_indices(data, start, end, bilateral, bindex, base, cesindex, sigma, simindex,fbmulti, fbwindow, splicemulti, splicewindow, splice, namebilateral, namebindex, namecesindex, namesimindex,namefbmulti, namesplicemulti, interval=TRUE) 
graph$date<-as.Date(paste(graph$date,"-01",sep=""))
graph<-reshape::melt(graph, id.var='date') 
colnames(graph)<-c("date","formula","value")
ggplot2::ggplot(graph, ggplot2::aes(x=date, y=value, col=formula)) + ggplot2::geom_point()+ggplot2::geom_line()+ggplot2::labs(x="date",y="price index value")+ggplot2::scale_x_date(date_labels="%Y %m",date_breaks  ="1 month")+ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust = 1)) 
}

#' @title  The most general package function to compute the price dynamics
#'
#' @description This function returns a value or values of the selected (final) price index taking into consideration aggregation over product subgroups and/or over outlets. 
#' @param datasets The user's list of data frames with subgroups of sold products. Each data frame must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities} (as positive numeric), \code{prodID} (as numeric or character) and \code{retID} (as numeric or character). 
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param formula The character string indicating the (final or main) price index formula is to be calculated. To see available options please use the link: \code{\link{PriceIndices}}.
#' @param window The length of the time window if the multilateral index is selected (as positive integer: typically multilateral methods are based on the 13-month time window and thus the default value is 13).
#' @param splice A character string indicating the splicing method (if the multilateral splicing index is selected). Available options are: "movement", "window","half", "mean" and also "window_published", "half_published" and "mean_published".
#' @param base The prior period used in the Young- or Lowe-type price indices (as character) limited to the year and month, e.g. "2020-01".
#' @param sigma The elasticity of substitution parameter used in the Lloyed-Moulton and AG Mean indices (as numeric).
#' @param aggrret A character string indicating the formula for aggregation over outlets (retailer sale points). Available options are: "none", "laspeyres", "paasche", "geolaspeyres", "geopaasche", "fisher", "tornqvist", "arithmetic" and "geometric". The first option means that there is no aggregating over outlets. The last two options mean unweighted methods of aggregating, i.e. the arithmetic or geometric mean is used. 
#' @param aggrsets A character string indicating the formula for aggregation over product subgroups. Available options are: "none", "laspeyres", "paasche", "geolaspeyres", "geopaasche", "fisher", "tornqvist", "arithmetic" and "geometric". The first option means that there is no aggregating over product subgroups. The last two options mean unweighted methods of aggregating, i.e. the arithmetic or geometric mean is used. 
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname final_index
#' @return This function returns a value or values of the selected (final) price index taking into consideration aggregation over product subgroups and/or over outlets (retailer sale points defined in \code{retID} column) . To be more precise: if both types of aggregation are selected, then for each subgroup of products and for each outlet (point of sale) price indices are calculated separately and then aggregated (according to the aggregation methods indicated) to the form of the final price index. If the \code{interval} parameter is set to TRUE then it returns a data frame with two columns: dates and final index values (after optional aggregating). Please note that different index formulas may use different time intervals (or time periods) for calculations and each time, aggregation over outlets is done for the set of retIDs being available during the whole considered time interval. 
#' @examples 
#'\donttest{ final_index(datasets=list(milk),start="2018-12",end="2020-02",
#' formula="walsh",aggrret="paasche",aggrsets="none")}
#' ## defining two subgroups of milk
#' \donttest{g1<-dplyr::filter(milk, milk$description=="full-fat milk UHT")}
#' \donttest{g2<-dplyr::filter(milk, milk$description=="low-fat milk UHT")}
#' ## Final price index calculations (for the whole time interval) 
#' ## with aggregating over subgroups g1 and g2 and over outlets
#' ## Please note that the default value (formula) for aggregating over outlets is "tornqvist""
#' \donttest{final_index(datasets=list(g1,g2), start="2018-12",
#' end="2019-12",formula="fisher",aggrsets="geometric",interval=TRUE)}
#' @export

final_index<-function(datasets=list(), start, end, formula="fisher", window=13, splice="movement", base=start, sigma=0.7, aggrret="tornqvist", aggrsets="tornqvist", interval=FALSE)
{ 
aaggrret<-c("none","laspeyres","paasche","geolaspeyres","geopaasche","fisher","tornqvist","arithmetic","geometric") 
if (!(aggrret %in% aaggrret)) stop ("The 'aggrret' parameter has a wrong value")
aaggrsets<-c("none","laspeyres","paasche","geolaspeyres","geopaasche","fisher","tornqvist","arithmetic","geometric") 
if (!(aggrsets %in% aaggrsets)) stop ("The 'aggrsets' parameter has a wrong value")
for (m in 1:length(datasets)) if (nrow(data.frame(datasets[[m]]))==0) stop("At least one data frame is empty")
start<-paste(start,"-01",sep="") 
end<-paste(end,"-01",sep="")  
start<-as.Date(start)
end<-as.Date(end)
if (aggrret=="none") for (i in 1:length(datasets)) datasets[[i]]$retID<-1
if (aggrsets=="none") {ds<-datasets[[1]][0:0,]
for (i in 1:length(datasets)) ds<-rbind(ds,datasets[[i]])
datasets<-list(ds)
                        } 
if (interval==FALSE) {
#weights for sets and retailers
w_start_set<-c() 
w_end_set<-c()
index_set<-c()
for (m in 1:length(datasets))  {set<-data.frame(datasets[[m]])
#limiting to matched products depending on the used price index formula 
if ((formula=="jevons") | (formula=="dutot") | (formula=="carli") | (formula=="cswd") | (formula=="harmonic") | (formula=="bmw") | (formula=="laspeyres") | (formula=="paasche") | (formula=="fisher") |(formula=="tornqvist") | (formula=="geolaspeyres") | (formula=="geopaasche") | (formula=="drobisch") | (formula=="marshall_edgeworth") | (formula=="walsh") | (formula=="bialek") | (formula=="banajree") | (formula=="davies") | (formula=="stuvel") | (formula=="palgrave") | (formula=="geary_khamis") | (formula=="lehr") | (formula=="vartia") | (formula=="sato_vartia") | (formula=="lloyd_moulton") | (formula=="agmean") | (formula=="young") | (formula=="geoyoung") | (formula=="lowe") | (formula=="geolowe") | (formula=="hybrid") | (formula=="geohybrid"))
id<-matched(set, period1=substr(start,0,7), period2=substr(end,0,7), type="retID", FALSE)
else if ((formula=="geks") | (formula=="geksw") | (formula=="geksj") | (formula=="ccdi") | (formula=="gk") | (formula=="tpd") | (formula=="geksl")| (formula=="wgeksl")) {
wend<-start
lubridate::month(wend)<-lubridate::month(wend)+window-1
id<-matched(set, period1=substr(start,0,7), period2=substr(end,0,7), type="retID", TRUE)
                                                                                                            }
else if ((formula=="geks_splice") | (formula=="geksw_splice") | (formula=="geksj_splice") | (formula=="ccdi_splice") | (formula=="gk_splice") | (formula=="tpd_splice") | (formula=="geksl_splice") | (formula=="wgeksl_splice")){
wend<-start
lubridate::month(wend)<-lubridate::month(wend)+window-1
id<-matched(set, period1=substr(start,0,7), period2=substr(max(wend,end),0,7), type="retID", TRUE)
             }
else if ((formula=="geks_fbmw") | (formula=="geksw_fbmw") | (formula=="geksj_fbmw") | (formula=="ccdi_fbmw") |    (formula=="gk_fbmw") | (formula=="tpd_wbmw") | (formula=="geksl_fbmw") | (formula=="wgeksl_fbmw")){
wstart<-end
lubridate::year(wstart)<-lubridate::year(wstart)-1
if (lubridate::year(start)==lubridate::year(end)) id<-matched(set, period1=substr(wstart,0,7), period2=substr(end,0,7), type="retID", TRUE)
else id<-matched(set, period1=substr(start,0,7), period2=substr(end,0,7), type="retID", TRUE)
             } 
else id<-matched(set, period1=substr(start,0,7), period2=substr(end,0,7), type="retID", TRUE)
retindex<-c()
w_start_ret<-c()
w_end_ret<-c()
##limiting to those IDs that have at least one matched product from set
id_ok<-c()
for (k in 1:length(id)) { 
subset<-dplyr::filter(set, set$retID==id[k])
if ((formula=="jevons") | (formula=="dutot") | (formula=="carli") | (formula=="cswd") | (formula=="harmonic") | (formula=="bmw") | (formula=="laspeyres") | (formula=="paasche") | (formula=="fisher") |(formula=="tornqvist") | (formula=="geolaspeyres") | (formula=="geopaasche") | (formula=="drobisch") | (formula=="marshall_edgeworth") | (formula=="walsh") | (formula=="bialek") | (formula=="banajree") | (formula=="davies") | (formula=="stuvel") | (formula=="palgrave") | (formula=="geary_khamis") | (formula=="lehr") | (formula=="vartia") | (formula=="sato_vartia") | (formula=="lloyd_moulton") | (formula=="agmean") | (formula=="young") | (formula=="geoyoung") | (formula=="lowe") | (formula=="geolowe") | (formula=="hybrid") | (formula=="geohybrid"))
idp<-matched(subset, period1=substr(start,0,7), period2=substr(end,0,7), type="prodID", FALSE)
else if ((formula=="geks") | (formula=="geksw") | (formula=="geksj") | (formula=="ccdi") | (formula=="gk") | (formula=="tpd") | (formula=="geksl") | (formula=="wgeksl")) {
wend<-start
lubridate::month(wend)<-lubridate::month(wend)+window-1
idp<-matched(subset, period1=substr(start,0,7), period2=substr(end,0,7), type="prodID", TRUE)
                                                                                                            }
else if ((formula=="geks_splice") | (formula=="geksw_splice") | (formula=="geksj_splice") | (formula=="ccdi_splice") | (formula=="gk_splice") | (formula=="tpd_splice") | (formula=="geksl_splice") | (formula=="wgeksl_splice")){
wend<-start
lubridate::month(wend)<-lubridate::month(wend)+window-1
idp<-matched(subset, period1=substr(start,0,7), period2=substr(max(wend,end),0,7), type="prodID", TRUE)
             }
else if ((formula=="geks_fbmw") | (formula=="geksw_fbmw") | (formula=="geksj_fbmw") | (formula=="ccdi_fbmw") |    (formula=="gk_fbmw") | (formula=="tpd_wbmw")| (formula=="geksl_fbmw") | (formula=="wgeksl_fbmw")){
wstart<-end
lubridate::year(wstart)<-lubridate::year(wstart)-1
if (lubridate::year(start)==lubridate::year(end)) idp<-matched(subset, period1=substr(wstart,0,7), period2=substr(end,0,7), type="prodID", TRUE)
else idp<-matched(subset, period1=substr(start,0,7), period2=substr(end,0,7), type="prodID", TRUE)
             } 
else idp<-matched(subset, period1=substr(start,0,7), period2=substr(end,0,7), type="prodID", TRUE)                       
if (length(idp)>0) id_ok<-c(id_ok, id[k])                         
                            }         
id<-id_ok  
if (length(id)==0) stop("At least one subset does not include matched products")
##------------------------------------------------------------------                                                        
                                 for (k in 1:length(id)) { 
                                                subset<-dplyr::filter(set, set$retID==id[k])
                                                retindex<-c(retindex, price_index(subset, substr(start,0,7), substr(end,0,7), formula, window, splice, base, sigma, interval=FALSE))
                                       d_start<-dplyr::filter(subset, lubridate::year(subset$time)==lubridate::year(start) & lubridate::month(subset$time)==lubridate::month(start))
                                       d_end<-dplyr::filter(subset, lubridate::year(subset$time)==lubridate::year(end) & lubridate::month(subset$time)==lubridate::month(end))                   
w_start_ret<-c(w_start_ret,sum(d_start$prices*d_start$quantities))  
w_end_ret<-c(w_end_ret,sum(d_end$prices*d_end$quantities))    
                                                         }
                                     #aggregating over outlets
                                     if (aggrret=="none") index_ret<-retindex[1]
                                     if (aggrret=="laspeyres")  index_ret<-sum(retindex*w_start_ret)/sum(w_start_ret)
                                     if (aggrret=="paasche")    index_ret<-sum(w_end_ret)/sum(w_end_ret/retindex)
                                     if (aggrret=="geolaspeyres")  {w_start<-sum(w_start_ret)
                                                                index_ret<-prod(retindex^(w_start_ret/w_start))
                                                                }
                                     if (aggrret=="geopaasche")  {
                                                                w_end<-sum(w_end_ret)
                                                                index_ret<-prod(retindex^(w_end_ret/w_end))
                                                                 }
                                     if (aggrret=="fisher")     { 
                                                                 index_retL<-sum(retindex*w_start_ret)
                                                                 index_retP<-sum(w_end_ret/retindex)
                                                                 index_retL<-index_retL/sum(w_start_ret)
                                                                 index_retP<-sum(w_end_ret)/index_retP
                                                                 index_ret<-(index_retL*index_retP)^(1/2)
                                                                }
                                     if (aggrret=="tornqvist")  {
                                                                w_start<-sum(w_start_ret)
                                                                w_end<-sum(w_end_ret)
index_ret<-prod(retindex^(0.5*(w_start_ret/w_start+w_end_ret/w_end)))
                                                                }
                                     if (aggrret=="arithmetic") index_ret<-sum(retindex)/length(id)
                                                                     
                                     if (aggrret=="geometric")  {index_ret<-prod(retindex)
                                                                index_ret<-index_ret^(1/length(id))
                                                                }
                                     index_set<-c(index_set,index_ret)
                                     w_start_set<-c(w_start_set,sum(w_start_ret))  
                                     w_end_set<-c(w_end_set,sum(w_end_ret))  
                                     }
                                     #aggregating over subsets/subgroups
                                     if (aggrsets=="none") index_final<-index_set[1]
                                     if (aggrsets=="laspeyres")  {index_final<-0
                                                                for (i in 1:length(datasets))  index_final<-index_final+index_set[i]*w_start_set[i]
                                                                index_final<-index_final/sum(w_start_set)
                                                                }                     
                                     if (aggrsets=="paasche")    {index_final<-0
                                                                for (i in 1:length(datasets))  index_final<-index_final+w_end_set[i]/index_set[i]
                                                                index_final<-sum(w_end_set)/index_final
                                                                }
                                     if (aggrsets=="geolaspeyres")  {index_final<-1
                                                                w_start_s<-sum(w_start_set)
                                                                for (i in 1:length(datasets))  index_final<-index_final*index_set[i]^(w_start_set[i]/w_start_s)
                                                                }
                                     if (aggrsets=="geopaasche")  {index_final<-1
                                                                w_end_s<-sum(w_end_set)
                                                                for (i in 1:length(datasets))  index_final<-index_final*index_set[i]^(w_end_set[i]/w_end_s)
                                                                }
                                     if (aggrsets=="fisher")     {index_setL<-0
                                                                 index_setP<-0
                                                                 for (i in 1:length(datasets))  {index_setL<-index_setL+index_set[i]*w_start_set[i]
                                                                                         index_setP<-index_setP+w_end_set[i]/index_set[i]
                                                                                         }
                                                                 index_setL<-index_setL/sum(w_start_set)
                                                                 index_setP<-sum(w_end_set)/index_setP
                                                                 index_final<-(index_setL*index_setP)^(1/2)
                                                                }
                                     if (aggrsets=="tornqvist") {index_final<-1
                                                                w_start_s<-sum(w_start_set)
                                                                w_end_s<-sum(w_end_set)
                                                                for (i in 1:length(datasets))  index_final<-index_final*index_set[i]^((w_start_set[i]/w_start_s+w_end_set[i]/w_end_s)/2)
                                                                }
                                     if (aggrsets=="arithmetic") {index_final<-0
                                                                for (i in 1:length(datasets))  index_final<-index_final+index_set[i]
                                                                index_final<-index_final/length(datasets)
                                                                 }                      
                                     if (aggrsets=="geometric")  {index_final<-1
                                                                for (i in 1:length(datasets))  index_final<-index_final*index_set[i]
                                                                index_final<-index_final^(1/length(datasets))
                                                                }
  return (index_final)
                           }
  if (interval==TRUE) {
                       results<-c(1)
                       idd<-list()
                       retindexx<-list(list())
                       #creating vector of dates
                       times<-c()
                       st<-start
                       while (st<=end)
                                       {
                       t<-substr(st,0,7)
                       times<-c(times,t)
                       lubridate::month(st)<-lubridate::month(st)+1
                                       }
                       #limiting to matched products depending on the used price index formula - part 1
                       if ((formula=="geks") | (formula=="geksw") | (formula=="geksj") | (formula=="ccdi") | (formula=="gk") | (formula=="tpd") | (formula=="geksl") | (formula=="wgeksl"))
                       {
                       wend<-start
                       lubridate::month(wend)<-lubridate::month(wend)+window-1
                       for (m in 1:length(datasets))  {set<-data.frame(datasets[[m]])
                       idd[[m]]<-matched(set, period1=substr(start,0,7), period2=substr(wend,0,7), type="retID", TRUE)
                                                        }
                        }
                       if ((formula=="geks_splice") | (formula=="geksw_splice") | (formula=="geksj_splice") | (formula=="ccdi_splice") | (formula=="gk_splice") | (formula=="tpd_splice")| (formula=="geksl_splice") | (formula=="wgeksl_splice"))
                      { 
                       wend<-start
                       lubridate::month(wend)<-lubridate::month(wend)+window-1
                       for (m in 1:length(datasets))  {set<-data.frame(datasets[[m]]) 
                       idd[[m]]<-matched(set, period1=substr(start,0,7), period2=substr(max(wend,end),0,7), type="retID", TRUE)
                       for (k in 1:length(idd[[m]])) { subset<-dplyr::filter(set, set$retID==idd[[m]][k])
                       pindex<-price_index(subset, substr(start,0,7), substr(end,0,7), formula, window, splice,  base, sigma, interval=TRUE) 
                       retindexx[[m]][[k]]<-pindex[,2]                                                                    
                                                        }
                                                        }
                       }      
                       end2<-end
                       end<-start
                       while (end<end2)
                                       {
                       lubridate::month(end)<-lubridate::month(end)+1
                       #start of procedure for final index calculation for time moments start and end
                       #weights for sets and retailers
w_start_set<-c() 
w_end_set<-c()
index_set<-c()
for (m in 1:length(datasets))  {set<-data.frame(datasets[[m]])
#limiting to matched products depending on the used price index formula - part 2 (rest of indices)
if ((formula=="jevons") | (formula=="dutot") | (formula=="carli") | (formula=="cswd") | (formula=="harmonic") | (formula=="bmw")| (formula=="laspeyres") | (formula=="paasche") | (formula=="fisher") |(formula=="tornqvist") | (formula=="geolaspeyres") | (formula=="geopaasche") | (formula=="drobisch") | (formula=="marshall_edgeworth") | (formula=="walsh") | (formula=="bialek") | (formula=="banajree") | (formula=="davies") | (formula=="stuvel") | (formula=="palgrave") | (formula=="geary_khamis") | (formula=="lehr") | (formula=="vartia") | (formula=="sato_vartia") | (formula=="lloyd_moulton") | (formula=="agmean") | (formula=="young") | (formula=="geoyoung") | (formula=="lowe") | (formula=="geolowe") | (formula=="hybrid") | (formula=="geohybrid"))
id<-matched(set, period1=substr(start,0,7), period2=substr(end,0,7), type="retID", FALSE)
else if ((formula=="geks") | (formula=="geksw") | (formula=="geksj") | (formula=="ccdi") | (formula=="gk") | (formula=="tpd") | (formula=="geksl") | (formula=="wgeksl")) id<-idd[[m]]
else if ((formula=="geks_splice") | (formula=="geksw_splice") | (formula=="geksj_splice") | (formula=="ccdi_splice") | (formula=="gk_splice") | (formula=="tpd_splice") | (formula=="geksl_splice") | (formula=="wgeksl_splice")) id<-idd[[m]]
else if ((formula=="geks_fbmw") | (formula=="geksw_fbmw") | (formula=="geksj_fbmw") | (formula=="ccdi_fbmw") |    (formula=="gk_fbmw") | (formula=="tpd_wbmw") | (formula=="geksl_fbmw") | (formula=="wgeksl_fbmw")){
wstart<-end
lubridate::year(wstart)<-lubridate::year(wstart)-1
if (lubridate::year(start)==lubridate::year(end)) id<-matched(set, period1=substr(wstart,0,7), period2=substr(end,0,7), type="retID", TRUE)
else id<-matched(set, period1=substr(start,0,7), period2=substr(end,0,7), type="retID", TRUE)
             } 
else id<-matched(set, period1=substr(start,0,7), period2=substr(end,0,7), type="retID", TRUE)
retindex<-c()
w_start_ret<-c()
w_end_ret<-c()
##limiting to those IDs that have at least one matched product from set
id_ok<-c()
for (k in 1:length(id)) { 
subset<-dplyr::filter(set, set$retID==id[k])
                            
if ((formula=="jevons") | (formula=="dutot") | (formula=="carli") | (formula=="cswd") | (formula=="harmonic") | (formula=="bmw") | (formula=="laspeyres") | (formula=="paasche") | (formula=="fisher") |(formula=="tornqvist") | (formula=="geolaspeyres") | (formula=="geopaasche") | (formula=="drobisch") | (formula=="marshall_edgeworth") | (formula=="walsh") | (formula=="bialek") | (formula=="banajree") | (formula=="davies") | (formula=="stuvel") | (formula=="palgrave") | (formula=="geary_khamis") | (formula=="lehr") | (formula=="vartia") | (formula=="sato_vartia") | (formula=="lloyd_moulton") | (formula=="agmean") | (formula=="young") | (formula=="geoyoung") | (formula=="lowe") | (formula=="geolowe") | (formula=="hybrid") | (formula=="geohybrid"))
idp<-matched(subset, period1=substr(start,0,7), period2=substr(end,0,7), type="prodID", FALSE)
else if ((formula=="geks") | (formula=="geksw") | (formula=="geksj") | (formula=="ccdi") | (formula=="gk") | (formula=="tpd")| (formula=="geksl") | (formula=="wgeksl")) {
wend<-start
lubridate::month(wend)<-lubridate::month(wend)+window-1
idp<-matched(subset, period1=substr(start,0,7), period2=substr(end,0,7), type="prodID", TRUE)
                                                                                                            }
else if ((formula=="geks_splice") | (formula=="geksw_splice") | (formula=="geksj_splice") | (formula=="ccdi_splice") | (formula=="gk_splice") | (formula=="tpd_splice")| (formula=="geksl_splice")| (formula=="wgeksl_splice")){
wend<-start
lubridate::month(wend)<-lubridate::month(wend)+window-1
idp<-matched(subset, period1=substr(start,0,7), period2=substr(max(wend,end),0,7), type="prodID", TRUE)
             }
else if ((formula=="geks_fbmw") | (formula=="geksw_fbmw") | (formula=="geksj_fbmw") | (formula=="ccdi_fbmw") |    (formula=="gk_fbmw") | (formula=="tpd_wbmw") | (formula=="geksl_fbmw") | (formula=="wgeksl_fbmw")){
wstart<-end
lubridate::year(wstart)<-lubridate::year(wstart)-1
if (lubridate::year(start)==lubridate::year(end)) idp<-matched(subset, period1=substr(wstart,0,7), period2=substr(end,0,7), type="prodID", TRUE)
else idp<-matched(subset, period1=substr(start,0,7), period2=substr(end,0,7), type="prodID", TRUE)
             } 
else idp<-matched(subset, period1=substr(start,0,7), period2=substr(end,0,7), type="prodID", TRUE)                       
                            
if (length(idp)>0) id_ok<-c(id_ok, id[k])                         
                            }         
id<-id_ok 
if (length(id)==0) stop("At least one subset does not include matched products")
##------------------------------------------------------------------                                   
for (k in 1:length(id)) { subset<-dplyr::filter(set, set$retID==id[k])
if ((formula=="geks_splice") | (formula=="geksw_splice") | (formula=="geksj_splice") | (formula=="ccdi_splice") | (formula=="gk_splice") | (formula=="tpd_splice") | (formula=="geksl_splice") | (formula=="wgeksl_splice"))
retindex<-c(retindex,retindexx[[m]][[k]][dist(start,end)+1]) 
else retindex<-c(retindex, price_index(subset, substr(start,0,7), substr(end,0,7), formula, window, splice, base, sigma, interval=FALSE))
d_start<-dplyr::filter(subset, lubridate::year(subset$time)==lubridate::year(start) & lubridate::month(subset$time)==lubridate::month(start))
d_end<-dplyr::filter(subset, lubridate::year(subset$time)==lubridate::year(end) & lubridate::month(subset$time)==lubridate::month(end))                   
w_start_ret<-c(w_start_ret,sum(d_start$prices*d_start$quantities))  
w_end_ret<-c(w_end_ret,sum(d_end$prices*d_end$quantities))    
                                                         }
                                     #aggregating over outlets
                                     if (aggrret=="none") index_ret<-retindex[1]
                                     if (aggrret=="laspeyres")  index_ret<-sum(retindex*w_start_ret)/sum(w_start_ret)
                                     if (aggrret=="paasche")    index_ret<-sum(w_end_ret)/sum(w_end_ret/retindex)
                                     if (aggrret=="geolaspeyres")  {w_start<-sum(w_start_ret)
                                                                index_ret<-prod(retindex^(w_start_ret/w_start))
                                                                }
                                     if (aggrret=="geopaasche")  {
                                                                w_end<-sum(w_end_ret)
                                                                index_ret<-prod(retindex^(w_end_ret/w_end))
                                                                 }
                                     if (aggrret=="fisher")     { 
                                                                 index_retL<-sum(retindex*w_start_ret)
                                                                 index_retP<-sum(w_end_ret/retindex)
                                                                 index_retL<-index_retL/sum(w_start_ret)
                                                                 index_retP<-sum(w_end_ret)/index_retP
                                                                 index_ret<-(index_retL*index_retP)^(1/2)
                                                                }
                                     if (aggrret=="tornqvist")  {
                                                                w_start<-sum(w_start_ret)
                                                                w_end<-sum(w_end_ret)
index_ret<-prod(retindex^(0.5*(w_start_ret/w_start+w_end_ret/w_end)))
                                                                }
                                     if (aggrret=="arithmetic") index_ret<-sum(retindex)/length(id)
                                                                     
                                     if (aggrret=="geometric")  {index_ret<-prod(retindex)
                                                                index_ret<-index_ret^(1/length(id))
                                                                }    
                                     index_set<-c(index_set,index_ret)
                                     w_start_set<-c(w_start_set,sum(w_start_ret))  
                                     w_end_set<-c(w_end_set,sum(w_end_ret)) 
                                     }
                                     #aggregating over subsets/subgroups 
                                     if (aggrsets=="none") index_final<-index_set[1]
                                      
                                     if (aggrsets=="laspeyres")  {index_final<-0
                                                                for (i in 1:length(datasets))  index_final<-index_final+index_set[i]*w_start_set[i]
                                                                index_final<-index_final/sum(w_start_set)
                                                                }                     
                                     if (aggrsets=="paasche")    {index_final<-0
                                                                for (i in 1:length(datasets))  index_final<-index_final+w_end_set[i]/index_set[i]
                                                                index_final<-sum(w_end_set)/index_final
                                                                }
                                     if (aggrsets=="geolaspeyres")  {index_final<-1
                                                                w_start_s<-sum(w_start_set)
                                                                for (i in 1:length(datasets))  index_final<-index_final*index_set[i]^(w_start_set[i]/w_start_s)
                                                                }
                                     if (aggrsets=="geopaasche")  {index_final<-1
                                                                w_end_s<-sum(w_end_set)
                                                                for (i in 1:length(datasets))  index_final<-index_final*index_set[i]^(w_end_set[i]/w_end_s)
                                                               }
                                     if (aggrsets=="fisher")     {index_setL<-0
                                                                 index_setP<-0
                                                                 for (i in 1:length(datasets))  {index_setL<-index_setL+index_set[i]*w_start_set[i]
                                                                                         index_setP<-index_setP+w_end_set[i]/index_set[i]
                                                                                         }
                                                                 index_setL<-index_setL/sum(w_start_set)
                                                                 index_setP<-sum(w_end_set)/index_setP
                                                                 index_final<-(index_setL*index_setP)^(1/2)
                                                                }
                                     if (aggrsets=="tornqvist")  {index_final<-1
                                                                w_start_s<-sum(w_start_set)
                                                                w_end_s<-sum(w_end_set)
                                                                for (i in 1:length(datasets)) 
index_final<-index_final*index_set[i]^(0.5*(w_start_set[i]/w_start_s+w_end_set[i]/w_end_s))
                                                                 }
                                     if (aggrsets=="arithmetic") {index_final<-0
                                                                for (i in 1:length(datasets))  index_final<-index_final+index_set[i]
                                                                index_final<-index_final/length(datasets)
                                                                 }                      
                                     if (aggrsets=="geometric")  {index_final<-1
                                                                for (i in 1:length(datasets))  index_final<-index_final*index_set[i]
                                                                index_final<-index_final^(1/length(datasets))
                                                                }
                                     # end of the procedure
                                     results<-c(results, index_final)
                                    }
datfr<-data.frame(c(times),c(results)) 
colnames(datfr)<-c("date",formula)
return (datfr)
 }
 }
 
#' @title  A general function for graphical comparison of price indices
#'
#' @description This function returns a figure with plots of previously calculated price indices. 
#' @param finalindices A list of data frames with previously calculated price indices. Each data frame must consist of two columns, i.e. the first column must includes dates limited to the year and month (e.g.: "2020-04") and the second column must indicate price index values for corresponding dates. The above-mentioned single data frame may be created manually in the previous step or it may be a result of functions: \code{price_index} or \code{final_index}. All considered data frames must have an identical number of rows.
#' @param names A vector of character strings describing names of presented indices.
#' @rdname compare_final_indices
#' @return This function returns a figure with plots of previously calculated price indices. It allows for graphical comparison of price index values which were previously calculated and now are provided as data frames (see \code{finalindices} parameter).
#' @examples 
#' ## Caluclating two indices by using two different package functions:
#' \donttest{index1<-final_index(datasets=list(milk), start="2018-12", 
#' end="2019-12",formula="walsh",interval=TRUE)}
#' \donttest{index2<-price_index(milk,start="2018-12", end="2019-12",
#' formula="geks",interval=TRUE)}
#' ## Graphical comparison of these two indices 
#' \donttest{compare_final_indices(finalindices=list(index1,index2), 
#' names=c("Walsh index", "GEKS index"))}
#' @export

compare_final_indices<-function(finalindices=list(), names=c())
{
date<-value<-formula<-NULL
if (length(finalindices)==0) stop("at least one final index must be chosen")
for (m in 1:length(finalindices)) {if (nrow(data.frame(finalindices[[m]]))==0) stop("At least one data frame is empty")
  }
for (m in 1:length(finalindices)) {if (!(nrow(data.frame(finalindices[[m]]))==nrow(data.frame(finalindices[[1]])))) stop("Data frames must have identical number of rows")  
}
graph<-data.frame(finalindices[1])
if (length(finalindices)>1) for (i in 2:length(finalindices)) graph<-cbind(graph, data.frame(finalindices[i])[2]) 
if (length(names)>0) for (i in 1:length(names)) colnames(graph)[i+1]<-names[i]
graph$date<-as.Date(paste(graph$date,"-01",sep=""))
graph<-reshape::melt(graph, id.var='date') 
colnames(graph)<-c("date","formula","value")
ggplot2::ggplot(graph, ggplot2::aes(x=date, y=value, col=formula)) + ggplot2::geom_point()+ggplot2::geom_line()+ggplot2::labs(x="date",y="price index value")+ggplot2::scale_x_date(date_labels="%Y %m",date_breaks  ="1 month")+ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust = 1)) 
}


