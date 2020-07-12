

#' @title  Calculating the bilateral hybrid price index
#'
#' @description This function returns a value (or vector of values) of the bilateral hybrid price index. The hybrid index was proposed by Bialek (2020) and it uses correlation coefficients between prices and quantities.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the hybrid price index formula (as character) limited to the year and month, e.g. "2020-01"
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname hybrid
#' @return The function returns a value (or vector of values) of the bilateral hybrid price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Bialek, J. (2020). \emph{Proposition of a Hybrid Price Index Formula for the Consumer Price Index Measurement}. Equilibrium. Quarterly Journal of Economics and Economic Policy (in review).}
#' @examples 
#' hybrid(milk, start="2019-12", end="2020-08", base="2018-12")
#' hybrid(milk, start="2019-12", end="2020-08", base="2018-12", interval=TRUE)
#' @export
hybrid<-function(data,start,end,base=start,interval=FALSE)  { if (start==end) return (1)
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
                                   sale_base<-prices(data2,period=base,set=id)*quantities(data2,period=base,set=id)
                                   sale_start<-prices(data2,period=start,set=id)*quantities(data2,period=base,set=id)
                                   sale_end<-prices(data2,period=end,set=id)*quantities(data2,period=base,set=id)
                                   vol_base<-sum(sale_base)
                                   vol_start<-sum(sale_start)
                                   vol_end<-sum(sale_end)
  corr_base<-stats::cor(prices(data2, period=base,set=id), quantities(data2, period=base,set=id))
  corr_start<-stats::cor(prices(data2, period=start,set=id), quantities(data2, period=base,set=id))
  corr_end<-stats::cor(prices(data2, period=end,set=id), quantities(data2, period=base,set=id))
                                   if ((abs(corr_base)+abs(corr_start)+abs(corr_end))==0)
                                   {corr_base<-1/3
                                    corr_start<-1/3
                                    corr_end<-1/3
                                   }
                                   weight<-c()
                                   for (i in 1:length(id)) weight<-c(weight, abs(corr_base)/(abs(corr_base)+abs(corr_start)+abs(corr_end))*sale_base[i]/vol_base+abs(corr_start)/(abs(corr_base)+abs(corr_start)+abs(corr_end))*sale_start[i]/vol_start+abs(corr_end)/(abs(corr_base)+abs(corr_start)+abs(corr_end))*sale_end[i]/vol_end)
                                   result<-c(result,sum(weight*(price_end/price_start)))     
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
                                   sale_base<-prices(data,period=base,set=id)*quantities(data,period=base,set=id)
                                   sale_start<-prices(data,period=start,set=id)*quantities(data,period=base,set=id)
                                   sale_end<-prices(data,period=end,set=id)*quantities(data,period=base,set=id)
                                   vol_base<-sum(sale_base)
                                   vol_start<-sum(sale_start)
                                   vol_end<-sum(sale_end)
corr_base<-stats::cor(prices(data, period=base,set=id), quantities(data, period=base,set=id))
corr_start<-stats::cor(prices(data, period=start,set=id), quantities(data, period=base,set=id))
corr_end<-stats::cor(prices(data, period=end,set=id), quantities(data, period=base,set=id))
                                   if ((abs(corr_base)+abs(corr_start)+abs(corr_end))==0)
                                   {corr_base<-1/3
                                    corr_start<-1/3
                                    corr_end<-1/3
                                   }
                                   weight<-c()
                                   for (i in 1:length(id)) weight<-c(weight, abs(corr_base)/(abs(corr_base)+abs(corr_start)+abs(corr_end))*sale_base[i]/vol_base+abs(corr_start)/(abs(corr_base)+abs(corr_start)+abs(corr_end))*sale_start[i]/vol_start+abs(corr_end)/(abs(corr_base)+abs(corr_start)+abs(corr_end))*sale_end[i]/vol_end)
                                   return(sum(weight*(price_end/price_start)))
                                   }
                                  }

#' @title  Calculating the bilateral geohybrid price index
#'
#' @description This function returns a value (or vector of values) of the bilateral geohybrid price index. The geohybrid index was proposed by Bialek (2020) and it uses correlation coefficients between prices and quantities.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the geohybrid price index formula (as character) limited to the year and month, e.g. "2020-01"
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname geohybrid
#' @return The function returns a value (or vector of values) of the bilateral geohybrid price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Bialek, J. (2020). \emph{Proposition of a Hybrid Price Index Formula for the Consumer Price Index Measurement}. Equilibrium. Quarterly Journal of Economics and Economic Policy (in review).}
#' @examples 
#' geohybrid(milk, start="2019-12", end="2020-08", base="2018-12")
#' geohybrid(milk, start="2019-12", end="2020-08", base="2018-12", interval=TRUE)
#' @export
geohybrid<-function(data,start,end,base=start,interval=FALSE)  { if (start==end) return (1)
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
                                   sale_base<-prices(data2,period=base,set=id)*quantities(data2,period=base,set=id)
                                   sale_start<-prices(data2,period=start,set=id)*quantities(data2,period=base,set=id)
                                   sale_end<-prices(data2,period=end,set=id)*quantities(data2,period=base,set=id)
                                   vol_base<-sum(sale_base)
                                   vol_start<-sum(sale_start)
                                   vol_end<-sum(sale_end)
corr_base<-stats::cor(prices(data2, period=base,set=id), quantities(data2, period=base,set=id))
corr_start<-stats::cor(prices(data2, period=start,set=id), quantities(data2, period=base,set=id))
corr_end<-stats::cor(prices(data2, period=end,set=id), quantities(data2, period=base,set=id))
                                    if ((abs(corr_base)+abs(corr_start)+abs(corr_end))==0)
                                   {corr_base<-1/3
                                    corr_start<-1/3
                                    corr_end<-1/3
                                   }
                                   weight<-c()
                                   for (i in 1:length(id)) weight<-c(weight, abs(corr_base)/(abs(corr_base)+abs(corr_start)+abs(corr_end))*sale_base[i]/vol_base+abs(corr_start)/(abs(corr_base)+abs(corr_start)+abs(corr_end))*sale_start[i]/vol_start+abs(corr_end)/(abs(corr_base)+abs(corr_start)+abs(corr_end))*sale_end[i]/vol_end)
                                   result<-c(result,prod((price_end/price_start)^weight))     
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
                                   sale_base<-prices(data,period=base,set=id)*quantities(data,period=base,set=id)
                                   sale_start<-prices(data,period=start,set=id)*quantities(data,period=base,set=id)
                                   sale_end<-prices(data,period=end,set=id)*quantities(data,period=base,set=id)
                                   vol_base<-sum(sale_base)
                                   vol_start<-sum(sale_start)
                                   vol_end<-sum(sale_end)
corr_base<-stats::cor(prices(data, period=base,set=id), quantities(data, period=base,set=id))
corr_start<-stats::cor(prices(data, period=start,set=id), quantities(data, period=base,set=id))
corr_end<-stats::cor(prices(data, period=end,set=id), quantities(data, period=base,set=id))
                                   if ((abs(corr_base)+abs(corr_start)+abs(corr_end))==0)
                                   {corr_base<-1/3
                                    corr_start<-1/3
                                    corr_end<-1/3
                                   }
                                   weight<-c()
                                   for (i in 1:length(id)) weight<-c(weight, abs(corr_base)/(abs(corr_base)+abs(corr_start)+abs(corr_end))*sale_base[i]/vol_base+abs(corr_start)/(abs(corr_base)+abs(corr_start)+abs(corr_end))*sale_start[i]/vol_start+abs(corr_end)/(abs(corr_base)+abs(corr_start)+abs(corr_end))*sale_end[i]/vol_end)
                                   return(prod((price_end/price_start)^weight))
                                   }
                                  }


#' @title  Calculating the the monthly chained hybrid price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained hybrid price index. The hybrid index was proposed by Bialek (2020) and it uses correlation coefficients between prices and quantities.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the hybrid price index formula (as character) limited to the year and month, e.g. "2020-01"
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chhybrid
#' @return The function returns a value (or vector of values) of the monthly chained hybrid price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Bialek, J. (2020). \emph{Proposition of a Hybrid Price Index Formula for the Consumer Price Index Measurement}. Equilibrium. Quarterly Journal of Economics and Economic Policy (in review).}
#' @examples 
#' chhybrid(milk, start="2019-12", end="2020-08", base="2018-12")
#' chhybrid(milk, start="2019-12", end="2020-08", base="2018-12", interval=TRUE)
#' @export
chhybrid<-function(data,start,end, base=start,interval=FALSE)  { if (start==end) return (1)
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
                                   f<-function (i) return (hybrid(data, start=dates[i],end=dates[i+1],base))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                        }
                                   return(chained)    
                                  }

#' @title  Calculating the the monthly chained geohybrid price index
#'
#' @description This function returns a value (or vector of values) of the monthly chained geohybrid price index. The geohybrid index was proposed by Bialek (2020) and it uses correlation coefficients between prices and quantities.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param base The prior period used in the geohybrid price index formula (as character) limited to the year and month, e.g. "2020-01"
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname chgeohybrid
#' @return The function returns a value (or vector of values) of the monthly chained geohybrid price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @references
#' {Bialek, J. (2020). \emph{Proposition of a Hybrid Price Index Formula for the Consumer Price Index Measurement}. Equilibrium. Quarterly Journal of Economics and Economic Policy (in review).}
#' @examples 
#' chgeohybrid(milk, start="2019-12", end="2020-08", base="2018-12")
#' chgeohybrid(milk, start="2019-12", end="2020-08", base="2018-12", interval=TRUE)
#' @export
chgeohybrid<-function(data,start,end, base=start,interval=FALSE)  { if (start==end) return (1)
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
                                   f<-function (i) return (geohybrid(data, start=dates[i],end=dates[i+1],base))
                                   ind<-seq(1:(length(dates)-1))
                                   chained1<-sapply(ind, f)
                                   chained<-prod(chained1)
                                   if (interval==TRUE) {#optional returning all fixed base chain indices
                                   chained<-c(1)
                                   for (i in 1:length(chained1)) chained<-c(chained,prod(chained1[seq(1,i)]))
                                                        }
                                   return(chained)    
                                  }




