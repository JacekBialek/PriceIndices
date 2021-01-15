

#' @title  Calculating the bilateral hybrid price index
#'
#' @description This function returns a value (or a vector of values) of the bilateral hybrid price index. The hybrid index was proposed by Bialek (2020) and it uses correlation coefficients between prices and quantities.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. '2020-03'.
#' @param end The research period (as character) limited to the year and month, e.g. '2020-04'.
#' @param base The prior period used in the hybrid price index formula (as character) limited to the year and month, e.g. '2020-01'.
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname hybrid
#' @return The function returns a value (or a vector of values) of the bilateral hybrid price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}}, \code{\link{final_index}} or \code{\link{final_index2}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or \code{\link{final_index2}} function).   
#' @references
#' {Bialek, J. (2020). \emph{Proposition of a Hybrid Price Index Formula for the Consumer Price Index Measurement}. Equilibrium. Quarterly Journal of Economics and Economic Policy, 15(4), 697-716.}
#'
#' @examples 
#' \donttest{hybrid(milk, start="2019-12", end="2020-08", base="2018-12")}
#' \donttest{hybrid(milk, start="2019-12", end="2020-08", base="2018-12", interval=TRUE)}
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
#' @return The function returns a value (or vector of values) of the bilateral geohybrid price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Bialek, J. (2020). \emph{Proposition of a Hybrid Price Index Formula for the Consumer Price Index Measurement}. Equilibrium. Quarterly Journal of Economics and Economic Policy, 15(4), 697-716.}
#'
#' @examples 
#' \donttest{geohybrid(milk, start="2019-12", end="2020-08", base="2018-12")}
#' \donttest{geohybrid(milk, start="2019-12", end="2020-08", base="2018-12", interval=TRUE)}
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
#' @return The function returns a value (or vector of values) of the monthly chained hybrid price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Bialek, J. (2020). \emph{Proposition of a Hybrid Price Index Formula for the Consumer Price Index Measurement}. Equilibrium. Quarterly Journal of Economics and Economic Policy, 15(4), 697-716.}
#'
#' @examples 
#' \donttest{chhybrid(milk, start="2019-12", end="2020-08", base="2018-12")}
#' \donttest{chhybrid(milk, start="2019-12", end="2020-08", base="2018-12", interval=TRUE)}
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
#' @return The function returns a value (or vector of values) of the monthly chained geohybrid price index depending on the \code{interval} parameter. If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Bialek, J. (2020). \emph{Proposition of a Hybrid Price Index Formula for the Consumer Price Index Measurement}. Equilibrium. Quarterly Journal of Economics and Economic Policy, 15(4), 697-716.}
#'
#' @examples 
#' \donttest{chgeohybrid(milk, start="2019-12", end="2020-08", base="2018-12")}
#' \donttest{chgeohybrid(milk, start="2019-12", end="2020-08", base="2018-12", interval=TRUE)}
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
#' @return This function returns an artificial scanner dataset where prices and quantities are lognormally distributed. The characteristics for these lognormal distributions are set by \code{pmi}, \code{sigma}, \code{qmi} and \code{qsigma} parameters. This function works for a fixed number of products and outlets (see \code{n} and \code{r} parameters). The generated dataset is ready for further price index calculations.   
#'
#' @examples 
#' generate(pmi=c(1.02,1.03,1.04),psigma=c(0.05,0.09,0.02),qmi=c(3,4,4),
#' qsigma=c(0.1,0.1,0.15),start="2020-01",days=TRUE)
#' generate(pmi=c(1.02,1.03,1.04),psigma=c(0.05,0.09,0.02),qmi=c(6,6,7),
#' qsigma=c(0.1,0.1,0.15),start="2020-01",n=1000,n0=132578,r=10)

#' @export
generate<-function(pmi=c(),psigma=c(),qmi=c(),qsigma=c(),prec=c(2,0),n=100,n0=1,r=1,r0=1,start,days=FALSE)
{
 if ((length(pmi)<=1)|(length(psigma)<=1)|(length(qmi)<=1)|(length(qsigma)<=1)) stop("Lengths of parameters: pmi, psigma, qmi and qsigma must be 2 or more!")
 if (!((length(pmi)==length(psigma)) & (length(pmi)==length(qmi)) & (length(qmi)==length(qsigma)))) stop("Lengths of parameters: pmi, psigma, qmi and qsigma must be identical!")
 if (!(length(prec)==2)) stop("A length of 'prec' parameter must be 2!")
 start<-paste(start,"-01",sep="")
 start<-as.Date(start)
 #rand data frames for all periods
 DT<-data.frame()
 for (k in 1:length(pmi))
 { 
 #time
 time<-c()
 for (i in 1:n) {
                if (days==TRUE) {nd<-lubridate::days_in_month(start)
                lubridate::day(start)<-sample(nd,1)
                }
                time<-c(time,as.character(start))
                }
 time<-as.Date(time)
 #prodID
 prodID<-seq(n0,n0+n-1)
 ret<-r0+r-1
 for (i in r0:ret) {
 #retID
 retID<-replicate(n, i)
 #prices
 prices<-stats::rlnorm(n,pmi[k],psigma[k])
 #quantities
 quantities<-stats::rlnorm(n,qmi[k],qsigma[k]) 
 DT<-rbind(DT,data.frame(time, prices, quantities, prodID,retID))
 } 
 lubridate::month(start)<-lubridate::month(start)+1
 }
 DT$prices<-round(DT$prices, prec[1])
 DT$quantities<-round(DT$quantities, prec[2])
 return (DT)  
}
  

#' @title  Calculating theoretical (expected) values of the unweighted price index
#'
#' @description This function calculates the theoretical value of the unweighted price index for lognormally distributed prices.
#' @param pmi A numeric vector indicating \code{mi} parameters for lognormally distributed prices from the subsequent months.
#' @param psigma A numeric vector indicating \code{sigma} parameters for lognormally distributed prices from the subsequent months.
#' @param start The first period in the generated data frame (as character) limited to the year and month, e.g. '2019-12'.
#' @param ratio A logical parameter indicating how we define the theoretical unweighted price index. If it is set to TRUE, then the resulting value is a ratio of expected price values from compared months; otherwise the resulting value is the expected value of the ratio of prices from compared months.
#' @rdname tindex
#' @return This function calculates the theoretical value of the unweighted price index for lognormally distributed prices (the month defined by \code{start} parameter plays a role of the fixed base period). The characteristics for these lognormal distributions are set by \code{pmi} and \code{sigma} parameters. The \code{ratio} parameter allows to control the definition of resulting theoretical price index values. The function provides a data frame consisting of dates and corresponding expected values of the theoretical unweighted price index. The generated dataset is ready for further price index calculations.
#'
#' @examples 
#' tindex(pmi=c(1,1.2,1.3),psigma=c(0.1,0.2,0.15),start="2020-01")
#' tindex(pmi=c(1,1.2,1.3),psigma=c(0.1,0.2,0.15),start="2020-01",ratio=FALSE)
#' @export

tindex<-function(pmi=c(),psigma=c(),start, ratio=TRUE)
{
 if ((length(pmi)<=1)|(length(psigma)<=1)) stop("Lengths of parameters: pmi and psigma must be 2 or more!")
 if (!(length(pmi)==length(psigma))) stop("Lengths of parameters: pmi and psigma must be identical!")
 date<-c(start)
 tindex<-c(1)
 start<-paste(start,"-01",sep="")
 start<-as.Date(start)
 #values of parameters for base period
 mi0<-replicate(length(pmi)-1,pmi[1])
 sigma0<-replicate(length(psigma)-1,psigma[1])
 mi<-c()
 sigma<-c()
 #values of parameters for other periods
 for (k in 2:length(pmi))
 { 
 lubridate::month(start)<-lubridate::month(start)+1
 date<-c(date,substr(start,0,7))
 mi<-c(mi,pmi[k])
 sigma<-c(sigma,psigma[k])
 }
 #calculating the expected value of the theoretical price index
 if (ratio==TRUE) {
 tindexx<-mi-mi0+0.5*(sigma0^2+sigma^2)
 tindexx<-exp(tindexx)
                 }
 else {
 tindexx1<-mi0+0.5*sigma0^2 
 tindexx1<-exp(tindexx1)
 tindexx2<-mi+0.5*sigma^2
 tindexx2<-exp(tindexx2)
 tindexx<-tindexx2/tindexx1
      }
 
 tindex<-c(tindex, tindexx)
 #the resulting data frame
 DT<-data.frame(date, tindex)
 return (DT)  
}


#' @title  Calculating the relative price and/or quantity dissimilarity measure between periods
#'
#' @description This function returns a value of the relative price and/or quantity dissimilarity measure.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
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

dissimilarity<-function (data, period1, period2, type="p")
{if (nrow(data)==0) stop("A data frame is empty") 
allowed<-c("p","q","pq")
if (length(base::intersect(type,allowed))==0) stop("there are no such types of dissimilarity measures")
r<-paste(period1,"-01",sep="")
t<-paste(period2,"-01",sep="")
r<-as.Date(r)
t<-as.Date(t)
  
data<-dplyr::filter(data,(lubridate::year(data$time)==lubridate::year(r) & lubridate::month(data$time)==lubridate::month(r)) | (lubridate::year(data$time)==lubridate::year(t) & lubridate::month(data$time)==lubridate::month(t)))
   id<-matched(data,r,t) 
   price_r<-prices(data,period=r,set=id)
   price_t<-prices(data,period=t,set=id)
   quantity_r<-quantities(data,period=r,set=id)
   quantity_t<-quantities(data,period=t,set=id)
   sales_r<-sales(data,period=r,set=id)
   sales_t<-sales(data,period=t,set=id)
   sum_r<-sum(sales_r)
   sum_t<-sum(sales_t)
   prqt<-sum(price_r*quantity_t) 
   ptqr<-sum(price_t*quantity_r)
   
   if (type=="p") {
     sum1<-0.5*sum(((sales_t/sum_t)+(sales_r/sum_r))*((sales_t/sum_t)-price_r*quantity_t/prqt)^2)
     sum2<-0.5*sum(((sales_t/sum_t)+(sales_r/sum_r))*((sales_r/sum_r)-price_t*quantity_r/ptqr)^2)
     return (sum1+sum2)
   }
   if (type=="q") {
     sum1<-0.5*sum(((sales_t/sum_t)+(sales_r/sum_r))*((sales_t/sum_t)-price_t*quantity_r/ptqr)^2)
     sum2<-0.5*sum(((sales_t/sum_t)+(sales_r/sum_r))*((sales_r/sum_r)-price_r*quantity_t/prqt)^2)
     return (sum1+sum2)
     
   }
   if (type=="pq") {
     sum1<-0.5*sum(((sales_t/sum_t)+(sales_r/sum_r))*((sales_t/sum_t)-price_r*quantity_t/prqt)^2)
     sum2<-0.5*sum(((sales_t/sum_t)+(sales_r/sum_r))*((sales_r/sum_r)-price_t*quantity_r/ptqr)^2) 
     sum3<-0.5*sum(((sales_t/sum_t)+(sales_r/sum_r))*((sales_t/sum_t)-price_t*quantity_r/ptqr)^2)
     sum4<-0.5*sum(((sales_t/sum_t)+(sales_r/sum_r))*((sales_r/sum_r)-price_r*quantity_t/prqt)^2)
     return (min(sum1+sum2,sum3+sum4))
   } 
  
}

#' @title  Presenting the relative price and/or quantity dissimilarity measure over time
#'
#' @description This function presents values of the relative price and/or quantity dissimilarity measure over time.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. '2019-03'.
#' @param end The research period (as character) limited to the year and month, e.g. '2019-07'.
#' @param type The parameter indicates what type of dissimilarity measure is to be calculated. Possible values of the \code{type} parameter are: \code{p} (for the price dissimilarity measure calculation), \code{q} (for the quantity dissimilarity measure calculation) or \code{pq} (for the dSPQ measure calculation, i.e. the measure of relative price and quantity dissimilarity - see \code{References}).
#' @param benchmark The benchmark period (as character) limited to the year and month, e.g. '2019-07'.
#' @param figure A logical parameter indicating the resulting object. If it is TRUE, the function presents the above-mentioned dissimilarities over time via a figure. Otherwise, the function returns a dataframe.  
#' @rdname dissimilarity_fig
#' @return This function presents values of the relative price and/or quantity dissimilarity measure over time. The user can choose a benchmark period (defined by \code{benchmark}) and the type of dissimilarity measure is to be calculated (defined by \code{type}). The obtained results of dissimilarities over time can be presented in a dataframe form or via a figure (the default value of \code{figure} is TRUE, which results in a figure). 
#' @references
#' {Diewert, E. (2020). \emph{The Chain Drift Problem and Multilateral Indexes.} Chapter 6 in: Consumer Price Index Theory (draft)}
#'
#' @examples 
#' \donttest{dissimilarity_fig(milk, start="2018-12",end="2019-12",type="q",figure=FALSE)}
#' \donttest{dissimilarity_fig(milk, start="2018-12",end="2019-12",type="pq",benchmark="start")}
#' @export

dissimilarity_fig<-function (data, start, end, type="p", benchmark="end", figure=TRUE)
{
if (nrow(data)==0) stop("A data frame is empty") 
allowed_type<-c("p","q","pq")
if (length(base::intersect(type,allowed_type))==0) stop("there are no such types of dissimilarity measures")
allowed_benchmark<-c("start","end")
if (length(base::intersect(benchmark,allowed_benchmark))==0) stop("bad specification of the 'benchmark' parameter")
start<-paste(start,"-01",sep="")
end<-paste(end,"-01",sep="")
start<-as.Date(start)
end<-as.Date(end)
if (start>=end) stop("parameters must satisfy: start<end")
times<-c()  
values<-c()  

if (benchmark=="end")
{
  {
t2<-substr(end,0,7)
while (start<end)
                                    
              {  
               t1<-substr(start,0,7)
               times<-c(times,t1)
               values<-c(values, dissimilarity(data, period1=t1,period2=t2,type))
               lubridate::month(start)<-lubridate::month(start)+1 
              }
times<-c(times, t2)
values<-c(values,0)
tab<-data.frame(c(times), c(values))
colnames(tab)<-c("date", "dissimilarity")
if (figure==FALSE) return (tab)  
#returning a figure 
else
 {
tab$date<-as.Date(paste(tab$date,"01",sep="-"))
ggplot2::ggplot(tab, ggplot2::aes(x=date, y=dissimilarity)) + ggplot2::geom_point()+ggplot2::geom_line()+ggplot2::labs(x="date",y="dissimilarity")+ggplot2::scale_x_date(date_labels="%Y %m",date_breaks  ="1 month")+ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust = 1))  
 }  
}  
}
else
{
values<-c(values,0)
t1<-substr(start,0,7)
times<-c(times,t1)
while (start<end)
                                    
              {  
               lubridate::month(start)<-lubridate::month(start)+1 
               t2<-substr(start,0,7)
               times<-c(times,t2)
               values<-c(values, dissimilarity(data, period1=t1,period2=t2,type))
              }
tab<-data.frame(c(times), c(values))
colnames(tab)<-c("date", "dissimilarity")
if (figure==FALSE) return (tab)  
#returning a figure 
else
 {
tab$date<-as.Date(paste(tab$date,"01",sep="-"))
ggplot2::ggplot(tab, ggplot2::aes(x=date, y=dissimilarity)) + ggplot2::geom_point()+ggplot2::geom_line()+ggplot2::labs(x="date",y="dissimilarity")+ggplot2::scale_x_date(date_labels="%Y %m",date_breaks  ="1 month")+ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust = 1))  
 }  
}
}

#' @title  Calculating the multilateral SPQ price index 
#'
#' @description This function returns a value of the multilateral SPQ price index which is based on the relative price and quantity dissimilarity measure.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. '2019-03'.
#' @param end The research period (as character) limited to the year and month, e.g. '2019-07'.
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE). 
#' @rdname SPQ
#' @return This function returns a value of the multilateral SPQ price index which is based on the relative price and quantity dissimilarity measure (see \code{References}). If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}}, \code{\link{final_index}} or \code{\link{final_index2}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or \code{\link{final_index2}} function). 
#' @references
#' {Diewert, E. (2020). \emph{The Chain Drift Problem and Multilateral Indexes.} Chapter 6 in: Consumer Price Index Theory (draft)}
#'
#' @examples 
#' \donttest{SPQ(milk, start="2018-12",end="2019-12")}
#' \donttest{SPQ(milk, start="2018-12",end="2019-12",interval=TRUE)}
#' @export
SPQ<-function (data, start, end, interval=FALSE)
{
if (nrow(data)==0) stop("A data frame is empty")
start<-paste(start,"-01",sep="")
end<-paste(end,"-01",sep="")
start<-as.Date(start)
end<-as.Date(end)
if (start>end) stop("parameters must satisfy: start<=end")
if (start==end) return (1)

times<-c()
while (start<=end)
                                    
              {times<-c(times,substr(start,0,7)) 
               lubridate::month(start)<-lubridate::month(start)+1 
              }
if (length(times)==2) 
  {if (interval==FALSE) return (fisher(data,start=times[1],end=times[2]))
  else return (c(1,fisher(data,start=times[1],end=times[2])))
  }
spq<-c(1)
spq<-c(spq,fisher(data,start=times[1],end=times[2]))
for (i in 3:length(times))
 {#main body
  
  #delta sp for r=1,2,...,i-1 (we drop the last element)
  sp<-dissimilarity_fig(data, start=times[1], end=times[i], type="pq", benchmark="end", figure=FALSE)$dissimilarity[-i]
  
  #position of the minimal element
  pos_min<-max(which(sp==min(sp)))
  spq<-c(spq,fisher(data,start=times[pos_min],end=times[i])*spq[pos_min])
 }
if (interval==TRUE) return (spq)
else return (spq[length(spq)])
}  
  

#' @title  Calculating the multilateral GEKS-L price index
#'
#' @description This function returns a value of the multilateral GEKS-L price index (to be more precise: the GEKS index based on the Laspeyres formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksl
#' @return This function returns a value of the multilateral GEKS-L price index (to be more precise: the GEKS index based on the Laspeyres formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function). 
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' \donttest{geksl(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{geksl(milk, start="2018-12", end="2019-12")}
#' @export

geksl<-function(data,start,end, wstart=start,window=13)  { if (start==end) return (1)
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
                                   gksl<-function (tt) return (c(laspeyres(data,tt,end),laspeyres(data,tt,start)))
                                   vec<-sapply(dates, gksl)
                                   geksl<-prod(vec[1,]/vec[2,])
                                   geksl<-geksl^(1/window)
                                   return(geksl)    
                                   }

#' @title  Calculating the multilateral WGEKS-L price index
#'
#' @description This function returns a value of the multilateral weighted WGEKS-L price index (to be more precise: the weighted GEKS index based on the Laspeyres formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname wgeksl
#' @return This function returns a value of the multilateral weighted WGEKS-L price index (to be more precise: the weighted GEKS index based on the Laspeyres formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples 
#' \donttest{wgeksl(milk, start="2019-01", end="2019-08",window=10)}
#' \donttest{wgeksl(milk, start="2018-12", end="2019-12")}
#' @export

wgeksl<-function(data,start,end, wstart=start,window=13)  { if (start==end) return (1)
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
                                   wgksl<-function (tt) return (c(laspeyres(data,tt,end),laspeyres(data,tt,start)))
                                   vec<-sapply(dates, wgksl)
                                   sales_in_time<-function (tt) return (sum(sales(data,tt)))
                                   expenditures<-sapply(dates, sales_in_time)
                                   expenditures<-expenditures/sum(expenditures)
                                   wgeksl<-prod((vec[1,]/vec[2,])^expenditures)
                                   return(wgeksl)    
                                   }




#' @title  Extending the multilateral GEKS-L price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral GEKS-L price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksl_fbew
#' @return This function returns a value of the multilateral GEKS-L price index (the GEKS index based on the Laspeyres formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating,please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#'
#' @examples 
#' \donttest{geksl_fbew(milk, start="2018-12", end="2019-08")}
#' @export

geksl_fbew<-function(data,start,end)  { if (start==end) return (1)
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
                                                       ind<-ind*geksl(data,substr(start,0,7),substr(new,0,7), window=dist(start, new)+1)
                                                       lubridate::year(start)<-lubridate::year(start)+1
                                                      }
                                   return (ind)
                                  
                                   }

#' @title  Extending the multilateral weighted GEKS-L price index by using the FBEW method.
#'
#' @description This function returns a value of the multilateral weighted GEKS-L price index extended by using the FBEW (Fixed Base Monthly Expanding Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname wgeksl_fbew
#' @return This function returns a value of the multilateral weighted GEKS-L price index (the weighted GEKS index based on the Laspeyres formula) extended by using the FBEW (Fixed Base Monthly Expanding Window) method. The FBEW method uses a time window with a fixed base month every year (December). The  window  is  enlarged  every month  with  one  month in order to include information from a new month. The full window length (13 months) is reached in December of each year. The function measures the price dynamics between periods \code{end} and \code{start}. The month of the \code{start} parameter must be December. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#'
#' @examples 
#' \donttest{wgeksl_fbew(milk, start="2018-12", end="2019-08")}
#' @export

wgeksl_fbew<-function(data,start,end)  { if (start==end) return (1)
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
                                                       ind<-ind*wgeksl(data,substr(start,0,7),substr(new,0,7), window=dist(start, new)+1)
                                                       lubridate::year(start)<-lubridate::year(start)+1
                                                      }
                                   return (ind)
                                  
                                   }

#' @title  Extending the multilateral GEKS-L price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral GEKS-L price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname geksl_fbmw
#' @return This function returns a value of the multilateral GEKS-L price index (the GEKS index based on the Laspeyres formula) extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#'
#' @examples 
#' \donttest{geksl_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

geksl_fbmw<-function(data,start,end)  { if (start==end) return (1)
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
                                                       ind<-ind*geksl_fbmw2(data,substr(start,0,7),substr(new,0,7))
                                                       lubridate::year(start)<-lubridate::year(start)+1
                                                      }
                                   return (ind)
                                   }

#' @title  Extending the multilateral weighted GEKS-L price index by using the FBMW method.
#'
#' @description This function returns a value of the multilateral weighted GEKS-L price index extended by using the FBMW (Fixed Base Moving Window) method.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @rdname wgeksl_fbmw
#' @return This function returns a value of the multilateral weighted GEKS-L price index (the GEKS index based on the Laspeyres formula) extended by using the FBMW (Fixed Base Moving Window) method. It measures the price dynamics between periods \code{end} and \code{start} and it uses a 13-month time window with a fixed base month taken as \code{year(end)-1}. If the distance between \code{end} and \code{start} exceeds 13 months, then internal Decembers play a role of chain-linking months. The month of the \code{start} parameter must be December. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lamboray, C.(2017). \emph{The Geary Khamis index and the Lehr index: how much do they differ?} Paper presented at the 15th Ottawa Group meeting, 10-12 May 2017, Elville am Rhein, Germany.}
#'
#' @examples 
#' \donttest{wgeksl_fbmw(milk, start="2019-12", end="2020-04")}
#' @export

wgeksl_fbmw<-function(data,start,end)  { if (start==end) return (1)
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
                                                       ind<-ind*wgeksl_fbmw2(data,substr(start,0,7),substr(new,0,7))
                                                       lubridate::year(start)<-lubridate::year(start)+1
                                                      }
                                   return (ind)
                                   }

#an additional function used in geksl_fbmw
geksl_fbmw2<-function(data,start,end)  { if (start==end) return (1)
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
                                   else return (geksl(data, substr(start,0,7), substr(end,0,7), substr(wstart,0,7),window=13))
                                   }


#an additional function used in wgeksl_fbmw
wgeksl_fbmw2<-function(data,start,end)  { if (start==end) return (1)
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
                                   else return (wgeksl(data, substr(start,0,7), substr(end,0,7), substr(wstart,0,7),window=13))
                                   }



#' @title  Extending the multilateral GEKS-L price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral GEKS-L price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname geksl_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral GEKS-L price index (the GEKS index based on the Laspeyres formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
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
#'
#' @examples 
#' \donttest{geksl_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{geksl_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

geksl_splice<-function (data,start,end, window=13, splice="movement",interval=FALSE)
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
                                       if (start<=wend) set<-c(set,geksl(data,t0,t,wstart=t0,window))
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
                                       if (splice=="movement") set<-c(set, set[length(set)]*geksl(data,t1,t,wstart=tT,window))
                                       if (splice=="window")   set<-c(set, set[length(set)]*geksl(data,tT,t,wstart=tT,window)/geksl(data,tT,t1,wstart=tT1,window))
                                       if (splice=="half")   set<-c(set, set[length(set)]*geksl(data,th,t,wstart=tT,window)/geksl(data,th,t1,wstart=tT1,window))
                                       if (splice=="mean") {var<-1
                                       for (m in 1:(window-1)) {tm<-start
                                       lubridate::month(tm)<-lubridate::month(tm)-m
                                       tm<-substr(tm,0,7)
var<-var*geksl(data,tm,t,wstart=tT,window)/geksl(data,tm,t1,wstart=tT1,window) 
                                                                }
                                       var<-var^(1/(window-1))
                                       set<-c(set, set[length(set)]*var)
                                                                }
                                       if (splice=="window_published")
                                       set<-c(set, set[length(set)+1-(window-1)]*geksl(data,tT,t,wstart=tT,window))
                                       if (splice=="half_published")
                                       set<-c(set, set[length(set)+1-floor(window/2)]*geksl(data,th,t,wstart=tT,window))  
                                       if (splice=="mean_published") {var<-1
                                       for (m in 1:(window-1)) {tm<-start
                                       lubridate::month(tm)<-lubridate::month(tm)-m
                                       tm<-substr(tm,0,7)
var<-var*set[length(set)+1-m]*geksl(data,tm,t,wstart=tT,window) 
                                       }
                                       var<-var^(1/(window-1))
                                       set<-c(set, var)
                                                              }
                                             } 
                                       }
if (interval==FALSE) return (set[length(set)])
else return(set)  
}


#' @title  Extending the multilateral weighted GEKS-L price index by using window splicing methods.
#'
#' @description This function returns a value (or values) of the multilateral weighted GEKS-L price index extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method. Available options are: "movement", "window","half","mean", "window_published","half_published","mean_published".
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base multilateral indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname wgeksl_splice
#' @return This function returns a value or values (depending on \code{interval} parameter) of the multilateral weighted GEKS-L price index (the weighted GEKS index based on the Laspeyres formula) extended by using window splicing methods. Available splicing methods are: movement splice, window splice, half splice, mean splice and their additional variants: window splice on published indices (WISP), half splice on published indices (HASP) and mean splice on published indices (see \code{References}). The time window starts in \code{start} and should consist of at least two months. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_index}}, \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} or the \code{\link{final_index2}} function).   
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
#'
#' @examples 
#' \donttest{wgeksl_splice(milk, start="2018-12", end="2020-02",splice="half")}
#' \donttest{wgeksl_splice(milk, start="2018-12", end="2020-02",window=10,interval=TRUE)}
#' @export

wgeksl_splice<-function (data,start,end, window=13, splice="movement",interval=FALSE)
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
                                       if (start<=wend) set<-c(set,wgeksl(data,t0,t,wstart=t0,window))
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
                                       if (splice=="movement") set<-c(set, set[length(set)]*wgeksl(data,t1,t,wstart=tT,window))
                                       if (splice=="window")   set<-c(set, set[length(set)]*wgeksl(data,tT,t,wstart=tT,window)/wgeksl(data,tT,t1,wstart=tT1,window))
                                       if (splice=="half")   set<-c(set, set[length(set)]*wgeksl(data,th,t,wstart=tT,window)/wgeksl(data,th,t1,wstart=tT1,window))
                                       if (splice=="mean") {var<-1
                                       for (m in 1:(window-1)) {tm<-start
                                       lubridate::month(tm)<-lubridate::month(tm)-m
                                       tm<-substr(tm,0,7)
var<-var*wgeksl(data,tm,t,wstart=tT,window)/wgeksl(data,tm,t1,wstart=tT1,window) 
                                                                }
                                       var<-var^(1/(window-1))
                                       set<-c(set, set[length(set)]*var)
                                                                }
                                       if (splice=="window_published")
                                       set<-c(set, set[length(set)+1-(window-1)]*wgeksl(data,tT,t,wstart=tT,window))
                                       if (splice=="half_published")
                                       set<-c(set, set[length(set)+1-floor(window/2)]*wgeksl(data,th,t,wstart=tT,window))  
                                       if (splice=="mean_published") {var<-1
                                       for (m in 1:(window-1)) {tm<-start
                                       lubridate::month(tm)<-lubridate::month(tm)-m
                                       tm<-substr(tm,0,7)
var<-var*set[length(set)+1-m]*wgeksl(data,tm,t,wstart=tT,window) 
                                       }
                                       var<-var^(1/(window-1))
                                       set<-c(set, var)
                                                              }
                                             } 
                                       }
if (interval==FALSE) return (set[length(set)])
else return(set)  
}

#' @title  The most general package function to compute the price dynamics
#'
#' @description This function returns a value or values of the selected (final) price index taking into consideration aggregation over product subgroups and/or over outlets. Optionally, the function returns a data frame or a figure presenting calculated indices, i.e. the price index for the whole data set and price indices for product subgroups.
#' @param data The user's data frame with subgroups of sold products (see \code{by} parameter). Each data frame must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities} (as positive numeric), \code{prodID} (as numeric or character) and \code{retID} (as numeric or character). An additional column indicated via \code{by} parameter is also needed.
#' @param by The column name indicating grouping variable, i.e. this column is used for creating subgroups of products.
#' @param all A logical value indicating whether the the selected price index is to be calculated only for the whole set of products or also for created subgroups of products (then \code{all} is set to TRUE).
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
#' @param figure A logical value indicating whether the function returns a figure presenting all calculated indices (it works if \code{all} and \code{interval} are set to TRUE)
#' @rdname final_index2
#' @return This function returns a value or values of the selected (final) price index taking into consideration aggregation over product subgroups and/or over outlets (retailer sale points defined in \code{retID} column). Optionally, the function returns a data frame or a figure presenting calculated indices, i.e. the price index for the whole data set and price indices for product subgroups.To be more precise: if both types of aggregation are selected, then for each subgroup of products and for each outlet (point of sale) price indices are calculated separately and then aggregated (according to the aggregation methods indicated) to the form of the final price index. If the \code{interval} parameter is set to TRUE then it returns a data frame (or a figure) with dates and final index values (after optional aggregating). Please note that different index formulas may use different time intervals (or time periods) for calculations and each time, aggregation over outlets is done for the set of retIDs being available during the whole considered time interval. 
#' @examples 
#' \donttest{final_index2(data=coffee, by="description",all=TRUE,start="2018-12",end="2019-12",
#' formula="fisher",interval=TRUE,aggrsets="laspeyres",aggrret="none",figure=FALSE)}
#' \donttest{final_index2(data=coffee, by="retID",all=TRUE,start="2018-12",end="2019-12",
#' formula="fisher",interval=TRUE,aggrsets="none",aggrret="none",figure=TRUE)}
#' @export

final_index2<-function(data=data.frame(), by, all=FALSE, start, end, formula="fisher", window=13, splice="movement", base=start, sigma=0.7, aggrret="tornqvist", aggrsets="tornqvist", interval=FALSE, figure=FALSE)
{ if (nrow(data)==0) stop("A data set is empty!")
  date<-group<-value<-NULL
  names<-colnames(data)
  if (!(by %in% names)) stop ("There is no column specified via 'by' parameter!")
  group<-as.character(unique(data[,by]))
  datasets<-list()
  for (i in 1:length(group)) datasets[[i]]<-dplyr::filter(data, data[,by]==group[i])
  if (all==FALSE) 
  return(final_index(datasets,start,end,formula,window,splice,base,sigma,aggrret,aggrsets,interval))
  else {
  if (interval==FALSE) {
  index<-c()
  for (i in 1:length(group)) index<-c(index,final_index(list(datasets[[i]]),start,end,formula,window,splice,base,sigma,aggrret,aggrsets="none",interval=FALSE))
index<-c(index,final_index(datasets,start,end,formula,window,splice,base,sigma,aggrret,aggrsets,interval=FALSE)) 
group<-c(group,"all groups: ")
result<-data.frame(group, index)
colnames(result)[2]<-formula
return(result)
                       }
else                   {
result<-final_index(datasets,start,end,formula,window,splice,base,sigma,aggrret,aggrsets,interval=TRUE)
for (i in 1:length(group)) {index_subgroup<-final_index(list(datasets[[i]]),start,end,formula,window,splice,base,sigma,aggrret,aggrsets="none",interval=TRUE)
result[,i+2]<-index_subgroup[,2]
                       }  
n<-2+length(group)
colnames(result)[2]<-paste(formula,": all groups")
colnames(result)[3:n]<-group
if (figure==FALSE) return(result)   
else
{#drawing a plot
result$date<-as.Date(paste(result$date,"-01",sep=""))
result<-reshape::melt(result, id.var='date') 
colnames(result)<-c("date","group","value")
ggplot2::ggplot(result, ggplot2::aes(x=date, y=value, col=group)) + ggplot2::geom_point()+ggplot2::geom_line()+ggplot2::labs(x="date",y="price index value")+ggplot2::scale_x_date(date_labels="%Y %m",date_breaks  ="1 month")+ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust = 1))  
}
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
#' outlets<-as.character(unique(milk$retID))
#' sales_groups2(milk,by="retID",start="2019-04",end="2019-04",
#' shares=TRUE,barplot=TRUE,names=outlets)
#' @export

sales_groups2<-function(data=data.frame(),by, start, end, shares=FALSE, barplot=FALSE, names=c())
                                  {
  if (nrow(data)==0) stop("A data set is empty!")
  ns<-colnames(data)
  if (!(by %in% ns)) stop ("There is no column specified via 'by' parameter!")
  group<-as.character(unique(data[,by]))
  datasets<-list()
  for (i in 1:length(group)) datasets[[i]]<-dplyr::filter(data, data[,by]==group[i])
  return (sales_groups(datasets, start, end, shares, barplot, names))
  }






