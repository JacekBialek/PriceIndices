#' @title  A function for graphical comparison of price indices
#'
#' @description This function returns a figure with plots of selected price indices. 
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} is also essential (as positive numeric) because unit values are calculated. 
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
#' @param splice A vector of character strings. Each element of the vector indicates the splicing method is to be used for the corresponding multilateral index (if it is selected by \code{splicemulti} ). Available values of vector elements are: "movement", "window","half","mean","window_published","half_published","mean_published".
#' @param namebilateral A vector of character strings describing names of bilateral price indices that are to be displayed. If this vector is empty, then default names are used.
#' @param namebindex A vector of character strings describing names of Young- and/or Lowe-type price indices are to be displayed. If this vector is empty then default names are used.
#' @param namecesindex A vector of character strings describing names of CES price indices that are to be displayed. If this vector is empty, then default names are used.
#' @param namesimindex A vector of character strings describing names of multilateral price index formulas based on relative price and quantity similarity that are to be displayed. If this vector is empty, then default names are used.
#' @param namefbmulti A vector of character strings describing names of full-window multilateralindices or their FBEW and FBMW extensions that are to be displayed. If this vector is empty, then default names are used.
#' @param namesplicemulti  A vector of character strings describing names of multilateral splice indices that are to be displayed. If this vector is empty, then default names are used.
#' @param date_breaks A string giving the distance between breaks on the X axis like "1 month" (default value) or "4 months".
#' @rdname compare_indices
#' @return This function calculates selected bilateral or/and multilateral price indices and returns a figure with plots of these indices (together with dates on X-axis and a corresponding legend). The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use functions: \code{\link{final_index}} and \code{\link{compare_final_indices}}).   
#' @examples 
#' \donttest{compare_indices(milk, start="2018-12", end="2019-04",
#' bilateral=c("jevons"),fbmulti=c("tpd"),fbwindow=c(6))}
#' \donttest{compare_indices(milk, start="2018-12", end="2019-05",
#' fbmulti=c("tpd","geks"),fbwindow=c(10,12))}
#' @export

compare_indices <-
  function(data,
  start,
  end,
  bilateral = c(),
  bindex = c(),
  base = c(),
  cesindex = c(),
  sigma = c(),
  simindex = c(),
  fbmulti = c(),
  fbwindow = c(),
  splicemulti = c(),
  splicewindow = c(),
  splice = c(),
  namebilateral = bilateral,
  namebindex = bindex,
  namecesindex = cesindex,
  namesimindex = simindex,
  namefbmulti = fbmulti,
  namesplicemulti = splicemulti,
  date_breaks  = "1 month")
  {
  date <- value <- formula <- NULL
  if (nrow(data) == 0)
  stop("A data frame is empty")
  if (length(bilateral) + length(bindex) + length(cesindex) + length(fbmulti) +
  length(splicemulti) + length(simindex) == 0)
  stop("at least one price index formula must be chosen")
  #main body
  graph <-
  price_indices(
  data,
  start,
  end,
  bilateral,
  bindex,
  base,
  cesindex,
  sigma,
  simindex,
  fbmulti,
  fbwindow,
  splicemulti,
  splicewindow,
  splice,
  namebilateral,
  namebindex,
  namecesindex,
  namesimindex,
  namefbmulti,
  namesplicemulti,
  interval = TRUE
  )
  graph$date <- as.Date(paste(graph$date, "-01", sep = ""))
  graph <- reshape::melt(graph, id.var = 'date')
  colnames(graph) <- c("date", "formula", "value")
  ggplot2::ggplot(graph, ggplot2::aes(x = date, y = value, col = formula)) + ggplot2::geom_point() +
  ggplot2::geom_line() + ggplot2::labs(x = "date", y = "price index value") +
  ggplot2::scale_x_date(date_labels = "%Y %m", date_breaks  = date_breaks) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

#' @title  A general function for graphical comparison of price indices
#'
#' @description This function returns a figure with plots of previously calculated price indices. 
#' @param finalindices A list of data frames with previously calculated price indices. Each data frame must consist of two columns, i.e. the first column must includes dates limited to the year and month (e.g.: "2020-04") and the second column must indicate price index values for corresponding dates. The above-mentioned single data frame may be created manually in the previous step or it may be a result of functions: \code{price_index} or \code{final_index}. All considered data frames must have an identical number of rows.
#' @param names A vector of character strings describing names of presented indices.
#' @param date_breaks A string giving the distance between breaks on the X axis like "1 month" (default value) or "4 months".
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

compare_final_indices<-function(finalindices = list(), names = c(), date_breaks = "1 month")
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
ggplot2::ggplot(graph, ggplot2::aes(x=date, y=value, col=formula)) + ggplot2::geom_point()+ggplot2::geom_line()+ggplot2::labs(x="date",y="price index value")+ggplot2::scale_x_date(date_labels="%Y %m",date_breaks  =date_breaks)+ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust = 1)) 
}


#' @title  Calculating distances between price indices
#'
#' @description The function calculates distances between price indices
#' @param data A data frame containg values of indices which are to be compared
#' @param measure A parameter specifying what measure should be used to compare the indexes. Possible parameter values are: "MAD" (Mean Absolute Distance) or "RMSD" (Root Mean Square Distance).
#' @param pp Logical parameter indicating whether the results are to be presented in percentage points (then \code{pp} = TRUE).
#' @param first A logical parameter that determines whether the first row of the data frame is to be taken into account when calculating the distance between the indices (then \code{first} = TRUE). Usually, the first row concerns the index values for the base period - all indexes are then set to one.
#' @param prec Parameter that determines how many decimal places are to be used in the presentation of results.
#' @rdname compare_distances
#' @return The function calculates average distances between price indices and it returns a data frame with these values for each pair of price indices.
#' @examples 
#' #Creating a data frame with unweighted bilateral index values
#' df<-price_indices(milk, 
#' bilateral=c("jevons","dutot","carli"),
#' start="2018-12",end="2019-12",interval=TRUE)
#' #Calculating average distances between indices (in p.p)
#' compare_distances(df)
#' @export

compare_distances<-function (data=data.frame(),measure="MAD", pp=TRUE, first=TRUE, prec=3)
{
  if (!(is.data.frame(data))) stop("The parameter 'data' must indicate a data frame.")
  #checking values of parameters
  good_measure<-c("MAD","RMSD") #Mean absolute distance vs Root mean square distance
  if (!(measure %in% good_measure)) stop ("The 'measure' parameter takes values: 'MAD' or 'MSD'")
  if (prec<=0) stop("The parameter 'prec' takes natural values and it must equal at least one!")
  columns<-c()
  for (i in 1:ncol(data)) if (is.numeric(data[,i])) columns<-c(columns,i)
  if (length(columns)<=1) stop("Data frame must contain at least two numerical columns")
  data<-data[,columns] #taking only numeric columns
  if (first==TRUE) data<-data[-1,]
  #distances of two indices
  MAD<-function (v1, v2) return (mean(abs(v1-v2)))
  MSD<-function (v1, v2) return (sqrt(mean((v1-v2)^2)))
  #data manipulation
  data2<-data
  data2<-data2[1:ncol(data),]
  rownames(data2)<-colnames(data)
  #diagonal operations
  for (i in 1:ncol(data2)) data2[i,i]<-0
  #the rest of a matrix
  pairs<-utils::combn(colnames(data),2)
  #main body
  for (i in 1:ncol(pairs)) {
    x<-data[,pairs[1,i]]
    y<-data[,pairs[2,i]]
    if (measure=="MAD") distance<-MAD(x,y)
    else distance<-MSD(x,y)
    if (pp==TRUE) distance<-distance*100
    distance<-round(distance, digits=prec)
    data2[pairs[1,i],pairs[2,i]]<-distance
    data2[pairs[2,i],pairs[1,i]]<-distance
  }
  return (data2)
}


#' @title  Calculating distances between considered price indices and the target price index
#'
#' @description The function calculates distances between considered price indices and the target price index
#' @param data A data frame containg values of indices which are to be compared to the target price index
#' @param target A data frame or a vector containg values of the target price index
#' @param measure A parameter specifying what measure should be used to compare indices. Possible parameter values are: "MAD" (Mean Absolute Distance) or "RMSD" (Root Mean Square Distance).
#' @param pp Logical parameter indicating whether the results are to be presented in percentage points (then \code{pp} = TRUE).
#' @param first A logical parameter that determines whether the first row of the data frame and the first row of the 'target' data frame (or its first element if it is a vector) are to be taken into account when calculating the distance between the indices (then \code{first} = TRUE). Usually, the first row concerns the index values for the base period - all indexes are then set to one.
#' @param prec Parameter that determines how many decimal places are to be used in the presentation of results.
#' @rdname compare_to_target
#' @return The function calculates average distances between considered price indices and the target price index and it returns a data frame with: average distances on the basis of all values of compared indices ('distance' column), average semi-distances on the basis of values of compared indices which overestimate the target index values ('distance_upper' column) and average semi-distances on the basis of values of compared indices which underestimate the target index values ('distance_lower' column).
#' @examples 
#' #Creating a data frame with example bilateral indices
#' \donttest{df<-price_indices(milk, 
#' bilateral=c("jevons","laspeyres","paasche","walsh"),
#' start="2018-12",end="2019-12",interval=TRUE)}
#' #Calculating the target Fisher price index
#' \donttest{target_index<-fisher(milk,start="2018-12",end="2019-12",interval=TRUE)}
#' #Calculating average distances between considered indices and the Fisher index (in p.p)
#' \donttest{compare_to_target(df,target=target_index)}
#' @export

compare_to_target<-function (data = data.frame(), target, measure = "MAD", pp = TRUE, first = TRUE, prec = 3)
{
  if (!(is.data.frame(data))) stop("The parameter 'data' must indicate a data frame.")
  if (!((is.data.frame(target)) | (is.vector(target)))) stop("The parameter 'target' must indicate a data frame or a vector") 
  if (is.data.frame(target)) 
  { if (ncol(target)>2) stop("Data frame 'target' should contain only two columns: dates and index values!")
    if (is.numeric(target[,1])) target<-target[,1]
    else if (is.numeric(target[,2])) target<-target[,2]
    else stop("Data frame 'target' should contain one numeric column with target index values!")
  }
  if (!(length(target)==nrow(data))) stop("Objects 'data' and 'target' must have the same number of cases!")
  #checking values of parameters
  good_measure<-c("MAD","RMSD") #Mean absolute distance vs Root mean square distance
  if (!(measure %in% good_measure)) stop ("The 'measure' parameter takes values: 'MAD' or 'MSD'")
  if (prec<=0) stop("The parameter 'prec' takes natural values and it must equal at least one!")
  columns<-c()
  for (i in 1:ncol(data)) if (is.numeric(data[,i])) columns<-c(columns,i)
  if (length(columns)==0) stop("Data frame must contain at least one numerical column")
  data<-data[,columns] #taking only numeric columns
  if (first==TRUE) {data<-data[-1,]
  target<-target[-1]
  }
  index<-colnames(data)
  #distances of two indices
  MAD<-function (v1, v2, type="all") {diff<-v1-v2
                               if (type=="upper") diff[which(diff<0)]<-0
                               if (type=="lower") diff[which(diff>0)]<-0
                               return (mean(abs(diff)))
  }
  MSD<-function (v1, v2, type="all") {diff<-v1-v2
                               if (type=="upper") diff[which(diff<0)]<-0
                               if (type=="lower") diff[which(diff>0)]<-0
                               return (sqrt(mean((diff)^2)))
  }
  distance<-c()
  distance_upper<-c()
  distance_lower<-c()
  for (i in 1:ncol(data)) {
    x<-data[,i]
    #distance
    if (measure=="MAD") dist<-MAD(x,target,type="all")
    else dist<-MSD(x,target,type="all")
    if (pp==TRUE) dist<-dist*100
    dist<-round(dist, digits=prec)
    distance<-c(distance,dist)
    #distance_upper
    if (measure=="MAD") dist2<-MAD(x,target,type="upper")
    else dist2<-MSD(x,target,type="upper")
    if (pp==TRUE) dist2<-dist2*100
    dist2<-round(dist2, digits=prec)
    distance_upper<-c(distance_upper,dist2)
    #distance_lower
    if (measure=="MAD") dist3<-MAD(x,target,type="lower")
    else dist3<-MSD(x,target,type="lower")
    if (pp==TRUE) dist3<-dist3*100
    dist3<-round(dist3, digits=prec)
    distance_lower<-c(distance_lower,dist3)
  }
  result<-data.frame(index=index, distance=distance,distance_lower=distance_lower,distance_upper=distance_upper)
  return (result)
}





