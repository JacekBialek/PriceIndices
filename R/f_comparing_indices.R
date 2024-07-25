#' @title  A function for graphical comparison of price indices
#'
#' @description This function returns a figure with plots of selected price indices. 
#' @param data The user's data frame with price index values. It must contain columns: \code{time} (as character in format: year-month, e.g. '2020-12') and columns with index values. 
#' @param names A vector of strings indicating names of indices which are to be used in the figure's legend.
#' @param date_breaks A string giving the distance between breaks on the X axis like "1 month" (default value) or "4 months".
#' @rdname compare_indices_df
#' @return This function returns a figure with plots of previously calculated indices (together with dates on X-axis and a corresponding legend). Indices must be provided as a data frame, where the the first column must includes dates limited to the year and month (e.g.: "2020-04").   
#' @examples 
#' \donttest{df<-price_indices(milk, start = "2018-12", end = "2019-12", 
#' formula=c("laspeyres", "fisher"), interval = TRUE)}
#' \donttest{compare_indices_df(df)}
#' @export

compare_indices_df <-
  function(data,
  names = colnames(data)[2:length(colnames(data))],
  date_breaks  = "1 month")
  {
  time<-value<-formula<-NULL
  if (nrow(data)==0) stop("Your data frame is empty!")
  if (!(length(names)==ncol(data)-1)) stop("Number of columns and number of names are different!")  
  data$time <- as.Date(paste(data$time, "-01", sep = ""))
  colnames(data)<-c("time", names)
  data <- reshape::melt(data, id.var = 'time')
  colnames(data) <- c("time", "formula", "value")
  ggplot2::ggplot(data, ggplot2::aes(x = time, y = value, col = formula)) + ggplot2::geom_point() +
  ggplot2::geom_line() + ggplot2::labs(x = "date", y = "price index value") +
  ggplot2::scale_x_date(date_labels = "%Y %m", date_breaks  = date_breaks) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

#' @title  A general function for graphical comparison of price indices
#'
#' @description This function returns a figure with plots of previously calculated price indices. 
#' @param data A list of data frames with previously calculated price indices. Each data frame must consist of two columns, i.e. the first column must includes dates limited to the year and month (e.g.: "2020-04") and the second column must indicate price index values for corresponding dates. The above-mentioned single data frame may be created manually in the previous step or it may be a result of functions: \code{price_index} or \code{final_index}. All considered data frames must have an identical number of rows.
#' @param names A vector of character strings describing names of presented indices.
#' @param date_breaks A string giving the distance between breaks on the X axis like "1 month" (default value) or "4 months".
#' @rdname compare_indices_list
#' @return This function returns a figure with plots of previously calculated price indices. It allows for graphical comparison of price index values which were previously calculated and now are provided as a list of data frames (see \code{data} parameter).
#' @examples 
#' ## Caluclating two indices by using two different package functions:
#' \donttest{index1<-final_index(data=milk, start="2018-12", 
#' end="2019-12",formula="walsh",interval=TRUE)}
#' \donttest{index2<-price_indices(milk,start="2018-12", end="2019-12",
#' formula="geks",window=13,interval=TRUE)}
#' ## Graphical comparison of these two indices 
#' \donttest{compare_indices_list(data=list(index1,index2), 
#' names=c("Walsh index", "GEKS index"))}
#' @export

compare_indices_list<-function(data = list(), names = c(), date_breaks = "1 month")
{
time<-value<-formula<-NULL
if (length(data)==0) stop("at least one final index must be chosen")
for (m in 1:length(data)) {if (nrow(data.frame(data[[m]]))==0) stop("At least one data frame is empty")
}
for (m in 1:length(data)) {if (!(nrow(data.frame(data[[m]]))==nrow(data.frame(data[[1]])))) stop("Data frames must have identical number of rows")  
}
graph<-data.frame(data[1])
if (length(data)>1) for (i in 2:length(data)) graph<-cbind(graph, data.frame(data[i])[2]) 
if (length(names)>0) for (i in 1:length(names)) colnames(graph)[i+1]<-names[i]
graph$time<-as.Date(paste(graph$time,"-01",sep=""))
graph<-reshape::melt(graph, id.var='time') 
colnames(graph)<-c("time","formula","value")
ggplot2::ggplot(graph, ggplot2::aes(x=time, y=value, col=formula)) + ggplot2::geom_point()+ggplot2::geom_line()+ggplot2::labs(x="date",y="price index value")+ggplot2::scale_x_date(date_labels="%Y %m",date_breaks =date_breaks)+ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust = 1)) 
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
#' \donttest{df<-price_indices(milk, 
#' formula=c("jevons","dutot","carli"), 
#' start="2018-12", end="2019-12",interval=TRUE)}
#' #Calculating average distances between indices (in p.p)
#' \donttest{compare_distances(df)}
#' @export

compare_distances<-function (data=data.frame(),measure="MAD", pp=TRUE, first=FALSE, prec=3)
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
  if (first==FALSE) data<-data[2:nrow(data),]
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
#' formula=c("jevons","laspeyres","paasche","walsh"),
#' start="2018-12",end="2019-12",interval=TRUE)}
#' #Calculating the target Fisher price index
#' \donttest{target_index<-fisher(milk,start="2018-12",end="2019-12",interval=TRUE)}
#' #Calculating average distances between considered indices and the Fisher index (in p.p)
#' \donttest{compare_to_target(df,target=target_index)}
#' @export

compare_to_target<-function (data = data.frame(), target, measure = "MAD", pp = TRUE, first = FALSE, prec = 3)
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
  if (first==FALSE) {data<-data[2:nrow(data),]
  target<-target[-1]
  }
  index<-colnames(data)[columns]
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
  for (col in columns) {
    x<-data[,col]
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


#' @title  A general function to compare indices by using the jackknife method
#'
#' @description This function presents a comparison of selected indices obtained by using the jackknife method. 
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also essential even if the selected index is an unweighted formula (unit values are calculated). 
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param by A character string which indicates a column name for creating product subgroups (in the classical jackknife method \code{by} should indicate \code{prodID}). In each, successive repetition, the indicated price indexes are counted on the set of products reduced by the subset determined by the successive element of the column indicated by the \code{by} parameter.
#' @param formula A vector of character strings indicating price index formulas that are to be calculated. To see available options please use the link: \code{\link{PriceIndices}}.
#' @param window A vector of integers. Each element of the vector defines the length of the time window of the corresponding multilateral index.
#' @param splice A vector of character strings. Each element of the vector indicates the splicing method is to be used for the corresponding multilateral index. Available values of vector elements are: "movement", "window","half","mean" and their additional variants: "window_published", "half_published" and "mean_published".
#' @param base The vector of prior periods used in the Young- or Lowe-type price indices or hybrid/geohybrid index. Each element of the vector (as character) must be limited to the year and month, e.g. "2020-01".
#' @param sigma The vector of elasticity of substitution parameters used in the Lloyed-Moulton, AG Mean or GEKS-LM indices (as numeric).
#' @param r The vector of non-zero parameters used in the quadratic mean of order r quantity / price index or in the GEKS-QM index (as numeric).
#' @param names A vector of strings indicating names of indices which are to be used in the resulting data frame.
#' @param title_iterations A character string indicating a title of the created box-plot for iteration index values.
#' @param title_pseudovalues A character string indicating a title of the created box-plot for obtained (jackknife) index pseudovalues.
#' @rdname compare_indices_jk
#' @return This function presents a comparison of selected indices obtained by using the jackknife method. In particular, it returns a list with four elements: \code{iterations}, which is a data frame with basic characteristics of the calculated iteration index values (means, standard deviations, coefficients of variation and results for all sample), \code{pseudovalues}, which is a data frame with basic characteristics of the calculated index pseudovalues obtained in the jackknife procedure (i.e. the jackknife estimators and their standard deviations and coefficients of variation), \code{figure_iterations} which presents a box-plot for the calculated iteration index values, and \code{figure_pseudovalues} which presents a box-plot for the calculated index pseudovalues obtained in the jackknife procedure.
#' @references
#' {Quenouille, M.H. (1956). \emph{Notes on bias in estimation}. Biometrika, 43 (3–4), 353–360}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples 
#' \donttest{milk.<-dplyr::filter(milk, milk$prodID %in% 
#' sample(unique(milk$prodID),4))
#' #creating a list with jackknife results
#' comparison<-compare_indices_jk(milk.,
#' formula=c("jevons","fisher"),
#' start="2018-12",
#' end="2019-12", 
#' names=c("Jevons","Fisher"), 
#' title_iterations="Box-plots for iteration values (milk products)",
#' title_pseudovalues="Box-plots for pseudovalues (milk products)")
#' #displaying results
#' comparison$iterations
#' comparison$pseudovalues
#' comparison$figure_iterations
#' comparison$figure_pseudovalues}
#' @export

compare_indices_jk<-function(data, 
                   start, 
                   end,
                   by="prodID",
                   formula =c(),
                   window=c(),
                   splice=c(),
                   base=c(),
                   sigma=c(),
                   r=c(),
                   names=c(), 
                   title_iterations=c(),
                   title_pseudovalues=c())
{
value<-variable<-NULL
if (length(by)==0) stop("You must indicate a column for grouping (see 'by' parameter)!")
av_col<-colnames(data)
if (!(by %in% av_col)) stop("Bad specification of the 'by' parameter!")
data$help<-data$prodID
colnames(data)[which(colnames(data)==by)]<-"groupID" 
data$prodID<-data$help
data$help<-NULL
n<-length(unique(data$groupID)) #number of calculated iteration indices
#sampling a subset of products
reduce<-function(x) 
{
  subset<-dplyr::filter(data, !(data$groupID==x))
  return (subset)
}
list_group<-lapply(unique(data$groupID),reduce) 
price_index_help<-function (set)
return (price_indices(data=set, 
                   start=start, 
                   end=end,
                   formula=formula,
                   window=window,
                   splice=splice,
                   base=base,
                   sigma=sigma,
                   r=r,
                   names=names,
                   interval=FALSE))
list_indices<-lapply(list_group,price_index_help)
list_joined<-dplyr::bind_rows(list_indices)
#jack-knife estimates
indices<-price_indices(data=data, 
                   start=start, 
                   end=end,
                   formula=formula,
                   window=window,
                   splice=splice,
                   base=base,
                   sigma=sigma,
                   r=r,
                   names=names,
                   interval=FALSE)
indices_names<-unique(indices$price_index)
global_index<-c()
index_values<-c()
index_values2<-c()
df_index<-data.frame(matrix(ncol = length(indices_names), nrow = n))
df_index2<-data.frame(matrix(ncol = length(indices_names), nrow = n))
colnames(df_index)<-indices_names
colnames(df_index2)<-indices_names
for (index in indices_names)
{ #global value
  teta<-NULL
  index_values<-NULL
  teta<-dplyr::filter(indices, indices$price_index==index)$value
  index_values<-dplyr::filter(list_joined,  list_joined$price_index==index)$value
  index_values2<-n*teta-(n-1)*index_values
  df_index[,index]<-index_values  #iteration values
  df_index2[,index]<-index_values2 #pseudovalues
  global_index<-c(global_index, teta)
  }
#data frame with iteration index results
df_index<-reshape::melt(data=df_index,value.name = value)
df<-dplyr::summarise(dplyr::group_by(df_index, variable), 
                      mean_iterations=mean(value), 
                      sd_iterations=stats::sd(value),
                      cv_iterations=stats::sd(value)/mean(value))
#data frame with pseudovalues index results
df_index2<-reshape::melt(data=df_index2,value.name = value)
df2<-dplyr::summarise(dplyr::group_by(df_index2, variable), 
                      jk_estimator=mean(value), 
                      sd_jk_estimator=stats::sd(value),
                      cv_jk=stats::sd(value)/mean(value))
#box-plot for iteration values
figure<-ggplot2::ggplot(data=df_index, 
                        mapping=ggplot2::aes(x=variable, y=value))+ggplot2::geom_boxplot()+ggplot2::xlab("price index")+ggplot2::ylab("price index value")
if (length(title_iterations)>0) figure<-figure+ggplot2::labs(title=title_iterations)
#box-plot for pseudovalues
figure2<-ggplot2::ggplot(data=df_index2, 
                        mapping=ggplot2::aes(x=variable, y=value))+ggplot2::geom_boxplot()+ggplot2::xlab("price index")+ggplot2::ylab("price index value")
if (length(title_pseudovalues)>0) figure2<-figure2+ggplot2::labs(title=title_pseudovalues)
df$all_sample<-global_index
return (list(iterations=df,
             pseudovalues=df2, 
             figure_iterations=figure, 
             figure_pseudovalues=figure2))
}

