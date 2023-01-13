#' @title  A general function to compute one or more price indices
#'
#' @description This function returns a value or values of the selected price indices. 
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also essential even if the selected index is an unweighted formula (unit values are calculated). 
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param formula A vector of character strings indicating price index formulas that are to be calculated. To see available options please use the link: \code{\link{PriceIndices}}.
#' @param window A vector of integers. Each element of the vector defines the length of the time window of the corresponding multilateral index.
#' @param splice A vector of character strings. Each element of the vector indicates the splicing method is to be used for the corresponding multilateral index. Available values of vector elements are: "movement", "window","half","mean" and their additional variants: "window_published", "half_published" and "mean_published".
#' @param base The vector of prior periods used in the Young- or Lowe-type price indices or hybrid/geohybrid index. Each element of the vector (as character) must be limited to the year and month, e.g. "2020-01".
#' @param sigma The vector of elasticity of substitution parameters used in the Lloyed-Moulton, AG Mean or GEKS-LM indices (as numeric).
#' @param r The vector of non-zero parameters used in the quadratic mean of order r quantity / price index or in the GEKS-QM index (as numeric).
#' @param interval A logical value indicating whether the function is to provide price indices comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be presented (the fixed base month is defined by \code{start}).
#' @param names A vector of strings indicating names of indices which are to be used in the resulting data frame.
#' @rdname price_indices
#' @return This general function returns a value or values of the selected price indices. If the \code{interval} parameter is set to TRUE, then it returns a data frame where its first column indicates dates and the remaining columns show corresponding values of all selected price indices. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).   
#' @examples 
#' \donttest{price_indices(milk, 
#'          start="2018-12",end="2019-12",
#'          formula=c("geks","ccdi","hybrid","fisher",
#'          "QMp","young","geksl_fbew"),
#'          window=c(13,13),
#'          base=c("2019-03","2019-03"),
#'          r=c(3),interval=TRUE)}
#' \donttest{price_indices(milk, 
#'          start="2018-12",end="2019-12",
#'          formula=c("geks","ccdi","hybrid","fisher",
#'          "QMp","young","geksl_fbew"),
#'          window=c(13,13),
#'          base=c("2019-03","2019-03"),
#'          r=c(3),interval=FALSE)}
#' @export

price_indices <-
  function(data,
  start,
  end,
  formula = c(),
  window = c(),
  splice = c(),
  base = c(),
  sigma = c(),
  r=c(),
  interval = FALSE,
  names=c())
  {
  aformula <-
  c(
  "jevons",
  "dutot",
  "carli",
  "cswd",
  "harmonic",
  "bmw",
  "laspeyres",
  "paasche",
  "fisher",
  "tornqvist",
  "geolaspeyres",
  "geopaasche",
  "drobisch",
  "marshall_edgeworth",
  "walsh",
  "bialek",
  "banajree",
  "davies",
  "stuvel",
  "palgrave",
  "geary_khamis",
  "lehr",
  "vartia",
  "sato_vartia",
  "lloyd_moulton",
  "agmean",
  "young",
  "geoyoung",
  "lowe",
  "geolowe",
  "QMq",
  "QMp",
  "IQMp",
  "chQMq",
  "chQMp",
  "chIQMp",
  "value_index",
  "unit_value_index",
  "chjevons",
  "chdutot",
  "chcarli",
  "chcswd",
  "chharmonic",
  "chlaspeyres",
  "chpaasche",
  "chfisher",
  "chtornqvist",
  "chgeolaspeyres",
  "chgeopaasche",
  "chdrobisch",
  "chmarshall_edgeworth",
  "chwalsh",
  "chbialek",
  "chbanajree",
  "chdavies",
  "chstuvel",
  "chpalgrave",
  "chgeary_khamis",
  "chlehr",
  "chvartia",
  "chsato_vartia",
  "chlloyd_moulton",
  "chagmean",
  "chyoung",
  "chgeoyoung",
  "chlowe",
  "chgeolowe",
  "chbmw",
  "geks",
  "wgeks",
  "geksj",
  "geksw",
  "ccdi",
  "gk",
  "tpd",
  "utpd",
  "geks_splice",
  "wgeks_splice",
  "geksj_splice",
  "geksw_splice",
  "ccdi_splice",
  "gk_splice",
  "tpd_splice",
  "utpd_splice",
  "geks_fbew",
  "geks_fbmw",
  "wgeks_fbew",
  "wgeks_fbmw",
  "geksj_fbew",
  "geksj_fbmw",
  "geksw_fbew",
  "geksw_fbmw",
  "ccdi_fbew",
  "ccdi_fbmw",
  "gk_fbew",
  "gk_fbmw",
  "tpd_fbew",
  "tpd_fbmw",
  "utpd_fbew",
  "utpd_fbmw",
  "hybrid",
  "geohybrid",
  "chhybrid",
  "chgeohybrid",
  "SPQ",
  "geksl",
  "wgeksl",
  "geksl_splice",
  "wgeksl_splice",
  "geksl_fbew",
  "wgeksl_fbew",
  "geksl_fbmw",
  "wgeksl_fbmw",
  "geksgl",
  "wgeksgl",
  "geksgl_splice",
  "wgeksgl_splice",
  "geksgl_fbew",
  "wgeksgl_fbew",
  "geksgl_fbmw",
  "wgeksgl_fbmw",
  "geksaqu",
  "wgeksaqu",
  "geksaqu_splice",
  "wgeksaqu_splice",
  "geksaqu_fbew",
  "wgeksaqu_fbew",
  "geksaqu_fbmw",
  "wgeksaqu_fbmw",
  "geksaqi",
  "wgeksaqi",
  "geksaqi_splice",
  "wgeksaqi_splice",
  "geksaqi_fbew",
  "wgeksaqi_fbew",
  "geksaqi_fbmw",
  "wgeksaqi_fbmw",
  "geksgaqi",
  "wgeksgaqi",
  "geksgaqi_splice",
  "wgeksgaqi_splice",
  "geksgaqi_fbew",
  "wgeksgaqi_fbew",
  "geksgaqi_fbmw",
  "wgeksgaqi_fbmw",
  "geksiqm","geksiqm_splice","geksiqm_fbmw","geksiqm_fbew",
  "geksqm","geksqm_splice","geksqm_fbmw","geksqm_fbew",
  "gekslm","gekslm_splice","gekslm_fbmw","gekslm_fbew" 
  )
  for (index in formula) if (!(index %in% aformula))
  stop ("There is a typo in the index name")
  
  if (length(names)>0) if (!(length(names)==length(formula))) stop ("Parameters 'formula' and 'names' must have the same length!")
  
  #indices for increasing parameters:
  inc_splice<-c("geks_splice","wgeks_splice",
  "geksj_splice","geksw_splice","ccdi_splice",
  "gk_splice","tpd_splice","utpd_splice","geksl_splice","wgeksl_splice",
  "geksgl_splice","wgeksgl_splice","geksaqu_splice",
  "wgeksaqu_splice","geksaqi_splice","wgeksaqi_splice",
  "geksgaqi_splice","wgeksgaqi_splice")
  
  inc_window<-c("geks","wgeks","geksj","geksw","ccdi",
  "gk","tpd","utpd","geksl","wgeksl","geksgl","wgeksgl",
  "geksaqu","wgeksaqu","geksaqi","wgeksaqi",
  "geksgaqi","wgeksgaqi")
  
  inc_base<-c("young","geoyoung","lowe","geolowe","hybrid","geohybrid","chyoung","chgeoyoung","chlowe","chgeolowe","chhybrid","chgeohybrid")
  
  inc_sigma<-c("lloyd_moulton","agmean","chlloyd_moulton","chagmean","gekslm_fbew","gekslm_fbmw")
  
  inc_r<-c("QMq","QMp","IQMp","chQMq","chQMp","chIQMp","geksqm_fbew","geksqm_fbmw","geksiqm_fbew","geksiqm_fbmw")
  
  inc_window_sigma<-c("gekslm")
  
  inc_splice_sigma<-c("gekslm_splice")
  
  inc_window_r<-c("geksqm","geksiqm")
  
  inc_splice_r<-c("geksqm_splice","geksiqm_splice")
  
  considered_indices<-c(inc_splice,inc_window,inc_base,
  inc_sigma,inc_r,inc_window_sigma,inc_splice_sigma,
  inc_window_r,inc_splice_r)  
  
  #testing parameters
  ok_window<-0
  ok_splice<-0
  ok_base<-0
  ok_sigma<-0
  ok_r<-0
  for (index in formula) {
    if (index %in% inc_splice) {
      ok_window<-ok_window+1 
      ok_splice<-ok_splice+1}
    if (index %in% inc_splice_sigma) {
      ok_window<-ok_window+1
      ok_splice<-ok_splice+1
      ok_sigma<-ok_sigma+1}
    if (index %in% inc_splice_r) {
      ok_window<-ok_window+1
      ok_splice<-ok_splice+1
      ok_r<-ok_r+1}
    if (index %in% inc_window_sigma) {
      ok_window<-ok_window+1
      ok_sigma<-ok_sigma+1}
    if (index %in% inc_window_r) {
      ok_window<-ok_window+1
      ok_r<-ok_r+1}
    if (index %in% inc_window) {
      ok_window<-ok_window+1}
    if (index %in% inc_base) {
      ok_base<-ok_base+1}
    if (index %in% inc_sigma) {
      ok_sigma<-ok_sigma+1}
    if (index %in% inc_r) {
      ok_r<-ok_r+1}
    }  
    if (!((length(window)==ok_window) & (length(splice)==ok_splice) & (length(base)==ok_base) & (length(sigma)==ok_sigma) & (length(r)==ok_r))) stop("The number of needed parameters is not correct!") 
#taking the index parameters  
  p_window<-c()
  l_window<-1
  p_splice<-c()
  l_splice<-1
  p_base<-c()
  l_base<-1
  p_sigma<-c()
  l_sigma<-1
  p_r<-c()
  l_r<-1
  for (index in formula) {
    if (!(index %in% considered_indices)) {
      p_window<-c(p_window, 13)
      p_splice<-c(p_splice, "movement")
      p_base<-c(p_base, start)
      p_sigma<-c(p_sigma, 0.7)
      p_r<-c(p_r, 2)}
    if (index %in% inc_splice) {
      p_window<-c(p_window, window[l_window])
      l_window<-l_window+1
      p_splice<-c(p_splice, splice[l_splice])
      l_splice<-l_splice+1
      p_base<-c(p_base, start)
      p_sigma<-c(p_sigma, 0.7)
      p_r<-c(p_r, 2)}
    if (index %in% inc_splice_sigma) {
      p_window<-c(p_window, window[l_window])
      l_window<-l_window+1
      p_splice<-c(p_splice, splice[l_splice])
      l_splice<-l_splice+1
      p_base<-c(p_base, start)
      p_sigma<-c(p_sigma, sigma[l_sigma])
      l_sigma<-l_sigma+1
      p_r<-c(p_r, 2)}
    if (index %in% inc_splice_r) {
      p_window<-c(p_window, window[l_window])
      l_window<-l_window+1
      p_splice<-c(p_splice, splice[l_splice])
      l_splice<-l_splice+1
      p_base<-c(p_base, start)
      p_sigma<-c(p_sigma, 0.7)
      p_r<-c(p_r, r[l_r])
      l_r<-l_r+1}
    if (index %in% inc_window_sigma) {
      p_window<-c(p_window, window[l_window])
      l_window<-l_window+1
      p_splice<-c(p_splice, "movement")
      p_base<-c(p_base, start)
      p_sigma<-c(p_sigma, sigma[l_sigma])
      l_sigma<-l_sigma+1
      p_r<-c(p_r, 2)}
    if (index %in% inc_window_r) {
      p_window<-c(p_window, window[l_window])
      l_window<-l_window+1
      p_splice<-c(p_splice, "movement")
      p_base<-c(p_base, start)
      p_sigma<-c(p_sigma, 0.7)
      p_r<-c(p_r, r[l_r])
      l_r<-l_r+1}                         
    if (index %in% inc_window) {
      p_window<-c(p_window, window[l_window])
      l_window<-l_window+1
      p_splice<-c(p_splice, "movement")
      p_base<-c(p_base, start)
      p_sigma<-c(p_sigma, 0.7)
      p_r<-c(p_r, 2)}
    if (index %in% inc_base) {
      p_window<-c(p_window, 13)
      p_splice<-c(p_splice, "movement")
      p_base<-c(p_base, base[l_base])
      l_base<-l_base+1
      p_sigma<-c(p_sigma, 0.7)
      p_r<-c(p_r, 2)}
    if (index %in% inc_sigma) {
      p_window<-c(p_window, 13)
      p_splice<-c(p_splice, "movement")
      p_base<-c(p_base, start)
      p_sigma<-c(p_sigma, sigma[l_sigma])
      l_sigma<-l_sigma+1
      p_r<-c(p_r, 2)}
    if (index %in% inc_r) {
      p_window<-c(p_window, 13)
      p_splice<-c(p_splice, "movement")
      p_base<-c(p_base, start)
      p_sigma<-c(p_sigma, 0.7)
      p_r<-c(p_r, r[l_r])
      l_r<-l_r+1}
     }
   #main body  
   results_list<-list()
   if (interval==FALSE) {for (form in 1:length(formula))
     {results_list<-append(results_list, price_index(data, start, end, 
                                     formula[form], 
                                     window=p_window[form],
                                     splice=p_splice[form],
                                     base=p_base[form],
                                     sigma=p_sigma[form],
                                     r=p_r[form],
                                     interval=FALSE))
    } 
   if (length(names)>0) df<-data.frame(price_index=names, value=unlist(results_list))
   else df<-data.frame(price_index=formula, value=unlist(results_list))
   return(df)
  }
  else
  {
  for (form in 1:length(formula))
     {
  dfs<-price_index(data, start, end, 
                                     formula[form], 
                                     window=p_window[form],
                                     splice=p_splice[form],
                                     base=p_base[form],
                                     sigma=p_sigma[form],
                                     r=p_r[form],
                                     interval=TRUE)
  if (length(names)>0) colnames(dfs)[2]<-names[form]   
  results_list<-append(results_list, dfs[2])
  }
  results_list<-dplyr::bind_rows(results_list)
  start<-paste(start, "-01", sep="")
  start<-as.Date(start)
  end<-paste(end, "-01", sep="")
  end<-as.Date(end)
  time <- seq.Date(from = start, to = end, by = "month")
  time <- format(time, format = "%Y-%m")
  df<-cbind(time, results_list)
  df$time<-as.character(df$time)
  return (df)
  }
  } 


#' @title  A general function to compute a final price index
#'
#' @description This function returns a value (or values) of the selected final price index for the selected type of aggregation of partial results. 
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{retID} (as numeric, factor or character) is also essential if the aggregation over outlets is considered. A column with grouping variable (as numeric, factor or character - indicated by the \code{by} parameter) is essential if the aggregation over product subgroups is considered. 
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param formula The character string indicating the price index formula is to be calculated. To see available options please use the link: \code{\link{PriceIndices}}.
#' @param window The length of the time window if the multilateral index is selected (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method (if the multilateral splicing index is selected). Available options are: "movement", "window","half","mean" and their additional variants: "window_published", "half_published" and "mean_published". 
#' @param base The prior period used in the Young- or Lowe-type price indices (as character) limited to the year and month, e.g. "2020-01".
#' @param sigma The elasticity of substitution parameter used in the Lloyed-Moulton, AG Mean or GEKS-LM indices (as numeric).
#' @param r The non-zero parameter used in the quadratic mean of order r quantity / price index or in the GEKS-QM index (as numeric).
#' @param outlets A logical parameter indicating whether the aggregation over outlets (defined by \code{retID} column) should be done.
#' @param groups A logical parameter indicating whether the aggregation over product subgroups (indicated by 'by' parameter) should be done.
#' @param by A character string which indicates a column name for creating product subgroups.
#' @param aggr The formula used for aggregating partial index results (available values are: "arithmetic", "geometric", "laspeyres", "paasche", "fisher", "tornqvist").
#' @param interval A logical value indicating whether the function is to provide price indices comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be presented (the fixed base month is defined by \code{start}).
#' @rdname final_index
#' @return This general function returns a value or values of the selected final price index for the selected type of aggregation of partial results. If the \code{interval} parameter is set to TRUE, then it returns a data frame where its first column indicates dates and the remaining columns show corresponding values of all selected price indices.    
#' @examples 
#' \donttest{final_index(coffee, start = "2018-12", end = "2019-12", 
#'          formula = "fisher", groups = TRUE, outlets = FALSE, 
#'          aggr = "tornqvist", by = "description")}
#' \donttest{final_index(milk, start = "2018-12", end = "2019-12", 
#'          formula = "fisher", groups = TRUE, outlets = TRUE, 
#'          aggr = "laspeyres", by = "description", 
#'          interval = TRUE)}
#' @export

final_index <-
  function(data=data.frame(),
  start=c(),
  end=c(),
  formula=c(),
  window=c(),
  splice=c(),
  base=c(),
  sigma=c(),
  r=c(),
  outlets=FALSE,
  groups=FALSE,
  by=c(),
  aggr = "fisher",
  interval=FALSE
  )
  {
  if (start==end) return (1)
  if (interval==FALSE) 
    return(final_index2(data, start, end, formula, window, splice, base, sigma, r, outlets, groups, by, aggr))   
  if (interval==TRUE) {
  start_<-paste(start, "-01", sep="")
  start_<-as.Date(start_)
  end_<-paste(end, "-01", sep="")
  end_<-as.Date(end_)
  time <- seq.Date(from = start_, to = end_, by = "month")
  time <- format(time, format = "%Y-%m")
  result<-c(1)
  for (i in 2:length(time)) 
    result<-c(result, final_index2(data, start, as.character(time[i]), formula, window, splice, base, sigma, r, outlets, groups, by, aggr))
  df<-data.frame(time, final_index=result)
  df$time<-as.character(df$time)
  return (df)
  }  
  }






