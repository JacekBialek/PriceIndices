#' @title  A general function to compute a price index
#'
#' @description This function returns a value or values of the selected price index. 
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also essential even if the selected index is an unweighted formula (unit values are calculated). 
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

price_index <-
  function(data,
  start,
  end,
  formula = "fisher",
  window = 13,
  splice = "movement",
  base = start,
  sigma = 0.7,
  interval = FALSE)
  {
  asplice <-
  c("movement", "window", "half", "mean","window_published", "half_published","mean_published") #allowed values for 'splice' parameter
  if (!(splice %in% asplice))
  stop ("The 'splice' parameter has a wrong value")
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
  "geks_splice",
  "wgeks_splice",
  "geksj_splice",
  "geksw_splice",
  "ccdi_splice",
  "gk_splice",
  "tpd_splice",
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
  "wgeksgaqi_fbmw"
  )
  if (!(formula %in% aformula))
  stop ("There is a typo in the index name")
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  if (sigma == 1)
  stop("A specification of the parameter 'sigma' is wrong")
  if (interval == FALSE) {
  #unweighted formulas
  if (formula == "jevons")
  set <- jevons(data, start, end)
  if (formula == "dutot")
  set <- dutot(data, start, end)
  if (formula == "carli")
  set <- carli(data, start, end)
  if (formula == "cswd")
  set <- cswd(data, start, end)
  if (formula == "harmonic")
  set <- harmonic(data, start, end)
  if (formula == "bmw")
  set <- bmw(data, start, end)
  #weighted formulas
  if (formula == "laspeyres")
  set <- laspeyres(data, start, end)
  if (formula == "paasche")
  set <- paasche(data, start, end)
  if (formula == "fisher")
  set <- fisher(data, start, end)
  if (formula == "tornqvist")
  set <- tornqvist(data, start, end)
  if (formula == "geolaspeyres")
  set <- geolaspeyres(data, start, end)
  if (formula == "geopaasche")
  set <- geopaasche(data, start, end)
  if (formula == "drobisch")
  set <- drobisch(data, start, end)
  if (formula == "marshall_edgeworth")
  set <- marshall_edgeworth(data, start, end)
  if (formula == "walsh")
  set <- walsh(data, start, end)
  if (formula == "bialek")
  set <- bialek(data, start, end)
  if (formula == "banajree")
  set <- banajree(data, start, end)
  if (formula == "davies")
  set <- davies(data, start, end)
  if (formula == "stuvel")
  set <- stuvel(data, start, end)
  if (formula == "palgrave")
  set <- palgrave(data, start, end)
  if (formula == "geary_khamis")
  set <- geary_khamis(data, start, end)
  if (formula == "lehr")
  set <- lehr(data, start, end)
  if (formula == "vartia")
  set <- vartia(data, start, end)
  if (formula == "sato_vartia")
  set <- sato_vartia(data, start, end)
  if (formula == "lloyd_moulton")
  set <- lloyd_moulton(data, start, end, sigma)
  if (formula == "agmean")
  set <- agmean(data, start, end, sigma)
  if (formula == "young")
  set <- young(data, start, end, base)
  if (formula == "geoyoung")
  set <- geoyoung(data, start, end, base)
  if (formula == "lowe")
  set <- lowe(data, start, end, base)
  if (formula == "geolowe")
  set <- geolowe(data, start, end, base)
  if (formula == "hybrid")
  set <- hybrid(data, start, end, base)
  if (formula == "geohybrid")
  set <- geohybrid(data, start, end, base)
  #chain indices
  if (formula == "chjevons")
  set <- chjevons(data, start, end)
  if (formula == "chdutot")
  set <- chdutot(data, start, end)
  if (formula == "chcarli")
  set <- chcarli(data, start, end)
  if (formula == "chcswd")
  set <- chcswd(data, start, end)
  if (formula == "chbmw")
  set <- chbmw(data, start, end)
  if (formula == "chharmonic")
  set <- chharmonic(data, start, end)
  if (formula == "chlaspeyres")
  set <- chlaspeyres(data, start, end)
  if (formula == "chpaasche")
  set <- chpaasche(data, start, end)
  if (formula == "chfisher")
  set <- chfisher(data, start, end)
  if (formula == "chtornqvist")
  set <- chtornqvist(data, start, end)
  if (formula == "chgeolaspeyres")
  set <- chgeolaspeyres(data, start, end)
  if (formula == "chgeopaasche")
  set <- chgeopaasche(data, start, end)
  if (formula == "chdrobisch")
  set <- chdrobisch(data, start, end)
  if (formula == "chmarshall_edgeworth")
  set <- chmarshall_edgeworth(data, start, end)
  if (formula == "chwalsh")
  set <- chwalsh(data, start, end)
  if (formula == "chbialek")
  set <- chbialek(data, start, end)
  if (formula == "chbanajree")
  set <- chbanajree(data, start, end)
  if (formula == "chdavies")
  set <- chdavies(data, start, end)
  if (formula == "chstuvel")
  set <- chstuvel(data, start, end)
  if (formula == "chpalgrave")
  set <- chpalgrave(data, start, end)
  if (formula == "chgeary_khamis")
  set <- chgeary_khamis(data, start, end)
  if (formula == "chlehr")
  set <- chlehr(data, start, end)
  if (formula == "chvartia")
  set <- chvartia(data, start, end)
  if (formula == "chsato_vartia")
  set <- chsato_vartia(data, start, end)
  if (formula == "chlloyd_moulton")
  set <- chlloyd_moulton(data, start, end, sigma)
  if (formula == "chagmean")
  set <- chagmean(data, start, end, sigma)
  if (formula == "chyoung")
  set <- chyoung(data, start, end, base)
  if (formula == "chgeoyoung")
  set <- chgeoyoung(data, start, end, base)
  if (formula == "chlowe")
  set <- chlowe(data, start, end, base)
  if (formula == "chgeolowe")
  set <- chgeolowe(data, start, end, base)
  if (formula == "chhybrid")
  set <- chhybrid(data, start, end, base)
  if (formula == "chgeohybrid")
  set <- chgeohybrid(data, start, end, base)
  #multilateral indices
  if (formula == "geks")
  set <- geks(data, start, end, start, window)
  if (formula == "wgeks")
  set <- wgeks(data, start, end, start, window)
  if (formula == "geksj")
  set <- geksj(data, start, end, start, window)
  if (formula == "geksw")
  set <- geksw(data, start, end, start, window)
  if (formula == "ccdi")
  set <- ccdi(data, start, end, start, window)
  if (formula == "gk")
  set <- gk(data, start, end, start, window)
  if (formula == "tpd")
  set <- tpd(data, start, end, start, window)
  if (formula == "SPQ")
  set <- SPQ(data, start, end, interval)
  if (formula == "geksl")
  set <- geksl(data, start, end, start, window)
  if (formula == "wgeksl")
  set <- wgeksl(data, start, end, start, window)
  if (formula == "geksgl")
  set <- geksgl(data, start, end, start, window)
  if (formula == "wgeksgl")
  set <- wgeksgl(data, start, end, start, window)
  if (formula == "geksaqu")
  set <- geksaqu(data, start, end, start, window)
  if (formula == "wgeksaqu")
  set <- wgeksaqu(data, start, end, start, window)
  if (formula == "geksaqi")
  set <- geksaqi(data, start, end, start, window)
  if (formula == "wgeksaqi")
  set <- wgeksaqi(data, start, end, start, window)
  if (formula == "geksgaqi")
  set <- geksaqi(data, start, end, start, window)
  if (formula == "wgeksgaqi")
  set <- wgeksaqi(data, start, end, start, window)
  #extended multilateral indices
  if (formula == "geks_splice")
  set <- geks_splice(data, start, end, window, splice, interval)
  if (formula == "wgeks_splice")
  set <- wgeks_splice(data, start, end, window, splice, interval)
  if (formula == "geksl_splice")
  set <- geksl_splice(data, start, end, window, splice, interval)
  if (formula == "wgeksl_splice")
  set <- wgeksl_splice(data, start, end, window, splice, interval)
  if (formula == "geksgl_splice")
  set <- geksgl_splice(data, start, end, window, splice, interval)
  if (formula == "wgeksgl_splice")
  set <- wgeksgl_splice(data, start, end, window, splice, interval)
  if (formula == "geksaqu_splice")
  set <- geksaqu_splice(data, start, end, window, splice, interval)
  if (formula == "wgeksaqu_splice")
  set <- wgeksaqu_splice(data, start, end, window, splice, interval)
  if (formula == "geksaqi_splice")
  set <- geksaqi_splice(data, start, end, window, splice, interval)
  if (formula == "wgeksaqi_splice")
  set <- wgeksaqi_splice(data, start, end, window, splice, interval)
  if (formula == "geksgaqi_splice")
  set <- geksgaqi_splice(data, start, end, window, splice, interval)
  if (formula == "wgeksgaqi_splice")
  set <- wgeksgaqi_splice(data, start, end, window, splice, interval)
  if (formula == "geksj_splice")
  set <- geksj_splice(data, start, end, window, splice, interval)
  if (formula == "geksw_splice")
  set <- geksw_splice(data, start, end, window, splice, interval)
  if (formula == "ccdi_splice")
  set <- ccdi_splice(data, start, end, window, splice, interval)
  if (formula == "gk_splice")
  set <- gk_splice(data, start, end, window, splice, interval)
  if (formula == "tpd_splice")
  set <- tpd_splice(data, start, end, window, splice, interval)
  if (formula == "geks_fbew")
  set <- geks_fbew(data, start, end)
  if (formula == "wgeks_fbew")
  set <- wgeks_fbew(data, start, end)
  if (formula == "geksl_fbew")
  set <- geksl_fbew(data, start, end)
  if (formula == "wgeksl_fbew")
  set <- wgeksl_fbew(data, start, end)
  if (formula == "geksgl_fbew")
  set <- geksgl_fbew(data, start, end)
  if (formula == "wgeksgl_fbew")
  set <- wgeksgl_fbew(data, start, end)
  if (formula == "geksaqu_fbew")
  set <- geksaqu_fbew(data, start, end)
  if (formula == "wgeksaqu_fbew")
  set <- wgeksaqu_fbew(data, start, end)
  if (formula == "geksaqi_fbew")
  set <- geksaqi_fbew(data, start, end)
  if (formula == "wgeksaqi_fbew")
  set <- wgeksaqi_fbew(data, start, end)
  if (formula == "geksgaqi_fbew")
  set <- geksgaqi_fbew(data, start, end)
  if (formula == "wgeksgaqi_fbew")
  set <- wgeksgaqi_fbew(data, start, end)
  if (formula == "geks_fbmw")
  set <- geks_fbmw(data, start, end)
  if (formula == "wgeks_fbmw")
  set <- wgeks_fbmw(data, start, end)
  if (formula == "geksl_fbmw")
  set <- geksl_fbmw(data, start, end)
  if (formula == "wgeksl_fbmw")
  set <- wgeksl_fbmw(data, start, end)
  if (formula == "geksgl_fbmw")
  set <- geksgl_fbmw(data, start, end)
  if (formula == "wgeksgl_fbmw")
  set <- wgeksgl_fbmw(data, start, end)
  if (formula == "geksaqu_fbmw")
  set <- geksaqu_fbmw(data, start, end)
  if (formula == "wgeksaqu_fbmw")
  set <- wgeksaqu_fbmw(data, start, end)
  if (formula == "geksaqi_fbmw")
  set <- geksaqi_fbmw(data, start, end)
  if (formula == "wgeksaqi_fbmw")
  set <- wgeksaqi_fbmw(data, start, end)
  if (formula == "geksgaqi_fbmw")
  set <- geksgaqi_fbmw(data, start, end)
  if (formula == "wgeksgaqi_fbmw")
  set <- wgeksgaqi_fbmw(data, start, end)
  if (formula == "geksj_fbew")
  set <- geksj_fbew(data, start, end)
  if (formula == "geksj_fbmw")
  set <- geksj_fbmw(data, start, end)
  if (formula == "geksw_fbew")
  set <- geksw_fbew(data, start, end)
  if (formula == "geksw_fbmw")
  set <- geksw_fbmw(data, start, end)
  if (formula == "ccdi_fbew")
  set <- ccdi_fbew(data, start, end)
  if (formula == "ccdi_fbmw")
  set <- ccdi_fbmw(data, start, end)
  if (formula == "gk_fbew")
  set <- gk_fbew(data, start, end)
  if (formula == "gk_fbmw")
  set <- gk_fbmw(data, start, end)
  if (formula == "tpd_fbew")
  set <- tpd_fbew(data, start, end)
  if (formula == "tpd_fbmw")
  set <- tpd_fbmw(data, start, end)
  return (set)
  }
  else                 {
  set <- c(1)
  #unweighted formulas
  if (formula == "jevons")
  set <- jevons(data, start, end, interval)
  if (formula == "dutot")
  set <- dutot(data, start, end, interval)
  if (formula == "carli")
  set <- carli(data, start, end, interval)
  if (formula == "cswd")
  set <- cswd(data, start, end, interval)
  if (formula == "harmonic")
  set <- harmonic(data, start, end, interval)
  if (formula == "bmw")
  set <- bmw(data, start, end, interval)
  #weighted formulas
  if (formula == "laspeyres")
  set <- laspeyres(data, start, end, interval)
  if (formula == "paasche")
  set <- paasche(data, start, end, interval)
  if (formula == "fisher")
  set <- fisher(data, start, end, interval)
  if (formula == "tornqvist")
  set <- tornqvist(data, start, end, interval)
  if (formula == "geolaspeyres")
  set <- geolaspeyres(data, start, end, interval)
  if (formula == "geopaasche")
  set <- geopaasche(data, start, end, interval)
  if (formula == "drobisch")
  set <- drobisch(data, start, end, interval)
  if (formula == "marshall_edgeworth")
  set <- marshall_edgeworth(data, start, end, interval)
  if (formula == "walsh")
  set <- walsh(data, start, end, interval)
  if (formula == "bialek")
  set <- bialek(data, start, end, interval)
  if (formula == "banajree")
  set <- banajree(data, start, end, interval)
  if (formula == "davies")
  set <- davies(data, start, end, interval)
  if (formula == "stuvel")
  set <- stuvel(data, start, end, interval)
  if (formula == "palgrave")
  set <- palgrave(data, start, end, interval)
  if (formula == "geary_khamis")
  set <- geary_khamis(data, start, end, interval)
  if (formula == "lehr")
  set <- lehr(data, start, end, interval)
  if (formula == "vartia")
  set <- vartia(data, start, end, interval)
  if (formula == "sato_vartia")
  set <- sato_vartia(data, start, end, interval)
  if (formula == "lloyd-moulton")
  set <- lloyd_moulton(data, start, end, sigma, interval)
  if (formula == "agmean")
  set <- agmean(data, start, end, sigma, interval)
  if (formula == "young")
  set <- young(data, start, end, base, interval)
  if (formula == "geoyoung")
  set <- geoyoung(data, start, end, base, interval)
  if (formula == "lowe")
  set <- lowe(data, start, end, base, interval)
  if (formula == "geolowe")
  set <- geolowe(data, start, end, base, interval)
  if (formula == "hybrid")
  set <- hybrid(data, start, end, base, interval)
  if (formula == "geohybrid")
  set <- geohybrid(data, start, end, base, interval)
  #chain indices
  if (formula == "chjevons")
  set <- chjevons(data, start, end, interval)
  if (formula == "chdutot")
  set <- chdutot(data, start, end, interval)
  if (formula == "chcarli")
  set <- chcarli(data, start, end, interval)
  if (formula == "chcswd")
  set <- chcswd(data, start, end, interval)
  if (formula == "chbmw")
  set <- chbmw(data, start, end, interval)
  if (formula == "chharmonic")
  set <- chharmonic(data, start, end, interval)
  if (formula == "chlaspeyres")
  set <- chlaspeyres(data, start, end, interval)
  if (formula == "chpaasche")
  set <- chpaasche(data, start, end, interval)
  if (formula == "chfisher")
  set <- chfisher(data, start, end, interval)
  if (formula == "chtornqvist")
  set <- chtornqvist(data, start, end, interval)
  if (formula == "chgeolaspeyres")
  set <- chgeolaspeyres(data, start, end, interval)
  if (formula == "chgeopaasche")
  set <- chgeopaasche(data, start, end, interval)
  if (formula == "chdrobisch")
  set <- chdrobisch(data, start, end, interval)
  if (formula == "chmarshall_edgeworth")
  set <- chmarshall_edgeworth(data, start, end, interval)
  if (formula == "chwalsh")
  set <- chwalsh(data, start, end, interval)
  if (formula == "chbialek")
  set <- chbialek(data, start, end, interval)
  if (formula == "chbanajree")
  set <- chbanajree(data, start, end, interval)
  if (formula == "chdavies")
  set <- chdavies(data, start, end, interval)
  if (formula == "chstuvel")
  set <- chstuvel(data, start, end, interval)
  if (formula == "chpalgrave")
  set <- chpalgrave(data, start, end, interval)
  if (formula == "chgeary_khamis")
  set <- chgeary_khamis(data, start, end, interval)
  if (formula == "chlehr")
  set <- chlehr(data, start, end, interval)
  if (formula == "chvartia")
  set <- chvartia(data, start, end, interval)
  if (formula == "chsato_vartia")
  set <- chsato_vartia(data, start, end, interval)
  if (formula == "chlloyd-moulton")
  set <- chlloyd_moulton(data, start, end, sigma, interval)
  if (formula == "chagmean")
  set <- chagmean(data, start, end, sigma, interval)
  if (formula == "chyoung")
  set <- chyoung(data, start, end, base, interval)
  if (formula == "chgeoyoung")
  set <- chgeoyoung(data, start, end, base, interval)
  if (formula == "chlowe")
  set <- chlowe(data, start, end, base, interval)
  if (formula == "chgeolowe")
  set <- chgeolowe(data, start, end, base, interval)
  if (formula == "chhybrid")
  set <- chhybrid(data, start, end, base, interval)
  if (formula == "chgeohybrid")
  set <- chgeohybrid(data, start, end, base, interval)
  #SPQ multilateral
  if (formula == "SPQ")
  set <- SPQ(data, start, end, interval)
  #extended multilateral indices
  if (formula == "geks_splice")
  set <- geks_splice(data, start, end, window, splice, interval)
  if (formula == "wgeks_splice")
  set <- wgeks_splice(data, start, end, window, splice, interval)
  if (formula == "geksl_splice")
  set <- geksl_splice(data, start, end, window, splice, interval)
  if (formula == "wgeksl_splice")
  set <- wgeksl_splice(data, start, end, window, splice, interval)
  if (formula == "geksgl_splice")
  set <- geksgl_splice(data, start, end, window, splice, interval)
  if (formula == "wgeksgl_splice")
  set <- wgeksgl_splice(data, start, end, window, splice, interval)
  if (formula == "geksaqu_splice")
  set <- geksaqu_splice(data, start, end, window, splice, interval)
  if (formula == "wgeksaqu_splice")
  set <- wgeksaqu_splice(data, start, end, window, splice, interval)
  if (formula == "geksaqi_splice")
  set <- geksaqi_splice(data, start, end, window, splice, interval)
  if (formula == "wgeksaqi_splice")
  set <- wgeksaqi_splice(data, start, end, window, splice, interval)
  if (formula == "geksgaqi_splice")
  set <- geksgaqi_splice(data, start, end, window, splice, interval)
  if (formula == "wgeksgaqi_splice")
  set <- wgeksgaqi_splice(data, start, end, window, splice, interval)
  if (formula == "geksj_splice")
  set <- geksj_splice(data, start, end, window, splice, interval)
  if (formula == "geksw_splice")
  set <- geksw_splice(data, start, end, window, splice, interval)
  if (formula == "ccdi_splice")
  set <- ccdi_splice(data, start, end, window, splice, interval)
  if (formula == "gk_splice")
  set <- gk_splice(data, start, end, window, splice, interval)
  if (formula == "tpd_splice")
  set <- tpd_splice(data, start, end, window, splice, interval)
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  t0 <- c(substr(start, 0, 7))
  times <- t0  #dates for writing
  if (formula == "geks") denominator<-geks_denom(data, start, end, start, window)
  if (formula == "wgeks") denominator<-wgeks_denom(data, start, end, start, window)
  if (formula == "geksl") denominator<-geksl_denom(data, start, end, start, window)
  if (formula == "wgeksl") denominator<-wgeksl_denom(data, start, end, start, window)
  if (formula == "geksgl") denominator<-geksgl_denom(data, start, end, start, window)
  if (formula == "wgeksgl") denominator<-wgeksgl_denom(data, start, end, start, window)
  if (formula == "geksaqu") denominator<-geksaqu_denom(data, start, end, start, window)
  if (formula == "wgeksaqu") denominator<-wgeksaqu_denom(data, start, end, start, window)
  if (formula == "geksaqi") denominator<-geksaqi_denom(data, start, end, start, window)
  if (formula == "wgeksaqi") denominator<-wgeksaqi_denom(data, start, end, start, window)
  if (formula == "geksgaqi") denominator<-geksgaqi_denom(data, start, end, start, window)
  if (formula == "wgeksgaqi") denominator<-wgeksgaqi_denom(data, start, end, start, window)
  if (formula == "geksj") denominator<-geksj_denom(data, start, end, start, window)
  if (formula == "geksw") denominator<-geksw_denom(data, start, end, start, window)
  if (formula == "ccdi") denominator<-ccdi_denom(data, start, end, start, window)
  while (start < end)
  {
  start2 <- start
  lubridate::month(start2) <-
  lubridate::month(start2) + 1
  t <- substr(start2, 0, 7)
  #multilateral indices
  if (formula == "geks")
  set <- c(set, geks_num(data, t0, t, t0, window)/denominator)
  if (formula == "wgeks")
  set <- c(set, wgeks_num(data, t0, t, t0, window)/denominator)
  if (formula == "geksl")
  set <- c(set, geksl_num(data, t0, t, t0, window)/denominator)
  if (formula == "wgeksl")
  set <- c(set, wgeksl_num(data, t0, t, t0, window)/denominator)
  if (formula == "geksgl")
  set <- c(set, geksgl_num(data, t0, t, t0, window)/denominator)
  if (formula == "wgeksgl")
  set <- c(set, wgeksgl_num(data, t0, t, t0, window)/denominator)
  if (formula == "geksaqu")
  set <- c(set, denominator*geksaqu_num(data, t0, t, t0, window)/denominator)
  if (formula == "wgeksaqu")
  set <- c(set, wgeksaqu_num(data, t0, t, t0, window)/denominator)
  if (formula == "geksaqi")
  set <- c(set, geksaqi_num(data, t0, t, t0, window)/denominator)
  if (formula == "wgeksaqi")
  set <- c(set, wgeksaqi_num(data, t0, t, t0, window)/denominator)
  if (formula == "geksgaqi")
  set <- c(set, geksgaqi_num(data, t0, t, t0, window)/denominator)
  if (formula == "wgeksgaqi")
  set <- c(set, wgeksgaqi_num(data, t0, t, t0, window)/denominator)
  if (formula == "geksj")
  set <- c(set, geksj_num(data, t0, t, t0, window)/denominator)
  if (formula == "geksw")
  set <- c(set, geksw_num(data, t0, t, t0, window)/denominator)
  if (formula == "ccdi")
  set <- c(set, ccdi_num(data, t0, t, t0, window)/denominator)
  if (formula == "gk")
  set <- c(set, gk(data, t0, t, t0, window))
  if (formula == "tpd")
  set <- c(set, tpd(data, t0, t, t0, window))
  if (formula == "geks_fbew")
  set <- c(set, geks_fbew(data, t0, t))
  if (formula == "wgeks_fbew")
  set <- c(set, wgeks_fbew(data, t0, t))
  if (formula == "geksl_fbew")
  set <- c(set, geksl_fbew(data, t0, t))
  if (formula == "wgeksl_fbew")
  set <- c(set, wgeksl_fbew(data, t0, t))
  if (formula == "geksgl_fbew")
  set <- c(set, geksgl_fbew(data, t0, t))
  if (formula == "wgeksgl_fbew")
  set <- c(set, wgeksgl_fbew(data, t0, t))
  if (formula == "geksaqu_fbew")
  set <- c(set, geksaqu_fbew(data, t0, t))
  if (formula == "wgeksaqu_fbew")
  set <- c(set, wgeksaqu_fbew(data, t0, t))
  if (formula == "geksaqi_fbew")
  set <- c(set, geksaqi_fbew(data, t0, t))
  if (formula == "wgeksaqi_fbew")
  set <- c(set, wgeksaqi_fbew(data, t0, t))
  if (formula == "geksgaqi_fbew")
  set <- c(set, geksgaqi_fbew(data, t0, t))
  if (formula == "wgeksgaqi_fbew")
  set <- c(set, wgeksgaqi_fbew(data, t0, t))
  if (formula == "geks_fbmw")
  set <- c(set, geks_fbmw(data, t0, t))
  if (formula == "wgeks_fbmw")
  set <- c(set, wgeks_fbmw(data, t0, t))
  if (formula == "geksl_fbmw")
  set <- c(set, geksl_fbmw(data, t0, t))
  if (formula == "wgeksl_fbmw")
  set <- c(set, wgeksl_fbmw(data, t0, t))
  if (formula == "geksgl_fbmw")
  set <- c(set, geksgl_fbmw(data, t0, t))
  if (formula == "wgeksgl_fbmw")
  set <- c(set, wgeksgl_fbmw(data, t0, t))
  if (formula == "geksaqu_fbmw")
  set <- c(set, geksaqu_fbmw(data, t0, t))
  if (formula == "wgeksaqu_fbmw")
  set <- c(set, wgeksaqu_fbmw(data, t0, t))
  if (formula == "geksaqi_fbmw")
  set <- c(set, geksaqi_fbmw(data, t0, t))
  if (formula == "wgeksaqi_fbmw")
  set <- c(set, wgeksaqi_fbmw(data, t0, t))
  if (formula == "geksgaqi_fbmw")
  set <- c(set, geksgaqi_fbmw(data, t0, t))
  if (formula == "wgeksgaqi_fbmw")
  set <- c(set, wgeksgaqi_fbmw(data, t0, t))
  if (formula == "geksj_fbew")
  set <- c(set, geksj_fbew(data, t0, t))
  if (formula == "geksj_fbmw")
  set <- c(set, geksj_fbmw(data, t0, t))
  if (formula == "geksw_fbew")
  set <- c(set, geksw_fbew(data, t0, t))
  if (formula == "geksw_fbmw")
  set <- c(set, geksw_fbmw(data, t0, t))
  if (formula == "ccdi_fbew")
  set <- c(set, ccdi_fbew(data, t0, t))
  if (formula == "ccdi_fbmw")
  set <- c(set, ccdi_fbmw(data, t0, t))
  if (formula == "gk_fbew")
  set <- c(set, gk_fbew(data, t0, t))
  if (formula == "gk_fbmw")
  set <- c(set, gk_fbmw(data, t0, t))
  if (formula == "tpd_fbew")
  set <- c(set, tpd_fbew(data, t0, t))
  if (formula == "tpd_fbmw")
  set <- c(set, tpd_fbmw(data, t0, t))
  times <- c(times, substr(start2, 0, 7))
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  datfr <- data.frame(c(times), c(set))
  colnames(datfr) <- c("date", formula)
  return (datfr)
  }
  }

#' @title  A very general function to compute one or more price indices
#'
#' @description This function returns a value or values of the selected price indices. 
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also essential even if the selected index is an unweighted formula (unit values are calculated). 
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

price_indices <-
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
  interval = FALSE)
  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  if (!(length(bindex) == length(base)))
  stop("Length of 'bindex' must be the same as length of 'base'")
  if (!(length(cesindex) == length(sigma)))
  stop("Length of 'cesindex' must be the same as length of 'sigma'")
  if (!(length(fbmulti) == length(fbwindow)))
  stop("Length of 'fbmulti' must be the same as length of 'fbwindow'")
  if (!(length(splicemulti) == length(splicewindow)))
  stop("Length of 'splicemulti' must be the same as length of 'splicewindow'")
  if (!(length(splicemulti) == length(splice)))
  stop("Length of 'splicemulti' must be the same as length of 'splice'")
  if (length(bilateral) + length(bindex) + length(cesindex) + length(fbmulti) +
  length(splicemulti) + length(simindex) == 0)
  stop("at least one price index formula must be chosen")
  if (interval == FALSE) {
  results1 <- NULL
  results2 <- NULL
  results3 <- NULL
  results4 <- NULL
  results5 <- NULL
  results6 <- NULL
  bil <- c()
  b <- c()
  ces <- c()
  full <- c()
  ex <- c()
  sind <- c()
  if (length(bilateral) > 0) {
  for (i in 1:length(bilateral))
  bil <-
  c(bil,
  price_index(
  data,
  start,
  end,
  formula = bilateral[i],
  interval = FALSE
  ))
  results1 <- data.frame(namebilateral, bil)
  colnames(results1) <-
  c("index formula", "value")
  }
  if (length(bindex) > 0) {
  for (i in 1:length(bindex))
  b <-
  c(b,
  price_index(
  data,
  start,
  end,
  formula = bindex[i],
  base = base[i],
  interval = FALSE
  ))
  results2 <- data.frame(namebindex, b)
  colnames(results2) <-
  c("index formula", "value")
  }
  if (length(cesindex) > 0) {
  for (i in 1:length(cesindex))
  ces <-
  c(
  ces,
  price_index(
  data,
  start,
  end,
  formula = cesindex[i],
  sigma = sigma[i],
  interval = FALSE
  )
  )
  results3 <- data.frame(namecesindex, ces)
  colnames(results3) <-
  c("index formula", "value")
  }
  if (length(fbmulti) > 0) {
  for (i in 1:length(fbmulti))
  full <-
  c(
  full,
  price_index(
  data,
  start,
  end,
  formula = fbmulti[i],
  window = fbwindow[i],
  interval = FALSE
  )
  )
  results4 <- data.frame(namefbmulti, full)
  colnames(results4) <-
  c("index formula", "value")
  }
  if (length(splicemulti) > 0) {
  for (i in 1:length(splicemulti))
  ex <-
  c(
  ex,
  price_index(
  data,
  start,
  end,
  formula = splicemulti[i],
  window = splicewindow[i],
  splice = splice[i],
  interval = FALSE
  )
  )
  results5 <- data.frame(namesplicemulti, ex)
  colnames(results5) <-
  c("index formula", "value")
  }
  if (length(simindex) > 0) {
  for (i in 1:length(simindex))
  sind <-
  c(sind,
  price_index(
  data,
  start,
  end,
  formula = simindex[i],
  interval = FALSE
  ))
  results6 <- data.frame(namesimindex, sind)
  colnames(results6) <-
  c("index formula", "value")
  }
  results <-
  rbind(results1, results2, results3, results4, results5, results6)
  colnames(results) <- c("index formula", "value")
  }
  else {
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  st <- start
  end <- as.Date(end)
  dates <- c()
  while (st <= end) {
  dates <- c(dates, substr(st, 0, 7))
  lubridate::month(st) <- lubridate::month(st) + 1
  }
  results <- data.frame(c(dates))
  colnames(results)[1] = "date"
  if (length(bilateral) > 0) {
  for (i in 2:(length(bilateral) + 1)) {
  results$i <-
  price_index(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  formula = bilateral[i - 1],
  interval = TRUE
  )[[2]]
  colnames(results)[i] = namebilateral[i - 1]
  }
  }
  if (length(bindex) > 0) {
  for (i in 1:length(bindex)) {
  k <- length(bilateral) + 1 + i
  results$k <-
  price_index(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  formula = bindex[i],
  base = base[i],
  interval = TRUE
  )[[2]]
  colnames(results)[k] = namebindex[i]
  }
  }
  if (length(cesindex) > 0) {
  for (i in 1:length(cesindex)) {
  k <- length(bilateral) + 1 + length(bindex) + i
  results$k <-
  price_index(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  formula = cesindex[i],
  sigma = sigma[i],
  interval = TRUE
  )[[2]]
  colnames(results)[k] = namecesindex[i]
  }
  }
  if (length(fbmulti) > 0) {
  for (i in 1:length(fbmulti)) {
  k <- length(bilateral) + 1 + length(bindex) + length(cesindex) + i
  results$k <-
  price_index(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  formula = fbmulti[i],
  window = fbwindow[i],
  interval = TRUE
  )[[2]]
  colnames(results)[k] = namefbmulti[i]
  }
  }
  if (length(splicemulti) > 0) {
  for (i in 1:length(splicemulti)) {
  k <-
  length(bilateral) + 1 + length(bindex) + length(cesindex) + length(fbmulti) +
  i
  results$k <-
  price_index(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  formula = splicemulti[i],
  window = splicewindow[i],
  splice = splice[i],
  interval = TRUE
  )[[2]]
  colnames(results)[k] = namesplicemulti[i]
  }
  }
  if (length(simindex) > 0) {
  for (i in 1:length(simindex)) {
  k <-
  length(bilateral) + 1 + length(bindex) + length(cesindex) + length(fbmulti) +
  length(splicemulti) + i
  results$k <-
  price_index(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  formula = simindex[i],
  interval = TRUE
  )[[2]]
  colnames(results)[k] = namesimindex[i]
  }
  }
  }
  return(results)
  }


#' @title  The most general package function to compute the price dynamics
#'
#' @description This function returns a value or values of the selected (final) price index taking into consideration aggregation over product subgroups and/or over outlets. 
#' @param datasets The user's list of data frames with subgroups of sold products. Each data frame must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities} (as positive numeric), \code{prodID} (as numeric, factor or character) and \code{retID} (as numeric, factor or character). 
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

final_index <-
  function(datasets = list(),
  start,
  end,
  formula = "fisher",
  window = 13,
  splice = "movement",
  base = start,
  sigma = 0.7,
  aggrret = "tornqvist",
  aggrsets = "tornqvist",
  interval = FALSE)
  {
  sign<-time1<-time2<-NULL
  aaggrret <-
  c(
  "none",
  "laspeyres",
  "paasche",
  "geolaspeyres",
  "geopaasche",
  "fisher",
  "tornqvist",
  "arithmetic",
  "geometric"
  )
  if (!(aggrret %in% aaggrret))
  stop ("The 'aggrret' parameter has a wrong value")
  aaggrsets <-
  c(
  "none",
  "laspeyres",
  "paasche",
  "geolaspeyres",
  "geopaasche",
  "fisher",
  "tornqvist",
  "arithmetic",
  "geometric"
  )
  if (!(aggrsets %in% aaggrsets))
  stop ("The 'aggrsets' parameter has a wrong value")
  for (m in 1:length(datasets))
  if (nrow(data.frame(datasets[[m]])) == 0)
  stop("At least one data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  if (aggrret == "none")
  for (i in 1:length(datasets))
  datasets[[i]]$retID <- 1
  if (aggrsets == "none") {
  ds <- datasets[[1]][0:0, ]
  for (i in 1:length(datasets))
  ds <- rbind(ds, datasets[[i]])
  datasets <- list(ds)
  }
  if (interval == FALSE) {
  #weights for sets and retailers
  w_start_set <- c()
  w_end_set <- c()
  index_set <- c()
  for (m in 1:length(datasets))  {
  set <- data.frame(datasets[[m]])
  #limiting to matched products depending on the used price index formula
  if ((formula == "jevons") |
  (formula == "dutot") |
  (formula == "carli") |
  (formula == "cswd") |
  (formula == "harmonic") |
  (formula == "bmw") |
  (formula == "laspeyres") |
  (formula == "paasche") |
  (formula == "fisher") |
  (formula == "tornqvist") |
  (formula == "geolaspeyres") |
  (formula == "geopaasche") |
  (formula == "drobisch") |
  (formula == "marshall_edgeworth") |
  (formula == "walsh") |
  (formula == "bialek") |
  (formula == "banajree") |
  (formula == "davies") |
  (formula == "stuvel") |
  (formula == "palgrave") |
  (formula == "geary_khamis") |
  (formula == "lehr") |
  (formula == "vartia") |
  (formula == "sato_vartia") |
  (formula == "lloyd_moulton") |
  (formula == "agmean") |
  (formula == "young") |
  (formula == "geoyoung") |
  (formula == "lowe") |
  (formula == "geolowe") |
  (formula == "hybrid") | (formula == "geohybrid"))
  id <-
  matched(
  set,
  period1 = substr(start, 0, 7),
  period2 = substr(end, 0, 7),
  type = "retID",
  FALSE
  )
  else if ((formula == "geks") |
  (formula == "wgeks") |
  (formula == "geksw") |
  (formula == "geksj") |
  (formula == "ccdi") |
  (formula == "gk") |
  (formula == "tpd") | (formula == "geksl") | (formula == "wgeksl") | (formula == "geksgl") | (formula == "wgeksgl") | (formula == "geksaqu") | (formula == "wgeksaqu") | (formula == "geksaqi") | (formula == "wgeksaqi") | (formula == "geksgaqi") | (formula == "wgeksgaqi")) {
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  id <-
  matched(
  set,
  period1 = substr(start, 0, 7),
  period2 = substr(end, 0, 7),
  type = "retID",
  TRUE
  )
  }
  else if ((formula == "geks_splice") |
  (formula == "wgeks_splice") |
  (formula == "geksw_splice") |
  (formula == "geksj_splice") |
  (formula == "ccdi_splice") |
  (formula == "gk_splice") |
  (formula == "tpd_splice") |
  (formula == "geksl_splice") | (formula == "wgeksl_splice") |
  (formula == "geksgl_splice") | (formula == "wgeksgl_splice") |
  (formula == "geksaqu_splice") | (formula == "wgeksaqu_splice") |
  (formula == "geksaqi_splice") | (formula == "wgeksaqi_splice") |
  (formula == "geksgaqi_splice") | (formula == "wgeksgaqi_splice")) {
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  id <-
  matched(
  set,
  period1 = substr(start, 0, 7),
  period2 = substr(max(wend, end), 0, 7),
  type = "retID",
  TRUE
  )
  }
  else if ((formula == "geks_fbmw") |
  (formula == "wgeks_fbmw") |
  (formula == "geksw_fbmw") |
  (formula == "geksj_fbmw") |
  (formula == "ccdi_fbmw") |
  (formula == "gk_fbmw") |
  (formula == "tpd_wbmw") |
  (formula == "geksl_fbmw") | (formula == "wgeksl_fbmw") |
  (formula == "geksgl_fbmw") | (formula == "wgeksgl_fbmw") |
  (formula == "geksaqu_fbmw") | (formula == "wgeksaqu_fbmw") |
  (formula == "geksaqi_fbmw") | (formula == "wgeksaqi_fbmw") |
  (formula == "geksgaqi_fbmw") | (formula == "wgeksgaqi_fbmw")) {
  wstart <- end
  lubridate::year(wstart) <- lubridate::year(wstart) - 1
  if (lubridate::year(start) == lubridate::year(end))
  id <-
  matched(
  set,
  period1 = substr(wstart, 0, 7),
  period2 = substr(end, 0, 7),
  type = "retID",
  TRUE
  )
  else
  id <-
  matched(
  set,
  period1 = substr(start, 0, 7),
  period2 = substr(end, 0, 7),
  type = "retID",
  TRUE
  )
  }
  
  else
  id <-
  matched(
  set,
  period1 = substr(start, 0, 7),
  period2 = substr(end, 0, 7),
  type = "retID",
  TRUE
  )
  
  retindex <- c()
  w_start_ret <- c()
  w_end_ret <- c()
  ##limiting to those retIDs that have at least one matched product from set
  id_ok <- c()
  idp<-c()
  for (k in 1:length(id)) {
  subset <- dplyr::filter(set, set$retID == id[k])
  sign<-1
  if ((formula == "jevons") |
  (formula == "dutot") |
  (formula == "carli") |
  (formula == "cswd") |
  (formula == "harmonic") |
  (formula == "bmw") |
  (formula == "laspeyres") |
  (formula == "paasche") |
  (formula == "fisher") |
  (formula == "tornqvist") |
  (formula == "geolaspeyres") |
  (formula == "geopaasche") |
  (formula == "drobisch") |
  (formula == "marshall_edgeworth") |
  (formula == "walsh") |
  (formula == "bialek") |
  (formula == "banajree") |
  (formula == "davies") |
  (formula == "stuvel") |
  (formula == "palgrave") |
  (formula == "geary_khamis") |
  (formula == "lehr") |
  (formula == "vartia") |
  (formula == "sato_vartia") |
  (formula == "lloyd_moulton") |
  (formula == "agmean") |
  (formula == "young") |
  (formula == "geoyoung") |
  (formula == "lowe") |
  (formula == "geolowe") |
  (formula == "hybrid") | (formula == "geohybrid"))
  idp <-
  matched(
  subset,
  period1 = substr(start, 0, 7),
  period2 = substr(end, 0, 7),
  type = "prodID",
  FALSE
  )
  else if ((formula == "geks") |
  (formula == "wgeks") |
  (formula == "geksw") |
  (formula == "geksj") |
  (formula == "ccdi") |
  (formula == "gk") |
  (formula == "tpd") | (formula == "geksl") | (formula == "wgeksl") | 
  (formula == "geksgl") | (formula == "wgeksgl") |
  (formula == "geksaqu") | (formula == "wgeksaqu") |
  (formula == "geksaqi") | (formula == "wgeksaqi") |
  (formula == "geksgaqi") | (formula == "wgeksgaqi")) {
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  idp <-
  matched(
  subset,
  period1 = substr(start, 0, 7),
  period2 = substr(end, 0, 7),
  type = "prodID",
  TRUE
  )
  }
  else if ((formula == "geks_splice") |
  (formula == "wgeks_splice") |
  (formula == "geksw_splice") |
  (formula == "geksj_splice") |
  (formula == "ccdi_splice") |
  (formula == "gk_splice") |
  (formula == "tpd_splice") |
  (formula == "geksl_splice") | (formula == "wgeksl_splice") |
  (formula == "geksgl_splice") | (formula == "wgeksgl_splice") |
  (formula == "geksaqu_splice") | (formula == "wgeksaqu_splice") |
  (formula == "geksaqi_splice") | (formula == "wgeksaqi_splice") |
  (formula == "geksgaqi_splice") | (formula == "wgeksgaqi_splice")) {
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  idp <-
  matched(
  subset,
  period1 = substr(start, 0, 7),
  period2 = substr(max(wend, end), 0, 7),
  type = "prodID",
  TRUE
  )
  }
  else if ((formula == "geks_fbmw") |
  (formula == "wgeks_fbmw") |
  (formula == "geksw_fbmw") |
  (formula == "geksj_fbmw") |
  (formula == "ccdi_fbmw") |
  (formula == "gk_fbmw") |
  (formula == "tpd_wbmw") |
  (formula == "geksl_fbmw") | (formula == "wgeksl_fbmw") |
  (formula == "geksgl_fbmw") | (formula == "wgeksgl_fbmw") |
  (formula == "geksaqu_fbmw") | (formula == "wgeksaqu_fbmw") |
  (formula == "geksaqi_fbmw") | (formula == "wgeksaqi_fbmw") |
  (formula == "geksgaqi_fbmw") | (formula == "wgeksgaqi_fbmw")) {
  wstart <- end
  lubridate::year(wstart) <- lubridate::year(wstart) - 1
  if (lubridate::year(start) == lubridate::year(end))
  idp <-
  matched(
  subset,
  period1 = substr(wstart, 0, 7),
  period2 = substr(end, 0, 7),
  type = "prodID",
  TRUE
  )
  else
  idp <-
  matched(
  subset,
  period1 = substr(start, 0, 7),
  period2 = substr(end, 0, 7),
  type = "prodID",
  TRUE
  )
  }
  else
  #chain indices case
  {
  period1 = substr(start, 0, 7)
  period2 = substr(end, 0, 7)
  time1<-start
  time2<-start
  lubridate::month(time2)<-lubridate::month(time2)+1
  while (time2<=end) {
  idp <-
  matched(
  subset,
  period1 = substr(time1, 0, 7),
  period2 = substr(time2, 0, 7),
  type = "prodID",
  FALSE
  )
  if (length(idp)==0) sign<-sign-1
  lubridate::month(time1)<-lubridate::month(time1)+1
  lubridate::month(time2)<-lubridate::month(time2)+1
  }  
  }
  if (length(idp) > 0 & sign > 0)
  id_ok <- c(id_ok, id[k])
  }
  id <- id_ok
  if (length(id) == 0)
  stop("At least one subset does not include matched products")
  ##------------------------------------------------------------------
  for (k in 1:length(id)) {
  subset <- dplyr::filter(set, set$retID == id[k])
  retindex <-
  c(
  retindex,
  price_index(
  subset,
  substr(start, 0, 7),
  substr(end, 0, 7),
  formula,
  window,
  splice,
  base,
  sigma,
  interval = FALSE
  )
  )
  d_start <-
  dplyr::filter(
  subset,
  lubridate::year(subset$time) == lubridate::year(start) &
  lubridate::month(subset$time) == lubridate::month(start)
  )
  d_end <-
  dplyr::filter(
  subset,
  lubridate::year(subset$time) == lubridate::year(end) &
  lubridate::month(subset$time) == lubridate::month(end)
  )
  w_start_ret <- c(w_start_ret, sum(d_start$prices * d_start$quantities))
  w_end_ret <- c(w_end_ret, sum(d_end$prices * d_end$quantities))
  }
  
  #aggregating over outlets
  if (aggrret == "none")
  index_ret <- retindex[1]
  if (aggrret == "laspeyres")
  index_ret <- sum(retindex * w_start_ret) / sum(w_start_ret)
  if (aggrret == "paasche")
  index_ret <- sum(w_end_ret) / sum(w_end_ret / retindex)
  if (aggrret == "geolaspeyres")  {
  w_start <- sum(w_start_ret)
  index_ret <-
  prod(retindex ^ (w_start_ret / w_start))
  }
  if (aggrret == "geopaasche")  {
  w_end <- sum(w_end_ret)
  index_ret <-
  prod(retindex ^ (w_end_ret / w_end))
  }
  if (aggrret == "fisher")     {
  index_retL <- sum(retindex * w_start_ret)
  index_retP <-
  sum(w_end_ret / retindex)
  index_retL <-
  index_retL / sum(w_start_ret)
  index_retP <-
  sum(w_end_ret) / index_retP
  index_ret <-
  (index_retL * index_retP) ^ (1 / 2)
  }
  if (aggrret == "tornqvist")  {
  w_start <- sum(w_start_ret)
  w_end <-
  sum(w_end_ret)
  index_ret <-
  prod(retindex ^ (0.5 * (
  w_start_ret / w_start + w_end_ret / w_end
  )))
  }
  if (aggrret == "arithmetic")
  index_ret <- sum(retindex) / length(id)
  
  if (aggrret == "geometric")  {
  index_ret <- prod(retindex)
  index_ret <-
  index_ret ^ (1 / length(id))
  }
  index_set <-
  c(index_set, index_ret)
  w_start_set <-
  c(w_start_set, sum(w_start_ret))
  w_end_set <-
  c(w_end_set, sum(w_end_ret))
  }
  #aggregating over subsets/subgroups
  if (aggrsets == "none")
  index_final <- index_set[1]
  if (aggrsets == "laspeyres")  {
  index_final <- 0
  for (i in 1:length(datasets))
  index_final <- index_final + index_set[i] * w_start_set[i]
  index_final <-
  index_final / sum(w_start_set)
  }
  if (aggrsets == "paasche")    {
  index_final <- 0
  for (i in 1:length(datasets))
  index_final <- index_final + w_end_set[i] / index_set[i]
  index_final <-
  sum(w_end_set) / index_final
  }
  if (aggrsets == "geolaspeyres")  {
  index_final <- 1
  w_start_s <-
  sum(w_start_set)
  for (i in 1:length(datasets))
  index_final <- index_final * index_set[i] ^ (w_start_set[i] / w_start_s)
  }
  if (aggrsets == "geopaasche")  {
  index_final <- 1
  w_end_s <-
  sum(w_end_set)
  for (i in 1:length(datasets))
  index_final <- index_final * index_set[i] ^ (w_end_set[i] / w_end_s)
  }
  if (aggrsets == "fisher")     {
  index_setL <- 0
  index_setP <-
  0
  for (i in 1:length(datasets))  {
  index_setL <- index_setL + index_set[i] * w_start_set[i]
  index_setP <-
  index_setP + w_end_set[i] / index_set[i]
  }
  index_setL <-
  index_setL / sum(w_start_set)
  index_setP <-
  sum(w_end_set) / index_setP
  index_final <-
  (index_setL * index_setP) ^ (1 / 2)
  }
  if (aggrsets == "tornqvist") {
  index_final <- 1
  w_start_s <-
  sum(w_start_set)
  w_end_s <-
  sum(w_end_set)
  for (i in 1:length(datasets))
  index_final <-
  index_final * index_set[i] ^ ((w_start_set[i] / w_start_s + w_end_set[i] /
  w_end_s) / 2)
  }
  if (aggrsets == "arithmetic") {
  index_final <- 0
  for (i in 1:length(datasets))
  index_final <- index_final + index_set[i]
  index_final <-
  index_final / length(datasets)
  }
  if (aggrsets == "geometric")  {
  index_final <- 1
  for (i in 1:length(datasets))
  index_final <- index_final * index_set[i]
  index_final <-
  index_final ^ (1 / length(datasets))
  }
  return (index_final)
  }
  if (interval == TRUE) {
  results <- c(1)
  idd <- list()
  retindexx <- list(list())
  #creating vector of dates
  times <- c()
  st <- start
  while (st <= end)
  {
  t <- substr(st, 0, 7)
  times <- c(times, t)
  lubridate::month(st) <- lubridate::month(st) + 1
  }
  #limiting to matched products depending on the used price index formula - part 1
  if ((formula == "geks") |
  (formula == "geksw") |
  (formula == "geksj") |
  (formula == "ccdi") |
  (formula == "gk") |
  (formula == "tpd") | (formula == "geksl") | (formula == "wgeksl") | 
  (formula == "geksaqu") | (formula == "wgeksaqu"))
  {
  wend <- start
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  for (m in 1:length(datasets))  {
  set <- data.frame(datasets[[m]])
  idd[[m]] <-
  matched(
  set,
  period1 = substr(start, 0, 7),
  period2 = substr(wend, 0, 7),
  type = "retID",
  TRUE
  )
  }
  }
  if ((formula == "geks_splice") |
  (formula == "geksw_splice") |
  (formula == "geksj_splice") |
  (formula == "ccdi_splice") |
  (formula == "gk_splice") |
  (formula == "tpd_splice") |
  (formula == "geksl_splice") | (formula == "wgeksl_splice") |
  (formula == "geksaqu_splice") | (formula == "wgeksaqu_splice"))
  {
  wend <- start
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  for (m in 1:length(datasets))  {
  set <- data.frame(datasets[[m]])
  idd[[m]] <-
  matched(
  set,
  period1 = substr(start, 0, 7),
  period2 = substr(max(wend, end), 0, 7),
  type = "retID",
  TRUE
  )
  for (k in 1:length(idd[[m]])) {
  subset <- dplyr::filter(set, set$retID == idd[[m]][k])
  pindex <-
  price_index(
  subset,
  substr(start, 0, 7),
  substr(end, 0, 7),
  formula,
  window,
  splice,
  base,
  sigma,
  interval = TRUE
  )
  retindexx[[m]][[k]] <-
  pindex[, 2]
  }
  }
  }
  end2 <- end
  end <- start
  while (end < end2)
  {
  lubridate::month(end) <- lubridate::month(end) + 1
  #start of procedure for final index calculation for time moments start and end
  #weights for sets and retailers
  w_start_set <- c()
  w_end_set <- c()
  index_set <- c()
  for (m in 1:length(datasets))  {
  set <- data.frame(datasets[[m]])
  #limiting to matched products depending on the used price index formula - part 2 (rest of indices)
  if ((formula == "jevons") |
  (formula == "dutot") |
  (formula == "carli") |
  (formula == "cswd") |
  (formula == "harmonic") |
  (formula == "bmw") |
  (formula == "laspeyres") |
  (formula == "paasche") |
  (formula == "fisher") |
  (formula == "tornqvist") |
  (formula == "geolaspeyres") |
  (formula == "geopaasche") |
  (formula == "drobisch") |
  (formula == "marshall_edgeworth") |
  (formula == "walsh") |
  (formula == "bialek") |
  (formula == "banajree") |
  (formula == "davies") |
  (formula == "stuvel") |
  (formula == "palgrave") |
  (formula == "geary_khamis") |
  (formula == "lehr") |
  (formula == "vartia") |
  (formula == "sato_vartia") |
  (formula == "lloyd_moulton") |
  (formula == "agmean") |
  (formula == "young") |
  (formula == "geoyoung") |
  (formula == "lowe") |
  (formula == "geolowe") |
  (formula == "hybrid") | (formula == "geohybrid"))
  id <-
  matched(
  set,
  period1 = substr(start, 0, 7),
  period2 = substr(end, 0, 7),
  type = "retID",
  FALSE
  )
  else if ((formula == "geks") |
  (formula == "wgeks") |
  (formula == "geksw") |
  (formula == "geksj") |
  (formula == "ccdi") |
  (formula == "gk") |
  (formula == "tpd") |
  (formula == "geksl") | (formula == "wgeksl") |
  (formula == "geksgl") | (formula == "wgeksgl") |
  (formula == "geksaqu") | (formula == "wgeksaqu") |
  (formula == "geksaqi") | (formula == "wgeksaqi") |
  (formula == "geksgaqi") | (formula == "wgeksgaqi"))
  id <- idd[[m]]
  else if ((formula == "geks_splice") |
  (formula == "wgeks_splice") |
  (formula == "geksw_splice") |
  (formula == "geksj_splice") |
  (formula == "ccdi_splice") |
  (formula == "gk_splice") |
  (formula == "tpd_splice") |
  (formula == "geksl_splice") |
  (formula == "wgeksl_splice") |
  (formula == "geksgl_splice") |
  (formula == "wgeksgl_splice") |
  (formula == "geksaqu_splice") |
  (formula == "wgeksaqu_splice") |
  (formula == "geksaqi_splice") |
  (formula == "wgeksaqi_splice") |
  (formula == "geksgaqi_splice") |
  (formula == "wgeksgaqi_splice"))
  id <- idd[[m]]
  else if ((formula == "geks_fbmw") |
  (formula == "wgeks_fbmw") |
  (formula == "geksw_fbmw") |
  (formula == "geksj_fbmw") |
  (formula == "ccdi_fbmw") |
  (formula == "gk_fbmw") |
  (formula == "tpd_wbmw") |
  (formula == "geksl_fbmw") | (formula == "wgeksl_fbmw") |
  (formula == "geksgl_fbmw") | (formula == "wgeksgl_fbmw") |
  (formula == "geksaqu_fbmw") | (formula == "wgeksaqu_fbmw") |
  (formula == "geksaqi_fbmw") | (formula == "wgeksaqi_fbmw") |
  (formula == "geksgaqi_fbmw") | (formula == "wgeksgaqi_fbmw")) {
  wstart <- end
  lubridate::year(wstart) <- lubridate::year(wstart) - 1
  if (lubridate::year(start) == lubridate::year(end))
  id <-
  matched(
  set,
  period1 = substr(wstart, 0, 7),
  period2 = substr(end, 0, 7),
  type = "retID",
  TRUE
  )
  else
  id <-
  matched(
  set,
  period1 = substr(start, 0, 7),
  period2 = substr(end, 0, 7),
  type = "retID",
  TRUE
  )
  }
  else
  id <-
  matched(
  set,
  period1 = substr(start, 0, 7),
  period2 = substr(end, 0, 7),
  type = "retID",
  TRUE
  )
  retindex <- c()
  w_start_ret <- c()
  w_end_ret <- c()
  ##limiting to those IDs that have at least one matched product from set
  id_ok <- c()
  for (k in 1:length(id)) {
  subset <- dplyr::filter(set, set$retID == id[k])
  sign<-1
  if ((formula == "jevons") |
  (formula == "dutot") |
  (formula == "carli") |
  (formula == "cswd") |
  (formula == "harmonic") |
  (formula == "bmw") |
  (formula == "laspeyres") |
  (formula == "paasche") |
  (formula == "fisher") |
  (formula == "tornqvist") |
  (formula == "geolaspeyres") |
  (formula == "geopaasche") |
  (formula == "drobisch") |
  (formula == "marshall_edgeworth") |
  (formula == "walsh") |
  (formula == "bialek") |
  (formula == "banajree") |
  (formula == "davies") |
  (formula == "stuvel") |
  (formula == "palgrave") |
  (formula == "geary_khamis") |
  (formula == "lehr") |
  (formula == "vartia") |
  (formula == "sato_vartia") |
  (formula == "lloyd_moulton") |
  (formula == "agmean") |
  (formula == "young") |
  (formula == "geoyoung") |
  (formula == "lowe") |
  (formula == "geolowe") |
  (formula == "hybrid") | (formula == "geohybrid"))
  idp <-
  matched(
  subset,
  period1 = substr(start, 0, 7),
  period2 = substr(end, 0, 7),
  type = "prodID",
  FALSE
  )
  else if ((formula == "geks") |
  (formula == "wgeks") |
  (formula == "geksw") |
  (formula == "geksj") |
  (formula == "ccdi") |
  (formula == "gk") |
  (formula == "tpd") | (formula == "geksl") | (formula == "wgeksl") | 
  (formula == "geksgl") | (formula == "wgeksgl") |
  (formula == "geksaqu") | (formula == "wgeksaqu") |
  (formula == "geksaqi") | (formula == "wgeksaqi") |
  (formula == "geksgaqi") | (formula == "wgeksgaqi")) {
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  idp <-
  matched(
  subset,
  period1 = substr(start, 0, 7),
  period2 = substr(end, 0, 7),
  type = "prodID",
  TRUE
  )
  }
  else if ((formula == "geks_splice") |
  (formula == "wgeks_splice") |
  (formula == "geksw_splice") |
  (formula == "geksj_splice") |
  (formula == "ccdi_splice") |
  (formula == "gk_splice") |
  (formula == "tpd_splice") |
  (formula == "geksl_splice") | (formula == "wgeksl_splice") |
  (formula == "geksgl_splice") | (formula == "wgeksgl_splice") |
  (formula == "geksaqu_splice") | (formula == "wgeksaqu_splice") |
  (formula == "geksaqi_splice") | (formula == "wgeksaqi_splice") |
  (formula == "geksgaqi_splice") | (formula == "wgeksgaqi_splice")) {
  wend <- start
  lubridate::month(wend) <- lubridate::month(wend) + window - 1
  idp <-
  matched(
  subset,
  period1 = substr(start, 0, 7),
  period2 = substr(max(wend, end), 0, 7),
  type = "prodID",
  TRUE
  )
  }
  else if ((formula == "geks_fbmw") |
  (formula == "wgeks_fbmw") |
  (formula == "geksw_fbmw") |
  (formula == "geksj_fbmw") |
  (formula == "ccdi_fbmw") |
  (formula == "gk_fbmw") |
  (formula == "tpd_wbmw") |
  (formula == "geksl_fbmw") | (formula == "wgeksl_fbmw") |
  (formula == "geksgl_fbmw") | (formula == "wgeksgl_fbmw") |
  (formula == "geksaqu_fbmw") | (formula == "wgeksaqu_fbmw") |
  (formula == "geksaqi_fbmw") | (formula == "wgeksaqi_fbmw") |
  (formula == "geksgaqi_fbmw") | (formula == "wgeksgaqi_fbmw")) {
  wstart <- end
  lubridate::year(wstart) <- lubridate::year(wstart) - 1
  if (lubridate::year(start) == lubridate::year(end))
  idp <-
  matched(
  subset,
  period1 = substr(wstart, 0, 7),
  period2 = substr(end, 0, 7),
  type = "prodID",
  TRUE
  )
  else
  idp <-
  matched(
  subset,
  period1 = substr(start, 0, 7),
  period2 = substr(end, 0, 7),
  type = "prodID",
  TRUE
  )
  }
  else
  #chain indices case
  {
  period1 = substr(start, 0, 7)
  period2 = substr(end, 0, 7)
  time1<-start
  time2<-start
  lubridate::month(time2)<-lubridate::month(time2)+1
  sign<-1
  while (time2<=end) {
  idp <-
  matched(
  subset,
  period1 = substr(time1, 0, 7),
  period2 = substr(time2, 0, 7),
  type = "prodID",
  FALSE
  )
  if (length(idp)==0) sign<-sign-1
  lubridate::month(time1)<-lubridate::month(time1)+1
  lubridate::month(time2)<-lubridate::month(time2)+1
  }  
  }  
  if (length(idp) > 0 & sign > 0)
  id_ok <- c(id_ok, id[k])
  }
  id <- id_ok
  if (length(id) == 0)
  stop("At least one subset does not include matched products")
  ##------------------------------------------------------------------
  for (k in 1:length(id)) {
  subset <- dplyr::filter(set, set$retID == id[k])
  if ((formula == "geks_splice") |
  (formula == "wgeks_splice") |
  (formula == "geksw_splice") |
  (formula == "geksj_splice") |
  (formula == "ccdi_splice") |
  (formula == "gk_splice") |
  (formula == "tpd_splice") |
  (formula == "geksl_splice") | (formula == "wgeksl_splice") |
  (formula == "geksgl_splice") | (formula == "wgeksgl_splice") |
  (formula == "geksaqu_splice") | (formula == "wgeksaqu_splice") |
  (formula == "geksaqi_splice") | (formula == "wgeksaqi_splice") |
  (formula == "geksgaqi_splice") | (formula == "wgeksgaqi_splice"))
  retindex <- c(retindex, retindexx[[m]][[k]][dist(start, end) + 1])
  else
  retindex <-
  c(
  retindex,
  price_index(
  subset,
  substr(start, 0, 7),
  substr(end, 0, 7),
  formula,
  window,
  splice,
  base,
  sigma,
  interval = FALSE
  )
  )
  d_start <-
  dplyr::filter(
  subset,
  lubridate::year(subset$time) == lubridate::year(start) &
  lubridate::month(subset$time) == lubridate::month(start)
  )
  d_end <-
  dplyr::filter(
  subset,
  lubridate::year(subset$time) == lubridate::year(end) &
  lubridate::month(subset$time) == lubridate::month(end)
  )
  w_start_ret <- c(w_start_ret,
  sum(d_start$prices * d_start$quantities))
  w_end_ret <- c(w_end_ret, sum(d_end$prices * d_end$quantities))
  }
  #aggregating over outlets
  if (aggrret == "none")
  index_ret <- retindex[1]
  if (aggrret == "laspeyres")
  index_ret <- sum(retindex * w_start_ret) / sum(w_start_ret)
  if (aggrret == "paasche")
  index_ret <- sum(w_end_ret) / sum(w_end_ret / retindex)
  if (aggrret == "geolaspeyres")  {
  w_start <- sum(w_start_ret)
  index_ret <-
  prod(retindex ^ (w_start_ret / w_start))
  }
  if (aggrret == "geopaasche")  {
  w_end <- sum(w_end_ret)
  index_ret <-
  prod(retindex ^ (w_end_ret / w_end))
  }
  if (aggrret == "fisher")     {
  index_retL <- sum(retindex * w_start_ret)
  index_retP <-
  sum(w_end_ret / retindex)
  index_retL <-
  index_retL / sum(w_start_ret)
  index_retP <-
  sum(w_end_ret) / index_retP
  index_ret <-
  (index_retL * index_retP) ^ (1 / 2)
  }
  if (aggrret == "tornqvist")  {
  w_start <- sum(w_start_ret)
  w_end <-
  sum(w_end_ret)
  index_ret <-
  prod(retindex ^ (0.5 * (
  w_start_ret / w_start + w_end_ret / w_end
  )))
  }
  if (aggrret == "arithmetic")
  index_ret <- sum(retindex) / length(id)
  
  if (aggrret == "geometric")  {
  index_ret <- prod(retindex)
  index_ret <-
  index_ret ^ (1 / length(id))
  }
  index_set <-
  c(index_set, index_ret)
  w_start_set <-
  c(w_start_set, sum(w_start_ret))
  w_end_set <-
  c(w_end_set, sum(w_end_ret))
  }
  #aggregating over subsets/subgroups
  if (aggrsets == "none")
  index_final <- index_set[1]
  
  if (aggrsets == "laspeyres")  {
  index_final <- 0
  for (i in 1:length(datasets))
  index_final <- index_final + index_set[i] * w_start_set[i]
  index_final <-
  index_final / sum(w_start_set)
  }
  if (aggrsets == "paasche")    {
  index_final <- 0
  for (i in 1:length(datasets))
  index_final <- index_final + w_end_set[i] / index_set[i]
  index_final <-
  sum(w_end_set) / index_final
  }
  if (aggrsets == "geolaspeyres")  {
  index_final <- 1
  w_start_s <-
  sum(w_start_set)
  for (i in 1:length(datasets))
  index_final <- index_final * index_set[i] ^ (w_start_set[i] / w_start_s)
  }
  if (aggrsets == "geopaasche")  {
  index_final <- 1
  w_end_s <-
  sum(w_end_set)
  for (i in 1:length(datasets))
  index_final <- index_final * index_set[i] ^ (w_end_set[i] / w_end_s)
  }
  if (aggrsets == "fisher")     {
  index_setL <- 0
  index_setP <- 0
  for (i in 1:length(datasets))  {
  index_setL <- index_setL + index_set[i] * w_start_set[i]
  index_setP <-
  index_setP + w_end_set[i] / index_set[i]
  }
  index_setL <-
  index_setL / sum(w_start_set)
  index_setP <-
  sum(w_end_set) / index_setP
  index_final <-
  (index_setL * index_setP) ^ (1 / 2)
  }
  if (aggrsets == "tornqvist")  {
  index_final <- 1
  w_start_s <-
  sum(w_start_set)
  w_end_s <-
  sum(w_end_set)
  for (i in 1:length(datasets))
  index_final <-
  index_final * index_set[i] ^ (0.5 * (w_start_set[i] / w_start_s + w_end_set[i] /
  w_end_s))
  }
  if (aggrsets == "arithmetic") {
  index_final <- 0
  for (i in 1:length(datasets))
  index_final <- index_final + index_set[i]
  index_final <-
  index_final / length(datasets)
  }
  if (aggrsets == "geometric")  {
  index_final <- 1
  for (i in 1:length(datasets))
  index_final <- index_final * index_set[i]
  index_final <-
  index_final ^ (1 / length(datasets))
  }
  # end of the procedure
  results <-
  c(results, index_final)
  }
  datfr <- data.frame(c(times), c(results))
  colnames(datfr) <- c("date", formula)
  return (datfr)
  }
  }

#' @title  The most general package function to compute the price dynamics
#'
#' @description This function returns a value or values of the selected (final) price index taking into consideration aggregation over product subgroups and/or over outlets. Optionally, the function returns a data frame or a figure presenting calculated indices, i.e. the price index for the whole data set and price indices for product subgroups.
#' @param data The user's data frame with subgroups of sold products (see \code{by} parameter). Each data frame must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities} (as positive numeric), \code{prodID} (as numeric, factor or character) and \code{retID} (as numeric, factor or character). An additional column indicated via \code{by} parameter is also needed.
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
#' @param date_breaks A string giving the distance between breaks on the X axis like "1 month" (default value) or "4 months".
#' @rdname final_index2
#' @return This function returns a value or values of the selected (final) price index taking into consideration aggregation over product subgroups and/or over outlets (retailer sale points defined in \code{retID} column). Optionally, the function returns a data frame or a figure presenting calculated indices, i.e. the price index for the whole data set and price indices for product subgroups.To be more precise: if both types of aggregation are selected, then for each subgroup of products and for each outlet (point of sale) price indices are calculated separately and then aggregated (according to the aggregation methods indicated) to the form of the final price index. If the \code{interval} parameter is set to TRUE then it returns a data frame (or a figure) with dates and final index values (after optional aggregating). Please note that different index formulas may use different time intervals (or time periods) for calculations and each time, aggregation over outlets is done for the set of retIDs being available during the whole considered time interval. 
#' @examples 
#' \donttest{final_index2(data=coffee, by="description",all=TRUE,start="2018-12",end="2019-12",
#' formula="fisher",interval=TRUE,aggrsets="laspeyres",aggrret="none",figure=FALSE)}
#' \donttest{final_index2(data=coffee, by="retID",all=TRUE,start="2018-12",end="2019-12",
#' formula="fisher",interval=TRUE,aggrsets="none",aggrret="none",figure=TRUE)}
#' @export

final_index2 <-
  function(data = data.frame(),
  by,
  all = FALSE,
  start,
  end,
  formula = "fisher",
  window = 13,
  splice = "movement",
  base = start,
  sigma = 0.7,
  aggrret = "tornqvist",
  aggrsets = "tornqvist",
  interval = FALSE,
  figure = FALSE,
  date_breaks = "1 month")
  {
  if (nrow(data) == 0)
  stop("A data set is empty!")
  date <- group <- value <- NULL
  names <- colnames(data)
  if (!(by %in% names))
  stop ("There is no column specified via 'by' parameter!")
  group <- as.character(unique(data[, by]))
  datasets <- list()
  for (i in 1:length(group))
  datasets[[i]] <- dplyr::filter(data, data[, by] == group[i])
  if (all == FALSE)
  return(
  final_index(
  datasets,
  start,
  end,
  formula,
  window,
  splice,
  base,
  sigma,
  aggrret,
  aggrsets,
  interval
  )
  )
  else {
  if (interval == FALSE) {
  index <- c()
  for (i in 1:length(group))
  index <-
  c(
  index,
  final_index(
  list(datasets[[i]]),
  start,
  end,
  formula,
  window,
  splice,
  base,
  sigma,
  aggrret,
  aggrsets = "none",
  interval = FALSE
  )
  )
  index <-
  c(
  index,
  final_index(
  datasets,
  start,
  end,
  formula,
  window,
  splice,
  base,
  sigma,
  aggrret,
  aggrsets,
  interval = FALSE
  )
  )
  group <- c(group, "all groups: ")
  result <- data.frame(group, index)
  colnames(result)[2] <- formula
  return(result)
  }
  else                   {
  result <-
  final_index(
  datasets,
  start,
  end,
  formula,
  window,
  splice,
  base,
  sigma,
  aggrret,
  aggrsets,
  interval = TRUE
  )
  for (i in 1:length(group)) {
  index_subgroup <-
  final_index(
  list(datasets[[i]]),
  start,
  end,
  formula,
  window,
  splice,
  base,
  sigma,
  aggrret,
  aggrsets = "none",
  interval = TRUE
  )
  result[, i + 2] <- index_subgroup[, 2]
  }
  n <- 2 + length(group)
  colnames(result)[2] <- paste(formula, ": all groups")
  colnames(result)[3:n] <- group
  if (figure == FALSE)
  return(result)
  else
  {
  #drawing a plot
  result$date <- as.Date(paste(result$date, "-01", sep = ""))
  result <- reshape::melt(result, id.var = 'date')
  colnames(result) <- c("date", "group", "value")
  ggplot2::ggplot(result, ggplot2::aes(x = date, y = value, col = group)) + ggplot2::geom_point() +
  ggplot2::geom_line() + ggplot2::labs(x = "date", y = "price index value") +
  ggplot2::scale_x_date(date_labels = "%Y %m", date_breaks  = date_breaks) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
  }
  }
  }
  

