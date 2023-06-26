#' A general function to compute a price index
#' This function returns a value or values of the selected price index. 
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also essential even if the selected index is an unweighted formula (unit values are calculated). 
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param formula The character string indicating the price index formula is to be calculated. To see available options please use the link: \code{\link{PriceIndices}}.
#' @param window The length of the time window if the multilateral index is selected (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method (if the multilateral splicing index is selected). Available options are: "movement", "window","half","mean" and their additional variants: "window_published", "half_published" and "mean_published". 
#' @param base The prior period used in the Young- or Lowe-type price indices (as character) limited to the year and month, e.g. "2020-01".
#' @param sigma The elasticity of substitution parameter used in the Lloyed-Moulton, AG Mean or GEKS-LM indices (as numeric).
#' @param r The non-zero parameter used in the quadratic mean of order r quantity / price index or in the GEKS-QM index (as numeric).
#' @param interval A logical value indicating whether the function is to provide the price index comparing the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be presented (the fixed base month is defined by \code{start}).
#' @noRd


price_index <-
  function(data,
  start,
  end,
  formula,
  window,
  splice,
  base,
  sigma,
  r,
  interval)
  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  if (sigma == 1)
  stop("A specification of the parameter 'sigma' is wrong")
  if (r == 0)
  stop("A specification of the parameter 'r' is wrong")
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
  if (formula == "QMq")
  set <- QMq(data, start, end, r)
  if (formula == "QMp")
  set <- QMp(data, start, end, r)
  if (formula == "IQMp")
  set <- IQMp(data, start, end, r)
  if (formula == "value_index")
  set <- value_index(data, start, end)
  if (formula == "unit_value_index")
  set <- unit_value_index(data, start, end)  
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
  if (formula == "chQMq")
  set <- chQMq(data, start, end, r)
  if (formula == "chQMp")
  set <- chQMp(data, start, end, r)
  if (formula == "chIQMp")
  set <- chIQMp(data, start, end, r)
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
  if (formula == "utpd")
  set <- utpd(data, start, end, start, window)
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
  if (formula == "geksiqm")
  set <- geksiqm(data, start, end, r, start, window)
  if (formula == "geksqm")
  set <- geksqm(data, start, end, r, start, window)
  if (formula == "gekslm")
  set <- gekslm(data, start, end, sigma, start, window)
  #extended multilateral indices
  #splice
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
  if (formula == "utpd_splice")
  set <- utpd_splice(data, start, end, window, splice, interval)
  if (formula == "geksiqm_splice")
  set <- geksiqm_splice(data, start, end, r, window, splice, interval)
  if (formula == "geksqm_splice")
  set <- geksqm_splice(data, start, end, r, window, splice, interval)
  if (formula == "gekslm_splice")
  set <- gekslm_splice(data, start, end, sigma, window, splice, interval)
  #FBEW
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
  if (formula == "tpd_fbew")
  set <- tpd_fbew(data, start, end)
   if (formula == "utpd_fbew")
  set <- utpd_fbew(data, start, end)
  if (formula == "geksiqm_fbew")
  set <- geksiqm_fbew(data, start, end, r)
  if (formula == "geksqm_fbew")
  set <- geksqm_fbew(data, start, end, r)
  if (formula == "gekslm_fbew")
  set <- gekslm_fbew(data, start, end, sigma)
  #FBMW
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
  if (formula == "tpd_fbmw")
  set <- tpd_fbmw(data, start, end)
  if (formula == "utpd_fbmw")
  set <- utpd_fbmw(data, start, end)  
  if (formula == "geksiqm_fbmw")
  set <- geksiqm_fbmw(data, start, end, r)
  if (formula == "geksqm_fbmw")
  set <- geksqm_fbmw(data, start, end, r)
  if (formula == "gekslm_fbmw")
  set <- gekslm_fbmw(data, start, end, sigma)
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
  if (formula == "lloyd_moulton")
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
  if (formula == "QMq")
  set <- QMq(data, start, end, r, interval)
  if (formula == "QMp")
  set <- QMp(data, start, end, r, interval)
  if (formula == "IQMp")
  set <- IQMp(data, start, end, r, interval)
  if (formula == "value_index")
  set <- value_index(data, start, end, interval)
  if (formula == "unit_value_index")
  set <- unit_value_index(data, start, end, interval)
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
  if (formula == "chlloyd_moulton")
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
  if (formula == "chQMq")
  set <- chQMq(data, start, end, r, interval)
  if (formula == "chQMp")
  set <- chQMp(data, start, end, r, interval)
  if (formula == "chIQMp")
  set <- chIQMp(data, start, end, r, interval)
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
  if (formula == "utpd_splice")
  set <- utpd_splice(data, start, end, window, splice, interval)
  if (formula == "geksqm_splice")
  set <- geksqm_splice(data, start, end, r, window, splice, interval)
  if (formula == "geksiqm_splice")
  set <- geksiqm_splice(data, start, end, r, window, splice, interval)
  if (formula == "gekslm_splice")
  set <- gekslm_splice(data, start, end, sigma, window, splice, interval)
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
  if (formula == "geksiqm") denominator<-geksiqm_denom(data, start, end, r, start, window)
  if (formula == "geksqm") denominator<-geksqm_denom(data, start, end, r, start, window)
  if (formula == "gekslm") denominator<-gekslm_denom(data, start, end, sigma, start, window)
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
  set <- c(set, geksaqu_num(data, t0, t, t0, window)/denominator)
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
  if (formula == "geksiqm")
  set <- c(set, geksiqm_num(data, t0, t, r, t0, window)/denominator)  
  if (formula == "geksqm")
  set <- c(set, geksqm_num(data, t0, t, r, t0, window)/denominator)  
  if (formula == "gekslm")
  set <- c(set, gekslm_num(data, t0, t, sigma, t0, window)/denominator)
  if (formula == "gk")
  set <- c(set, gk(data, t0, t, t0, window))
  if (formula == "tpd")
  set <- c(set, tpd(data, t0, t, t0, window))
  if (formula == "utpd")
  set <- c(set, utpd(data, t0, t, t0, window))
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
  if (formula == "utpd_fbew")
  set <- c(set, utpd_fbew(data, t0, t))
  if (formula == "utpd_fbmw")
  set <- c(set, utpd_fbmw(data, t0, t))
  if (formula == "geksiqm_fbew")
  set <- c(set, geksiqm_fbew(data, t0, t, r))
  if (formula == "geksiqm_fbmw")
  set <- c(set, geksiqm_fbmw(data, t0, t, r))
  if (formula == "geksqm_fbew")
  set <- c(set, geksqm_fbew(data, t0, t, r))
  if (formula == "geksqm_fbmw")
  set <- c(set, geksqm_fbmw(data, t0, t, r))
  if (formula == "gekslm_fbew")
  set <- c(set, gekslm_fbew(data, t0, t, sigma))
  if (formula == "gekslm_fbmw")
  set <- c(set, gekslm_fbmw(data, t0, t, sigma))
  times <- c(times, substr(start2, 0, 7))
  lubridate::month(start) <-
  lubridate::month(start) + 1
  }
  datfr <- data.frame(c(times), c(set))
  colnames(datfr) <- c("date", formula)
  return (datfr)
  }
  }

#' Filtering where only two months are compared
#' This function returns a filtered data set, i.e. a reduced user's data frame with the same columns and rows limited by a criterion defined by filters
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param filters A vector of filter names (options are: \code{extremeprices}, \code{dumpprices} and/or \code{lowsales}). 
#' @param plimits A two-dimensional vector of thresholds  for minimum and maximum price change (it works if one of the chosen filters is \code{extremeprices} filter). 
#' @param pquantiles A two-dimensional vector of quantile levels for minimum and maximum price change (it works if one of the chosen filters is \code{extremeprices} filter).
#' @param dplimits A two-dimensional vector of thresholds for maximum price drop and maximum ependiture drop (it works if one of the chosen filters is \code{dumpprices} filter). 
#' @param lambda The lambda parameter for \code{lowsales} filter (see \code{References} below).
#' @noRd

filtering <-
  function(data,
  start,
  end,
  filters = c(),
  plimits = c(),
  pquantiles = c(),
  dplimits = c(),
  lambda = 1.25)
  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  start <- as.Date(start)
  start2 <- start
  end <- paste(end, "-01", sep = "")
  end <- as.Date(end)
  end2 <- end
  lubridate::day(start2) <- lubridate::days_in_month(start2)
  lubridate::day(end2) <- lubridate::days_in_month(end2)
  data <-
  dplyr::filter(data,
  (data$time >= start &
  data$time <= start2) | (data$time >= end & data$time <= end2))
  filter1 <- "extremeprices"
  filter2 <- "lowsales"
  filter3 <- "dumpprices"
  afilters <- c(filter1, filter2, filter3)
  if ((start == end) | length(filters) == 0)
  return (data)
  else if (length(base::intersect(filters, afilters)) == 0)
  stop("there are no such filters")
  if (length(base::setdiff(filters, base::intersect(filters, afilters))) >
  0)
  stop("At least one filter's name is wrong")
  data1 <- data[0:0, ]
  data2 <- data[0:0, ]
  data3 <- data[0:0, ]
  data4 <- data[0:0, ]
  if (filter1 %in% filters) {
  if ((length(pquantiles) + length(plimits)) == 0)
  data1 <- data
  else {
  id <- sort(matched(data, start, end))
  priceshares<-prices(data,period=end,set=id)/prices(data,period=start,set=id)
  }
  if (length(pquantiles) >
  0)
  {
  tresh <- c(0, 1)
  if ((pquantiles[1] ==
  tresh[1]) & (pquantiles[2] == tresh[2])) {
  data1 <- data
  }
  else {
  qq <- stats::quantile(priceshares, probs = pquantiles, names = FALSE)
  #selecting the sample by checking condition
  id1 <- c()
  for (i in 1:length(id))
  if ((priceshares[i] >=
  qq[1]) & (priceshares[i] <= qq[2]))
  id1 <- c(id1, id[i])
  data1 <-
  dplyr::filter(data, data$prodID %in% id1)
  }
  } else
  data1 <- data
  if (length(plimits) >
  0)
  {
  #selecting the sample by chacking condition
  id2 <- c()
  for (i in 1:length(id))
  if ((priceshares[i] >= plimits[1]) &
  (priceshares[i] <= plimits[2]))
  id2 <- c(id2, id[i])
  data2 <-
  dplyr::filter(data, data$prodID %in% id2)
  } else
  data2 <- data
  } else
  {
  data1 <- data
  data2 <- data
  }
  if (filter2 %in% filters) {
  if (lambda <= 0)
  data3 <- data
  id <-
  sort(matched(data, start, end))
  expenditures_start <-
  expenditures(data, period = start, set = id)
  expenditures_end <-
  expenditures(data, period = end, set = id)
  sum_start <-
  sum(expenditures_start)
  sum_end <-
  sum(expenditures_end)
  id3 <- c()
  for (i in 1:length(id))
  if (0.5 * ((expenditures_start[i] /
  sum_start) + (expenditures_end[i] / sum_end)) > (1 / (length(id) * lambda)))
  id3 <-
  c(id3, id[i])
  data3 <-
  dplyr::filter(data, data$prodID %in% id3)
  } else
  data3 <- data
  if (filter3 %in% filters) {
  if (!(length(dplimits) == 2))
  data4 <- data
  else {
  id <- sort(matched(data, start, end))
  expenditures_start <-
  expenditures(data, period = start, set = id)
  expenditures_end <-
  expenditures(data, period = end, set = id)
  priceshares<-prices(data,period=end,set=id)/prices(data,period=start,set=id)
  id4 <- c()
  for (i in 1:length(id))
  if ((priceshares[i] >=
  dplimits[1]) |
  ((expenditures_end[i] / expenditures_start[i]) >= dplimits[2]))
  id4 <-
  c(id4, id[i])
  data4 <-
  dplyr::filter(data, data$prodID %in% id4)
  }
  } else
  data4 <- data
  data_final <- dplyr::intersect(data1, data2)
  data_final <- dplyr::intersect(data_final, data3)
  data_final <- dplyr::intersect(data_final, data4)
  return (data_final)
  }
  
#' Filtering where each subsequent months from the considered time interval are compared
#' This function returns a filtered data set, i.e. a reduced user's data frame with the same columns and rows limited by a criterion defined by filters
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param filters A vector of filter names (options are: \code{extremeprices}, \code{dumpprices} and/or \code{lowsales}). 
#' @param plimits A two-dimensional vector of thresholds  for minimum and maximum price change (it works if one of the chosen filters is \code{extremeprices} filter). 
#' @param pquantiles A two-dimensional vector of quantile levels for minimum and maximum price change (it works if one of the chosen filters is \code{extremeprices} filter).
#' @param dplimits A two-dimensional vector of thresholds for maximum price drop and maximum ependiture drop (it works if one of the chosen filters is \code{dumpprices} filter). 
#' @param lambda The lambda parameter for \code{lowsales} filter (see \code{References} below).
#' @noRd

filtering_interval <-
  function(data,
  start,
  end,
  filters = c(),
  plimits = c(),
  pquantiles = c(),
  dplimits = c(),
  lambda = 1.25)
  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  start <- as.Date(start)
  end <- paste(end, "-01", sep = "")
  end <- as.Date(end)
  if (start == end) {
  data <-
  dplyr::filter(
  data,
  (lubridate::year(data$time) == lubridate::year(start)) &
  (lubridate::month(data$time) == lubridate::month(start))
  )
  return (data)
  }
  dates<-seq.Date(from=start, to=end, by="month")
  dates <- format(dates, format = "%Y-%m")
  filtr<-function (i) 
    filtering(data,dates[i-1],dates[i],filters,plimits,pquantiles,dplimits,lambda)
  data<-lapply(seq(2,length(dates)), filtr)
  data<-dplyr::bind_rows(data)
  data<-dplyr::distinct(data)
  return (data)
  }
  
#' The function returns the quantity of a given product which was sold in a given period. 
#' @param data The user's data frame. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{quantities} (as positive numeric) and \code{prodID} (as numeric or character) with unique product IDs. 
#' @param period The time period (as character) limited to the year and month, e.g. "2019-03".
#' @param ID The ID of unique product which is used for determining the quantity 
#' @noRd

quantity <- function(data, period, ID)
  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  period <- paste(period, "-01", sep = "")
  period <- as.Date(period)
  lubridate::day(period) <- 1
  period2 <- period
  lubridate::day(period2) <-
  lubridate::days_in_month(period2)
  data <- dplyr::filter(data, data$prodID == ID)
  data <-
  dplyr::filter(data, data$time >= period & data$time <= period2)
  if (nrow(data) == 0)
  stop("There are no data in selected period")
  return(sum(data$quantities))
  }
  
#' The function returns the price of a given product which was sold in a given period. 
#' @param data The user's data frame. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{quantities} (as positive numeric) and \code{prodID} (as numeric or character) with unique product IDs. 
#' @param period The time period (as character) limited to the year and month, e.g. "2019-03".
#' @param ID The ID of unique product which is used for determining the quantity 
#' @noRd


price <- function(data, period, ID)
  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  period <- paste(period, "-01", sep = "")
  period <- as.Date(period)
  lubridate::day(period) <- 1
  period2 <- period
  lubridate::day(period2) <-
  lubridate::days_in_month(period2)
  data <- dplyr::filter(data, data$prodID == ID)
  data <-
  dplyr::filter(data, data$time >= period & data$time <= period2)
  if (nrow(data) == 0)
  stop("There are no data in selected period")
  return(sum(data$prices * data$quantities) / sum(data$quantities))
  }
  
#' The function returns the expenditure of a given product which was sold in a given period. 
#' @param data The user's data frame. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{quantities} (as positive numeric) and \code{prodID} (as numeric or character) with unique product IDs. 
#' @param period The time period (as character) limited to the year and month, e.g. "2019-03".
#' @param ID The ID of unique product which is used for determining the price 
#' @noRd

expenditure<-function (data, period, ID)
{
  if (nrow(data) == 0)
  stop("A data frame is empty")
  period <- paste(period, "-01", sep = "")
  period <- as.Date(period)
  lubridate::day(period) <- 1
  period2 <- period
  lubridate::day(period2) <-
  lubridate::days_in_month(period2)
  data <- dplyr::filter(data, data$prodID == ID)
  data <-
  dplyr::filter(data, data$time >= period & data$time <= period2)
  if (nrow(data) == 0)
  stop("There are no data in selected period")
  return(sum(data$prices * data$quantities))
  }

#' The function returns the logarithmic mean of two numbers. 
#' @param x A real positive number
#' @param y A real positive number
#' @noRd
  
  L <- function (x, y) {
  if (x == y)
  return (x)
  else
  return ((y - x) / log(y / x))
  }
  
#' The function returns the logarithmic mean of two numbers. 
#' @param x A real positive number vector
#' @param y A real positive number vector
#' @noRd 

  LL <- function (x) {
  if (x[1] == x[2])
  return (x[1])
  else
  return ((x[1] - x[2]) / log(x[1] / x[2]))
  }
  
#' The function returns the logarithmic mean of elements of two vectors. 
#' @param x A real positive number vector
#' @param y A real positive number vector
#' @noRd

Lv <- function (x, y) {
  nx<-seq(1,length(x))
  help<-function (i) 
  {
   if (x[i]==y[i]) return (x[i])
    else return ((x[i]-y[i])/log(x[i]/y[i]))}
   return (sapply(nx, help))   
  }

#' An additional function used in the 'geks_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd
  
geks_fbmw2 <- function(data, start, end)  {
if (start == end)
return (1)
if (nrow(data) == 0)
stop("A data frame is empty")
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
wstart <- end
lubridate::year(wstart) <-
lubridate::year(wstart) - 1
#checking conditions
if (start > end)
stop("parameters must satisfy: start<=end")
if (lubridate::month(start) < 12)
stop("a month of the 'start' parameter must be December")
if (start == end)
return (1)
else
return (geks(
data,
substr(start, 0, 7),
substr(end, 0, 7),
substr(wstart, 0, 7),
window = 13
))
}

#' An additional function used in the 'wgeks_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd
  
wgeks_fbmw2 <- function(data, start, end)  {
if (start == end)
return (1)
if (nrow(data) == 0)
stop("A data frame is empty")
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
wstart <- end
lubridate::year(wstart) <-
lubridate::year(wstart) - 1
#checking conditions
if (start > end)
stop("parameters must satisfy: start<=end")
if (lubridate::month(start) < 12)
stop("a month of the 'start' parameter must be December")
if (start == end)
return (1)
else
return (wgeks(
data,
substr(start, 0, 7),
substr(end, 0, 7),
substr(wstart, 0, 7),
window = 13
))
}

#' An additional function used in the 'geksw_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

geksw_fbmw2 <- function(data, start, end)  {
if (start == end)
return (1)
if (nrow(data) == 0)
stop("A data frame is empty")
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
wstart <- end
lubridate::year(wstart) <-
lubridate::year(wstart) - 1
#checking conditions
if (start > end)
stop("parameters must satisfy: start<=end")
if (lubridate::month(start) < 12)
stop("a month of the 'start' parameter must be December")
if (start == end)
return (1)
else
return (geksw(
data,
substr(start, 0, 7),
substr(end, 0, 7),
substr(wstart, 0, 7),
window = 13
))
}  

#' An additional function used in the 'geksj_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

geksj_fbmw2 <- function(data, start, end)  {
if (start == end)
return (1)
if (nrow(data) == 0)
stop("A data frame is empty")
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
wstart <- end
lubridate::year(wstart) <-
lubridate::year(wstart) - 1
#checking conditions
if (start > end)
stop("parameters must satisfy: start<=end")
if (lubridate::month(start) < 12)
stop("a month of the 'start' parameter must be December")
if (start == end)
return (1)
else
return (geksj(
data,
substr(start, 0, 7),
substr(end, 0, 7),
substr(wstart, 0, 7),
window = 13
))
}


#' An additional function used in the 'geksqm_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter used in the implicit quadratic mean of order r price index.
#' @noRd
  
geksqm_fbmw2 <- function(data, start, end, r)  {
if (start == end)
return (1)
if (nrow(data) == 0)
stop("A data frame is empty")
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
wstart <- end
lubridate::year(wstart) <-
lubridate::year(wstart) - 1
#checking conditions
if (start > end)
stop("parameters must satisfy: start<=end")
if (lubridate::month(start) < 12)
stop("a month of the 'start' parameter must be December")
if (start == end)
return (1)
else
return (geksqm(
data,
substr(start, 0, 7),
substr(end, 0, 7),
r,
substr(wstart, 0, 7),
window = 13
))
}


#' An additional function used in the 'geksiqm_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter used in the implicit quadratic mean of order r price index.
#' @noRd
  
geksiqm_fbmw2 <- function(data, start, end, r)  {
if (start == end)
return (1)
if (nrow(data) == 0)
stop("A data frame is empty")
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
wstart <- end
lubridate::year(wstart) <-
lubridate::year(wstart) - 1
#checking conditions
if (start > end)
stop("parameters must satisfy: start<=end")
if (lubridate::month(start) < 12)
stop("a month of the 'start' parameter must be December")
if (start == end)
return (1)
else
return (geksiqm(
data,
substr(start, 0, 7),
substr(end, 0, 7),
r,
substr(wstart, 0, 7),
window = 13
))
}


#' An additional function used in the 'gekslm_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param sigma The elasticity of substitution (a parameter used in the Lloyd-Moulton index formula).
#' @noRd
  
gekslm_fbmw2 <- function(data, start, end, sigma)  {
if (start == end)
return (1)
if (nrow(data) == 0)
stop("A data frame is empty")
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
wstart <- end
lubridate::year(wstart) <-
lubridate::year(wstart) - 1
#checking conditions
if (start > end)
stop("parameters must satisfy: start<=end")
if (lubridate::month(start) < 12)
stop("a month of the 'start' parameter must be December")
if (start == end)
return (1)
else
return (gekslm(
data,
substr(start, 0, 7),
substr(end, 0, 7),
sigma,
substr(wstart, 0, 7),
window = 13
))
}

#' An additional function used in the 'ccdi_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

ccdi_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (ccdi(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}

#' An additional function used in the 'gk' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

gkreal <- function(data, start, end)  {
if (start == end)
return (1)
if (nrow(data) == 0)
stop("A data frame is empty")
time<-NULL
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
lubridate::day(end)<-lubridate::days_in_month(end)
#checking conditions
if (start > end)
stop("parameters must satisfy: start<=end")
if (lubridate::month(start) < 12)
stop("a month of the 'start' parameter must be December")
#data filtration
d <-
dplyr::filter(data, data$time >= start & data$time <= end)
prodID <- unique(d$prodID)
#main body
#initial values of indices
index1 <- c()
index2 <- c()
#set of dates
dates <- c()
dates <- seq.Date(from = start, to = end, by = "month")
dates <- format(dates, format = "%Y-%m")
index1<-replicate(length(dates),1)
index2<-replicate(length(dates),2)
d2<-d
d2$time<-as.character(d2$time)
d2$time<-substr(d2$time,0,7)
gr<-dplyr::summarise(dplyr::group_by(d2, time, prodID), expend=sum(prices*quantities), quant=sum(quantities),.groups="drop")
#quantity weights - quality adjusted factors vi
while (sqrt(sum((index1 - index2) ^ 2)) >
0.005)
{
val <- function (id)  {
xx<-
function (tt)
{
gr_subset1<-dplyr::filter(gr, gr$time==tt & gr$prodID==id)
if (nrow(gr_subset1)>0) return (sum(gr_subset1$expend) / index1[which(dates == tt)])
else return (0)
}
yy <-
function (tt)
{
gr_subset2<-dplyr::filter(gr, gr$time==tt & gr$prodID==id)
if (nrow(gr_subset2)>0) return (sum(gr_subset2$quant))
else return (0)
}
x <- sum(sapply(dates, xx))
y <- sum(sapply(dates, yy))
return (x / y)
}
values <- sapply(prodID, val)
v <- data.frame(prodID, values)
#series  of indices
indd <-
function(tt)
return (QU(d, substr(start, 0, 7), tt, v))
ind <- sapply(dates, indd)
index2 <- index1
index1 <- ind
}
result <-
index1[which(dates == substr(end, 0, 7))]
result <- result[[1]]
return (result)
}

#' An additional function used in the 'geksl_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

geksl_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (geksl(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}

#' An additional function used in the 'geksgl_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

geksgl_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (geksgl(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}


#' An additional function used in the 'gk_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

gk_fbmw2 <- function(data, start, end) {
if (start == end)
return (1)
if (nrow(data) == 0)
stop("A data frame is empty")
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
wstart <- end
lubridate::year(wstart) <-
lubridate::year(wstart) - 1
#checking conditions
if (start > end)
stop("parameters must satisfy: start<=end")
if (lubridate::month(start) < 12)
stop("a month of the 'start' parameter must be December")
if (start == end)
return (1)
else
return (gk(
data,
substr(start, 0, 7),
substr(end, 0, 7),
substr(wstart, 0, 7),
window = 13
))
}

#' The function returns a distance between two dates (in months) - it is not exported
#' @param data1 The first date (as Date type) written in the format with a year, month and day, e.g. "2020-04-03".
#' @param data2 The second date (as Date type) written in the format with a year, month and day, e.g. "2020-04-03".
#' @noRd

dist <- function(data1, data2)
{
n <- 0
while (data1 <= data2)
{
n <- n + 1
lubridate::month(data1) <- lubridate::month(data1) + 1
}
return (n - 1)
}

#' An additional function used in the 'tpd_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

tpd_fbmw2 <- function(data, start, end)  {
if (start == end)
return (1)
if (nrow(data) == 0)
stop("A data frame is empty")
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
wstart <- end
lubridate::year(wstart) <-
lubridate::year(wstart) - 1
#checking conditions
if (start > end)
stop("parameters must satisfy: start<=end")
if (lubridate::month(start) < 12)
stop("A month of the 'start' parameter must be December")
if (start == end)
return (1)
else
return (tpd(
data,
substr(start, 0, 7),
substr(end, 0, 7),
substr(wstart, 0, 7),
window = 13
))
}


#' An additional function used in the 'utpd_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

utpd_fbmw2 <- function(data, start, end)  {
if (start == end)
return (1)
if (nrow(data) == 0)
stop("A data frame is empty")
start <- paste(start, "-01", sep = "")
end <- paste(end, "-01", sep = "")
start <- as.Date(start)
end <- as.Date(end)
wstart <- end
lubridate::year(wstart) <-
lubridate::year(wstart) - 1
#checking conditions
if (start > end)
stop("parameters must satisfy: start<=end")
if (lubridate::month(start) < 12)
stop("A month of the 'start' parameter must be December")
if (start == end)
return (1)
else
return (utpd(
data,
substr(start, 0, 7),
substr(end, 0, 7),
substr(wstart, 0, 7),
window = 13
))
}


#' An additional function used in the 'wgeksl_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

wgeksl_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (wgeksl(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}

#' An additional function used in the 'wgeksgl_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

wgeksgl_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (wgeksgl(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}


#' An additional function used in the 'unit' function
#' @param string A string which contains the grammage of the product and its unit
#' @noRd

numextract <- function(string) {
  unlist(regmatches(string, gregexpr(
  "[[:digit:]]+\\.*[[:digit:]]*", string
  )))
} 

#' An additional function used in the 'data_unit' function
#' @param string A string which contains the grammage of the product and its unit
#' @param units Units of products which are to be detected
#' @param multiplication A sign of the multiplication used in product descriptions
#' @param space A maximum space between the product grammage and its unit 
#' @noRd

unit <-
  function (string,
  units = c("g", "ml", "kg", "l"),
  multiplication = "x",
  space = 1)
  {
  detect <- FALSE
  string <- tolower(stringr::str_replace_all(string, ',', '.'))
  units <- tolower(units)
  numbers <- n <- pattern <- text <- sizes <- unit <- grammage <- NULL
  numbers <- as.numeric(numextract(string))
  if (length(numbers) == 0)
  return (list("1", "item"))
  #recalculating expressions with a sign of the product
  n <- length(numbers)
  if (stringr::str_detect(string, multiplication) &
  length(numbers) > 1)
  {
  nn <- n - 1
  for (i in 1:nn)  {
  patt <-
  paste(as.character(numbers[i]),
  multiplication,
  as.character(numbers[i + 1]),
  sep = "")
  if (stringr::str_detect(string, patt)) {
  string <-
  stringr::str_replace(string, patt,  as.character(as.numeric(numbers[i]) * as.numeric(numbers[i +
  1])))
  detect <- TRUE
  }
  if (detect == TRUE) {
  numbers <- numextract(string)
  n <- length(numbers)
  }
  }
  }
  #main body
  #initial values (no unit detected)
  unit <- "item"
  grammage <- 1
  #checking for units
  unit_list <- c()
  
  for (i in 1:n) {
  text <-
  strex::str_after_first(string, pattern = as.character(numbers[i]))
  string<-text
  for (k in 1:length(units)) {
  sizes <- nchar(units[k]) + space
  text2 <- substr(text, 0, sizes)
  if (!(is.na(stringr::str_detect(text2, units[k])))) 
    if (stringr::str_detect(text2, units[k])) {
  unit_list <- c(unit_list, units[k])
  grammage <- numbers[i]
  }
  }
  }
  if (length(unit_list) > 0)
  unit <-
  unit_list[max(which(nchar(unit_list) == max(nchar(unit_list))))]
  return (list(grammage, unit))
  }

#' An additional function used in the 'geksl' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

nl <-
  function(data, start, end)  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
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
  id <-
  matched(data, start, end, type = "prodID", interval = FALSE)
  price_start <-
  prices(data, period = start, set = id)
  price_end <-
  prices(data, period = end, set = id)
  quantity_start <-
  quantities(data, period = start, set = id)
  return(sum(quantity_start * price_end)/sum(quantity_start * price_start))
  }

#' An additional function used in the 'geksaqu' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param v The data frame which contains quality adjusted factors.
#' @noRd

aqu <-
  function(data, start, end, v=data.frame())  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  prodID<-NULL
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
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
  id <-
  matched(data, start, end, type = "prodID", interval = FALSE)
  data<-dplyr::filter(data, prodID %in% id) 
  price_end <-
  prices(data, period = end)
  quantity_start <-
  quantities(data, period = start)
  v_v<-dplyr::filter(v, prodID %in% id)
  v_v<-dplyr::arrange(v_v, prodID)
  val<-v_v$values
  return(sum(quantity_start * price_end) / sum(val*quantity_start))
  }

#' An additional function used in the 'geksaqu_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

geksaqu_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (geksaqu(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}

#' An additional function used in the 'wgeksaqu_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

wgeksaqu_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (wgeksaqu(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}

#' An additional function used in the 'geksaqi' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param v The data frame which contains quality adjusted factors.
#' @noRd

aqi <-
  function(data, start, end, v=data.frame())  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  prodID<-NULL
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
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
  id <-
  matched(data, start, end, type = "prodID", interval = FALSE)
  data<-dplyr::filter(data, prodID %in% id) 
  price_start <-
  prices(data, period = start)
  price_end <-
  prices(data, period = end)
  quantity_start <-
  quantities(data, period = start)
  v_v<-dplyr::filter(v, prodID %in% id)
  v_v<-dplyr::arrange(v_v, prodID)
  val<-v_v$values
  return(sum(val*quantity_start * price_end/price_start) / sum(val*quantity_start))
  }

#' An additional function used in the 'geksaqi_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

geksaqi_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (geksaqi(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}

#' An additional function used in the 'wgeksaqi_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

wgeksaqi_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (wgeksaqi(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}


#' An additional function used in the 'geksgaqi' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param v The data frame which contains quality adjusted factors.
#' @noRd

gaqi <-
  function(data, start, end, v=data.frame())  {
  if (nrow(data) == 0)
  stop("A data frame is empty")
  prodID<-NULL
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
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
  id <-
  matched(data, start, end, type = "prodID", interval = FALSE)
  data<-dplyr::filter(data, prodID %in% id) 
  price_start <-
  prices(data, period = start)
  price_end <-
  prices(data, period = end)
  quantity_start <-
  quantities(data, period = start)
  v_v<-dplyr::filter(v, prodID %in% id)
  v_v<-dplyr::arrange(v_v, prodID)
  val<-v_v$values
  coef<-c()
  coef<-val*quantity_start/sum(val*quantity_start)
  return(prod((price_end/price_start)^coef))
  }

#' An additional function used in the 'geksgaqi_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

geksgaqi_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (geksgaqi(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}

#' An additional function used in the 'wgeksgaqi_fbmw' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @noRd

wgeksgaqi_fbmw2 <- function(data, start, end)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- end
  lubridate::year(wstart) <-
  lubridate::year(wstart) - 1
  #checking conditions
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (lubridate::month(start) < 12)
  stop("a month of the 'start' parameter must be December")
  if (start == end)
  return (1)
  else
  return (wgeksgaqi(
  data,
  substr(start, 0, 7),
  substr(end, 0, 7),
  substr(wstart, 0, 7),
  window = 13
  ))
}

#' An additional function used in the 'model_classification' function
#' @param str A character string which is transformed into unique integer number.
#' @noRd

conversion<-function (str)
{
  utf<-utf8ToInt(str)
  int<-seq(1:nchar(str))
  return (sum(utf*int))
}

#' Calculating the Lloyd-Moulton price index
#' This function returns the Lloyd-Moulton price index value  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param sigma The elasticity of substitution parameter (as numeric).
#' @noRd

lm <-
    function(data,
    start,
    end,
    sigma
    )  {
    if (start == end)
    return (1)
    if (sigma == 1)
    stop("A specification of the parameter 'sigma' is wrong")
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
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
    id <- matched(data, start, end)
    price_end <-
    prices(data, period = end, set = id)
    price_start <-
    prices(data, period = start, set = id)
    quantity_start <-
    quantities(data, period = start, set = id)
    v0 <-
    sum(price_start * quantity_start)
    sum <-
    sum(price_start * quantity_start / v0 * (price_end / price_start) ^ (1 -
    sigma))
    sum <-
    sum ^ (1 / (1 - sigma))
    return(sum)
    }

#' Calculating a current weight (CW) counterpart of the Lloyd-Moulton price index
#' This function returns the Lloyd-Moulton price index value  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param sigma The elasticity of substitution parameter (as numeric).
#' @noRd

cw <-
    function(data,
    start,
    end,
    sigma
    )  {
    if (start == end)
    return (1)
    if (sigma == 1)
    stop("A specification of the parameter 'sigma' is wrong")
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
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
    id <- matched(data, start, end)
    price_end <-
    prices(data, period = end, set = id)
    price_start <-
    prices(data, period = start, set = id)
    quantity_end <-
    quantities(data, period = end, set = id)
    v1 <-
    sum(price_end * quantity_end)
    sum <-
    sum(price_end * quantity_end / v1 * (price_end / price_start) ^ (sigma -
    1))
    sum <-
    sum ^ (1 / (sigma - 1))
    return(sum)
    }

#' Calculating a numerator of the GEKS formula
#' This function returns a numerator of the GEKS formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geks_num <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gks_num <-
  function (tt) fisher(data, tt, end)
  vec <- sapply(dates, gks_num)
  geks_num <- prod(vec)
  geks_num <- geks_num ^ (1 / window)
  return(geks_num)
  }

#' Calculating a denomerator of the GEKS formula
#' This function returns a denomerator of the GEKS formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geks_denom <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gks_denom <-
  function (tt) fisher(data, tt, start)
  vec <- sapply(dates, gks_denom)
  geks_denom <- prod(vec)
  geks_denom <- geks_denom ^ (1 / window)
  return(geks_denom)
  }


#' Calculating a numerator of the GEKS-W formula
#' This function returns a numerator of the GEKS-W formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geksw_num <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gksw_num <-
  function (tt) walsh(data, tt, end)
  vec <- sapply(dates, gksw_num)
  geksw_num <- prod(vec)
  geksw_num <- geksw_num ^ (1 / window)
  return(geksw_num)
  }

#' Calculating a denomerator of the GEKS-W formula
#' This function returns a denomerator of the GEKS-W formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geksw_denom <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gksw_denom <-
  function (tt) walsh(data, tt, start)
  vec <- sapply(dates, gksw_denom)
  geksw_denom <- prod(vec)
  geksw_denom <- geksw_denom ^ (1 / window)
  return(geksw_denom)
  }

#' Calculating a numerator of the GEKS-J formula
#' This function returns a numerator of the GEKS-J formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geksj_num <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gksj_num <-
  function (tt) jevons(data, tt, end)
  vec <- sapply(dates, gksj_num)
  geksj_num <- prod(vec)
  geksj_num <- geksj_num ^ (1 / window)
  return(geksj_num)
  }

#' Calculating a denomerator of the GEKS-J formula
#' This function returns a denomerator of the GEKS-J formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geksj_denom <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gksj_denom <-
  function (tt) jevons(data, tt, start)
  vec <- sapply(dates, gksj_denom)
  geksj_denom <- prod(vec)
  geksj_denom <- geksj_denom ^ (1 / window)
  return(geksj_denom)
  }

#' Calculating a numerator of the CCDI formula
#' This function returns a numerator of the CCDI formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

ccdi_num <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  ccdi_num <-
  function (tt) tornqvist(data, tt, end)
  vec <- sapply(dates, ccdi_num)
  ccdi_num <- prod(vec)
  ccdi_num <- ccdi_num ^ (1 / window)
  return(ccdi_num)
  }

#' Calculating a denomerator of the CCDI formula
#' This function returns a denomerator of the CCDI formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

ccdi_denom <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  ccdi_denom <-
  function (tt) tornqvist(data, tt, start)
  vec <- sapply(dates, ccdi_denom)
  ccdi_denom <- prod(vec)
  ccdi_denom <- ccdi_denom ^ (1 / window)
  return(ccdi_denom)
  }

#' Calculating a numerator of the WGEKS formula
#' This function returns a numerator of the WGEKS formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

wgeks_num <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  wgks_num <-
  function (tt) fisher(data, start=tt, end=end)
  vec <- sapply(dates, wgks_num)
  sales_in_time <-
  function (tt)
  return (sum(sales(data, tt)))
  expenditures <-
  sapply(dates, sales_in_time)
  expenditures <-
  expenditures / sum(expenditures)
  wgeks_num <-
  prod(vec ^ expenditures)
  return(wgeks_num)
  }


#' Calculating a denomerator of the WGEKS formula
#' This function returns a denomerator of the WGEKS formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

wgeks_denom <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  wgks_denom <-
  function (tt) fisher(data, start=tt, end=start)
  vec <- sapply(dates, wgks_denom)
  sales_in_time <-
  function (tt)
  return (sum(sales(data, tt)))
  expenditures <-
  sapply(dates, sales_in_time)
  expenditures <-
  expenditures / sum(expenditures)
  wgeks_denom <-
  prod(vec ^ expenditures)
  return(wgeks_denom)
  }


#' Calculating a numerator of the GEKS-L formula
#' This function returns a numerator of the GEKS-L formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geksl_num <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gksl_num <-
  function (tt) nl(data, tt, end)
  vec <- sapply(dates, gksl_num)
  geksl_num <- prod(vec)
  geksl_num <- geksl_num ^ (1 / window)
  return(geksl_num)
  }

#' Calculating a denomerator of the GEKS-L formula
#' This function returns a denomerator of the GEKS-L formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geksl_denom <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gksl_denom <-
  function (tt) nl(data, tt, start)
  vec <- sapply(dates, gksl_denom)
  geksl_denom <- prod(vec)
  geksl_denom <- geksl_denom ^ (1 / window)
  return(geksl_denom)
  }

#' Calculating a numerator of the WGEKS-L formula
#' This function returns a numerator of the WGEKS-L formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

wgeksl_num <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  wgksl_num <-
  function (tt) nl(data, start=tt, end=end)
  vec <- sapply(dates, wgksl_num)
  sales_in_time <-
  function (tt)
  return (sum(sales(data, tt)))
  expenditures <-
  sapply(dates, sales_in_time)
  expenditures <-
  expenditures / sum(expenditures)
  wgeksl_num <-
  prod(vec ^ expenditures)
  return(wgeksl_num)
  }


#' Calculating a denomerator of the WGEKS-L formula
#' This function returns a denomerator of the WGEKS-L formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

wgeksl_denom <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  wgksl_denom <-
  function (tt) nl(data, start=tt, end=start)
  vec <- sapply(dates, wgksl_denom)
  sales_in_time <-
  function (tt)
  return (sum(sales(data, tt)))
  expenditures <-
  sapply(dates, sales_in_time)
  expenditures <-
  expenditures / sum(expenditures)
  wgeksl_denom <-
  prod(vec ^ expenditures)
  return(wgeksl_denom)
  }


#' Calculating a numerator of the GEKS-GL formula
#' This function returns a numerator of the GEKS-GL formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geksgl_num <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gksgl_num <-
  function (tt) geolaspeyres(data, tt, end)
  vec <- sapply(dates, gksgl_num)
  geksgl_num <- prod(vec)
  geksgl_num <- geksgl_num ^ (1 / window)
  return(geksgl_num)
  }

#' Calculating a denomerator of the GEKS-GL formula
#' This function returns a denomerator of the GEKS-GL formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geksgl_denom <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gksgl_denom <-
  function (tt) geolaspeyres(data, tt, start)
  vec <- sapply(dates, gksgl_denom)
  geksgl_denom <- prod(vec)
  geksgl_denom <- geksgl_denom ^ (1 / window)
  return(geksgl_denom)
  }

#' Calculating a numerator of the WGEKS-GL formula
#' This function returns a numerator of the WGEKS-GL formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

wgeksgl_num <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  wgksgl_num <-
  function (tt) geolaspeyres(data, start=tt, end=end)
  vec <- sapply(dates, wgksgl_num)
  sales_in_time <-
  function (tt)
  return (sum(sales(data, tt)))
  expenditures <-
  sapply(dates, sales_in_time)
  expenditures <-
  expenditures / sum(expenditures)
  wgeksgl_num <-
  prod(vec ^ expenditures)
  return(wgeksgl_num)
  }


#' Calculating a denomerator of the WGEKS-GL formula
#' This function returns a denomerator of the WGEKS-GL formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

wgeksgl_denom <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  wgksgl_denom <-
  function (tt) geolaspeyres(data, start=tt, end=start)
  vec <- sapply(dates, wgksgl_denom)
  sales_in_time <-
  function (tt)
  return (sum(sales(data, tt)))
  expenditures <-
  sapply(dates, sales_in_time)
  expenditures <-
  expenditures / sum(expenditures)
  wgeksgl_denom <-
  prod(vec ^ expenditures)
  return(wgeksgl_denom)
  }


#' Calculating a numerator of the GEKS-AQU formula
#' This function returns a numerator of the GEKS-AQU formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geksaqu_num <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  prodID<-NULL
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  data<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
  #data frame with quality adjusted factors
  v<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop") 
  #main body
  gksaqu_num <-
  function (tt) aqu(data, start=tt, end=end,v)
  vec <- sapply(dates, gksaqu_num)
  geksaqu_num <- prod(vec)
  geksaqu_num <- geksaqu_num ^ (1 / window)
  return(geksaqu_num)
  }


#' Calculating a denomerator of the GEKS-AQU formula
#' This function returns a denomerator of the GEKS-AQU formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geksaqu_denom <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  prodID<-NULL
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  data<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
  #data frame with quality adjusted factors
  v<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop")
  #main body
  gksaqu_denom <-
  function (tt) aqu(data, start=tt, end=start,v)
  vec <- sapply(dates, gksaqu_denom)
  geksaqu_denom <- prod(vec)
  geksaqu_denom <- geksaqu_denom ^ (1 / window)
  return(geksaqu_denom)
  }

#' Calculating a numerator of the WGEKS-AQU formula
#' This function returns a numerator of the WGEKS-AQU formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

wgeksaqu_num <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  prodID<-NULL
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  data<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
  #data frame with quality adjusted factors
  v<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop")
  #main body
  wgksaqu_num <-
  function (tt) aqu(data, start=tt, end=end,v)
  vec <- sapply(dates, wgksaqu_num)
  sales_in_time <-
  function (tt)
  return (sum(sales(data, tt)))
  expenditures <-
  sapply(dates, sales_in_time)
  expenditures <-
  expenditures / sum(expenditures)
  wgeksaqu_num <- prod(vec ^ expenditures)
  return(wgeksaqu_num)
  }

#' Calculating a denomerator of the WGEKS-AQU formula
#' This function returns a denomerator of the WGEKS-AQU formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

wgeksaqu_denom <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  prodID<-NULL
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  data<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
  #data frame with quality adjusted factors
  v<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop")
  #main body
  wgksaqu_denom <-
  function (tt) aqu(data, start=tt, end=start,v)
  vec <- sapply(dates, wgksaqu_denom)
  sales_in_time <-
  function (tt)
  return (sum(sales(data, tt)))
  expenditures <-
  sapply(dates, sales_in_time)
  expenditures <-
  expenditures / sum(expenditures)
  wgeksaqu_denom <- prod(vec ^ expenditures)
  return(wgeksaqu_denom)
  }

#' Calculating a numerator of the GEKS-AQI formula
#' This function returns a numerator of the GEKS-AQI formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geksaqi_num <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  prodID<-NULL
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  data<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
  #data frame with quality adjusted factors
  v<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop")
  #main body
  gksaqi_num <-
  function (tt) aqi(data, start=tt, end=end,v)
  vec <- sapply(dates, gksaqi_num)
  geksaqi_num <- prod(vec)
  geksaqi_num <- geksaqi_num ^ (1 / window)
  return(geksaqi_num)
  }


#' Calculating a denomerator of the GEKS-AQI formula
#' This function returns a denomerator of the GEKS-AQI formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geksaqi_denom <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  prodID<-NULL
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  data<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
  #data frame with quality adjusted factors
  v<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop")
  #main body
  gksaqi_denom <-
  function (tt) aqi(data, start=tt, end=start,v)
  vec <- sapply(dates, gksaqi_denom)
  geksaqi_denom <- prod(vec)
  geksaqi_denom <- geksaqi_denom ^ (1 / window)
  return(geksaqi_denom)
  }


#' Calculating a numerator of the WGEKS-AQI formula
#' This function returns a numerator of the WGEKS-AQI formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

wgeksaqi_num <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  prodID<-NULL
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  data<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
  #data frame with quality adjusted factors
  v<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop")
  #main body
  wgksaqi_num <-
  function (tt) aqi(data, start=tt, end=end,v)
  vec <- sapply(dates, wgksaqi_num)
  sales_in_time <-
  function (tt)
  return (sum(sales(data, tt)))
  expenditures <-
  sapply(dates, sales_in_time)
  expenditures <-
  expenditures / sum(expenditures)
  wgeksaqi_num <- prod(vec ^ expenditures)
  return(wgeksaqi_num)
  }

#' Calculating a denomerator of the WGEKS-AQI formula
#' This function returns a denomerator of the WGEKS-AQI formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

wgeksaqi_denom <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  prodID<-NULL
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  data<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
  #data frame with quality adjusted factors
  v<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop")
  #main body
  wgksaqi_denom <-
  function (tt) aqi(data, start=tt, end=start,v)
  vec <- sapply(dates, wgksaqi_denom)
  sales_in_time <-
  function (tt)
  return (sum(sales(data, tt)))
  expenditures <-
  sapply(dates, sales_in_time)
  expenditures <-
  expenditures / sum(expenditures)
  wgeksaqi_denom <- prod(vec ^ expenditures)
  return(wgeksaqi_denom)
  }


#' Calculating a numerator of the GEKS-GAQI formula
#' This function returns a numerator of the GEKS-GAQI formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geksgaqi_num <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  prodID<-NULL
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  data<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
  #data frame with quality adjusted factors
  v<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop")
  #main body
  gksgaqi_num <-
  function (tt) gaqi(data, start=tt, end=end,v)
  vec <- sapply(dates, gksgaqi_num)
  geksgaqi_num <- prod(vec)
  geksgaqi_num <- geksgaqi_num ^ (1 / window)
  return(geksgaqi_num)
  }


#' Calculating a denomerator of the GEKS-GAQI formula
#' This function returns a denomerator of the GEKS-GAQI formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geksgaqi_denom <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  prodID<-NULL
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  data<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
  #data frame with quality adjusted factors
  v<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop")
  #main body
  gksgaqi_denom <-
  function (tt) gaqi(data, start=tt, end=start,v)
  vec <- sapply(dates, gksgaqi_denom)
  geksgaqi_denom <- prod(vec)
  geksgaqi_denom <- geksgaqi_denom ^ (1 / window)
  return(geksgaqi_denom)
  }

#' Calculating a numerator of the WGEKS-GAQI formula
#' This function returns a numerator of the WGEKS-GAQI formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

wgeksgaqi_num <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  prodID<-NULL
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  data<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
  #data frame with quality adjusted factors
  v<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop")
  #main body
  wgksgaqi_num <-
  function (tt) gaqi(data, start=tt, end=end,v)
  vec <- sapply(dates, wgksgaqi_num)
  sales_in_time <-
  function (tt)
  return (sum(sales(data, tt)))
  expenditures <-
  sapply(dates, sales_in_time)
  expenditures <-
  expenditures / sum(expenditures)
  wgeksgaqi_num <- prod(vec ^ expenditures)
  return(wgeksgaqi_num)
  }

#' Calculating a denomerator of the WGEKS-GAQI formula
#' This function returns a denomerator of the WGEKS-GAQI formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

wgeksgaqi_denom <-
  function(data,
  start,
  end,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  prodID<-NULL
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  data<-dplyr::filter(data, data$time>=wstart & data$time<=wend)
  #data frame with quality adjusted factors
  v<-dplyr::summarise(dplyr::group_by(data,  prodID),values=sum(prices*quantities)/sum(quantities),.groups="drop")
  #main body
  wgksgaqi_denom <-
  function (tt) gaqi(data, start=tt, end=start,v)
  vec <- sapply(dates, wgksgaqi_denom)
  sales_in_time <-
  function (tt)
  return (sum(sales(data, tt)))
  expenditures <-
  sapply(dates, sales_in_time)
  expenditures <-
  expenditures / sum(expenditures)
  wgeksgaqi_denom <- prod(vec ^ expenditures)
  return(wgeksgaqi_denom)
  }

#' Calculating a numerator of the GEKS-IQM formula
#' This function returns a numerator of the GEKS-IQM formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter used in the implicit quadratic mean of order r price index.
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geksiqm_num <-
  function(data,
  start,
  end,
  r=2,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gksiqm_num <-
  function (tt) IQMp(data, tt, end, r)
  vec <- sapply(dates, gksiqm_num)
  geksiqm_num <- prod(vec)
  geksiqm_num <- geksiqm_num ^ (1 / window)
  return(geksiqm_num)
  }

#' Calculating a denomerator of the GEKS-IQM formula
#' This function returns a denomerator of the GEKS-IQM formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter used in the implicit quadratic mean of order r price index.
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geksiqm_denom <-
  function(data,
  start,
  end,
  r=2,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gksiqm_denom <-
  function (tt) IQMp(data, tt, start, r)
  vec <- sapply(dates, gksiqm_denom)
  geksiqm_denom <- prod(vec)
  geksiqm_denom <- geksiqm_denom ^ (1 / window)
  return(geksiqm_denom)
  }

#' Calculating a numerator of the GEKS-QM formula
#' This function returns a numerator of the GEKS-QM formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter used in the implicit quadratic mean of order r price index.
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geksqm_num <-
  function(data,
  start,
  end,
  r=2,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gksqm_num <-
  function (tt) QMp(data, tt, end, r)
  vec <- sapply(dates, gksqm_num)
  geksqm_num <- prod(vec)
  geksqm_num <- geksqm_num ^ (1 / window)
  return(geksqm_num)
  }

#' Calculating a denomerator of the GEKS-QM formula
#' This function returns a denomerator of the GEKS-QM formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter used in the implicit quadratic mean of order r price index.
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

geksqm_denom <-
  function(data,
  start,
  end,
  r=2,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gksqm_denom <-
  function (tt) QMp(data, tt, start, r)
  vec <- sapply(dates, gksqm_denom)
  geksqm_denom <- prod(vec)
  geksqm_denom <- geksqm_denom ^ (1 / window)
  return(geksqm_denom)
  }


#' Calculating a numerator of the GEKS-LM formula
#' This function returns a numerator of the GEKS-LM formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param sigma The elasticity of substitution (a parameter used in the Lloyd-Moulton index formula).
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

gekslm_num <-
  function(data,
  start,
  end,
  sigma=0.7,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gkslm_num <-
  function (tt) lloyd_moulton(data, tt, end, sigma)
  vec <- sapply(dates, gkslm_num)
  gekslm_num <- prod(vec)
  gekslm_num <- gekslm_num ^ (1 / window)
  return(gekslm_num)
  }

#' Calculating a denomerator of the GEKS-LM formula
#' This function returns a denomerator of the GEKS-LM formula  
#' @param data The user's data frame with information about products to be filtered. It must contain columns: \code{time} (as Date in format: year-month-day, e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{quantities}  (as positive numeric).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param sigma The elasticity of substitution (a parameter used in the Lloyd-Moulton index formula).
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @noRd

gekslm_denom <-
  function(data,
  start,
  end,
  sigma=2,
  wstart = start,
  window = 13)  {
  if (start == end)
  return (1)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  #main body
  gkslm_denom <-
  function (tt) lloyd_moulton(data, tt, start, sigma)
  vec <- sapply(dates, gkslm_denom)
  gekslm_denom <- prod(vec)
  gekslm_denom <- gekslm_denom ^ (1 / window)
  return(gekslm_denom)
  }

#' A general function to compute a final price index
#' This function returns a value of the selected final price index for the selected type of aggregation of partial results. 
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also essential even if the selected index is an unweighted formula (unit values are calculated). 
#' @param start The base period (as character) limited to the year and month, e.g. "2019-12".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param formula The character string indicating the price index formula is to be calculated. To see available options please use the link: \code{\link{PriceIndices}}.
#' @param window The length of the time window if the multilateral index is selected (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param splice A character string indicating the splicing method (if the multilateral splicing index is selected). Available options are: "movement", "window","half","mean" and their additional variants: "window_published", "half_published" and "mean_published". 
#' @param base The prior period used in the Young- or Lowe-type price indices (as character) limited to the year and month, e.g. "2020-01".
#' @param sigma The elasticity of substitution parameter used in the Lloyed-Moulton, AG Mean or GEKS-LM indices (as numeric).
#' @param r The non-zero parameter used in the quadratic mean of order r quantity / price index or in the GEKS-QM index (as numeric).
#' @param outlets A logical parameter indicating whether the aggregation over outlets (defined in 'retID' column) should be done.
#' @param groups A logical parameter indicating whether the aggregation over product subgroups (indicated by 'by' parameter) should be done.
#' @param by A character string which indicates a column name for creating product subgroups.
#' @param aggr The formula used for aggregating partial index results (available values are: "arithmetic", "geometric", "laspeyres", "paasche", "fisher", "tornqvist").
#' @noRd

final_index2 <-
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
  aggr = "fisher"
  )
  {
    #checking 'by'
    if (groups==TRUE) {
    if (length(by)==0) stop("You must indicate a column for grouping (see 'by' parameter)!")
    av_col<-colnames(data)
    if (!(by %in% av_col)) stop("Bad specification of the 'by' parameter!")
    colnames(data)[which(colnames(data)==by)]<-"groupID"
    }
    av_aggr<-c("arithmetic","geometric","laspeyres","paasche","fisher","tornqvist")
    if (!(aggr %in% av_aggr)) stop("Bad specification of the 'aggr' parameter!")
    
    #main body
    #no aggregation
    if ((groups==FALSE) & (outlets==FALSE)) 
    return (price_indices(data, start, end, formula, window, splice, base, sigma, r, interval=FALSE)$value)  
  
    #results depend on type of aggregation
    if ((groups==TRUE) & (outlets==FALSE)) df<-split(data, data$groupID)
    if ((groups==FALSE) & (outlets==TRUE)) df<-split(data, data$retID)
    if ((groups==TRUE) & (outlets==TRUE)) df<-split(data, list(data$groupID,data$retID))
    
    #cleaning df
    df<-df[lapply(df,nrow)>0]

    #indices
    indices<-c()
    for (i in 1:length(df)) 
      indices<-c(indices, price_indices(df[[i]], start, end, 
                                       formula, window, splice, base, 
                                       sigma, r, interval=FALSE)$value) 
    #weights
    weights_start<-c()
    weights_end<-c()
    d<-data.frame()
    if ((aggr=="fisher") | (aggr=="tornqvist")){
    for (i in 1:length(df)) {
      d<-df[[i]]
      d$time<-as.character(d$time)
      d$time<-substr(d$time,0,7)
      frame_start<-dplyr::filter(d, d$time==start)
      frame_end<-dplyr::filter(d, d$time==end)
      weights_start<-c(weights_start, sum(frame_start$prices*frame_start$quantities))
      weights_end<-c(weights_end, sum(frame_end$prices*frame_end$quantities))
                                    }  
    weights_start<-weights_start/sum(weights_start)
    weights_end<-weights_end/sum(weights_end)          }
    if (aggr=="laspeyres")                             {
    for (i in 1:length(df)) {
      d<-df[[i]]
      d$time<-as.character(d$time)
      d$time<-substr(d$time,0,7)
      frame_start<-dplyr::filter(d, d$time==start)
      weights_start<-c(weights_start, sum(frame_start$prices*frame_start$quantities))
      
                                    }  
    weights_start<-weights_start/sum(weights_start)
                                                      }
    if (aggr=="paasche")                              {
    for (i in 1:length(df)) {
      d<-df[[i]]
      d$time<-as.character(d$time)
      d$time<-substr(d$time,0,7)
      frame_end<-dplyr::filter(d, d$time==end)
      weights_end<-c(weights_end, sum(frame_end$prices*frame_end$quantities))
      
                                    }  
    weights_end<-weights_end/sum(weights_end)
                                                     }
    #final result depending on type of aggregation
    if (aggr=="laspeyres") return (sum(weights_start*indices))
    if (aggr=="paasche")   return (1/sum(weights_end*(1/indices)))                       
    if (aggr=="fisher")   return (((1/sum(weights_end*(1/indices)))*sum(weights_start*indices))^0.5) 
    if (aggr=="tornqvist") return (prod(indices^(0.5*(weights_start+weights_end))))
    if (aggr=="arithmetic") return (mean(indices))
    if (aggr=="geometric") return ((prod(indices))^(1/length(indices)))
    }


#' An additional function used in the 'bennet' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical parameter indicating whether calculations are to be made for the whole time interval (TRUE) or no (FALSE).
#' @param contributions A logical parameter indicating whether contributions of individual products are to be displayed. If it is \code{TRUE}, then contributions are calculated for the the base period \code{start} and the current period \code{end}.
#' @param prec A numeric vector indicating precision, i.e. the number of decimal places for presenting results.
#' @noRd

bennet_internal <-
  function(data,
  start,
  end,
  interval=FALSE,
  contributions=FALSE,
  prec=2)  {
  if (start == end)
  return (0)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- seq.Date(from = start, to = end, by = "month")
  pq<-function (tm) {frame<-prices(data, period=tm, ID=TRUE)
  frame$q<-quantities(data, period=tm, ID=FALSE)
  return (frame)}
  pq_list<-lapply(dates, pq)  
  #main body
  k<-2
  n<-length(dates)
  bt<-function (tt) 
  {
  setID<-union(pq_list[[1]]$by,pq_list[[tt]]$by)  #union of IDs
  #helping functions
  pq_help_start<-function (id) {
    if (!(id %in% pq_list[[1]]$by)) return (c(0,0))
    else {
      df_help<-dplyr::filter(pq_list[[1]],by==id)
      return (c(df_help$uv,df_help$q))
    }
  }
  pq_help_tt<-function (id) {
    if (!(id %in% pq_list[[tt]]$by)) return (c(0,0))
    else {
      df_help<-dplyr::filter(pq_list[[tt]],by==id)
      return (c(df_help$uv,df_help$q))
    }
  }
  pq_start<-sapply(setID,pq_help_start)
  pq_tt<-sapply(setID,pq_help_tt)
  p_start<-pq_start[1,]
  p_tt<-pq_tt[1,]
  q_start<-pq_start[2,]
  q_tt<-pq_tt[2,]
  #contributions
  price_contributions<-(0.5*(q_start+q_tt))*(p_tt-p_start)
  quantity_contributions<-(0.5*(p_start+p_tt))*(q_tt-q_start)
  value_differences<-p_tt*q_tt-p_start*q_start
  #indicators
  price_indicator<-sum(price_contributions)
  quantity_indicator<-sum(quantity_contributions)
  value_difference<-sum(value_differences)
  #returning list
  return (list(setID,round(value_differences,prec),
               round(price_contributions,prec),
               round(quantity_contributions,prec),
               round(value_difference,prec),
               round(price_indicator,prec),
               round(quantity_indicator,prec)))
  }
  if (contributions==TRUE)
    return (data.frame(row.names=NULL,
                      prodID=bt(n)[[1]],
                      value_differences=bt(n)[[2]],
                      price_contributions=bt(n)[[3]],
                      quantity_contributions=bt(n)[[4]]))
  else {
  if (interval==FALSE) k<-n
  dates<-dates[k:n]
  dates<-as.character(dates)
  dates<-substr(dates,0,7)
  v_diff<-c()
  p_ind<-c()
  q_ind<-c()
  for (period in k:n) {
    lista<-bt(period)
    v_diff<-c(v_diff,lista[[5]])
    p_ind<-c(p_ind, lista[[6]])
    q_ind<-c(q_ind, lista[[7]])
  }
  df_result<-data.frame()
  if (interval==FALSE) df_result<-data.frame(
                     Value_difference=v_diff, 
                     Price_indicator=p_ind,
                     Quantity_indicator=q_ind)
  else df_result<-data.frame(time=dates,
                     Value_difference=v_diff, 
                     Price_indicator=p_ind,
                     Quantity_indicator=q_ind)
  return (df_result) 
  }
  }

#' An additional function used in the 'bennet' function for matched products
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical parameter indicating whether calculations are to be made for the whole time interval (TRUE) or no (FALSE).
#' @param contributions A logical parameter indicating whether contributions of individual products are to be displayed. If it is \code{TRUE}, then contributions are calculated for the the base period \code{start} and the current period \code{end}.
#' @param prec A numeric vector indicating precision, i.e. the number of decimal places for presenting results.
#' @noRd

bennet_matched_internal <-
  function(data,
  start,
  end,
  interval=FALSE,
  contributions=FALSE,
  prec=2)  {
  if (start == end)
  return (0)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- seq.Date(from = start, to = end, by = "month")
  p<-function (tm) prices(data, period=tm, ID=TRUE)
  q<-function (tm) quantities(data, period=tm, ID=TRUE)
  p_list<-lapply(dates, p)  
  q_list<-lapply(dates, q)
  #main body
  k<-2
  n<-length(dates)
  bt<-function (tt) 
  {
  setID<-intersect(p_list[[1]]$by,p_list[[tt]]$by)  #intersection of IDs
  p_start<-dplyr::filter(p_list[[1]], by %in% setID)$uv
  p_tt<-dplyr::filter(p_list[[tt]], by %in% setID)$uv
  q_start<-dplyr::filter(q_list[[1]], by %in% setID)$q
  q_tt<-dplyr::filter(q_list[[tt]], by %in% setID)$q
  #resulting list
  #contributions
  price_contributions<-(0.5*(q_start+q_tt))*(p_tt-p_start)
  quantity_contributions<-(0.5*(p_start+p_tt))*(q_tt-q_start)
  value_differences<-p_tt*q_tt-p_start*q_start
  #indicators
  price_indicator<-sum(price_contributions)
  quantity_indicator<-sum(quantity_contributions)
  value_difference<-sum(value_differences)
  #returning list
  return (list(setID,round(value_differences,prec),
               round(price_contributions,prec),
               round(quantity_contributions,prec),
               round(value_difference,prec),
               round(price_indicator,prec),
               round(quantity_indicator,prec)))
  }
  if (contributions==TRUE)
    return (data.frame(row.names=NULL,
                      prodID=bt(n)[[1]],
                      value_differences=bt(n)[[2]],
                      price_contributions=bt(n)[[3]],
                      quantity_contributions=bt(n)[[4]]))
  else {
  if (interval==FALSE) k<-n
  dates<-dates[k:n]
  dates<-as.character(dates)
  dates<-substr(dates,0,7)
  v_diff<-c()
  p_ind<-c()
  q_ind<-c()
  for (period in k:n) {
    lista<-bt(period)
    v_diff<-c(v_diff,lista[[5]])
    p_ind<-c(p_ind, lista[[6]])
    q_ind<-c(q_ind, lista[[7]])
  }
  df_result<-data.frame()
  if (interval==FALSE) df_result<-data.frame(
                     Value_difference=v_diff, 
                     Price_indicator=p_ind,
                     Quantity_indicator=q_ind)
  else df_result<-data.frame(time=dates,
                     Value_difference=v_diff, 
                     Price_indicator=p_ind,
                     Quantity_indicator=q_ind)
  return (df_result)    
  }
  }

#' An additional function used in the 'mbennet' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The first period of the time window (as character) limited to the year and month, e.g. "2019-12".
#' @param matched A logical parameter indicating whether the matched sample approach is to be used (if yes, the parameter has the value TRUE).
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param interval A logical parameter indicating whether calculations are to be made for the whole time interval (TRUE) or no (FALSE).
#' @param contributions A logical parameter indicating whether contributions of individual products are to be displayed. If it is \code{TRUE}, then contributions are calculated for the the base period \code{start} and the current period \code{end}.
#' @param prec A numeric vector indicating precision, i.e. the number of decimal places for presenting results.
#' @noRd

mbennet_internal <-
  function(data,
  start,
  end,
  wstart=start,
  matched=FALSE,
  window=13,
  contributions=FALSE,
  prec=2)  {
  if (start == end)
  return (0)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  price_contributions<-NULL
  quantity_contributions<-NULL
  value_differences<-NULL
  no_start<-dist(wstart, start)+1
  no_end<-dist(wstart, end)+1
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  setID<-NULL
  if (matched==FALSE) setID<-available(data, period1=wstart, period2=wend, interval=TRUE)
  else setID<-matched(data, period1=wstart, period2=wend, interval=TRUE)  
  #frames with all prices and quantities for all periods
  pq<-function (tm) {frame<-prices(data, period=tm, ID=TRUE)
  frame$q<-quantities(data, period=tm, ID=FALSE)
  return (frame)}
  pq_list<-lapply(dates, pq)  
  #main body
  n<-window
  bt<-function (tt) 
  {
  #helping functions
  pq_help_start<-function (id) {
    if (!(id %in% pq_list[[no_start]]$by)) return (c(0,0))
    else {
      df_help_start<-dplyr::filter(pq_list[[no_start]],by==id)
      return (c(df_help_start$uv,df_help_start$q))
    }
  }
  pq_help_tt<-function (id) {
    if (!(id %in% pq_list[[tt]]$by)) return (c(0,0))
    else {
      df_help_tt<-dplyr::filter(pq_list[[tt]],by==id)
      return (c(df_help_tt$uv,df_help_tt$q))
    }
  }
  pq_help_end<-function (id) {
    if (!(id %in% pq_list[[no_end]]$by)) return (c(0,0))
    else {
      df_help_end<-dplyr::filter(pq_list[[no_end]],by==id)
      return (c(df_help_end$uv,df_help_end$q))
    }
  }  
  pq_start<-sapply(setID,pq_help_start)
  pq_tt<-sapply(setID,pq_help_tt)
  pq_end<-sapply(setID,pq_help_end)
  p_start<-pq_start[1,]
  p_tt<-pq_tt[1,]
  p_end<-pq_end[1,]
  q_start<-pq_start[2,]
  q_tt<-pq_tt[2,]
  q_end<-pq_end[2,]
  #contributions
  price_contributions<-0.5*(1/window)*(p_end*q_end-p_start*q_start+q_tt*(p_end-p_start)-p_tt*(q_end-q_start))
  quantity_contributions<-0.5*(1/window)*(p_end*q_end-p_start*q_start+p_tt*(q_end-q_start)-q_tt*(p_end-p_start))
  value_differences<-price_contributions+quantity_contributions
  return (data.frame(setID, value_differences,price_contributions,quantity_contributions))
  }
  list_df<-lapply(seq(1,n),bt)
  list_df<-dplyr::bind_rows(list_df)
  list_df<-dplyr::summarise(dplyr::group_by(list_df,by=setID),
                      value_differences=sum(value_differences),
                      price_contributions=sum(price_contributions),
                      quantity_contributions=sum(quantity_contributions),
                      .groups="drop")
  if (contributions==TRUE) {
    list_df$value_differences<-round(list_df$value_differences,prec)
    list_df$price_contributions<-round(list_df$price_contributions,prec)
    list_df$quantity_contributions<-round(list_df$quantity_contributions,prec)
    return (list_df)}
  else return (data.frame(
                     Value_difference=round(sum(list_df$value_differences),prec), 
                     Price_indicator=round(sum(list_df$price_contributions),prec),
                     Quantity_indicator=round(sum(list_df$quantity_contributions),prec)))    
  }


#' An additional function used in the 'montgomery' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical parameter indicating whether calculations are to be made for the whole time interval (TRUE) or no (FALSE).
#' @param contributions A logical parameter indicating whether contributions of individual products are to be displayed. If it is \code{TRUE}, then contributions are calculated for the the base period \code{start} and the current period \code{end}.
#' @param prec A numeric vector indicating precision, i.e. the number of decimal places for presenting results.
#' @noRd

montgomery_internal <-
  function(data,
  start,
  end,
  interval=FALSE,
  contributions=FALSE,
  prec=2)  {
  if (start == end)
  return (0)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- seq.Date(from = start, to = end, by = "month")
  pq<-function (tm) {frame<-prices(data, period=tm, ID=TRUE)
  frame$q<-quantities(data, period=tm, ID=FALSE)
  return (frame)}
  pq_list<-lapply(dates, pq)  
  #main body
  k<-2
  n<-length(dates)
  bt<-function (tt) 
  {
  setID<-union(pq_list[[1]]$by,pq_list[[tt]]$by)  #union of IDs
  #helping functions
  pq_help_start<-function (id) {
    if (!(id %in% pq_list[[1]]$by)) return (c(0.000001,0.000001))
    else {
      df_help<-dplyr::filter(pq_list[[1]],by==id)
      return (c(df_help$uv,df_help$q))
    }
  }
  pq_help_tt<-function (id) {
    if (!(id %in% pq_list[[tt]]$by)) return (c(0.000001,0.000001))
    else {
      df_help<-dplyr::filter(pq_list[[tt]],by==id)
      return (c(df_help$uv,df_help$q))
    }
  }
  pq_start<-sapply(setID,pq_help_start)
  pq_tt<-sapply(setID,pq_help_tt)
  p_start<-pq_start[1,]
  p_tt<-pq_tt[1,]
  q_start<-pq_start[2,]
  q_tt<-pq_tt[2,]
  #contributions
  price_contributions<-Lv(p_start*q_start, p_tt*q_tt)*(log(p_tt)-log(p_start))
  quantity_contributions<-Lv(p_start*q_start, p_tt*q_tt)*(log(q_tt)-log(q_start))
  value_differences<-price_contributions+quantity_contributions
  #value_differences<-p_tt*q_tt-p_start*q_start
  #indicators
  price_indicator<-sum(price_contributions)
  quantity_indicator<-sum(quantity_contributions)
  value_difference<-sum(value_differences)
  #returning list
  return (list(setID,round(value_differences,prec),
               round(price_contributions,prec),
               round(quantity_contributions,prec),
               round(value_difference,prec),
               round(price_indicator,prec),
               round(quantity_indicator,prec)))
  }
  if (contributions==TRUE)
    return (data.frame(row.names=NULL,
                      prodID=bt(n)[[1]],
                      value_differences=bt(n)[[2]],
                      price_contributions=bt(n)[[3]],
                      quantity_contributions=bt(n)[[4]]))
  else {
  if (interval==FALSE) k<-n
  dates<-dates[k:n]
  dates<-as.character(dates)
  dates<-substr(dates,0,7)
  v_diff<-c()
  p_ind<-c()
  q_ind<-c()
  for (period in k:n) {
    lista<-bt(period)
    v_diff<-c(v_diff,lista[[5]])
    p_ind<-c(p_ind, lista[[6]])
    q_ind<-c(q_ind, lista[[7]])
  }
  df_result<-data.frame()
  if (interval==FALSE) df_result<-data.frame(
                     Value_difference=v_diff, 
                     Price_indicator=p_ind,
                     Quantity_indicator=q_ind)
  else df_result<-data.frame(time=dates,
                     Value_difference=v_diff, 
                     Price_indicator=p_ind,
                     Quantity_indicator=q_ind)
  return (df_result) 
  }
  }


#' An additional function used in the 'montgomery' function for matched products
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param interval A logical parameter indicating whether calculations are to be made for the whole time interval (TRUE) or no (FALSE).
#' @param contributions A logical parameter indicating whether contributions of individual products are to be displayed. If it is \code{TRUE}, then contributions are calculated for the the base period \code{start} and the current period \code{end}.
#' @param prec A numeric vector indicating precision, i.e. the number of decimal places for presenting results.
#' @noRd

montgomery_matched_internal <-
  function(data,
  start,
  end,
  interval=FALSE,
  contributions=FALSE,
  prec=2)  {
  if (start == end)
  return (0)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- seq.Date(from = start, to = end, by = "month")
  p<-function (tm) prices(data, period=tm, ID=TRUE)
  q<-function (tm) quantities(data, period=tm, ID=TRUE)
  p_list<-lapply(dates, p)  
  q_list<-lapply(dates, q)
  #main body
  k<-2
  n<-length(dates)
  bt<-function (tt) 
  {
  setID<-intersect(p_list[[1]]$by,p_list[[tt]]$by)  #intersection of IDs
  p_start<-dplyr::filter(p_list[[1]], by %in% setID)$uv
  p_tt<-dplyr::filter(p_list[[tt]], by %in% setID)$uv
  q_start<-dplyr::filter(q_list[[1]], by %in% setID)$q
  q_tt<-dplyr::filter(q_list[[tt]], by %in% setID)$q
  #resulting list
  #contributions
  price_contributions<-Lv(p_start*q_start, p_tt*q_tt)*(log(p_tt)-log(p_start))
  quantity_contributions<-Lv(p_start*q_start, p_tt*q_tt)*(log(q_tt)-log(q_start))
  value_differences<-price_contributions+quantity_contributions
  #value_differences<-p_tt*q_tt-p_start*q_start
  #indicators
  price_indicator<-sum(price_contributions)
  quantity_indicator<-sum(quantity_contributions)
  value_difference<-sum(value_differences)
  #returning list
  return (list(setID,round(value_differences,prec),
               round(price_contributions,prec),
               round(quantity_contributions,prec),
               round(value_difference,prec),
               round(price_indicator,prec),
               round(quantity_indicator,prec)))
  }
  if (contributions==TRUE)
    return (data.frame(row.names=NULL,
                      prodID=bt(n)[[1]],
                      value_differences=bt(n)[[2]],
                      price_contributions=bt(n)[[3]],
                      quantity_contributions=bt(n)[[4]]))
  else {
  if (interval==FALSE) k<-n
  dates<-dates[k:n]
  dates<-as.character(dates)
  dates<-substr(dates,0,7)
  v_diff<-c()
  p_ind<-c()
  q_ind<-c()
  for (period in k:n) {
    lista<-bt(period)
    v_diff<-c(v_diff,lista[[5]])
    p_ind<-c(p_ind, lista[[6]])
    q_ind<-c(q_ind, lista[[7]])
  }
  df_result<-data.frame()
  if (interval==FALSE) df_result<-data.frame(
                     Value_difference=v_diff, 
                     Price_indicator=p_ind,
                     Quantity_indicator=q_ind)
  else df_result<-data.frame(time=dates,
                     Value_difference=v_diff, 
                     Price_indicator=p_ind,
                     Quantity_indicator=q_ind)
  return (df_result)    
  }
  }

#' An additional function used in the 'mmontgomery' function
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} (as positive numeric) is also needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The first period of the time window (as character) limited to the year and month, e.g. "2019-12".
#' @param matched A logical parameter indicating whether the matched sample approach is to be used (if yes, the parameter has the value TRUE).
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @param interval A logical parameter indicating whether calculations are to be made for the whole time interval (TRUE) or no (FALSE).
#' @param contributions A logical parameter indicating whether contributions of individual products are to be displayed. If it is \code{TRUE}, then contributions are calculated for the the base period \code{start} and the current period \code{end}.
#' @param prec A numeric vector indicating precision, i.e. the number of decimal places for presenting results.
#' @noRd

mmontgomery_internal <-
  function(data,
  start,
  end,
  wstart=start,
  matched=FALSE,
  window=13,
  contributions=FALSE,
  prec=2)  {
  if (start == end)
  return (0)
  if (nrow(data) == 0)
  stop("A data frame is empty")
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <-
  paste(wstart, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  wstart <- as.Date(wstart)
  #checking conditions
  if (window < 2)
  stop("window must be at least 2 months")
  if (start > end)
  stop("parameters must satisfy: start<=end")
  if (wstart > start)
  stop("parameters must satisfy: wstat<=start")
  wend <- wstart
  lubridate::month(wend) <-
  lubridate::month(wend) + window - 1
  if (end > wend)
  stop("parameters must satisfy: end<wstart+window")
  price_contributions<-NULL
  quantity_contributions<-NULL
  value_differences<-NULL
  no_start<-dist(wstart, start)+1
  no_end<-dist(wstart, end)+1
  start <- substr(start, 0, 7)
  end <- substr(end, 0, 7)
  dates <- seq.Date(from = wstart, to = wend, by = "month")
  dates<-substr(dates, 0, 7)
  setID<-NULL
  if (matched==FALSE) setID<-available(data, period1=wstart, period2=wend, interval=TRUE)
  else setID<-matched(data, period1=wstart, period2=wend, interval=TRUE)  
  #frames with all prices and quantities for all periods
  pq<-function (tm) {frame<-prices(data, period=tm, ID=TRUE)
  frame$q<-quantities(data, period=tm, ID=FALSE)
  return (frame)}
  pq_list<-lapply(dates, pq)  
  #main body
  n<-window
  bt<-function (tt) 
  {
  #helping functions
  pq_help_start<-function (id) {
    if (!(id %in% pq_list[[no_start]]$by)) return (c(0.000001,0.000001))
    else {
      df_help_start<-dplyr::filter(pq_list[[no_start]],by==id)
      return (c(df_help_start$uv,df_help_start$q))
    }
  }
  pq_help_tt<-function (id) {
    if (!(id %in% pq_list[[tt]]$by)) return (c(0.000001,0.000001))
    else {
      df_help_tt<-dplyr::filter(pq_list[[tt]],by==id)
      return (c(df_help_tt$uv,df_help_tt$q))
    }
  }
  pq_help_end<-function (id) {
    if (!(id %in% pq_list[[no_end]]$by)) return (c(0.000001,0.000001))
    else {
      df_help_end<-dplyr::filter(pq_list[[no_end]],by==id)
      return (c(df_help_end$uv,df_help_end$q))
    }
  }  
  pq_start<-sapply(setID,pq_help_start)
  pq_tt<-sapply(setID,pq_help_tt)
  pq_end<-sapply(setID,pq_help_end)
  p_start<-pq_start[1,]
  p_tt<-pq_tt[1,]
  p_end<-pq_end[1,]
  q_start<-pq_start[2,]
  q_tt<-pq_tt[2,]
  q_end<-pq_end[2,]
  #contributions
  price_contributions<-(1/window)*(Lv(p_tt*q_tt,p_end*q_end)*(log(p_end)-log(p_tt))-Lv(p_tt*q_tt,p_start*q_start)*(log(p_start)-log(p_tt)))
  quantity_contributions<-(1/window)*(Lv(p_tt*q_tt,p_end*q_end)*(log(q_end)-log(q_tt))-Lv(p_tt*q_tt,p_start*q_start)*(log(q_start)-log(q_tt)))
  value_differences<-price_contributions+quantity_contributions
  return (data.frame(setID, value_differences,price_contributions,quantity_contributions))
  }
  list_df<-lapply(seq(1,n),bt)
  list_df<-dplyr::bind_rows(list_df)
  list_df<-dplyr::summarise(dplyr::group_by(list_df,by=setID),
                      value_differences=sum(value_differences),
                      price_contributions=sum(price_contributions),
                      quantity_contributions=sum(quantity_contributions),
                      .groups="drop")
  if (contributions==TRUE) {
    list_df$value_differences<-round(list_df$value_differences,prec)
    list_df$price_contributions<-round(list_df$price_contributions,prec)
    list_df$quantity_contributions<-round(list_df$quantity_contributions,prec)
    return (list_df)}
  else return (data.frame(
                     Value_difference=round(sum(list_df$value_differences),prec), 
                     Price_indicator=round(sum(list_df$price_contributions),prec),
                     Quantity_indicator=round(sum(list_df$quantity_contributions),prec)))    
  }