#' @title  Calculating the multilateral GEKS price index
#'
#' @description This function returns a value of the multilateral GEKS price index (to be more precise: the GEKS index based on the Fisher formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geks
#' @return This function returns a value of the multilateral GEKS price index (to be more precise: the GEKS index based on the Fisher formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples
#' \donttest{
#' geks(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' geks(milk, start = "2018-12", end = "2019-12")
#' }
#' @export

geks <-
  function(
      data,
      start,
      end,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # distances of start and end from wstart plus 1
    d_start <- dist(wstart, start) + 1
    d_end <- dist(wstart, end) + 1
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstat<=start")
    }
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    start <- substr(start, 0, 7)
    end <- substr(end, 0, 7)
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    p <- function(tm) prices(data, period = tm, ID = TRUE)
    q <- function(tm) quantities(data, period = tm, ID = TRUE)
    p_list <- lapply(dates, p)
    q_list <- lapply(dates, q)
    # main body
    gks <- function(tt) {
      if (tt == d_end) {
        ID_tt_start <- intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
        pm_start <- dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
        pm_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
        qm_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
        qm_start <- dplyr::filter(q_list[[d_start]], by %in% ID_tt_start)$q
        pm_F <- sum(qm_tt * pm_start) * sum(qm_start * pm_start) / (sum(qm_tt * pm_tt) * sum(qm_start * pm_tt))
        return((1 / pm_F)^0.5)
      }
      if (tt == d_start) {
        ID_tt_end <- intersect(p_list[[tt]]$by, p_list[[d_end]]$by)
        pl_end <- dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
        pl_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
        ql_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
        ql_end <- dplyr::filter(q_list[[d_end]], by %in% ID_tt_end)$q
        pl_F <- sum(ql_tt * pl_end) * sum(ql_end * pl_end) / (sum(ql_tt * pl_tt) * sum(ql_end * pl_tt))
        return(pl_F^0.5)
      }
      if ((!(tt == d_end)) & (!(tt == d_start))) {
        ID_tt_start <- intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
        ID_tt_end <- intersect(p_list[[tt]]$by, p_list[[d_end]]$by)
        pl_end <- dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
        pl_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
        ql_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
        ql_end <- dplyr::filter(q_list[[d_end]], by %in% ID_tt_end)$q
        pm_start <- dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
        pm_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
        qm_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
        qm_start <- dplyr::filter(q_list[[d_start]], by %in% ID_tt_start)$q
        pl_F <- sum(ql_tt * pl_end) * sum(ql_end * pl_end) / (sum(ql_tt * pl_tt) * sum(ql_end * pl_tt))
        pm_F <- sum(qm_tt * pm_start) * sum(qm_start * pm_start) / (sum(qm_tt * pm_tt) * sum(qm_start * pm_tt))
        return((pl_F / pm_F)^0.5)
      }
    }
    geks <- sapply(seq(1, window), gks)
    geks <- prod(geks)^(1 / window)
    return(geks)
  }

#' @title  Calculating the multilateral GEKS price index based on the Walsh formula (GEKS-W)
#'
#' @description This function returns a value of the multilateral GEKS-W price index, i.e. the GEKS price index based on the superlative Walsh index formula.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksw
#' @return This function returns a value of the multilateral GEKS-W price index (to be more precise: the GEKS index based on the Walsh formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
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
#' \donttest{
#' geksw(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' geksw(milk, start = "2018-12", end = "2019-12")
#' }
#' @export

geksw <-
  function(
      data,
      start,
      end,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # distances of start and end from wstart plus 1
    d_start <- dist(wstart, start) + 1
    d_end <- dist(wstart, end) + 1
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstat<=start")
    }
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    start <- substr(start, 0, 7)
    end <- substr(end, 0, 7)
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    p <- function(tm) prices(data, period = tm, ID = TRUE)
    q <- function(tm) quantities(data, period = tm, ID = TRUE)
    p_list <- lapply(dates, p)
    q_list <- lapply(dates, q)
    # main body
    gksw <- function(tt) {
      if (tt == d_end) {
        ID_tt_start <- intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
        pm_start <- dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
        pm_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
        qm_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
        qm_start <- dplyr::filter(q_list[[d_start]], by %in% ID_tt_start)$q
        pm_W <- sum(pm_start * (qm_tt * qm_start)^0.5) / sum(pm_tt * (qm_tt * qm_start)^0.5)
        return(1 / pm_W)
      }
      if (tt == d_start) {
        ID_tt_end <- intersect(p_list[[tt]]$by, p_list[[d_end]]$by)
        pl_end <- dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
        pl_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
        ql_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
        ql_end <- dplyr::filter(q_list[[d_end]], by %in% ID_tt_end)$q
        pl_W <- sum(pl_end * (ql_tt * ql_end)^0.5) / sum(pl_tt * (ql_tt * ql_end)^0.5)
        return(pl_W)
      }
      if ((!(tt == d_end)) & (!(tt == d_start))) {
        ID_tt_start <- intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
        ID_tt_end <- intersect(p_list[[tt]]$by, p_list[[d_end]]$by)
        pl_end <- dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
        pl_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
        ql_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
        ql_end <- dplyr::filter(q_list[[d_end]], by %in% ID_tt_end)$q
        pm_start <- dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
        pm_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
        qm_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
        qm_start <- dplyr::filter(q_list[[d_start]], by %in% ID_tt_start)$q
        pl_W <- sum(pl_end * (ql_tt * ql_end)^0.5) / sum(pl_tt * (ql_tt * ql_end)^0.5)
        pm_W <- sum(pm_start * (qm_tt * qm_start)^0.5) / sum(pm_tt * (qm_tt * qm_start)^0.5)
        return(pl_W / pm_W)
      }
    }
    geksw <- sapply(seq(1, window), gksw)
    geksw <- prod(geksw)^(1 / window)
    return(geksw)
  }
#' @title  Calculating the multilateral GEKS price index based on the Jevons formula (typical notation: GEKS-J)
#'
#' @description This function returns a value of the multilateral GEKS-J price index (to be more precise: the GEKS index based on the Jevons formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character). A column \code{quantities} is needed because this function uses unit values as monthly prices.
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksj
#' @return This function returns a value of the multilateral GEKS-J price index (to be more precise: the GEKS index based on the Jevons formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples
#' \donttest{
#' geksj(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' geksj(milk, start = "2018-12", end = "2019-12")
#' }
#' @export

geksj <-
  function(
      data,
      start,
      end,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # distances of start and end from wstart plus 1
    d_start <- dist(wstart, start) + 1
    d_end <- dist(wstart, end) + 1
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstat<=start")
    }
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    start <- substr(start, 0, 7)
    end <- substr(end, 0, 7)
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    p <- function(tm) prices(data, period = tm, ID = TRUE)
    q <- function(tm) quantities(data, period = tm, ID = TRUE)
    p_list <- lapply(dates, p)
    q_list <- lapply(dates, q)
    # main body
    gksj <- function(tt) {
      if (tt == d_end) {
        ID_tt_start <- intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
        pm_start <- dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
        pm_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
        pm_J <- prod((pm_start / pm_tt)^(1 / length(pm_start)))
        return(1 / pm_J)
      }
      if (tt == d_start) {
        ID_tt_end <- intersect(p_list[[tt]]$by, p_list[[d_end]]$by)
        pl_end <- dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
        pl_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
        pl_J <- prod((pl_end / pl_tt)^(1 / length(pl_end)))
        return(pl_J)
      }
      if ((!(tt == d_end)) & (!(tt == d_start))) {
        ID_tt_start <- intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
        ID_tt_end <- intersect(p_list[[tt]]$by, p_list[[d_end]]$by)
        pl_end <- dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
        pl_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
        pm_start <- dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
        pm_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
        pl_J <- prod((pl_end / pl_tt)^(1 / length(pl_end)))
        pm_J <- prod((pm_start / pm_tt)^(1 / length(pm_start)))
        return(pl_J / pm_J)
      }
    }
    geksj <- sapply(seq(1, window), gksj)
    geksj <- prod(geksj)^(1 / window)
    return(geksj)
  }

#' @title  Calculating the multilateral GEKS price index based on the Tornqvist formula (typical notation: GEKS-T or CCDI)
#'
#' @description This function returns a value of the multilateral CCDI price index, i.e. the GEKS price index based on the superlative Tornqvist index formula.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname ccdi
#' @return This function returns a value of the multilateral CCDI price index (to be more precise: the GEKS index based on the Tornqvist formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Caves, D.W., Christensen, L.R. and Diewert, W.E. (1982). \emph{Multilateral comparisons of output, input, and productivity using superlative index numbers.} Economic Journal 92, 73-86.}
#' @examples
#' \donttest{
#' ccdi(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' ccdi(milk, start = "2018-12", end = "2019-12")
#' }
#' @export

ccdi <-
  function(
      data,
      start,
      end,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # distances of start and end from wstart plus 1
    d_start <- dist(wstart, start) + 1
    d_end <- dist(wstart, end) + 1
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstat<=start")
    }
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    start <- substr(start, 0, 7)
    end <- substr(end, 0, 7)
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    p <- function(tm) prices(data, period = tm, ID = TRUE)
    ex <- function(tm) expenditures(data, period = tm, ID = TRUE)
    p_list <- lapply(dates, p)
    ex_list <- lapply(dates, ex)
    # main body
    ccd <- function(tt) {
      if (tt == d_end) {
        ID_tt_start <- intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
        pm_start <- dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
        pm_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
        exm_tt <- dplyr::filter(ex_list[[tt]], by %in% ID_tt_start)$expend
        exm_start <- dplyr::filter(ex_list[[d_start]], by %in% ID_tt_start)$expend
        summ_ex_tt <- sum(exm_tt)
        summ_ex_start <- sum(exm_start)
        pm_T <- prod((pm_start / pm_tt)^((exm_tt / summ_ex_tt + exm_start / summ_ex_start) / 2))
        return(1 / pm_T)
      }
      if (tt == d_start) {
        ID_tt_end <- intersect(p_list[[tt]]$by, p_list[[d_end]]$by)
        pl_end <- dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
        pl_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
        exl_tt <- dplyr::filter(ex_list[[tt]], by %in% ID_tt_end)$expend
        exl_end <- dplyr::filter(ex_list[[d_end]], by %in% ID_tt_end)$expend
        suml_ex_tt <- sum(exl_tt)
        suml_ex_end <- sum(exl_end)
        pl_T <- prod((pl_end / pl_tt)^((exl_tt / suml_ex_tt + exl_end / suml_ex_end) / 2))
        return(pl_T)
      }
      if ((!(tt == d_end)) & (!(tt == d_start))) {
        ID_tt_start <- intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
        ID_tt_end <- intersect(p_list[[tt]]$by, p_list[[d_end]]$by)
        pl_end <- dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
        pl_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
        exl_tt <- dplyr::filter(ex_list[[tt]], by %in% ID_tt_end)$expend
        exl_end <- dplyr::filter(ex_list[[d_end]], by %in% ID_tt_end)$expend
        suml_ex_tt <- sum(exl_tt)
        suml_ex_end <- sum(exl_end)
        pm_start <- dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
        pm_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
        exm_tt <- dplyr::filter(ex_list[[tt]], by %in% ID_tt_start)$expend
        exm_start <- dplyr::filter(ex_list[[d_start]], by %in% ID_tt_start)$expend
        summ_ex_tt <- sum(exm_tt)
        summ_ex_start <- sum(exm_start)
        pl_T <- prod((pl_end / pl_tt)^((exl_tt / suml_ex_tt + exl_end / suml_ex_end) / 2))
        pm_T <- prod((pm_start / pm_tt)^((exm_tt / summ_ex_tt + exm_start / summ_ex_start) / 2))
        return(pl_T / pm_T)
      }
    }
    ccdi <- sapply(seq(1, window), ccd)
    ccdi <- prod(ccdi)^(1 / window)
    return(ccdi)
  }

#' @title  Calculating the quality adjusted unit value index (QU index)
#'
#' @description This function returns a value of the quality adjusted unit value index (QU index) for a given set of adjustment factors.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
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
#' \donttest{
#' prodID <- unique(milk$prodID)
#' }
#' \donttest{
#' values <- stats::runif(length(prodID), 1, 2)
#' }
#' \donttest{
#' v <- data.frame(prodID, values)
#' }
#' ## Calculating the QU index for the created data frame 'v'
#' \donttest{
#' QU(milk, start = "2018-12", end = "2019-12", v)
#' }
#' @export

QU <- function(data, start, end, v) {
  if (start == end) {
    return(1)
  }
  if (nrow(data) == 0) stop("A data frame is empty")
  prodID <- NULL
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  data1 <- dplyr::filter(data, lubridate::year(data$time) == lubridate::year(start) & lubridate::month(data$time) == lubridate::month(start))
  data2 <- dplyr::filter(data, lubridate::year(data$time) == lubridate::year(end) & lubridate::month(data$time) == lubridate::month(end))
  data <- dplyr::bind_rows(data1, data2)
  Gstart <- unique(data1$prodID)
  Gend <- unique(data2$prodID)
  sale_end <- expenditures(data2, period = end)
  sale_start <- expenditures(data1, period = start)
  quantity_end <- quantities(data2, period = end)
  quantity_start <- quantities(data1, period = start)
  # main body
  a <- sum(sale_end)
  b <- sum(sale_start)
  v_end <- dplyr::filter(v, prodID %in% Gend)
  v_end <- dplyr::arrange(v_end, prodID)
  val_end <- v_end$values
  v_start <- dplyr::filter(v, prodID %in% Gstart)
  v_start <- dplyr::arrange(v_start, prodID)
  val_start <- v_start$values
  c <- sum(val_end * quantity_end)
  d <- sum(val_start * quantity_start)
  return((a / b) / (c / d))
}

#' @title  Calculating the multilateral Geary-Khamis price index
#'
#' @description This function returns a value of the multilateral Geary-Khamis price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname gk
#' @return This function returns a value of the multilateral Geary-Khamis price index which considers the time window defined by \code{wstart} and \code{window} parameters. The Geary-Khamis price index is calculated by using a special iterative algorithm from \code{Chessa (2016)}. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Geary, R. G. (1958). \emph{A Note on Comparisons of Exchange Rates and Purchasing Power between Countries.} Journal of the Royal Statistical Society, Series A, 121, 97-99.}
#'
#' {Khamis, S. H. (1970). \emph{Properties and Conditions for the Existence of a new Type of Index Number.} Sankhya Series B32, 81-98.}
#'
#' {Chessa, A.G. (2016). \emph{A New Methodology for Processing Scanner Data in the Dutch CPI.} Eurona 1/2016, 49-69.}
#' @examples
#' \donttest{
#' gk(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' gk(milk, start = "2018-12", end = "2019-12")
#' }
#' @export

gk <-
  function(
      data,
      start,
      end,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    time <- expend <- NULL
    expend <- NULL
    quant <- NULL
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # calculating end of the window
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    lubridate::day(wend) <-
      lubridate::days_in_month(wend)
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstart<=start")
    }
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    # data filtration
    d <-
      dplyr::filter(data, data$time >= wstart & data$time <= wend)
    prodID <- unique(d$prodID)
    if (length(prodID) < 2) {
      stop("At least two prodIDs must be available during the considered time interval")
    }
    # main body
    # initial values of indices
    index1 <- rep(1, window)
    index2 <- rep(2, window)
    # set of dates
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    dates <- substr(dates, 0, 7)
    d2 <- d
    d2$time <- as.character(d2$time)
    d2$time <- substr(d2$time, 0, 7)
    # quantity weights - quality adjusted factors vi
    while (sqrt(sum((index1 - index2)^2)) >
      0.005) {
      gr <- dplyr::summarise(dplyr::group_by(d2, time, prodID), expend = sum(prices * quantities) / index1[which(dates == unique(time))], quant = sum(quantities), .groups = "drop")
      gr2 <- dplyr::summarise(dplyr::group_by(gr, prodID), value = sum(expend) / sum(quant), .groups = "drop")
      v <- data.frame(prodID = gr2$prodID, values = gr2$value)
      # series  of indices
      indd <-
        function(tt) {
          return(QU(d, substr(wstart, 0, 7), tt, v))
        }
      ind <- sapply(dates, indd)
      index2 <- index1
      index1 <- ind
    }
    result <-
      index1[which(dates == substr(end, 0, 7))] / index1[which(dates == substr(start, 0, 7))]
    result <- result[[1]]
    return(result)
  }


#' @title  Calculating the multilateral TPD price index
#'
#' @description This function returns a value of the multilateral TPD (Time Product Dummy) price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname tpd
#' @return This function returns a value of the multilateral TPD price index which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). Please note that a Weighted Least Squares (WLS) regression is run with the expenditure shares in each period serving as weights.To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {de  Haan,  J.  and  F.  Krsinich  (2014). \emph{Time  Dummy  Hedonic  and  Quality-Adjusted  Unit Value Indexes: Do They Really Differ?} Paper presented at the Society for Economic Measurement Conference, 18-20 August 2014, Chicago, U.S.}
#' @examples
#' \donttest{
#' tpd(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' tpd(milk, start = "2018-12", end = "2019-12")
#' }
#' @export

tpd <-
  function(
      data,
      start,
      end,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    prodID <- NULL
    time <- NULL
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # calculating end of the window
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    lubridate::day(wend) <-
      lubridate::days_in_month(wend)
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstart<=start")
    }
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    # data filtration,i.e. we obtan products which are available during the time window
    d <-
      dplyr::filter(data, data$time >= wstart & data$time <= wend)
    products <- sort(unique(d$prodID))
    if (length(products) < 2) {
      stop("At least two prodIDs must be available during the considered time interval")
    }
    # main body
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    dates <- substr(dates, 0, 7)
    dates <- dates[2:length(dates)]
    d$time <- as.character(d$time)
    d$time <- substr(d$time, 0, 7)
    # dates of availability of products
    df <- dplyr::group_by(d, prodID, time)
    df <- dplyr::summarise(df, pr = sum(prices * quantities) / sum(quantities), weig = sum(prices * quantities), .groups = "drop")
    av <- function(i) {
      sub <- dplyr::filter(d, prodID == i)
      return(as.character(unique(sub$time)))
    }
    av_dates <- sapply(products, av)
    av_dates <- as.vector(unlist(av_dates))
    # vector connected with the alfa parameter in TPD model
    alfa <-
      replicate(length(av_dates), 1) # unit vector
    # unit vectors corresponding to products
    vec <- list()
    i <- 0
    for (prod in products) {
      i <- i + 1
      vec[[i]] <- replicate(length(av(prod)), 1)
    }
    # gamma vectors connected with Di (i=1,2,...N-1)
    gm <- list()
    for (i in (1:(length(products) - 1))) {
      bin <- c()
      for (k in (1:length(products)))
      {
        if (i == k) {
          bin <- c(bin, vec[[k]])
        } else {
          bin <- c(bin, vec[[k]] - 1)
        }
      }
      gm[[i]] <- bin
    }
    # sigma vectors for time moments 1,2,...,T
    sigma <- list()
    for (i in (1:length(dates))) {
      sg <- c()
      for (k in (1:length(av_dates))) {
        if (dates[i] == av_dates[k]) {
          sg <- c(sg, 1)
        } else {
          sg <- c(sg, 0)
        }
      }
      sigma[[i]] <- sg
    }
    # vector of log prices
    logprices <- log(df$pr)
    # system of weights
    weights <- df$weig
    df_last <- data.frame(av_dates = av_dates, weights = weights)
    df_last <- dplyr::mutate(
      dplyr::group_by(
        df_last, av_dates
      ),
      w = weights / sum(weights)
    )
    weights <- diag(df_last$w)
    # creating matrix X
    x <- alfa
    for (i in (1:(length(products) - 1))) {
      x <- c(x, gm[[i]])
    }
    for (i in (1:length(dates))) {
      x <- c(x, sigma[[i]])
    }
    x <-
      matrix(x,
        nrow = length(av_dates),
        ncol = length(products) + length(dates)
      )
    # estimation of parameters
    b <- t(x) %*% weights
    b <- b %*% x
    b <- solve(b)
    b <- b %*% t(x)
    b <- b %*% weights
    b <- b %*% logprices
    # index calculation
    if (wstart == start) {
      return(exp(b[length(products) + dist(wstart, end)]))
    } else {
      return(exp(b[length(products) + dist(wstart, end)]) / exp(b[length(products) +
        dist(wstart, start)]))
    }
  }


#' @title  Calculating the unweighted multilateral TPD price index
#'
#' @description This function returns a value of the unweighted multilateral TPD (Time Product Dummy) price index.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname utpd
#' @return This function returns a value of the unweighted multilateral TPD price index which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). Please note, that the estimation procedure runs the Ordinary Least Squares (OLS) method instead of the Weighted Least Squares (WLS) method like in the case of the TPD index. To get information about both price index values and corresponding dates, please see functions:  \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {de  Haan,  J.  and  F.  Krsinich  (2014). \emph{Time  Dummy  Hedonic  and  Quality-Adjusted  Unit Value Indexes: Do They Really Differ?} Paper presented at the Society for Economic Measurement Conference, 18-20 August 2014, Chicago, U.S.}
#' @examples
#' \donttest{
#' utpd(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' utpd(milk, start = "2018-12", end = "2019-12")
#' }
#' @export

utpd <-
  function(
      data,
      start,
      end,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    prodID <- NULL
    time <- NULL
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # calculating end of the window
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    lubridate::day(wend) <-
      lubridate::days_in_month(wend)
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstart<=start")
    }
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    # data filtration,i.e. we obtan products which are available during the time window
    d <-
      dplyr::filter(data, data$time >= wstart & data$time <= wend)
    products <- sort(unique(d$prodID))
    if (length(products) < 2) {
      stop("At least two prodIDs must be available during the considered time interval")
    }
    # main body
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    dates <- substr(dates, 0, 7)
    dates <- dates[2:length(dates)]
    d$time <- as.character(d$time)
    d$time <- substr(d$time, 0, 7)
    # dates of availability of products
    df <- dplyr::group_by(d, prodID, time)
    df <- dplyr::summarise(df, pr = sum(prices * quantities) / sum(quantities), weig = sum(prices * quantities), .groups = "drop")
    av <- function(i) {
      sub <- dplyr::filter(d, prodID == i)
      return(as.character(unique(sub$time)))
    }
    av_dates <- sapply(products, av)
    av_dates <- as.vector(unlist(av_dates))
    # vector connected with the alfa parameter in TPD model
    alfa <-
      replicate(length(av_dates), 1) # unit vector
    # unit vectors corresponding to products
    vec <- list()
    i <- 0
    for (prod in products) {
      i <- i + 1
      vec[[i]] <- replicate(length(av(prod)), 1)
    }
    # gamma vectors connected with Di (i=1,2,...N-1)
    gm <- list()
    for (i in (1:(length(products) - 1))) {
      bin <- c()
      for (k in (1:length(products)))
      {
        if (i == k) {
          bin <- c(bin, vec[[k]])
        } else {
          bin <- c(bin, vec[[k]] - 1)
        }
      }
      gm[[i]] <- bin
    }
    # sigma vectors for time moments 1,2,...,T
    sigma <- list()
    for (i in (1:length(dates))) {
      sg <- c()
      for (k in (1:length(av_dates))) {
        if (dates[i] == av_dates[k]) {
          sg <- c(sg, 1)
        } else {
          sg <- c(sg, 0)
        }
      }
      sigma[[i]] <- sg
    }
    # vector of log prices
    logprices <- log(df$pr)
    # creating matrix X
    x <- alfa
    for (i in (1:(length(products) - 1))) {
      x <- c(x, gm[[i]])
    }
    for (i in (1:length(dates))) {
      x <- c(x, sigma[[i]])
    }
    x <-
      matrix(x,
        nrow = length(av_dates),
        ncol = length(products) + length(dates)
      )
    # estimation of parameters
    b <- t(x) %*% x
    b <- solve(b)
    b <- b %*% t(x)
    b <- b %*% logprices
    # index calculation
    if (wstart == start) {
      return(exp(b[length(products) + dist(wstart, end)]))
    } else {
      return(exp(b[length(products) + dist(wstart, end)]) / exp(b[length(products) +
        dist(wstart, start)]))
    }
  }


#' @title  Calculating the multilateral SPQ price index
#'
#' @description This function returns a value of the multilateral SPQ price index which is based on the relative price and quantity dissimilarity measure.
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. '2019-03'.
#' @param end The research period (as character) limited to the year and month, e.g. '2019-07'.
#' @param interval A logical value indicating whether the function is to compare the research period defined by \code{end} to the base period defined by \code{start} (then \code{interval} is set to FALSE) or all fixed base indices are to be calculated. In this latter case, all months from the time interval \code{<start,end>} are considered and \code{start} defines the base period (\code{interval} is set to TRUE).
#' @rdname SPQ
#' @return This function returns a value of the multilateral SPQ price index which is based on the relative price and quantity dissimilarity measure (see \code{References}). If the \code{interval} parameter is set to TRUE, the function returns a vector of price index values without dates. To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Diewert, E. (2020). \emph{The Chain Drift Problem and Multilateral Indexes.} Chapter 6 in: Consumer Price Index Theory (draft)}
#'
#' @examples
#' \donttest{
#' SPQ(sugar, start = "2018-12", end = "2019-02")
#' }
#' \donttest{
#' SPQ(milk, start = "2018-12", end = "2019-12", interval = TRUE)
#' }
#' @export

SPQ <- function(data, start, end, interval = FALSE) {
  if (nrow(data) == 0) {
    stop("A data frame is empty")
  }
  start <- paste(start, "-01", sep = "")
  end <- paste(end, "-01", sep = "")
  start <- as.Date(start)
  end <- as.Date(end)
  if (start > end) {
    stop("parameters must satisfy: start<=end")
  }
  if (start == end) {
    return(1)
  }

  times <- c()
  while (start <= end) {
    times <- c(times, substr(start, 0, 7))
    lubridate::month(start) <- lubridate::month(start) + 1
  }
  if (length(times) == 2) {
    if (interval == FALSE) {
      return(fisher(data, start = times[1], end = times[2]))
    } else {
      return(c(1, fisher(
        data,
        start = times[1], end = times[2]
      )))
    }
  }
  spq <- c(1)
  spq <- c(spq, fisher(data, start = times[1], end = times[2]))
  for (i in 3:length(times))
  {
    # main body

    # delta sp for r=1,2,...,i-1 (we drop the last element)
    sp <-
      dissimilarity_fig(
        data,
        start = times[1],
        end = times[i],
        type = "pq",
        benchmark = "end",
        figure = FALSE
      )$dissimilarity[-i]
    # position of the minimal element
    pos_min <- max(which(sp == min(sp)))
    spq <-
      c(spq, fisher(data, start = times[pos_min], end = times[i]) * spq[pos_min])
  }
  if (interval == TRUE) {
    return(spq)
  } else {
    return(spq[length(spq)])
  }
}


#' @title  Calculating the multilateral weighted WGEKS price index
#'
#' @description This function returns a value of the multilateral weighted WGEKS price index (to be more precise: the weighted GEKS index based on the Fisher formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname wgeks
#' @return This function returns a value of the multilateral weighted WGEKS price index (to be more precise: the weighted GEKS index based on the Fisher formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples
#' \donttest{
#' wgeks(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' wgeks(milk, start = "2018-12", end = "2019-12")
#' }
#' @export

wgeks <-
  function(
      data,
      start,
      end,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # distances of start and end from wstart plus 1
    d_start <- dist(wstart, start) + 1
    d_end <- dist(wstart, end) + 1
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstat<=start")
    }
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    start <- substr(start, 0, 7)
    end <- substr(end, 0, 7)
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    p <- function(tm) prices(data, period = tm, ID = TRUE)
    q <- function(tm) quantities(data, period = tm, ID = TRUE)
    p_list <- lapply(dates, p)
    q_list <- lapply(dates, q)
    # main body
    gks <- function(tt) {
      if (tt == d_end) {
        ID_tt_start <- intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
        pm_start <- dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
        pm_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
        qm_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
        qm_start <- dplyr::filter(q_list[[d_start]], by %in% ID_tt_start)$q
        pm_F <- sum(qm_tt * pm_start) * sum(qm_start * pm_start) / (sum(qm_tt * pm_tt) * sum(qm_start * pm_tt))
        return((1 / pm_F)^0.5)
      }
      if (tt == d_start) {
        ID_tt_end <- intersect(p_list[[tt]]$by, p_list[[d_end]]$by)
        pl_end <- dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
        pl_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
        ql_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
        ql_end <- dplyr::filter(q_list[[d_end]], by %in% ID_tt_end)$q
        pl_F <- sum(ql_tt * pl_end) * sum(ql_end * pl_end) / (sum(ql_tt * pl_tt) * sum(ql_end * pl_tt))
        return(pl_F^0.5)
      }
      if ((!(tt == d_end)) & (!(tt == d_start))) {
        ID_tt_start <- intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
        ID_tt_end <- intersect(p_list[[tt]]$by, p_list[[d_end]]$by)
        pl_end <- dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
        pl_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
        ql_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
        ql_end <- dplyr::filter(q_list[[d_end]], by %in% ID_tt_end)$q
        pm_start <- dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
        pm_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
        qm_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
        qm_start <- dplyr::filter(q_list[[d_start]], by %in% ID_tt_start)$q
        pl_F <- sum(ql_tt * pl_end) * sum(ql_end * pl_end) / (sum(ql_tt * pl_tt) * sum(ql_end * pl_tt))
        pm_F <- sum(qm_tt * pm_start) * sum(qm_start * pm_start) / (sum(qm_tt * pm_tt) * sum(qm_start * pm_tt))
        return((pl_F / pm_F)^0.5)
      }
    }
    wgeks <- sapply(seq(1, window), gks)
    sales_in_time <-
      function(tt) {
        return(sum(expenditures(data, tt)))
      }
    expenditures_ <-
      sapply(dates, sales_in_time)
    expenditures_ <-
      expenditures_ / sum(expenditures_)
    wgeks <-
      prod((wgeks)^expenditures_)
    return(wgeks)
  }

#' @title  Calculating the multilateral GEKS-L price index
#'
#' @description This function returns a value of the multilateral GEKS-L price index (to be more precise: the GEKS index based on the Laspeyres formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksl
#' @return This function returns a value of the multilateral GEKS-L price index (to be more precise: the GEKS index based on the Laspeyres formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Białek, J. (2022). \emph{The general class of multilateral indices and its two special cases.} Paper presented at the 17th Meeting of the Ottawa Group on Price Indices, Rome, Italy.}
#'
#' {Białek, J. (2022). \emph{Improving quality of the scanner CPI: proposition of new multilateral methods}, Quality & Quantity, https://doi.org/10.1007/s11135-022-01506-6.}
#'
#' @examples
#' \donttest{
#' geksl(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' geksl(milk, start = "2018-12", end = "2019-12")
#' }
#' @export

geksl <-
  function(
      data,
      start,
      end,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # distances of start and end from wstart plus 1
    d_start <- dist(wstart, start) + 1
    d_end <- dist(wstart, end) + 1
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstat<=start")
    }
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    start <- substr(start, 0, 7)
    end <- substr(end, 0, 7)
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    p <- function(tm) prices(data, period = tm, ID = TRUE)
    q <- function(tm) quantities(data, period = tm, ID = TRUE)
    p_list <- lapply(dates, p)
    q_list <- lapply(dates, q)
    # main body
    gksl <- function(tt) {
      if (tt == d_end) {
        ID_tt_start <- intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
        pm_start <- dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
        pm_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
        qm_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
        pm_L <- sum(qm_tt * pm_start) / sum(qm_tt * pm_tt)
        return(1 / pm_L)
      }
      if (tt == d_start) {
        ID_tt_end <- intersect(p_list[[tt]]$by, p_list[[d_end]]$by)
        pl_end <- dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
        pl_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
        ql_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
        pl_L <- sum(ql_tt * pl_end) / sum(ql_tt * pl_tt)
        return(pl_L)
      }
      if ((!(tt == d_end)) & (!(tt == d_start))) {
        ID_tt_start <- intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
        ID_tt_end <- intersect(p_list[[tt]]$by, p_list[[d_end]]$by)
        pl_end <- dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
        pl_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
        ql_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
        pm_start <- dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
        pm_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
        qm_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
        pl_L <- sum(ql_tt * pl_end) / sum(ql_tt * pl_tt)
        pm_L <- sum(qm_tt * pm_start) / sum(qm_tt * pm_tt)
        return(pl_L / pm_L)
      }
    }
    geksl <- sapply(seq(1, window), gksl)
    geksl <- prod(geksl)^(1 / window)
    return(geksl)
  }

#' @title  Calculating the multilateral weighted WGEKS-L price index
#'
#' @description This function returns a value of the multilateral weighted WGEKS-L price index (to be more precise: the weighted GEKS index based on the Laspeyres formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname wgeksl
#' @return This function returns a value of the multilateral weighted WGEKS-L price index (to be more precise: the weighted GEKS index based on the Laspeyres formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Białek, J. (2022). \emph{The general class of multilateral indices and its two special cases.} Paper presented at the 17th Meeting of the Ottawa Group on Price Indices, Rome, Italy.}
#'
#' {Białek, J. (2022). \emph{Improving quality of the scanner CPI: proposition of new multilateral methods}, Quality & Quantity, https://doi.org/10.1007/s11135-022-01506-6.}
#'
#' @examples
#' \donttest{
#' wgeksl(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' wgeksl(milk, start = "2018-12", end = "2019-12")
#' }
#' @export

wgeksl <-
  function(
      data,
      start,
      end,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # distances of start and end from wstart plus 1
    d_start <- dist(wstart, start) + 1
    d_end <- dist(wstart, end) + 1
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstat<=start")
    }
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    start <- substr(start, 0, 7)
    end <- substr(end, 0, 7)
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    p <- function(tm) prices(data, period = tm, ID = TRUE)
    q <- function(tm) quantities(data, period = tm, ID = TRUE)
    p_list <- lapply(dates, p)
    q_list <- lapply(dates, q)
    # main body
    gksl <- function(tt) {
      if (tt == d_end) {
        ID_tt_start <- intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
        pm_start <- dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
        pm_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
        qm_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
        pm_L <- sum(qm_tt * pm_start) / sum(qm_tt * pm_tt)
        return(1 / pm_L)
      }
      if (tt == d_start) {
        ID_tt_end <- intersect(p_list[[tt]]$by, p_list[[d_end]]$by)
        pl_end <- dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
        pl_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
        ql_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
        pl_L <- sum(ql_tt * pl_end) / sum(ql_tt * pl_tt)
        return(pl_L)
      }
      if ((!(tt == d_end)) & (!(tt == d_start))) {
        ID_tt_start <- intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
        ID_tt_end <- intersect(p_list[[tt]]$by, p_list[[d_end]]$by)
        pl_end <- dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
        pl_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
        ql_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_end)$q
        pm_start <- dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
        pm_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
        qm_tt <- dplyr::filter(q_list[[tt]], by %in% ID_tt_start)$q
        pl_L <- sum(ql_tt * pl_end) / sum(ql_tt * pl_tt)
        pm_L <- sum(qm_tt * pm_start) / sum(qm_tt * pm_tt)
        return(pl_L / pm_L)
      }
    }
    wgeksl <- sapply(seq(1, window), gksl)
    sales_in_time <-
      function(tt) {
        return(sum(expenditures(data, tt)))
      }
    expenditures_ <-
      sapply(dates, sales_in_time)
    expenditures_ <-
      expenditures_ / sum(expenditures_)
    wgeksl <-
      prod((wgeksl)^expenditures_)
    return(wgeksl)
  }


#' @title  Calculating the multilateral GEKS-GL price index
#'
#' @description This function returns a value of the multilateral GEKS-GL price index (to be more precise: the GEKS index based on the geometric Laspeyres formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksgl
#' @return This function returns a value of the multilateral GEKS-GL price index (to be more precise: the GEKS index based on the geometric Laspeyres formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Białek, J. (2022). \emph{The general class of multilateral indices and its two special cases.} Paper presented at the 17th Meeting of the Ottawa Group on Price Indices, Rome, Italy.}
#'
#' {Białek, J. (2022). \emph{Improving quality of the scanner CPI: proposition of new multilateral methods}, Quality & Quantity, https://doi.org/10.1007/s11135-022-01506-6.}
#'
#' @examples
#' \donttest{
#' geksgl(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' geksgl(milk, start = "2018-12", end = "2019-12")
#' }
#' @export

geksgl <-
  function(
      data,
      start,
      end,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # distances of start and end from wstart plus 1
    d_start <- dist(wstart, start) + 1
    d_end <- dist(wstart, end) + 1
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstat<=start")
    }
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    start <- substr(start, 0, 7)
    end <- substr(end, 0, 7)
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    p <- function(tm) prices(data, period = tm, ID = TRUE)
    ex <- function(tm) expenditures(data, period = tm, ID = TRUE)
    p_list <- lapply(dates, p)
    ex_list <- lapply(dates, ex)
    # main body
    gksgl <- function(tt) {
      if (tt == d_end) {
        ID_tt_start <- intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
        pm_start <- dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
        pm_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
        exm_tt <- dplyr::filter(ex_list[[tt]], by %in% ID_tt_start)$expend
        summ_ex_tt <- sum(exm_tt)
        pm_GL <- prod((pm_start / pm_tt)^(exm_tt / summ_ex_tt))
        return(1 / pm_GL)
      }
      if (tt == d_start) {
        ID_tt_end <- intersect(p_list[[tt]]$by, p_list[[d_end]]$by)
        pl_end <- dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
        pl_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
        exl_tt <- dplyr::filter(ex_list[[tt]], by %in% ID_tt_end)$expend
        suml_ex_tt <- sum(exl_tt)
        pl_GL <- prod((pl_end / pl_tt)^(exl_tt / suml_ex_tt))
        return(pl_GL)
      }
      if ((!(tt == d_end)) & (!(tt == d_start))) {
        ID_tt_start <- intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
        ID_tt_end <- intersect(p_list[[tt]]$by, p_list[[d_end]]$by)
        pl_end <- dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
        pl_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
        exl_tt <- dplyr::filter(ex_list[[tt]], by %in% ID_tt_end)$expend
        exl_end <- dplyr::filter(ex_list[[d_end]], by %in% ID_tt_end)$expend
        suml_ex_tt <- sum(exl_tt)
        suml_ex_end <- sum(exl_end)
        pm_start <- dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
        pm_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
        exm_tt <- dplyr::filter(ex_list[[tt]], by %in% ID_tt_start)$expend
        summ_ex_tt <- sum(exm_tt)
        pl_GL <- prod((pl_end / pl_tt)^(exl_tt / suml_ex_tt))
        pm_GL <- prod((pm_start / pm_tt)^(exm_tt / summ_ex_tt))
        return(pl_GL / pm_GL)
      }
    }
    geksgl <- sapply(seq(1, window), gksgl)
    geksgl <- prod(geksgl)^(1 / window)
    return(geksgl)
  }

#' @title  Calculating the multilateral weighted WGEKS-GL price index
#'
#' @description This function returns a value of the multilateral weighted WGEKS-GL price index (to be more precise: the weighted GEKS index based on the geometric Laspeyres formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname wgeksgl
#' @return This function returns a value of the multilateral weighted WGEKS-GL price index (to be more precise: the weighted GEKS index based on the geometric Laspeyres formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Białek, J. (2022). \emph{The general class of multilateral indices and its two special cases.} Paper presented at the 17th Meeting of the Ottawa Group on Price Indices, Rome, Italy.}
#'
#' {Białek, J. (2022). \emph{Improving quality of the scanner CPI: proposition of new multilateral methods}, Quality & Quantity, https://doi.org/10.1007/s11135-022-01506-6.}
#'
#' @examples
#' \donttest{
#' wgeksgl(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' wgeksgl(milk, start = "2018-12", end = "2019-12")
#' }
#' @export

wgeksgl <-
  function(
      data,
      start,
      end,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # distances of start and end from wstart plus 1
    d_start <- dist(wstart, start) + 1
    d_end <- dist(wstart, end) + 1
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstat<=start")
    }
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    start <- substr(start, 0, 7)
    end <- substr(end, 0, 7)
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    p <- function(tm) prices(data, period = tm, ID = TRUE)
    ex <- function(tm) expenditures(data, period = tm, ID = TRUE)
    p_list <- lapply(dates, p)
    ex_list <- lapply(dates, ex)
    # main body
    gksgl <- function(tt) {
      if (tt == d_end) {
        ID_tt_start <- intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
        pm_start <- dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
        pm_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
        exm_tt <- dplyr::filter(ex_list[[tt]], by %in% ID_tt_start)$expend
        summ_ex_tt <- sum(exm_tt)
        pm_GL <- prod((pm_start / pm_tt)^(exm_tt / summ_ex_tt))
        return(1 / pm_GL)
      }
      if (tt == d_start) {
        ID_tt_end <- intersect(p_list[[tt]]$by, p_list[[d_end]]$by)
        pl_end <- dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
        pl_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
        exl_tt <- dplyr::filter(ex_list[[tt]], by %in% ID_tt_end)$expend
        suml_ex_tt <- sum(exl_tt)
        pl_GL <- prod((pl_end / pl_tt)^(exl_tt / suml_ex_tt))
        return(pl_GL)
      }
      if ((!(tt == d_end)) & (!(tt == d_start))) {
        ID_tt_start <- intersect(p_list[[tt]]$by, p_list[[d_start]]$by)
        ID_tt_end <- intersect(p_list[[tt]]$by, p_list[[d_end]]$by)
        pl_end <- dplyr::filter(p_list[[d_end]], by %in% ID_tt_end)$uv
        pl_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_end)$uv
        exl_tt <- dplyr::filter(ex_list[[tt]], by %in% ID_tt_end)$expend
        exl_end <- dplyr::filter(ex_list[[d_end]], by %in% ID_tt_end)$expend
        suml_ex_tt <- sum(exl_tt)
        suml_ex_end <- sum(exl_end)
        pm_start <- dplyr::filter(p_list[[d_start]], by %in% ID_tt_start)$uv
        pm_tt <- dplyr::filter(p_list[[tt]], by %in% ID_tt_start)$uv
        exm_tt <- dplyr::filter(ex_list[[tt]], by %in% ID_tt_start)$expend
        summ_ex_tt <- sum(exm_tt)
        pl_GL <- prod((pl_end / pl_tt)^(exl_tt / suml_ex_tt))
        pm_GL <- prod((pm_start / pm_tt)^(exm_tt / summ_ex_tt))
        return(pl_GL / pm_GL)
      }
    }
    wgeksgl <- sapply(seq(1, window), gksgl)
    sales_in_time <-
      function(tt) {
        return(sum(expenditures(data, tt)))
      }
    expenditures_ <-
      sapply(dates, sales_in_time)
    expenditures_ <-
      expenditures_ / sum(expenditures_)
    wgeksgl <-
      prod((wgeksgl)^expenditures_)
    return(wgeksgl)
  }

#' @title  Calculating the multilateral GEKS-AQU price index
#'
#' @description This function returns a value of the multilateral GEKS-AQU price index (to be more precise: the GEKS index based on the asynchronous quality adjusted unit value formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksaqu
#' @return This function returns a value of the multilateral GEKS-AQU price index (to be more precise: the GEKS index based on the asynchronous quality adjusted unit value formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Białek, J. (2022). \emph{The general class of multilateral indices and its two special cases.} Paper presented at the 17th Meeting of the Ottawa Group on Price Indices, Rome, Italy.}
#'
#' {Białek, J. (2023). \emph{Quality adjusted GEKS-type indices for price comparisons based on scanner data.} Statistics in Transition – new series, 24(3), 151-169.}
#'
#' @examples
#' \donttest{
#' geksaqu(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' geksaqu(milk, start = "2018-12", end = "2019-12")
#' }
#' @export

geksaqu <-
  function(
      data,
      start,
      end,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    prodID <- NULL
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstat<=start")
    }
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    start <- substr(start, 0, 7)
    end <- substr(end, 0, 7)
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    dates <- substr(dates, 0, 7)
    data <- dplyr::filter(data, data$time >= wstart & data$time <= wend)
    # data frame with quality adjusted factors
    v <- dplyr::summarise(dplyr::group_by(data, prodID), values = sum(prices * quantities) / sum(quantities), .groups = "drop")
    # main body
    gksaqu <-
      function(tt) {
        return(c(aqu(data, start = tt, end = end, v), aqu(data, start = tt, end = start, v)))
      }
    vec <- sapply(dates, gksaqu)
    geksaqu <- prod(vec[1, ] / vec[2, ])
    geksaqu <- geksaqu^(1 / window)
    return(geksaqu)
  }

#' @title  Calculating the multilateral weighted WGEKS-AQU price index
#'
#' @description This function returns a value of the multilateral weighted WGEKS-AQU price index (to be more precise: the weighted GEKS index based on the asynchronous quality adjusted unit value formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname wgeksaqu
#' @return This function returns a value of the multilateral weighted WGEKS-AQU price index (to be more precise: the weighted GEKS index based on the asynchronous quality adjusted unit value formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Białek, J. (2022). \emph{The general class of multilateral indices and its two special cases.} Paper presented at the 17th Meeting of the Ottawa Group on Price Indices, Rome, Italy.}
#'
#' {Białek, J. (2023). \emph{Quality adjusted GEKS-type indices for price comparisons based on scanner data.} Statistics in Transition – new series, 24(3), 151-169.}
#'
#' @examples
#' \donttest{
#' wgeksaqu(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' wgeksaqu(milk, start = "2018-12", end = "2019-12")
#' }
#' @export

wgeksaqu <-
  function(
      data,
      start,
      end,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    prodID <- NULL
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstat<=start")
    }
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    start <- substr(start, 0, 7)
    end <- substr(end, 0, 7)
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    dates <- substr(dates, 0, 7)
    data <- dplyr::filter(data, data$time >= wstart & data$time <= wend)
    # data frame with quality adjusted factors
    v <- dplyr::summarise(dplyr::group_by(data, prodID), values = sum(prices * quantities) / sum(quantities), .groups = "drop")
    # main body
    wgksaqu <-
      function(tt) {
        return(c(aqu(data, start = tt, end = end, v), aqu(data, start = tt, end = start, v)))
      }
    vec <- sapply(dates, wgksaqu)
    sales_in_time <-
      function(tt) {
        return(sum(expenditures(data, tt)))
      }
    expenditures_ <-
      sapply(dates, sales_in_time)
    expenditures_ <-
      expenditures_ / sum(expenditures_)
    wgeksaqu <- prod((vec[1, ] / vec[2, ])^expenditures_)
    return(wgeksaqu)
  }


#' @title  Calculating the multilateral GEKS-AQI price index
#'
#' @description This function returns a value of the multilateral GEKS-AQI price index (to be more precise: the GEKS index based on the asynchronous quality adjusted price index formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksaqi
#' @return This function returns a value of the multilateral GEKS-AQI price index (to be more precise: the GEKS index based on the asynchronous quality adjusted price index formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Białek, J. (2022). \emph{The general class of multilateral indices and its two special cases.} Paper presented at the 17th Meeting of the Ottawa Group on Price Indices, Rome, Italy.}
#'
#' {Białek, J. (2023). \emph{Quality adjusted GEKS-type indices for price comparisons based on scanner data.} Statistics in Transition – new series, 24(3), 151-169.}
#'
#' @examples
#' \donttest{
#' geksaqi(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' geksaqi(milk, start = "2018-12", end = "2019-12")
#' }
#' @export

geksaqi <-
  function(
      data,
      start,
      end,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    prodID <- NULL
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstat<=start")
    }
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    start <- substr(start, 0, 7)
    end <- substr(end, 0, 7)
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    dates <- substr(dates, 0, 7)
    data <- dplyr::filter(data, data$time >= wstart & data$time <= wend)
    # data frame with quality adjusted factors
    v <- dplyr::summarise(dplyr::group_by(data, prodID), values = sum(prices * quantities) / sum(quantities), .groups = "drop")
    # main body
    gksaqi <-
      function(tt) {
        return(c(aqi(data, start = tt, end = end, v), aqi(data, start = tt, end = start, v)))
      }
    vec <- sapply(dates, gksaqi)
    geksaqi <- prod(vec[1, ] / vec[2, ])
    geksaqi <- geksaqi^(1 / window)
    return(geksaqi)
  }


#' @title  Calculating the multilateral weighted WGEKS-AQI price index
#'
#' @description This function returns a value of the multilateral weighted WGEKS-AQI price index (to be more precise: the weighted GEKS index based on the asynchronous quality adjusted price index formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname wgeksaqi
#' @return This function returns a value of the multilateral weighted WGEKS-AQI price index (to be more precise: the weighted GEKS index based on the asynchronous quality adjusted price index formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Białek, J. (2022). \emph{The general class of multilateral indices and its two special cases.} Paper presented at the 17th Meeting of the Ottawa Group on Price Indices, Rome, Italy.}
#'
#' {Białek, J. (2023). \emph{Quality adjusted GEKS-type indices for price comparisons based on scanner data.} Statistics in Transition – new series, 24(3), 151-169.}
#'
#' @examples
#' \donttest{
#' wgeksaqi(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' wgeksaqi(milk, start = "2018-12", end = "2019-12")
#' }
#' @export

wgeksaqi <-
  function(
      data,
      start,
      end,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    prodID <- NULL
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstat<=start")
    }
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    start <- substr(start, 0, 7)
    end <- substr(end, 0, 7)
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    dates <- substr(dates, 0, 7)
    data <- dplyr::filter(data, data$time >= wstart & data$time <= wend)
    # data frame with quality adjusted factors
    v <- dplyr::summarise(dplyr::group_by(data, prodID), values = sum(prices * quantities) / sum(quantities), .groups = "drop")
    # main body
    wgksaqi <-
      function(tt) {
        return(c(aqi(data, start = tt, end = end, v), aqi(data, start = tt, end = start, v)))
      }
    vec <- sapply(dates, wgksaqi)
    sales_in_time <-
      function(tt) {
        return(sum(expenditures(data, tt)))
      }
    expenditures_ <-
      sapply(dates, sales_in_time)
    expenditures_ <-
      expenditures_ / sum(expenditures_)
    wgeksaqi <- prod((vec[1, ] / vec[2, ])^expenditures_)
    return(wgeksaqi)
  }

#' @title  Calculating the multilateral GEKS-GAQI price index
#'
#' @description This function returns a value of the multilateral GEKS-GAQI price index (to be more precise: the GEKS index based on the geometric asynchronous quality adjusted price index formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksgaqi
#' @return This function returns a value of the multilateral GEKS-GAQI price index (to be more precise: the GEKS index based on the geometric asynchronous quality adjusted price index formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples
#' \donttest{
#' geksgaqi(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' geksgaqi(milk, start = "2018-12", end = "2019-12")
#' }
#' @export

geksgaqi <-
  function(
      data,
      start,
      end,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    prodID <- NULL
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstat<=start")
    }
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    start <- substr(start, 0, 7)
    end <- substr(end, 0, 7)
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    dates <- substr(dates, 0, 7)
    data <- dplyr::filter(data, data$time >= wstart & data$time <= wend)
    # data frame with quality adjusted factors
    v <- dplyr::summarise(dplyr::group_by(data, prodID), values = sum(prices * quantities) / sum(quantities), .groups = "drop")
    # main body
    gksgaqi <-
      function(tt) {
        return(c(gaqi(data, start = tt, end = end, v), gaqi(data, start = tt, end = start, v)))
      }
    vec <- sapply(dates, gksgaqi)
    geksgaqi <- prod(vec[1, ] / vec[2, ])
    geksgaqi <- geksgaqi^(1 / window)
    return(geksgaqi)
  }

#' @title  Calculating the multilateral weighted WGEKS-GAQI price index
#'
#' @description This function returns a value of the multilateral weighted WGEKS-GAQI price index (to be more precise: the weighted GEKS index based on the geometric asynchronous quality adjusted price index formula).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname wgeksgaqi
#' @return This function returns a value of the multilateral weighted WGEKS-GAQI price index (to be more precise: the weighted GEKS index based on the geometric asynchronous quality adjusted price index formula) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' @examples
#' \donttest{
#' wgeksgaqi(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' wgeksgaqi(milk, start = "2018-12", end = "2019-12")
#' }
#' @export

wgeksgaqi <-
  function(
      data,
      start,
      end,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    prodID <- NULL
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstat<=start")
    }
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    start <- substr(start, 0, 7)
    end <- substr(end, 0, 7)
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    dates <- substr(dates, 0, 7)
    data <- dplyr::filter(data, data$time >= wstart & data$time <= wend)
    # data frame with quality adjusted factors
    v <- dplyr::summarise(dplyr::group_by(data, prodID), values = sum(prices * quantities) / sum(quantities), .groups = "drop")
    # main body
    wgksgaqi <-
      function(tt) {
        return(c(aqu(data, start = tt, end = end, v), aqu(data, start = tt, end = start, v)))
      }
    vec <- sapply(dates, wgksgaqi)
    sales_in_time <-
      function(tt) {
        return(sum(expenditures(data, tt)))
      }
    expenditures_ <-
      sapply(dates, sales_in_time)
    expenditures_ <-
      expenditures_ / sum(expenditures_)
    wgeksgaqi <- prod((vec[1, ] / vec[2, ])^expenditures_)
    return(wgeksgaqi)
  }


#' @title  Calculating the multilateral GEKS-IQM price index
#'
#' @description This function returns a value of the multilateral GEKS-IQM price index (to be more precise: the GEKS index based on the implicit quadratic mean of order r price index IQMp).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter used in the implicit quadratic mean of order r price index.
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksiqm
#' @return This function returns a value of the multilateral GEKS-IQM price index (to be more precise: the GEKS index based on the the implicit quadratic mean of order r price index IQMp) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples
#' \donttest{
#' geksiqm(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' geksiqm(milk, start = "2018-12", end = "2019-12", r = 1.6)
#' }
#' @export

geksiqm <-
  function(
      data,
      start,
      end,
      r = 2,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstat<=start")
    }
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    start <- substr(start, 0, 7)
    end <- substr(end, 0, 7)
    dates <- c()
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    dates <- substr(dates, 0, 7)
    # main body
    gks <-
      function(tt) {
        return(c(IQMp(data, tt, end, r, FALSE), IQMp(data, tt, start, r, FALSE)))
      }
    vec <- sapply(dates, gks)
    geks_iqm <- prod(vec[1, ] / vec[2, ])
    geks_iqm <- geks_iqm^(1 / window)
    return(geks_iqm)
  }

#' @title  Calculating the multilateral GEKS-QM price index
#'
#' @description This function returns a value of the multilateral GEKS-QM price index (to be more precise: the GEKS index based on the quadratic mean of order r price index QMp).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param r The real and non-zero parameter used in the implicit quadratic mean of order r price index.
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname geksqm
#' @return This function returns a value of the multilateral GEKS-QM price index (to be more precise: the GEKS index based on the the quadratic mean of order r price index QMp) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {(2004). \emph{Consumer Price Index Manual. Theory and practice}. ILO/IMF/OECD/UNECE/Eurostat/The World Bank, International Labour Office (ILO), Geneva.}
#' @examples
#' \donttest{
#' geksqm(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' geksqm(milk, start = "2018-12", end = "2019-12", r = 1.6)
#' }
#' @export

geksqm <-
  function(
      data,
      start,
      end,
      r = 2,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstat<=start")
    }
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    start <- substr(start, 0, 7)
    end <- substr(end, 0, 7)
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    dates <- substr(dates, 0, 7)
    # main body
    gks <-
      function(tt) {
        return(c(QMp(data, tt, end, r, FALSE), QMp(data, tt, start, r, FALSE)))
      }
    vec <- sapply(dates, gks)
    geks_qm <- prod(vec[1, ] / vec[2, ])
    geks_qm <- geks_qm^(1 / window)
    return(geks_qm)
  }


#' @title  Calculating the multilateral GEKS-LM price index
#'
#' @description This function returns a value of the multilateral GEKS-LM price index (to be more precise: the GEKS index based on the Lloyd-Moulton price index).
#' @param data The user's data frame with information about sold products. It must contain columns: \code{time} (as Date in format: year-month-day,e.g. '2020-12-01'), \code{prices} (as positive numeric), \code{quantities}  (as positive numeric) and \code{prodID} (as numeric, factor or character).
#' @param start The base period (as character) limited to the year and month, e.g. "2020-03".
#' @param end The research period (as character) limited to the year and month, e.g. "2020-04".
#' @param sigma The elasticity of substitution (a parameter used in the Lloyd-Moulton index formula).
#' @param wstart The beginning of the time interval (which is used by multilateral methods) limited to the year and month, e.g. "2020-01".
#' @param window The length of the time window (as positive integer: typically multilateral methods are based on the 13-month time window).
#' @rdname gekslm
#' @return This function returns a value of the multilateral GEKS-LM price index (to be more precise: the GEKS index based on the Lloyd-Moulton price index) which considers the time window defined by \code{wstart} and \code{window} parameters. It measures the price dynamics by comparing period \code{end} to period \code{start} (both \code{start} and \code{end} must be inside the considered time window). To get information about both price index values and corresponding dates, please see functions: \code{\link{price_indices}} or \code{\link{final_index}}. The function does not take into account aggregating over outlets or product subgroups (to consider these types of aggregating, please use the \code{\link{final_index}} function).
#' @references
#' {Gini, C. (1931). \emph{On the Circular Test of Index Numbers.} Metron 9:9, 3-24.}
#'
#' {Elteto, O., and Koves, P. (1964). \emph{On a Problem of Index Number Computation Relating to International Comparisons.} Statisztikai Szemle 42, 507-518.}
#'
#' {Szulc, B. (1983). \emph{Linking Price Index Numbers.} In: Price Level Measurement, W. E. Diewert and C. Montmarquette (eds.), 537-566.}
#'
#' {Lloyd, P. J. (1975). \emph{Substitution Effects and Biases in Nontrue Price Indices.}  The American Economic Review, 65, 301-313.}
#'
#' {Moulton,  B.  R.  (1996). \emph{Constant  Elasticity  Cost-of-Living  Index  in  Share-Relative  Form.}  Washington DC: U. S. Bureau of Labor Statistics, mimeograph}
#' @examples
#' \donttest{
#' gekslm(milk, start = "2019-01", end = "2019-08", window = 10)
#' }
#' \donttest{
#' gekslm(milk, start = "2018-12", end = "2019-12", sigma = 0.5)
#' }
#' @export

gekslm <-
  function(
      data,
      start,
      end,
      sigma = 0.7,
      wstart = start,
      window = 13) {
    if (start == end) {
      return(1)
    }
    if (nrow(data) == 0) {
      stop("A data frame is empty")
    }
    start <- paste(start, "-01", sep = "")
    end <- paste(end, "-01", sep = "")
    wstart <-
      paste(wstart, "-01", sep = "")
    start <- as.Date(start)
    end <- as.Date(end)
    wstart <- as.Date(wstart)
    # checking conditions
    if (window < 2) {
      stop("window must be at least 2 months")
    }
    if (start > end) {
      stop("parameters must satisfy: start<=end")
    }
    if (wstart > start) {
      stop("parameters must satisfy: wstat<=start")
    }
    wend <- wstart
    lubridate::month(wend) <-
      lubridate::month(wend) + window - 1
    if (end > wend) {
      stop("parameters must satisfy: end<wstart+window")
    }
    start <- substr(start, 0, 7)
    end <- substr(end, 0, 7)
    dates <- seq.Date(from = wstart, to = wend, by = "month")
    dates <- substr(dates, 0, 7)
    # main body
    gks <-
      function(tt) {
        return(c(lloyd_moulton(data, tt, end, sigma, FALSE), lloyd_moulton(data, tt, start, sigma, FALSE)))
      }
    vec <- sapply(dates, gks)
    geks_lm <- prod(vec[1, ] / vec[2, ])
    geks_lm <- geks_lm^(1 / window)
    return(geks_lm)
  }
