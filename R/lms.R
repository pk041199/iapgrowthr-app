#' Calculate LMS z score
#'
#' @param x Observed value.
#' @param l LMS L parameter.
#' @param m LMS M parameter.
#' @param s LMS S parameter.
#'
#' @return Numeric z score.
lms_zscore <- function(x, l, m, s) {
  if (any(is.na(c(x, l, m, s)))) {
    return(NA_real_)
  }

  if (m <= 0 || s <= 0) {
    stop("`m` and `s` must be positive.")
  }

  if (isTRUE(all.equal(l, 0))) {
    return(log(x / m) / s)
  }

  (((x / m) ^ l) - 1) / (l * s)
}

#' Calculate LMS percentile
#'
#' @param x Observed value.
#' @param l LMS L parameter.
#' @param m LMS M parameter.
#' @param s LMS S parameter.
#'
#' @return Percentile on 0 to 100 scale.
lms_percentile <- function(x, l, m, s) {
  stats::pnorm(lms_zscore(x, l, m, s)) * 100
}

