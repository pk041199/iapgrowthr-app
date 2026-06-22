#' Score a measure against reference data
#'
#' @param measure One of `"height"`, `"weight"`, `"bmi"`, `"wc"`, `"hc"`,
#'   `"nc"`, `"hip"`, `"sbp"`, or `"dbp"`.
#' @param value Observed value.
#' @param age Age in completed years or age unit expected by the reference data.
#' @param sex Sex label expected by the reference data.
#' @param ref_data Data frame containing at least `measure`, `sex`, `age`,
#'   `l`, `m`, and `s`.
#'
#' @return A one-row data frame with z score and percentile.
iap_measure <- function(measure, value, age, sex, ref_data) {
  required_cols <- c("measure", "sex", "age", "l", "m", "s")

  if (!all(required_cols %in% names(ref_data))) {
    stop("`ref_data` must contain: ", paste(required_cols, collapse = ", "))
  }

  hit <- ref_data[
    ref_data$measure == measure &
      ref_data$sex == sex &
      ref_data$age == age,
    ,
    drop = FALSE
  ]

  if (nrow(hit) != 1) {
    stop("Expected exactly one matching reference row.")
  }

  z <- lms_zscore(value, hit$l[[1]], hit$m[[1]], hit$s[[1]])
  p <- lms_percentile(value, hit$l[[1]], hit$m[[1]], hit$s[[1]])

  data.frame(
    measure = measure,
    value = value,
    age = age,
    sex = sex,
    z_score = z,
    percentile = p
  )
}

