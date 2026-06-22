#' Score a value against percentile-table reference data
#'
#' This function is intended for percentile-table references such as the current
#' IAP height, weight, BMI, and waist-circumference CSV files. It interpolates
#' the expected value at the requested age for each percentile curve, then
#' interpolates the percentile that corresponds to the observed value.
#'
#' @param ref_data Normalized long-format reference data with columns
#'   `measure`, `sex`, `age`, `percentile`, `value`.
#' @param measure Measure name such as `"height"`, `"weight"`, `"bmi"`, or `"wc"`.
#' @param sex One of `"m"`, `"male"`, `"f"`, or `"female"`.
#' @param age Age in years.
#' @param value Observed value.
#'
#' @return One-row data frame with estimated percentile and z-score.
score_percentile_ref <- function(ref_data, measure, sex, age, value) {
  sex <- normalize_ref_sex(sex)
  measure <- tolower(trimws(measure))

  subset_data <- ref_data[
    ref_data$measure == measure & ref_data$sex == sex,
    ,
    drop = FALSE
  ]

  if (nrow(subset_data) == 0) {
    stop("No matching reference rows found for the requested measure and sex.")
  }

  percentiles <- suppressWarnings(as.numeric(unique(as.character(subset_data$percentile))))
  percentiles <- sort(percentiles[!is.na(percentiles)])

  if (length(percentiles) < 2) {
    stop("At least two percentile curves are required to estimate percentile and z-score.")
  }

  ref_values <- numeric(length(percentiles))

  for (i in seq_along(percentiles)) {
    pct <- as.character(percentiles[[i]])
    line_data <- subset_data[subset_data$percentile == pct, , drop = FALSE]
    line_data <- line_data[order(as.numeric(line_data$age)), , drop = FALSE]

    ref_values[[i]] <- stats::approx(
      x = as.numeric(line_data$age),
      y = as.numeric(line_data$value),
      xout = age,
      rule = 2
    )$y
  }

  est_percentile <- stats::approx(
    x = ref_values,
    y = percentiles,
    xout = value,
    rule = 2,
    ties = mean
  )$y

  bounded_percentile <- min(max(est_percentile, 0.1), 99.9)
  z_score <- stats::qnorm(bounded_percentile / 100)

  data.frame(
    measure = measure,
    sex = sex,
    age = age,
    value = value,
    percentile = est_percentile,
    z_score = z_score,
    stringsAsFactors = FALSE
  )
}

#' Score blood pressure against BP percentile reference tables
#'
#' @param bp_ref Blood pressure reference data from `read_iap_bp_ref()`.
#' @param measure One of `"sbp"` or `"dbp"`.
#' @param sex One of `"m"`, `"male"`, `"f"`, or `"female"`.
#' @param age Age in years.
#' @param height_percentile Height percentile used for BP reference lookup.
#' @param value Observed SBP or DBP value.
#'
#' @return One-row data frame with estimated percentile and z-score.
score_bp_ref <- function(bp_ref, measure, sex, age, height_percentile, value) {
  sex <- normalize_ref_sex(sex)
  measure <- tolower(trimws(measure))

  if (!measure %in% c("sbp", "dbp")) {
    stop("BP measure must be `sbp` or `dbp`.")
  }

  subset_data <- bp_ref[bp_ref$gender == sex, , drop = FALSE]
  if (nrow(subset_data) == 0) {
    stop("No matching BP reference rows found for the requested sex.")
  }

  bp_percentiles <- suppressWarnings(as.numeric(unique(as.character(subset_data$bp_percentile))))
  bp_percentiles <- sort(bp_percentiles[!is.na(bp_percentiles)])
  ht_bands <- c(5, 10, 25, 50, 75, 90, 95)

  ref_values <- numeric(length(bp_percentiles))

  for (i in seq_along(bp_percentiles)) {
    bp_pct <- bp_percentiles[[i]]
    pct_rows <- subset_data[as.numeric(subset_data$bp_percentile) == bp_pct, , drop = FALSE]
    pct_rows <- pct_rows[order(as.numeric(pct_rows$age)), , drop = FALSE]

    by_height <- numeric(length(ht_bands))
    for (j in seq_along(ht_bands)) {
      col_name <- paste0(measure, "_ht", ht_bands[[j]])
      by_height[[j]] <- stats::approx(
        x = as.numeric(pct_rows$age),
        y = as.numeric(pct_rows[[col_name]]),
        xout = age,
        rule = 2
      )$y
    }

    ref_values[[i]] <- stats::approx(
      x = ht_bands,
      y = by_height,
      xout = height_percentile,
      rule = 2
    )$y
  }

  est_percentile <- stats::approx(
    x = ref_values,
    y = bp_percentiles,
    xout = value,
    rule = 2,
    ties = mean
  )$y

  bounded_percentile <- min(max(est_percentile, 0.1), 99.9)
  z_score <- stats::qnorm(bounded_percentile / 100)

  data.frame(
    measure = measure,
    sex = sex,
    age = age,
    height_percentile = height_percentile,
    value = value,
    percentile = est_percentile,
    z_score = z_score,
    stringsAsFactors = FALSE
  )
}

#' Score a whole dataset and append percentile columns
#'
#' Expected input columns are `sex`, `age`, and any of `height`, `weight`,
#' `bmi`, `wc`, `sbp`, or `dbp`. If `bmi` is missing but both `height` and
#' `weight` are present, BMI is derived automatically. For BP scoring, the
#' dataset should include either `height_percentile` or `height`.
#'
#' @param data Input data frame.
#' @param ref_data Normalized long-format reference data.
#' @param bp_ref Optional BP reference data from `read_iap_bp_ref()`.
#'
#' @return Original data frame with added percentile and z-score columns.
score_dataset_ref <- function(data, ref_data, bp_ref = NULL) {
  names(data) <- normalize_dataset_names(names(data))

  required_cols <- c("sex", "age")
  if (!all(required_cols %in% names(data))) {
    stop("Input dataset must contain: sex, age")
  }

  data$sex <- normalize_ref_sex(data$sex)
  data$age <- as.numeric(data$age)

  if (!"bmi" %in% names(data) && all(c("weight", "height") %in% names(data))) {
    data$bmi <- mapply(bmi_value, weight_kg = as.numeric(data$weight), height_cm = as.numeric(data$height))
  }

  measures <- intersect(c("height", "weight", "bmi", "wc"), names(data))
  if (length(measures) == 0 && !any(c("sbp", "dbp") %in% names(data))) {
    stop("Input dataset must contain at least one of: height, weight, bmi, wc, sbp, dbp")
  }

  for (measure in measures) {
    pct_col <- paste0(measure, "_percentile")
    z_col <- paste0(measure, "_zscore")
    status_col <- paste0(measure, "_status")

    data[[pct_col]] <- NA_real_
    data[[z_col]] <- NA_real_
    data[[status_col]] <- NA_character_

    for (i in seq_len(nrow(data))) {
      value <- suppressWarnings(as.numeric(data[[measure]][[i]]))
      age <- suppressWarnings(as.numeric(data$age[[i]]))
      sex <- data$sex[[i]]

      if (is.na(value) || is.na(age) || is.na(sex) || !nzchar(sex)) {
        data[[status_col]][[i]] <- "missing_input"
        next
      }

      result <- tryCatch(
        score_percentile_ref(
          ref_data = ref_data,
          measure = measure,
          sex = sex,
          age = age,
          value = value
        ),
        error = function(e) e
      )

      if (inherits(result, "error")) {
        data[[status_col]][[i]] <- conditionMessage(result)
        next
      }

      data[[pct_col]][[i]] <- result$percentile[[1]]
      data[[z_col]][[i]] <- result$z_score[[1]]
      data[[status_col]][[i]] <- "ok"
    }
  }

  if (!"height_percentile" %in% names(data)) {
    data$height_percentile <- NA_real_
  }

  if ("height" %in% names(data)) {
    for (i in seq_len(nrow(data))) {
      if (is.na(suppressWarnings(as.numeric(data$height_percentile[[i]])))) {
        height_value <- suppressWarnings(as.numeric(data$height[[i]]))
        age <- suppressWarnings(as.numeric(data$age[[i]]))
        sex <- data$sex[[i]]

        if (is.na(height_value) || is.na(age) || is.na(sex) || !nzchar(sex)) {
          next
        }

        result <- tryCatch(
          score_percentile_ref(
            ref_data = ref_data,
            measure = "height",
            sex = sex,
            age = age,
            value = height_value
          ),
          error = function(e) e
        )

        if (!inherits(result, "error")) {
          data$height_percentile[[i]] <- result$percentile[[1]]
        }
      }
    }
  }

  data$bp_height_percentile <- suppressWarnings(as.numeric(data$height_percentile))

  bp_measures <- intersect(c("sbp", "dbp"), names(data))
  if (length(bp_measures) > 0) {
    if (is.null(bp_ref)) {
      for (measure in bp_measures) {
        data[[paste0(measure, "_percentile")]] <- NA_real_
        data[[paste0(measure, "_zscore")]] <- NA_real_
        data[[paste0(measure, "_status")]] <- "bp_ref_missing"
      }
      return(data)
    }

    for (measure in bp_measures) {
      pct_col <- paste0(measure, "_percentile")
      z_col <- paste0(measure, "_zscore")
      status_col <- paste0(measure, "_status")

      data[[pct_col]] <- NA_real_
      data[[z_col]] <- NA_real_
      data[[status_col]] <- NA_character_

      for (i in seq_len(nrow(data))) {
        value <- suppressWarnings(as.numeric(data[[measure]][[i]]))
        age <- suppressWarnings(as.numeric(data$age[[i]]))
        sex <- data$sex[[i]]
        hp <- suppressWarnings(as.numeric(data$bp_height_percentile[[i]]))

        if (is.na(value) || is.na(age) || is.na(sex) || !nzchar(sex)) {
          data[[status_col]][[i]] <- "missing_input"
          next
        }

        if (is.na(hp)) {
          data[[status_col]][[i]] <- "height_percentile_required"
          next
        }

        result <- tryCatch(
          score_bp_ref(
            bp_ref = bp_ref,
            measure = measure,
            sex = sex,
            age = age,
            height_percentile = hp,
            value = value
          ),
          error = function(e) e
        )

        if (inherits(result, "error")) {
          data[[status_col]][[i]] <- conditionMessage(result)
          next
        }

        data[[pct_col]][[i]] <- result$percentile[[1]]
        data[[z_col]][[i]] <- result$z_score[[1]]
        data[[status_col]][[i]] <- "ok"
      }
    }
  }

  data
}

normalize_dataset_names <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x[x %in% c("gender")] <- "sex"
  x[x %in% c("age_years", "age_yrs", "ageyr")] <- "age"
  x[x %in% c("ht", "height_cm", "stature")] <- "height"
  x[x %in% c("wt", "weight_kg")] <- "weight"
  x[x %in% c("waist", "waist_circumference", "wc_cm")] <- "wc"
  x[x %in% c("body_mass_index")] <- "bmi"
  x[x %in% c("systolic_bp", "systolic", "sbp_mmhg")] <- "sbp"
  x[x %in% c("diastolic_bp", "diastolic", "dbp_mmhg")] <- "dbp"
  x[x %in% c("ht_percentile", "height_p", "height_pct")] <- "height_percentile"
  x
}

normalize_ref_sex <- function(sex) {
  sex <- tolower(trimws(as.character(sex)))
  sex[sex %in% c("m", "male", "boy", "boys")] <- "m"
  sex[sex %in% c("f", "female", "girl", "girls")] <- "f"
  sex
}
