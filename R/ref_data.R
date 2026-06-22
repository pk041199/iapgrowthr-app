#' Read IAP anthropometry reference data
#'
#' Supports either normalized long-format files with columns
#' `measure`, `sex`, `age`, `percentile`, `value`, or the current repository's
#' wide-format IAP CSV files for height/weight, BMI, and waist circumference.
#'
#' @param path Path to a CSV file.
#'
#' @return Data frame in normalized long format.
read_iap_anthro_ref <- function(path) {
  ref_data <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  raw_names <- names(ref_data)
  norm_names <- normalize_names(raw_names)

  if (all(c("measure", "sex", "age", "percentile", "value") %in% norm_names)) {
    names(ref_data) <- norm_names
    ref_data$measure <- tolower(trimws(ref_data$measure))
    ref_data$sex <- normalize_sex_vector(ref_data$sex)
    ref_data$age <- as.numeric(ref_data$age)
    ref_data$percentile <- as.character(ref_data$percentile)
    ref_data$value <- as.numeric(ref_data$value)
    return(ref_data[, c("measure", "sex", "age", "percentile", "value")])
  }

  if (all(c("age", "sex") %in% norm_names) && any(grepl("^height_p", norm_names))) {
    return(read_iap_hw_ref(path))
  }

  if (all(c("age", "gender") %in% norm_names) && any(grepl("^bmi_", norm_names))) {
    return(read_iap_bmi_ref(path))
  }

  if (all(c("age", "sex") %in% norm_names) && any(grepl("^wc_", norm_names))) {
    return(read_iap_wc_ref(path))
  }

  stop(
    "Unsupported anthropometry CSV schema. Expected either long-format data or one of the current IAP ht/wt, BMI, or WC files."
  )
}

#' Read IAP height and weight percentile references
#'
#' Expected columns include `age`, `sex`, `height_p*`, and `wt_p*`.
#'
#' @param path Path to the height/weight CSV.
#'
#' @return Normalized long-format data frame.
read_iap_hw_ref <- function(path) {
  ref_data <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(ref_data) <- normalize_names(names(ref_data))

  required_cols <- c("age", "sex")
  if (!all(required_cols %in% names(ref_data))) {
    stop("Height/weight reference CSV must contain: age, sex")
  }

  height_cols <- grep("^height_p", names(ref_data), value = TRUE)
  weight_cols <- grep("^wt_p", names(ref_data), value = TRUE)

  if (length(height_cols) == 0 || length(weight_cols) == 0) {
    stop("Height/weight reference CSV must contain percentile columns for both height and weight.")
  }

  height_long <- wide_measure_to_long(ref_data, "height", "sex", "age", height_cols)
  weight_long <- wide_measure_to_long(ref_data, "weight", "sex", "age", weight_cols)

  rbind(height_long, weight_long)
}

#' Read IAP BMI percentile references
#'
#' Expected columns include `age`, `gender`, and `bmi_*` percentile columns.
#'
#' @param path Path to the BMI CSV.
#'
#' @return Normalized long-format data frame.
read_iap_bmi_ref <- function(path) {
  ref_data <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(ref_data) <- normalize_names(names(ref_data))

  required_cols <- c("age", "gender")
  if (!all(required_cols %in% names(ref_data))) {
    stop("BMI reference CSV must contain: age, gender")
  }

  bmi_cols <- grep("^bmi_", names(ref_data), value = TRUE)
  if (length(bmi_cols) == 0) {
    stop("BMI reference CSV must contain `bmi_*` percentile columns.")
  }

  wide_measure_to_long(ref_data, "bmi", "gender", "age", bmi_cols)
}

#' Read IAP waist circumference percentile references
#'
#' Expected columns include `age`, `sex`, and `wc_*` percentile columns.
#'
#' @param path Path to the waist circumference CSV.
#'
#' @return Normalized long-format data frame.
read_iap_wc_ref <- function(path) {
  ref_data <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(ref_data) <- normalize_names(names(ref_data))

  required_cols <- c("age", "sex")
  if (!all(required_cols %in% names(ref_data))) {
    stop("WC reference CSV must contain: age, sex")
  }

  wc_cols <- grep("^wc_", names(ref_data), value = TRUE)
  if (length(wc_cols) == 0) {
    stop("WC reference CSV must contain `wc_*` percentile columns.")
  }

  wide_measure_to_long(ref_data, "wc", "sex", "age", wc_cols)
}

#' Read IAP blood pressure reference data
#'
#' Expected wide columns:
#' `age`, `gender`, `bp_percentile`, and height-specific systolic/diastolic
#' columns such as `sbp_ht5` and `dbp_ht5`.
#'
#' @param path Path to a CSV file.
#'
#' @return Data frame with normalized BP reference columns.
read_iap_bp_ref <- function(path) {
  ref_data <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(ref_data) <- normalize_bp_names(names(ref_data))

  required_cols <- c(
    "age", "gender", "bp_percentile",
    "sbp_ht5", "sbp_ht10", "sbp_ht25", "sbp_ht50", "sbp_ht75", "sbp_ht90", "sbp_ht95",
    "dbp_ht5", "dbp_ht10", "dbp_ht25", "dbp_ht50", "dbp_ht75", "dbp_ht90", "dbp_ht95"
  )

  if (!all(required_cols %in% names(ref_data))) {
    stop("BP reference CSV must contain: ", paste(required_cols, collapse = ", "))
  }

  ref_data$age <- fill_down(ref_data$age)
  ref_data$gender <- fill_down(ref_data$gender)
  ref_data$age <- as.numeric(ref_data$age)
  ref_data$gender <- normalize_sex_vector(ref_data$gender)
  ref_data$bp_percentile <- as.character(ref_data$bp_percentile)

  numeric_cols <- setdiff(required_cols, c("age", "gender", "bp_percentile"))
  for (col in numeric_cols) {
    ref_data[[col]] <- as.numeric(ref_data[[col]])
  }

  ref_data
}

#' Read CDC head circumference reference data
#'
#' Expected columns:
#' `sex`, `age_months`, `percentile`, `value`
#'
#' @param path Path to a CSV file.
#'
#' @return Data frame with normalized column types.
read_cdc_hc_ref <- function(path) {
  ref_data <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(ref_data) <- normalize_names(names(ref_data))
  required_cols <- c("sex", "age_months", "percentile", "value")

  if (!all(required_cols %in% names(ref_data))) {
    stop("CDC HC reference CSV must contain: ",
         paste(required_cols, collapse = ", "))
  }

  ref_data$sex <- normalize_sex_vector(ref_data$sex)
  ref_data$age_months <- as.numeric(ref_data$age_months)
  ref_data$percentile <- as.character(ref_data$percentile)
  ref_data$value <- as.numeric(ref_data$value)

  ref_data[, required_cols]
}

wide_measure_to_long <- function(ref_data, measure, sex_col, age_col, value_cols) {
  out <- vector("list", length(value_cols))

  for (i in seq_along(value_cols)) {
    col <- value_cols[[i]]
    percentile <- extract_percentile_from_name(col)

    out[[i]] <- data.frame(
      measure = measure,
      sex = normalize_sex_vector(ref_data[[sex_col]]),
      age = as.numeric(ref_data[[age_col]]),
      percentile = percentile,
      value = as.numeric(ref_data[[col]]),
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

extract_percentile_from_name <- function(name_in) {
  hit <- regmatches(name_in, regexpr("[0-9]+", name_in))
  if (length(hit) == 0 || identical(hit, character(0)) || !nzchar(hit)) {
    stop("Could not extract percentile from column name: ", name_in)
  }
  hit
}

fill_down <- function(x) {
  last_value <- NA_character_
  out <- x

  for (i in seq_along(out)) {
    current <- trimws(as.character(out[[i]]))
    if (!nzchar(current) || identical(current, "NA")) {
      out[[i]] <- last_value
    } else {
      last_value <- current
      out[[i]] <- current
    }
  }

  out
}

normalize_bp_names <- function(names_in) {
  names_out <- normalize_names(names_in)

  if ("sbp_ht5.1" %in% names_out && !"dbp_ht5" %in% names_out) {
    names_out[names_out == "sbp_ht5.1"] <- "dbp_ht5"
  }

  if (sum(names_out == "sbp_ht5") == 2 && !"dbp_ht5" %in% names_out) {
    second <- which(names_out == "sbp_ht5")[2]
    names_out[second] <- "dbp_ht5"
  }

  names_out
}

normalize_names <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("%", "p", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

normalize_sex_vector <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x %in% c("male", "m", "boy", "boys")] <- "m"
  x[x %in% c("female", "f", "girl", "girls")] <- "f"
  x
}
