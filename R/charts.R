#' Plot IAP height and weight chart
#'
#' `ref_data` must be in long format with columns:
#' `measure`, `sex`, `age`, `percentile`, `value`.
#'
#' @param ref_data Reference data from [read_iap_anthro_ref()] or equivalent.
#' @param sex One of `"m"`, `"male"`, `"f"`, or `"female"`.
#' @param observations Optional data frame with columns `measure`, `age`, `value`.
#' @param main Optional chart title.
#'
#' @return Invisibly returns `NULL`.
plot_iap_height_weight_chart <- function(ref_data,
                                         sex,
                                         observations = NULL,
                                         main = NULL) {
  sex <- normalize_sex(sex)
  chart_data <- ref_data[
    ref_data$sex == sex & ref_data$measure %in% c("height", "weight"),
    ,
    drop = FALSE
  ]

  if (nrow(chart_data) == 0) {
    stop("No matching height/weight reference rows found for `sex`.")
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::layout(matrix(c(1, 2), ncol = 1), heights = c(1, 1))
  plot_single_measure(
    subset(chart_data, measure == "height"),
    observations = subset_or_null(observations, "height"),
    ylab = "Stature (cm)",
    main = if (is.null(main)) sprintf("IAP %s Height Chart", sex_label(sex)) else main
  )
  plot_single_measure(
    subset(chart_data, measure == "weight"),
    observations = subset_or_null(observations, "weight"),
    ylab = "Weight (kg)",
    main = sprintf("IAP %s Weight Chart", sex_label(sex))
  )

  invisible(NULL)
}

#' Plot IAP BMI chart
#'
#' `ref_data` must be in long format with columns:
#' `measure`, `sex`, `age`, `percentile`, `value`.
#'
#' @param ref_data Reference data from [read_iap_anthro_ref()] or equivalent.
#' @param sex One of `"m"`, `"male"`, `"f"`, or `"female"`.
#' @param observations Optional data frame with columns `measure`, `age`, `value`.
#' @param main Optional chart title.
#'
#' @return Invisibly returns `NULL`.
plot_iap_bmi_chart <- function(ref_data,
                               sex,
                               observations = NULL,
                               main = NULL) {
  sex <- normalize_sex(sex)
  chart_data <- ref_data[
    ref_data$sex == sex & ref_data$measure == "bmi",
    ,
    drop = FALSE
  ]

  if (nrow(chart_data) == 0) {
    stop("No matching BMI reference rows found for `sex`.")
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  plot_single_measure(
    chart_data,
    observations = subset_or_null(observations, "bmi"),
    ylab = expression(BMI ~ (kg/m^2)),
    main = if (is.null(main)) sprintf("IAP %s BMI Chart", sex_label(sex)) else main,
    bmi_mode = TRUE
  )

  invisible(NULL)
}

plot_single_measure <- function(ref_data,
                                observations = NULL,
                                ylab,
                                main,
                                bmi_mode = FALSE) {
  if (nrow(ref_data) == 0) {
    stop("No reference data available for requested measure.")
  }

  percentiles <- unique(as.character(ref_data$percentile))
  ages <- sort(unique(as.numeric(ref_data$age)))
  values <- as.numeric(ref_data$value)

  graphics::par(mar = c(4, 4, 3, 1))
  graphics::plot(
    x = range(ages, na.rm = TRUE),
    y = range(values, na.rm = TRUE),
    type = "n",
    xlab = "Age in years",
    ylab = ylab,
    main = main,
    xaxt = "n",
    yaxt = "n"
  )
  graphics::axis(1, at = pretty(ages, n = 12))
  graphics::axis(2, at = pretty(values, n = 12), las = 1)
  graphics::grid(nx = NA, ny = NULL, col = "#d9d9d9", lty = 1)

  for (pct in sort_percentiles(percentiles)) {
    line_data <- ref_data[as.character(ref_data$percentile) == pct, , drop = FALSE]
    line_data <- line_data[order(as.numeric(line_data$age)), , drop = FALSE]

    line_col <- percentile_color(pct, bmi_mode = bmi_mode)
    graphics::lines(
      x = as.numeric(line_data$age),
      y = as.numeric(line_data$value),
      lwd = if (pct %in% c("23", "27")) 2 else 1.5,
      col = line_col
    )

    label_x <- utils::tail(as.numeric(line_data$age), 1)
    label_y <- utils::tail(as.numeric(line_data$value), 1)
    graphics::text(label_x, label_y, labels = pct, pos = 4, cex = 0.75, col = line_col)
  }

  if (!is.null(observations) && nrow(observations) > 0) {
    graphics::points(
      x = as.numeric(observations$age),
      y = as.numeric(observations$value),
      pch = 16,
      col = "red",
      cex = 2
    )
    graphics::lines(
      x = as.numeric(observations$age),
      y = as.numeric(observations$value),
      col = "#1f3a5f",
      lwd = 1.5
    )
  }
}

subset_or_null <- function(observations, measure) {
  if (is.null(observations)) {
    return(NULL)
  }

  out <- observations[observations$measure == measure, , drop = FALSE]
  if (nrow(out) == 0) {
    return(NULL)
  }

  out
}

sort_percentiles <- function(x) {
  order_key <- suppressWarnings(as.numeric(x))
  order_key[is.na(order_key)] <- Inf
  x[order(order_key, x)]
}

percentile_color <- function(percentile, bmi_mode = FALSE) {
  if (!bmi_mode) {
    return("#202020")
  }

  if (percentile == "23") {
    return("#d9792b")
  }

  if (percentile == "27") {
    return("#cc4c4c")
  }

  "#202020"
}

normalize_sex <- function(sex) {
  sex <- tolower(trimws(sex))

  if (sex %in% c("male", "m", "boy", "boys")) {
    return("m")
  }

  if (sex %in% c("female", "f", "girl", "girls")) {
    return("f")
  }

  stop("Unsupported `sex` value.")
}

sex_label <- function(sex) {
  if (identical(sex, "m")) {
    return("Boys")
  }

  "Girls"
}

plot_bp_percentile_chart <- function(bp_ref,
                                     sex,
                                     measure = "sbp",
                                     age,
                                     height_pct,
                                     value) {
  
  sex <- tolower(sex)
  
  bp_data <- bp_ref[bp_ref$gender == sex, ]
  
  ht_band <- c(5,10,25,50,75,90,95)
  
  nearest_ht <- ht_band[
    which.min(abs(ht_band - height_pct))
  ]
  
  col_name <- paste0(measure, "_ht", nearest_ht)
  
  p90 <- bp_data[
    bp_data$bp_percentile == 90,
    c("age", col_name)
  ]
  
  p95 <- bp_data[
    bp_data$bp_percentile == 95,
    c("age", col_name)
  ]
  
  p99 <- bp_data[
    bp_data$bp_percentile == 99,
    c("age", col_name)
  ]
  
  names(p90)[2] <- "p90"
  names(p95)[2] <- "p95"
  names(p99)[2] <- "p99"
  
  plot_df <- Reduce(
    merge,
    list(p90, p95, p99)
  )
  
  library(ggplot2)
  
  ggplot(plot_df, aes(age)) +
    
    geom_line(
      aes(y = p90, colour = "90th Percentile"),
      linewidth = 1
    ) +
    
    geom_line(
      aes(y = p95, colour = "95th Percentile"),
      linewidth = 1
    ) +
    
    geom_line(
      aes(y = p99, colour = "99th Percentile"),
      linewidth = 1
    ) +
    
    geom_point(
      data = data.frame(
        age = age,
        bp = value
      ),
      aes(
        x = age,
        y = bp
      ),
      colour = "red",
      size = 4
    ) +
    
    labs(
      title = paste(
        toupper(measure),
        "Percentile Chart"
      ),
      subtitle = paste(
        "Height percentile:",
        nearest_ht
      ),
      x = "Age (years)",
      y = "Blood Pressure (mmHg)",
      colour = "Threshold"
    ) +
    
    theme_minimal()
}