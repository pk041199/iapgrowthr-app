test_that("generic anthro reader converts height and weight wide files", {
  path <- tempfile(fileext = ".csv")
  writeLines(
    c(
      "age,sex,height_p3,height_p50,wt_p3,wt_p50",
      "5,m,100,108,13,17"
    ),
    con = path
  )

  ref_data <- read_iap_anthro_ref(path)

  expect_equal(sort(unique(ref_data$measure)), c("height", "weight"))
  expect_equal(sort(unique(ref_data$percentile)), c("3", "50"))
  expect_equal(nrow(ref_data), 4)
})

test_that("bmi reader converts wide BMI files", {
  path <- tempfile(fileext = ".csv")
  writeLines(
    c(
      "age,gender,bmi_p3,bmi_p5,bmi_p50,bmi_75",
      "5,f,11.8,12.1,14.2,15.5"
    ),
    con = path
  )

  ref_data <- read_iap_bmi_ref(path)

  expect_true(all(ref_data$measure == "bmi"))
  expect_equal(sort(unique(ref_data$percentile)), c("3", "5", "50", "75"))
})

test_that("wc reader converts wide WC files", {
  path <- tempfile(fileext = ".csv")
  writeLines(
    c(
      "age,sex,wc_3p,wc_50p,wc_95",
      "5,m,47,53,63"
    ),
    con = path
  )

  ref_data <- read_iap_wc_ref(path)

  expect_true(all(ref_data$measure == "wc"))
  expect_equal(sort(unique(ref_data$percentile)), c("3", "50", "95"))
})

test_that("percentile scoring interpolates from percentile tables", {
  ref_data <- data.frame(
    measure = rep("height", 6),
    sex = rep("m", 6),
    age = rep(c(5, 10, 15), times = 2),
    percentile = rep(c("3", "50"), each = 3),
    value = c(100, 120, 140, 110, 130, 150),
    stringsAsFactors = FALSE
  )

  result <- score_percentile_ref(
    ref_data = ref_data,
    measure = "height",
    sex = "m",
    age = 10,
    value = 125
  )

  expect_true(result$percentile > 3)
  expect_true(result$percentile < 50)
})

test_that("bp reader normalizes the BP schema", {
  path <- tempfile(fileext = ".csv")
  writeLines(
    c(
      paste(
        c(
          "age", "gender", "bp_percentile",
          "sbp_ht5", "sbp_ht10", "sbp_ht25", "sbp_ht50", "sbp_ht75", "sbp_ht90", "sbp_ht95",
          "dbp_ht5", "dbp_ht10", "dbp_ht25", "dbp_ht50", "dbp_ht75", "dbp_ht90", "dbp_ht95"
        ),
        collapse = ","
      ),
      "3,m,50,90,91,92,93,94,95,96,60,61,62,63,64,65,66",
      "3,m,90,99,100,101,102,103,104,105,70,71,72,73,74,75,76"
    ),
    con = path
  )

  ref_data <- read_iap_bp_ref(path)

  expect_equal(ref_data$age[[1]], 3)
  expect_equal(ref_data$gender[[1]], "m")
  expect_equal(ref_data$dbp_ht95[[2]], 76)
})

test_that("cdc hc reader validates required columns", {
  path <- tempfile(fileext = ".csv")
  writeLines(
    c(
      "sex,age_months,percentile,value",
      "m,0,50,34.5",
      "f,0,50,33.8"
    ),
    con = path
  )

  ref_data <- read_cdc_hc_ref(path)

  expect_equal(nrow(ref_data), 2)
  expect_equal(ref_data$age_months[[1]], 0)
})

test_that("chart functions run with minimal valid reference data", {
  ref_data <- data.frame(
    measure = rep(c("height", "weight", "bmi"), each = 6),
    sex = "m",
    age = rep(c(5, 10, 15), times = 6),
    percentile = rep(c("3", "50"), each = 3, times = 3),
    value = c(
      100, 130, 155,
      110, 140, 170,
      14, 17, 19,
      18, 22, 26,
      12.5, 15, 16.5,
      14.5, 18, 21
    )
  )

  expect_invisible(plot_iap_height_weight_chart(ref_data, sex = "m"))
  expect_invisible(plot_iap_bmi_chart(ref_data, sex = "m"))
})
