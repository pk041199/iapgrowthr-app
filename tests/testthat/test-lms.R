test_that("lms z score matches median at zero", {
  expect_equal(lms_zscore(10, l = 1, m = 10, s = 0.1), 0)
})

test_that("lms percentile matches median at fifty", {
  expect_equal(lms_percentile(10, l = 1, m = 10, s = 0.1), 50)
})

test_that("bmi calculation works", {
  expect_equal(round(bmi_value(20, 110), 2), 16.53)
})
