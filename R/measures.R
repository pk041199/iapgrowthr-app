#' Calculate body mass index
#'
#' @param weight_kg Weight in kilograms.
#' @param height_cm Height in centimeters.
#'
#' @return Numeric BMI in kg/m^2.
bmi_value <- function(weight_kg, height_cm) {
  if (is.na(weight_kg) || is.na(height_cm)) {
    return(NA_real_)
  }

  if (height_cm <= 0) {
    stop("`height_cm` must be positive.")
  }

  weight_kg / ((height_cm / 100) ^ 2)
}

