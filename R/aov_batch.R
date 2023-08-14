#' Title Batch ANOVA Test.
#' @description The function provides a convenient way to perform batch analysis
#' of variance (ANOVA) tests on multiple predictor variables with a response
#' variable. It allows users to conduct hypothesis testing for multiple factors
#' simultaneously, making it efficient for analyzing experimental or observational
#' data where the effects of multiple factors need to be examined.
#' @param data data which you need to call.
#' @param y_variable response factor.
#' @param x_variables predictor factors.
#' @param output_file The test result will be output as an excel file. This parameter requires you to
#'  write a file name with the suffix "xlsx", such as: "outcome.xlsx"
#'
#'
#' @export
#'
#' @examples  my_data <- data.frame(
#' Group = rep(c("Control", "Treatment"), each = 10),
#' Age = rep(c("Young", "Middle-aged", "Elderly"), each = 10),
#' Value = c(22, 18, 20, 24, 21, 23, 25, 19, 20, 22,
#'          30, 28, 29, 33, 31, 32, 27, 28, 26, 29,
#'          40, 42, 41, 39, 38, 37, 45, 44, 43, 42))
#'
#' output <- aov_batch(data = my_data, x_variables = c("Group", "Age"),
#'           y_variable = "Value","output.xlsx")




aov_batch <- function(data, y_variable, x_variables, output_file) {
  library(openxlsx)
  results <- data.frame(Variable = character(), F_value = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

  for (variable in x_variables) {
    formula <- as.formula(paste(y_variable, "~", variable))
    aov_result <- summary(aov(formula, data = data))
    result <- data.frame(Variable = variable, F_value = aov_result[[1]][[4]], p_value = aov_result[[1]][[5]])
    results <- rbind(results, result)
  }

  write.xlsx(results, file = output_file, rownames = FALSE)
}
