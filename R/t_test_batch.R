#' Title Batch T-test
#' @description This function provides a convenient way to perform batch t-tests
#' on multiple groups or conditions. It allows users to compare means between
#' groups or conditions for multiple variables simultaneously, making it efficient
#' for analyzing experimental or observational data where the effects of multiple
#' factors need to be examined.
#' @param data data which you need to call.
#' @param y_variable response factor.
#' @param x_variables predictor factors.
#' @param output_file The test result will be output as an excel file. This
#' parameter requires you to write a file name with the suffix "xlsx", such as:
#' "outcome.xlsx"
#'
#'
#' @export
#'
#' @examples my_data <- data.frame(
#'Group = rep(c("A", "B", "C"), each = 10),
#'Condition = rep(c("Control", "Treatment"), each = 5),
#'Value = c(22, 18, 20, 24, 21, 23, 25, 19, 20, 22,
#'          30, 28, 29, 33, 31, 32, 27, 28, 26, 29,
#'          40, 42, 41, 39, 38, 37, 45, 44, 43, 42))
#'output <- t_test_batch(data = my_data, groups = c("Group", "Condition"),
#'          variables = "Value","t_test.xlsx")


t_test_batch <- function(data, y_variable, x_variables, output_file) {
  library(openxlsx)
  results <- data.frame(Variable = character(), t_value = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

  for (variable in x_variables) {
    test_result <- t.test(data[[y_variable]], data[[variable]])
    result <- data.frame(Variable = variable, t_value = test_result$statistic, p_value = test_result$p.value)
    results <- rbind(results, result)
  }

  write.xlsx(results, file = output_file, rownames = FALSE)
}
