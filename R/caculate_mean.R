#' Title Batch Caculate Mean
#' @description This function provides a simple and efficient way to calculate the mean value
#' of one or more variables in a dataset. It allows users to compute the means for
#' multiple variables simultaneously, making it convenient for analyzing and
#' summarizing data.
#' @param data data which you need to call.
#' @param y_variable outcome variable.
#' @param x_variables grouping variable.
#' @param output_file The test result will be output as an excel file. This
#' parameter requires you to write a file name with the suffix "xlsx", such as:
#' "outcome.xlsx"
#' @export
#'
#' @examples my_data <- data.frame(
#' Group = c("A", "A", "B", "B"),
#' Value1 = c(10, 15, 12, 18),
#' Value2 = c(5, 8, 6, 9))
#' calculate_mean(data = my_data, y_variable = "Value1", x_variables = c("Group"),
#' output_file = "output.xlsx")
calculate_mean <- function(data, y_variable, x_variables, output_file) {
  library(openxlsx)
  result <- data.frame(Variable = character(), Mean = double(), stringsAsFactors = FALSE)
  for (x_var in x_variables) {
    mean_results <- aggregate(data[[y_variable]] ~ data[[x_var]], data = data, FUN = mean, na.rm = TRUE)
    temp_var <- as.character(mean_results[[2]])
    temp_mean <- mean_results[[1]]
    result <- rbind(result, data.frame(Variable = temp_var, Mean = temp_mean))
  }
  write.csv(result, file = output_file, row.names = FALSE)
}
