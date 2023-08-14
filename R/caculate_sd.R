#' Title Batch Caculate SD
#' @description This function provides a function to calculate the standard
#' deviation of one or more variables in a dataset and save the results to an
#' Excel file using the openxlsx package.
#' @param data data which you need to call.
#' @param y_variable outcome variable.
#' @param x_variables grouping variable.
#' @param output_file The test result will be output as an excel file. This
#' parameter requires you to write a file name with the suffix "xlsx", such as:
#' "outcome.xlsx"
#'
#'
#' @export
#'
#' @examples my_data <- data.frame(
#' Group = c("A", "A", "B", "B"),
#' Value1 = c(10, 15, 12, 18),
#' Value2 = c(5, 8, 6, 9))
#' calculate_sd(data = my_data, y_variable = "Value1", x_variables = c("Group"),
#' output_file = "output.xlsx")

calculate_sd <- function(data, y_variable, x_variables, output_file) {
  library(openxlsx)
  result <- data.frame(Variable = character(), SD = double(), stringsAsFactors = FALSE)
  for (x_var in x_variables) {
    sd_results <- aggregate(data[[y_variable]] ~ data[[x_var]], data = data, FUN = sd, na.rm = TRUE)
    temp_var <- as.character(sd_results[[2]])
    temp_sd <- sd_results[[1]]
    result <- rbind(result, data.frame(Variable = temp_var, SD = temp_sd))
  }
  write.xlsx(result, file = output_file, rownames = FALSE)
}
