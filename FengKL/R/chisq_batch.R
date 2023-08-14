#' Title Batch Chi-Square Test
#' @description This function is used to implement batch chi-square test, and write the
#' results into excel file and output.
#' @param data data which you need to call. Note:The data should have a suitable
#'  structure for conducting chi-square test. It should contain categorical variables
#'  or qualitative variables for comparing different groups or categories and calculating
#'  the chi-square statistic.
#' @param x_variables This parameter specifies the predictor variables used in the batch
#'  chi-square test. Note: The predictor variables represent the independent variables
#'  or factors that are hypothesized to affect the outcome. These variables can be either
#'  categorical or qualitative and should be present in the dataset specified by the “data” parameter.
#' @param y_variable This parameter specifies the response variable used in the batch chi-square test.
#'  Note: The response variable represents the dependent variable or outcome of interest.
#'  It should be a categorical or qualitative variable present in the dataset specified by the “data”
#'  parameter. The batch chi-square test compares the distribution of this variable across different
#'  groups or categories defined by the predictor variables.
#' @param output_file The test result will be output as an excel file. This parameter requires you to
#'  write a file name with the suffix "xlsx", such as: "outcome.xlsx"
#'
#' @examples my_data <- data.frame(Gender = c("Male", "Female", "Male", "Male", "Female"),Age = c(
#' "Young", "Young", "Adult", "Adult", "Elder"),Education = c("High School", "College", "College",
#' "High School", "College"),Income = c("Low", "Medium", "Low", "High", "Medium"))
#'  output <- chisq_batch(data = my_data, x_variables = c("Age", "Gender", "Education"),
#'  y_variable = "Income", "chisq_results.xlsx")
#' @export
#'
#'
chisq_batch <- function(data, x_variables, y_variable, output_file) {
  library(openxlsx)
  results <- data.frame(Variable = character(), Chi_square = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

  for (variable in x_variables) {
    chisq_result <- chisq.test(data[[variable]], data[[y_variable]])
    result <- data.frame(Variable = variable, Chi_square = chisq_result$statistic, p_value = chisq_result$p.value)
    results <- rbind(results, result)
  }

  write.xlsx(results, file = output_file, rownames = FALSE)
  return(results)
}
