#' @title Analyze ROC Curves
#'
#' @description This function analyzes ROC curves from the results of the \code{\link{assess_models}} function
#'
#' @param modelAssessment The output of the \code{\link{assess_models}} function
#' @param plot If TRUE, plots a matrix displaying the results of all model comparisons. Defaults to TRUE.
#' @param plot_diagonal if TRUE, the matrix plot will show repeated (inverted) values on the opposite diagonal. Defaults to FALSE.
#'
#' @return A dataframe with the results of statistical tests conducted on the ROCs for each model pairing
#'
#' @seealso \code{\link{assess_models}}
#'
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' strong_movie_review_data$cleanText = clean_text(strong_movie_review_data$text)
#' mild_movie_review_data$cleanText = clean_text(mild_movie_review_data$text)
#'
#' # Using language to predict "Positive" vs. "Negative" reviews
#' # Only for strong reviews (ratings of 1 or 10)
#' movie_model_strong = language_model(strong_movie_review_data,
#'                                      outcomeVariableColumnName = "valence",
#'                                      outcomeVariableType = "binary",
#'                                      textColumnName = "cleanText",
#'                                      progressBar = FALSE)
#'
#' # Using language to predict "Positive" vs. "Negative" reviews
#' # Only for mild reviews (ratings of 4 or 7)
#' movie_model_mild = language_model(mild_movie_review_data,
#'                                      outcomeVariableColumnName = "valence",
#'                                      outcomeVariableType = "binary",
#'                                      textColumnName = "cleanText",
#'                                      progressBar = FALSE)
#'
#' # Create the model assessment
#' movie_assessment = assess_models(movie_model_strong, movie_model_mild)
#'
#' # Analyze ROC curves
#' auc_tests = analyze_roc(movie_assessment)
#' }

analyze_roc = function(modelAssessment, plot=TRUE, plot_diagonal=FALSE) {

  model1=model2=model2_auc=model1_auc=size=width=height=sig=font=sig_TF=NULL

  if (class(modelAssessment) != "modelAssessment") {
    stop("The input for this function must be an output from the `assess_models` function.")
  }

  if (modelAssessment@type != "binary") {
    stop("This function can only be used on assessments of models predicting a binary outcome.")
  }

  auc_tests = modelAssessment@auc_tests

  if (plot_diagonal) {
    auc_tests2 = auc_tests
    colnames(auc_tests2) = c("model2", "model1", "model2_auc", "model1_auc", "p_value", "sig_TF", "font", "size", "width", "height", "sig")
    auc_tests = rbind(auc_tests, auc_tests2)
  }

  if(plot & nrow(modelAssessment@model_labels) > 1) {
    r = suppressWarnings(ggplot(auc_tests) +
                           geom_tile(aes(x=model1, y=model2, fill=(model2_auc-model1_auc), size=size, width=width, height=height), color="black") +
                           geom_text(aes(x=model1, y=model2, label=sig, fontface=font)) +
                           scale_fill_gradient2(low="#ff4c4c", mid="#ffffff", high="#4c4cff", midpoint=0) +
                           scale_x_discrete(position = "top", drop=FALSE) +
                           scale_y_discrete(drop=FALSE) +
                           scale_size_discrete(range=c(1,2)) +
                           labs(fill="Difference\nbetween\nAUCs\n(Model2 - Model1)", title = "Testing differences between ROC curves\n(significance labeled)") +
                           guides(size=FALSE) +
                           coord_fixed() +
                           theme(panel.grid = element_blank(),
                                 panel.background = element_blank(),
                                 axis.text.x = element_text(angle=90)))
    print(r)
  }

  auc_tests = subset(auc_tests, select = -c(sig_TF, font, size, width, height))

  return(auc_tests)
}
