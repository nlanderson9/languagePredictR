#' compModel Class
#'
#' @slot call The function called to generate this model, with all arguments specified by the user
#' @slot data_predictor The predictor variable input to create the model
#' @slot data_outcome The outcome variable input to create the model
#' @slot type Model type, "binary" or "continuous"
#' @slot predictor The name of the column in the test dataframe containing the data_predictor
#' @slot outcome The name of the column in the test dataframe containing the data_outcome
#' @slot y The dependent (outcome) variable
#' @slot glm The general linear model created
#' @slot predicted_y The predicted outcomes based on the model and original predictor data
#' @slot predicted_probabilities (If binary) The predicted probabilities of the outcomes based on the model and original predictor data
#' @slot roc (If binary) The ROC calculated using the predicted_y
#' @slot roc_ci (If binary) The boostrapped confidence interval calculated for the ROC
#' @slot corr (If continuous) The correlation using the predicted_y
#' @slot level0 The bottom/first level of a binary variable, or the lowest value of a continuous variable
#' @slot level1 The top/second level of a binary variable, or the highest value of a continuous variable
#'
#' @export compModel
#' @exportClass compModel

compModel = setClass("compModel", slots = c("call", "data_predictor", "data_outcome", "type", "predictor", "outcome", "y", "glm", "predicted_y", "predicted_probabilities", "roc", "roc_ci", "corr", "level0", "level1"))










#' Create Comparison Model
#'
#' @description This function creates a regression model using a single numeric variable as a predictor, and a specified variable as the outcome. It is intended for comparison against models that use language as a predictor (created by \code{\link{language_model}}).
#'
#' @param input A dataframe containing a column with predictor data (numeric variable) and an outcome variable (numeric or two-level factor)
#' @param outcome A string consisting of the column name for the outcome variable in \code{inputDataframe}
#' @param outcomeType A string consisting of the type of outcome variable being used - options are "binary" or "continuous"
#' @param predictor A string consisting of the column name for the predictor data in \code{inputDataframe}
#' @param progressBar Show a progress bar. Defaults to TRUE.
#'
#' @return An object of the type "compModel"
#' @export
#'
#' @import quanteda
#' @import dplyr
#' @importFrom pROC roc
#' @importFrom stats cor.test
#' @importFrom methods setClass new
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' movie_review_data1$cleanText = clean_text(movie_review_data1$text)
#'
#' # Using language to predict "Positive" vs. "Negative" reviews
#' movie_model_valence_language = language_model(movie_review_data1,
#'                                      outcome = "valence",
#'                                      outcomeType = "binary",
#'                                      text = "cleanText")
#'
#' summary(movie_model_valence_language)
#'
#' # Is it possible that people write more for negative reviews?
#' # How does that compare to the language predictors?
#' movie_review_data1$word_count = corpus(movie_model_data1$cleanText) %>% tokens() %>% ntoken()
#'
#' # Using word count to predict "Positive" vs. "Negative" reviews
#' movie_model_valence_wordcount = comparison_model(movie_review_data1,
#'                                      outcome = "valence",
#'                                      outcomeType = "binary",
#'                                      predictor = "word_count")
#'
#' summary(movie_model_valence_wordcount)
#' }

comparison_model = function(input, outcome, outcomeType, predictor, progressBar=TRUE) {

  weights=words=NULL

  call = match.call()

  td = input

  if (!is.data.frame(td)) {
    stop("The `input` argument must be a dataframe.")
  }

  if (outcomeType == "binary") {
    if (!is.factor(td[[outcome]])) {
      stop("Your binary outcome variable must be a factor.")
    }
    else {
      if (nlevels(td[[outcome]]) != 2) {
        stop("Your binary outcome variable must have exactly 2 levels.")
      }
      level0 = levels(td[[outcome]])[1]
      level1 = levels(td[[outcome]])[2]
      td$cat = as.numeric(td[[outcome]]) - 1
    }
  }
  else if (outcomeType == "continuous") {
    if (!is.numeric(td[[outcome]])) {
      stop("Your continuous outcome variable must be numeric.")
    }
    level0 = paste("low-", outcome, sep="")
    level1 = paste("high-", outcome, sep="")
    td$cat = td[[outcome]]
  }
  else {
    stop("The `outcomeType` argument must be either 'binary' or 'continuous'.")
  }

  if (!is.numeric(td[[predictor]])) {
    stop("The `predictor` argument must be numeric.")
  }

  m1dat<-subset(td, !is.na(cat))

  x = m1dat[[predictor]]
  y<-m1dat$cat

  #train and cross validate model
  familytype = ifelse(outcomeType == "binary", "binomial", "gaussian")

  glm = glm(y ~ x, family=familytype)

  predicted_y = as.numeric(predict(glm, newx=x))

  if(progressBar){
    roc_progress = "text"
  }
  else {
    roc_progress = "none"
  }

  if (outcomeType == "binary") {
    predicted_probabilities = as.numeric(predict(glm, newx=x, type="response"))
    roc = suppressMessages(roc(y, predicted_y, ci=TRUE))
    if(progressBar){
      cat("Boostrapping ROC...")
    }
    roc_ci = ci.sp(roc, sensitivities=seq(0,1,.01), progress=roc_progress)
    corr = NA
  }
  else if (outcomeType == "continuous") {
    corr = cor.test(y, predicted_y)
    predicted_probabilities = NA
    roc = NA
    roc_ci = NA
    corr = NA
  }


  output = new("compModel", call=call, data_predictor=input[[predictor]], data_outcome=input[[outcome]], type=outcomeType, predictor=predictor, outcome=outcome, y=y, glm=glm, predicted_y=predicted_y, predicted_probabilities=predicted_probabilities, roc=roc, roc_ci=roc_ci, corr=corr, level0=level0, level1=level1)

  return(output)
}




#' Summary (compModel)
#'
#' @param object The compModel object to summarize
#' @param ... Additional arguments
#'
#' @export
#'
#' @importFrom yardstick metrics
#'
#' @method summary compModel

summary.compModel = function(object, ...){

  original=predicted_prob=predicted_class=predicted=NULL

  summary_list = list()
  summary_list[["model.name"]] = deparse(substitute(object))

  call_string = deparse(object@call)
  call_string = paste(call_string, collapse = " ")
  call_string = gsub("\\s+", " ", call_string)
  call_string = paste("Call::", call_string)

  cat(paste0(call_string,"\n\n"))
  summary_list[["call"]] = call_string
  cat(paste("Number of samples provided (n):", length(object@data_predictor),"\n"))
  summary_list[["samples"]] = nrow(object@data_predictor)
  cat(paste0("Predictor variable: '", object@predictor,"'\n"))
  summary_list[["predictor"]] = object@predictor
  cat(paste0("Outcome variable: '", object@outcome,"'\n\n"))
  summary_list[["outcome"]] = object@outcome
  cat("Various model evaluation metrics:\n")
  cat("   (Caution: these were obtained by using the model to predict outcomes based on the original dataset)\n\n")
  if (object@type == "binary") {
    metric_dataframe = data.frame(original=object@y, predicted_prob=object@predicted_probabilities)
    metric_dataframe$predicted_class = ifelse(metric_dataframe$predicted_prob >=.5, 1, 0)
    metric_dataframe$original = as.factor(metric_dataframe$original)
    metric_dataframe$predicted_class = as.factor(metric_dataframe$predicted_class)
    if (yardstick::roc_auc(metric_dataframe, original, predicted_prob)$.estimate < .5) {
      metric_dataframe$original = factor(metric_dataframe$original, levels=c(levels(metric_dataframe$original)[2], levels(metric_dataframe$original)[1]))
      metric_dataframe$predicted_class = factor(metric_dataframe$predicted_class, levels=c(levels(metric_dataframe$predicted_class)[2], levels(metric_dataframe$predicted_class)[1]))
    }
    metric_results = yardstick::metrics(metric_dataframe, original, predicted_class, predicted_prob)

    predictive_accuracy = metric_results$.estimate[1]
    kappa = metric_results$.estimate[2]
    log_loss = metric_results$.estimate[3]
    roc_auc = metric_results$.estimate[4]

    print_pred_acc = signif(predictive_accuracy, 3)
    print_kappa = signif(kappa, 3)
    print_log_loss = signif(log_loss, 3)
    print_auc = signif(roc_auc, 3)

    cat(paste("Predictive accuracy:",print_pred_acc,"\n"))
    summary_list[["predictive.accuracy"]] = predictive_accuracy
    cat(paste("Kappa:",print_kappa,"\n"))
    summary_list[["kappa"]] = kappa
    cat(paste("Log loss:",print_log_loss,"\n"))
    summary_list[["log.loss"]] = log_loss
    cat(paste("ROC AUC:",print_auc,"\n"))
    summary_list[["auc"]] = roc_auc
  }
  else if (object@type == "continuous") {
    metric_dataframe = data.frame(original=object@y, predicted=object@predicted_y)
    metric_results = yardstick::metrics(metric_dataframe, original, predicted)

    rmse = metric_results$.estimate[1]
    rsq = metric_results$.estimate[2]
    mae = metric_results$.estimate[3]

    print_rmse = ifelse(rmse > 1, round(rmse,3), signif(rmse,3))
    print_rsq = signif(rsq,3)
    print_mae = ifelse(mae > 1, round(mae,3), signif(mae,3))

    cat(paste("Root mean squared error:",print_rmse,"\n"))
    summary_list[["root.mean.squared.error"]] = rmse
    cat(paste("R-squared:",print_rsq,"\n"))
    summary_list[["r.squared"]] = rsq
    cat(paste("Mean absolute error:",print_mae,"\n"))
    summary_list[["mean.absolute.error"]] = mae
  }
  invisible(summary_list)
}
