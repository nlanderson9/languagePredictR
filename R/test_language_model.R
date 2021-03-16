#' testAssessment Class
#'
#' @slot call The function called to generate this model, with all arguments specified by the user
#' @slot data_text The text input to test the model
#' @slot data_outcome The outcome variable input to test the model
#' @slot type Model type, "binary" or "continuous"
#' @slot text The name of the column in the test dataframe containing the data_text
#' @slot outcome The name of the column in the test dataframe containing the data_outcome
#' @slot ngrams The ngrams used to generate the tokens
#' @slot dfmWeightScheme The weight scheme used to create the document-frequency matrix
#' @slot x The document-frequency matrix
#' @slot y The dependent (outcome) variable
#' @slot predicted_y The predicted outcomes based on the model and test data
#' @slot predicted_probabilities (If binary) The predicted probabilities of the outcomes based on the model and test data
#' @slot roc (If binary) The ROC calculated using the predicted_y
#' @slot roc_ci (If binary) The boostrapped confidence interval calculated for the ROC
#' @slot corr (If continuous) The correlation using the predicted_y
#' @slot level0 The bottom/first level of a binary variable, or the lowest value of a continuous variable
#' @slot level1 The top/second level of a binary variable, or the highest value of a continuous variable
#' @slot trainedModel The name of the model used for the test
#' @slot original_predictive_ngrams The list of ngram predictors from the model
#' @slot ngrams_present The number of original_predictive_ngrams that appear in the test lanuage sample
#'
#' @export testAssessment
#' @exportClass testAssessment
#'

testAssessment = setClass("testAssessment", slots = c("call", "data_text", "data_outcome", "type", "text", "outcome", "ngrams", "dfmWeightScheme", "x", "y", "predicted_y", "predicted_probabilities", "roc", "roc_ci", "corr", "level0", "level1", "trainedModel", "original_predictive_ngrams", "ngrams_present"))


#' @title Test Language Model
#'
#' @description This function tests a model created by the \code{\link{language_model}} function on a new dataset
#'
#' @param input A dataframe containing a column with text data (character strings) and an outcome variable (numeric or two-level factor)
#' @param outcome A string consisting of the column name for the outcome variable in \code{inputDataframe}
#' @param text A string consisting of the column name for the text data in \code{inputDataframe}
#' @param trainedModel A trained model created by the \code{\link{language_model}} function
#' @param ngrams A string defining the ngrams to serve as predictors in the model. Defaults to "1". For more information, see the \code{okens_ngrams} function in the \code{quanteda} package
#' @param dfmWeightScheme A string defining the weight scheme you wish to use for constructing a document-frequency matrix. Default is "count". For more information, see the \code{dfm_weight} function in the \code{quanteda} package
#' @param progressBar Show a progress bar. Defaults to TRUE.
#'
#' @return An object of the type "testAssessment"
#'
#' @seealso \code{\link{language_model}}
#'
#' @import quanteda
#' @import pROC
#' @importFrom stats cor.test
#' @importFrom methods setClass new as
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' movie_review_data1$cleanText = clean_text(movie_review_data1$text)
#' movie_review_data2$cleanText = clean_text(movie_review_data2$text)
#'
#' # Train a model on the \code{movie_review_data1} dataset
#' # Using language to predict "Positive" vs. "Negative" reviews
#' movie_model_valence = language_model(movie_review_data1,
#'                                      outcome = "valence",
#'                                      outcomeType = "binary",
#'                                      text = "cleanText")
#'
#' # Test the model on the \code{movie_review_data2} dataset
#' movie_model_valence_test = test_language_model(movie_review_data2,
#'                                     outcome = "valence",
#'                                     text = "cleanText",
#'                                     trainedModel = movie_model_valence)
#' summary(movie_model_valence_test)
#' }
#'
#' @details
#' This function is effectively a special version of the \code{\link{language_model}} function. Instead of creating a new model, the outputs are based on the results of testing a new, independent dataset using an existing model.
#' This allows for assessing how well a trained language model generalizes to other inputs - this function allows for comparisons between the models using many of the same functions that can be used with \code{\link{language_model}}.

test_language_model = function(input, outcome, text, trainedModel, ngrams="1", dfmWeightScheme="count", progressBar=TRUE) {

  call = match.call()

  td = input

  if (!is.data.frame(td)) {
    stop("The `input` argument must be a dataframe.")
  }

  if (trainedModel@type == "binary") {
    if (!is.factor(td[[outcome]])) {
      stop("Binary outcome variable must be type 'factor'")
    }
    else {
      if (nlevels(td[[outcome]]) != 2) {
        stop("Binary outcome variable must have exactly 2 levels")
      }
      level0 = levels(td[[outcome]])[1]
      level1 = levels(td[[outcome]])[2]
      td$cat = as.numeric(td[[outcome]]) - 1
    }
  }
  else if (trainedModel@type == "continuous") {
    if (!is.numeric(td[[outcome]])) {
      stop("Continuous outcome variable must be be type 'numeric'")
    }
    level0 = paste("low-", td[[outcome]], sep="")
    level1 = paste("high-", td[[outcome]], sep="")
    td$cat = td[[outcome]]
  }

  if(!class(trainedModel) == "langModel1") {
    stop("The `trainedModel` argument must be an output from `language_model`.")
  }

  if (suppressWarnings(is.na(as.numeric(ngrams[1])))) {
    splits = strsplit(ngrams, ":")[[1]]
    if (suppressWarnings(is.na(as.numeric(splits[1]))) | suppressWarnings(is.na(as.numeric(splits[2])))) {
      stop("The argument `ngrams` is formatted according to the guidance given in the 'tokens' function of the 'quanteda' package. Your ngrams value is not valid under those specifications.")
    }
  }

  if (ngrams != trainedModel@ngrams) {
    result = askYesNo(paste0("The argument `ngrams` for this dataset (`",ngrams,"`) does not match the ngrams used to create the original model (`",trainedModel@ngrams,"`). This is not advised - are you sure you want to continue?"))
    if (is.na(result)) {
      stop("Function aborted.")
    }
    if (!result) {
      stop("Function aborted.")
    }
  }

  if (!(dfmWeightScheme %in% c("count", "prop", "propmax", "logcount", "boolean", "augmented", "logave"))) {
    stop("Your `dfmWeightScheme` argument should include one of the valid 'scheme' options for the 'quanteda' function 'dfm_weight'.\nThese include:\n'count'\n'prop'\n'propmax'\n'logcount'\n'boolean'\n'augmented'\n'logave'")
  }

  if (dfmWeightScheme != trainedModel@dfmWeightScheme) {
    result = askYesNo(paste0("The argument `dfmWeightScheme` for this dataset (`",dfmWeightScheme,"`) does not match the dfmWeightScheme used to create the original model (`",trainedModel@dfmWeightScheme,"`). This is not advised - are you sure you want to continue?"))
    if (is.na(result)) {
      stop("Function aborted.")
    }
    if (!result) {
      stop("Function aborted.")
    }
  }


  m1dat<-subset(td, !is.na(cat))

  #***************CREATE THE DFM*************************
  corpus1<-corpus(m1dat[[text]])

  if (grepl(":", ngrams)) {
    splits = strsplit(ngrams, ":")[[1]]
    tokens1<- corpus1 %>% quanteda::tokens() %>% quanteda::tokens_ngrams(n=splits[1]:splits[2],concatenator = " ")
  }
  else {
    tokens1<- corpus1 %>% quanteda::tokens() %>% quanteda::tokens_ngrams(n=ngrams,concatenator = " ")
  }

  train_dict = as.dictionary(data.frame(word=dimnames(trainedModel@x)[[2]], sentiment=dimnames(trainedModel@x)[[2]]))

  dfm1<-dfm(tokens1, dictionary=train_dict) %>% dfm_weight(scheme=dfmWeightScheme)
  #possible schemes = c("count", "prop", "propmax", "logcount", "boolean")


  x<-as.matrix(dfm1)
  x = as(x, "dgCMatrix")

  #the dependent variable for fitting
  y<-m1dat$cat

  if(progressBar){
    roc_progress = "text"
  }
  else {
    roc_progress = "none"
  }

  predicted_y = as.numeric(predict(trainedModel@cv, newx=x, s=trainedModel@lambda))
  if (trainedModel@type == "binary") {
    predicted_probabilities = as.numeric(predict(trainedModel@cv, newx=x, s=trainedModel@lambda, type="response"))
    roc = suppressMessages(roc(y, predicted_y, ci=TRUE))
    if(progressBar){
      cat("Boostrapping ROC...")
    }
    roc_ci = ci.sp(roc, sensitivities=seq(0,1,.01), progress=roc_progress)
    corr = NA
  }
  else if (trainedModel@type == "continuous") {
    corr = cor.test(y, predicted_y)
    predicted_probabilities = NA
    roc = NA
    roc_ci = NA
    corr = NA
  }

  original_predictive_ngrams = c(trainedModel@cat0raw$words, trainedModel@cat1raw$words)

  ngrams_present = 0
  for (i in 1:length(original_predictive_ngrams)) {
    if(nrow(kwic(corpus1, original_predictive_ngrams[i] > 0))) {
      ngrams_present = ngrams_present + 1
    }
  }

  output = new("testAssessment", call=call, data_text=input[[text]], data_outcome=input[[outcome]], type=trainedModel@type, text=text, outcome=outcome, ngrams=ngrams, dfmWeightScheme=dfmWeightScheme, x=x, y=y, predicted_y=predicted_y, predicted_probabilities=predicted_probabilities, roc=roc, roc_ci=roc_ci, corr=corr, level0=level0, level1=level1, trainedModel=deparse(substitute(trainedModel)), original_predictive_ngrams=original_predictive_ngrams, ngrams_present=ngrams_present)
  return(output)
}






#' Summary (testAssessment)
#'
#' @param object The testAssessment object to summarize
#' @param ... Additional arguments
#'
#' @export
#'
#' @importFrom yardstick metrics
#'
#' @method summary testAssessment
#' @rdname testAssessment

summary.testAssessment = function(object, ...){

  original=predicted_prob=predicted_class=predicted=NULL

  corpus_object = quanteda::corpus(object@data_text)

  tokens_object = corpus_object %>% quanteda::tokens()

  if (grepl(":", object@ngrams)) {
    splits = strsplit(object@ngrams, ":")[[1]]
    ngram_tokens = tokens_object %>% quanteda::tokens_ngrams(n=splits[1]:splits[2],concatenator = " ")
  }
  else {
    ngram_tokens = tokens_object %>% quanteda::tokens_ngrams(n=object@ngrams,concatenator = " ")
  }

  total_tokens = ngram_tokens %>% ntoken() %>% sum()
  unique_tokens = ncol(object@x)


  call_string = deparse(object@call)
  call_string = paste(call_string, collapse = " ")
  call_string = gsub("\\s+", " ", call_string)
  call_string = paste("Call:", call_string)

  cat(paste0(call_string,"\n\n"))
  cat(paste("Number of language samples provided (n):", nrow(object@x),"\n"))
  cat(paste("Ngrams used:", object@ngrams,"\n"))
  cat(paste("Total number of ngrams in dataset:", total_tokens,"\n"))
  cat(paste("Number of unique ngrams in dataset:", ncol(object@x),"\n"))
  cat(paste("Number of predictive ngrams included in the original model:", length(object@original_predictive_ngrams),"\n"))
  cat(paste("Number of predictive ngrams appearing in dataset:",object@ngrams_present,"/",length(object@original_predictive_ngrams),"\n\n"))

  cat("Various model evaluation metrics:\n")
  cat("   (These were obtained by using the original cross-validated model to predict outcomes based on the current dataset)\n\n")
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
    cat(paste("Kappa:",print_kappa,"\n"))
    cat(paste("Log loss:",print_log_loss,"\n"))
    cat(paste("ROC AUC:",print_auc,"\n"))
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
    cat(paste("R-squared:",print_rsq,"\n"))
    cat(paste("Mean absolute error:",print_mae,"\n"))
  }
}
