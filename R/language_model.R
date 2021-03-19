#' @title langModel Class
#'
#' @slot call The function called to generate this model, with all arguments specified by the user
#' @slot data_text The text input to create the corpus/model
#' @slot data_outcome The outcome variable input to create the model
#' @slot type Model type, "binary" or "continuous"
#' @slot text The name of the column in the input dataframe containing the data_text
#' @slot outcome The name of the column in the input dataframe containing the data_outcome
#' @slot tokens The list of tokens in the language corpus
#' @slot ngrams The ngrams used to generate the tokens
#' @slot dfmWeightScheme The weight scheme used to create the document-frequency matrix
#' @slot x The document-frequency matrix
#' @slot y The dependent (outcome) variable
#' @slot cv The final model
#' @slot lambda The lambda value used
#' @slot predicted_y The predicted outcomes based on the model and original language data
#' @slot predicted_probabilities (If binary) The predicted probabilities of the outcomes based on the model and original language data
#' @slot roc (If binary) The ROC calculated using the predicted_y
#' @slot roc_ci (If binary) The boostrapped confidence interval calculated for the ROC
#' @slot corr (If continuous) The correlation using the predicted_y
#' @slot level0 The bottom/first level of a binary variable, or the lowest value of a continuous variable
#' @slot level1 The top/second level of a binary variable, or the highest value of a continuous variable
#' @slot cat0raw The predictors (word ngrams) predicting the level0 outcome, with their model weights
#' @slot cat1raw The predictors (word ngrams) predicting the level1 outcome, with their model weights
#' @slot p_value The p-value estimated via permutation test
#' @slot lossMeasure The loss measure chosen for the LASSO regression cross-validation
#' @slot cvm_type The loss measure - virtually always the same as lossMeasure except when the default "deviance" is specified
#' @slot cvm The value of the loss measure for the cross-validated model at the chosen lambda level
#' @slot permuted_cvms The value of the loss measure for each cross-validated model at the chosen lambda level across all randomized permutations
#' @slot permutationK The number of permutations conducted for permutation testing
#' @slot minimum_p The minimum p_value that can be achieved given the number permutationK specified
#' @slot st_err_p The standard error of the p_value based on the number permutationK specified
#'
#' @export langModel
#' @exportClass langModel

langModel = setClass("langModel", slots = c("call", "data_text", "data_outcome", "type", "text", "outcome", "tokens", "ngrams", "dfmWeightScheme", "x", "y", "cv", "lambda", "predicted_y", "predicted_probabilities", "roc", "roc_ci", "corr", "level0", "level1", "cat0raw", "cat1raw", "p_value", "lossMeasure", "cvm_type", "cvm", "permuted_cvms", "permutationK", "minimum_p", "st_err_p"))


#' @title Create Language Model
#'
#' @description This function creates a regression model using input text as predictors, and a specified variable as the outcome.
#'
#' @param input A dataframe containing a column with text data (character strings) and an outcome variable (numeric or two-level factor)
#' @param outcome A string consisting of the column name for the outcome variable in \code{inputDataframe}
#' @param outcomeType A string consisting of the type of outcome variable being used - options are "binary" or "continuous"
#' @param text A string consisting of the column name for the text data in \code{inputDataframe}
#' @param ngrams A string defining the ngrams to serve as predictors in the model. Defaults to "1". For more information, see the \code{okens_ngrams} function in the \code{quanteda} package
#' @param dfmWeightScheme A string defining the weight scheme you wish to use for constructing a document-frequency matrix. Default is "count". For more information, see the \code{dfm_weight} function in the \code{quanteda} package
#' @param lossMeasure A string defining the loss measure to use. Must be one of the options given by \code{cv.glmnet}. Default is "deviance".
#' @param lambda A string defining the lambda value to be used. Default is "lambda.min". For more information, see the \code{cv.glmnet} function in the \code{glmnet} package
#' @param parallelCores An integer defining the number of cores to use in parallel processing for model creation. Defaults to NULL (no parallel processing).
#' @param permutePValue If TRUE, a permutation test is run to estimate a p-value for the model (i.e. whether the language provided significantly predicts the outcome variable). Warning: this can take a while depending on the size of the dataset and number of permutations!
#' @param permutationK The number of permutations to run in a permutation test. Only used if \code{permutePValue = TRUE}. Defaults to 1000.
#' @param permuteByGroup A string consisting of the column name defining a grouping variable in the dataset (often a participant number). This means that when permutations are randomized, they will permute items on a group level rather than trial level. Default is NULL (no group variable considered).
#' @param progressBar Show a progress bar. Defaults to TRUE.
#'
#' @return An object of the type "langModel"
#'
#' @references
#' Dobbins, I. G., & Kantner, J. (2019). The language of accurate recognition memory. *Cognition, 192*, 103988.\cr
#' Tibshirani, R. (1996). Regression Shrinkage and Selection Via the Lasso. *Journal of the Royal Statistical Society: Series B (Methodological), 58*(1), 267-288.
#'
#' @import quanteda
#' @import glmnet
#' @import Matrix
#' @import progress
#' @importFrom stats cor.test
#' @importFrom doParallel registerDoParallel
#' @importFrom methods setClass new as
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' movie_review_data1$cleanText = clean_text(movie_review_data1$text)
#'
#' # Using language to predict "Positive" vs. "Negative" reviews
#' movie_model_valence = language_model(movie_review_data1,
#'                                      outcome = "valence",
#'                                      outcomeType = "binary",
#'                                      text = "cleanText")
#'
#' summary(movie_model_valence)
#'
#' # Using language to predict 1-10 scale ratings,
#' # but using both unigrams and bigrams, as well as a proportion weighting scheme
#' movie_model_rating = language_model(movie_review_data1,
#'                                     outcomeV = "rating",
#'                                     outcomeType = "continuous",
#'                                     textC = "cleanText",
#'                                     ngrams = "1:2",
#'                                     dfmWeightScheme = "prop")
#'
#' summary(movie_model_rating)
#' }
#'
#' @details
#' This is the core function of the \code{languagePredictR} package. It largely follows the analysis laid out in Dobbins & Kantner 2019 (see References).\cr\cr
#' In the broadest terms, this serves as a wrapper for the quanteda (text analysis) and glmnet (modeling) packages.\cr
#' The input text is converted into a document-frequency matrix (sometimes called a document-feature matrix) where each row represents a string of text, and each column represents a word that appears in the entire text corpus.\cr
#' Each cell is populated by a value defined by the dfmWeightScheme. For example, the default, "count", means that each word column contains a value representing the number of times that word appears in the given text string.\cr
#' This matrix is then used to train a regression algorithm appropriate to the outcome variable (standard linear regression for continuous variables, logistic regression for binary variables).\cr
#' See the documentation for the \code{\link[glmnet]{cv.glmnet}} function in the \code{glmnet} package for more information.\cr
#' 10-fold cross validation is currently implemented to reduce overfitting to the data.\cr
#' Additionally, a LASSO constraint is used (following Tibshirani, 1996; see References) to eliminate weakly-predictive variables. This reduces the number of predictors (i.e. word engrams) to sparse, interpretable set.

language_model = function(input, outcome, outcomeType, text, ngrams="1", dfmWeightScheme="count", lossMeasure="deviance", lambda="lambda.min", parallelCores=NULL, permutePValue=FALSE, permutationK = 1000, permuteByGroup=NULL, progressBar=TRUE) {

  weights=words=permuted_outcome=NULL

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

  if (!is.character(td[[text]])) {
    stop("The `text` argument must specify a column of character strings in your dataframe.")
  }

  if (suppressWarnings(is.na(as.numeric(ngrams[1])))) {
    splits = strsplit(ngrams, ":")[[1]]
    if (suppressWarnings(is.na(as.numeric(splits[1]))) | suppressWarnings(is.na(as.numeric(splits[2])))) {
      stop("The argument `ngrams` is formatted according to the guidance given in the 'tokens' function of the 'quanteda' package. Your `ngrams` value is not valid under those specifications.")
    }
  }

  if (!(dfmWeightScheme %in% c("count", "prop", "propmax", "logcount", "boolean", "augmented", "logave"))) {
    stop("Your `dfmWeightScheme` argument should include one of the valid 'scheme' options for the 'quanteda' function 'dfm_weight'.\nThese include:\n'count'\n'prop'\n'propmax'\n'logcount'\n'boolean'\n'augmented'\n'logave'")
  }

  if (!(lambda %in% c("lambda.min", "lambda.1se"))) {
    stop("Your `lambda` argument should be either 'lambda.min' (for the value of lambda that results in the minimum cross-validation error) or 'lambda.1se' (for the value of lambda 1 standard error above lambda.min).")
  }

  if(!is.null(parallelCores)) {
    if(!is.numeric(parallelCores)) {
      stop("The `parallelCores` argument must be an integer.")
    }
    else {
      registerDoParallel(parallelCores)
    }
  }

  if (!is.null(permuteByGroup)) {
    test_similarity = rename(count(input, !!sym(outcome), !!sym(permuteByGroup)), Freq = n)
    if (nrow(test_similarity) != length(unique(input[[permuteByGroup]]))) {
      stop("Your groups defined by the `permuteByGroup` argument have heterogeneous outcomes (i.e. a given group member does not have a single outcome type).")
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




  dfm1<-dfm(tokens1) %>% dfm_weight(scheme=dfmWeightScheme)
  #possible schemes = c("count", "prop", "propmax", "logcount", "boolean")

  #construct x matrix for predictors
  #order columns of dfm alphabetically by convention
  dfm1<-dfm1[,order(colnames(dfm1))]
  x<-convert(dfm1, to = "data.frame")
  x = x[,2:ncol(x)]
  x<-as.matrix(x)
  x = as(x, "dgCMatrix")

  #the dependent variable for fitting
  y<-m1dat$cat

  #train and cross validate model
  familytype = ifelse(outcomeType == "binary", "binomial", "gaussian")

  if(progressBar){
    show_progress = 1
    roc_progress = "text"
  }
  else {
    show_progress = 0
    roc_progress = "none"
  }


  if(is.null(parallelCores)) {
    cv1<-cv.glmnet(x,y,family=familytype,type.measure=lossMeasure,nfolds=10,standardize=FALSE,
                   intercept=T,alpha=1, trace.it=show_progress)
  }
  else {
    cv1<-cv.glmnet(x,y,family=familytype,type.measure=lossMeasure,nfolds=10,standardize=FALSE,
                   intercept=T,alpha=1, parallel=TRUE, trace.it=show_progress)
  }

  cvm = cv1$cvm[which(cv1$lambda==cv1[[lambda]])]
  if (lossMeasure == "deviance") {
    if (outcomeType == "continuous") {
      cvm_type = "mse"
    }
    else if (outcomeType == "binary") {
      cvm_type = "binomial_deviance"
    }
  }
  else {
    cvm_type = lossMeasure
  }

  predicted_y = as.numeric(predict(cv1, newx=x, s=lambda))
  if (outcomeType == "binary") {
    predicted_probabilities = as.numeric(predict(cv1, newx=x, s=lambda, type="response"))
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


  if (permutePValue) {
    if (!is.null(permuteByGroup)) {
      permutation_data = m1dat[,c(text, outcome, permuteByGroup)]
      correspondences = m1dat[!duplicated(m1dat[[permuteByGroup]]),c(outcome, permuteByGroup)]
      colnames(correspondences) = c("permuted_outcome", permuteByGroup)
    }
    cvms = c()
    if(progressBar){
      pb = progress_bar$new(total = permutationK, format = "[:bar]Permutations - :percent (eta :eta) (elapsed :elapsed) (:current / :total)")
    }
    for (i in 1:permutationK) {
      if(progressBar){
        pb$tick()
      }
      if(!is.null(permuteByGroup)) {
        permuted_correspondences = transform(correspondences, permuted_outcome = sample(permuted_outcome))
        this_by = enquo(permuteByGroup)
        permuted_data = permutation_data %>% left_join(permuted_correspondences, by = quo_name(this_by))
        if(outcomeType=="binary"){
          permuted_data$permuted_outcome = as.factor(permuted_data$permuted_outcome)
        }
        else if (outcomeType == "continuous") {
          permuted_data$permuted_outcome = as.numeric(permuted_data$permuted_outcome)
        }
        permuted_y = permuted_data$permuted_outcome
      }
      else {
        permuted_y = sample(y)
      }

      if(is.null(parallelCores)) {
        cv_permute<-cv.glmnet(x,permuted_y,family=familytype,type.measure=lossMeasure,nfolds=10,standardize=FALSE,
                              intercept=T,alpha=1, trace.it=0)
      }
      else {
        cv_permute<-cv.glmnet(x,permuted_y,family=familytype,type.measure=lossMeasure,nfolds=10,standardize=FALSE,
                              intercept=T,alpha=1, parallel=TRUE, trace.it=0)
      }
      permuted_cvm = cv_permute$cvm[which(cv_permute$lambda==cv_permute[[lambda]])]
      cvms = c(cvms, permuted_cvm)

    }

    calc_pvalue = c(cvms, cvm)
    if (lossMeasure != "auc") {
      p_value = mean(calc_pvalue <= cvm)
    }
    else {
      p_value = mean(calc_pvalue >= cvm)
    }
  }
  else {
    p_value = NA
    cvms = NA
  }

  #recover weights and words
  model1_weights<-as.data.frame(as.matrix(coef(cv1,s=lambda))) # min or 1 SE rule
  model1_words<-row.names(model1_weights)

  #the final model1
  mod1 = data.frame(words=model1_words[model1_weights!=0],
                    weights=model1_weights[model1_weights!=0])

  #data for plotting coefficients
  cat1raw<-subset(mod1,weights>0 & words !='(Intercept)')
  cat1raw$words <- factor(cat1raw$words, levels = cat1raw$words[order(cat1raw$weights,decreasing = F)])
  cat0raw<-subset(mod1,weights<0 & words !='(Intercept)')
  cat0raw$words <- factor(cat0raw$words, levels = cat0raw$words[order(cat0raw$weights,decreasing = T)])


  output = new("langModel", call=call, data_text=input[[text]], data_outcome=input[[outcome]], type=outcomeType, text=text, outcome=outcome, tokens=tokens1, ngrams=ngrams, dfmWeightScheme=dfmWeightScheme, x=x, y=y, cv=cv1, lambda=lambda, predicted_y=predicted_y, predicted_probabilities=predicted_probabilities, roc=roc, roc_ci=roc_ci, corr=corr, level0=level0, level1=level1, cat1raw=cat1raw, cat0raw=cat0raw, p_value=p_value, lossMeasure=lossMeasure, cvm_type=cvm_type, cvm=cvm, permuted_cvms=cvms, permutationK = permutationK, minimum_p = 1/(permutationK+1), st_err_p = sqrt((p_value*(1-p_value))/permutationK))

  return(output)
}


#' Summary (langModel)
#'
#' @param object The langModel object to summarize
#' @param ... Additional arguments
#'
#' @export
#'
#' @importFrom yardstick metrics
#'
#' @method summary langModel

summary.langModel = function(object, ...){

  original=predicted_prob=predicted_class=predicted=NULL

  summary_list = list()
  summary_list[["model.name"]] = deparse(substitute(object))

  lossName = switch(object@cvm_type,
                    binomial_deviance="Binomial Deviance",
                    mse="Mean Squared Error",
                    auc="AUC",
                    mae="Mean Absolute Error",
                    class="Misclassification Error"
  )

  summary_list[["loss.metric"]] = lossName

  if(object@cvm > 1) {
    print_cvm = round(object@cvm,3)
  }
  else {
    print_cvm = signif(object@cvm,3)
  }

  summary_list[["loss.metric.cv.lambda"]] = object@lambda
  summary_list[["loss.metric.cv.value"]] = object@cvm

  tokens_object = quanteda::corpus(object@data_text) %>% quanteda::tokens()

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
  call_string = paste("Call::", call_string)

  cat(paste0(call_string,"\n\n"))
  summary_list[["call"]] = call_string
  cat(paste("Number of language samples provided (n):", nrow(object@x),"\n"))
  summary_list[["language.samples"]] = nrow(object@x)
  cat(paste("Outcome variable:", object@outcome,"\n"))
  summary_list[["outcome"]] = object@outcome
  summary_list[["outcome.level.low"]] = object@level0
  summary_list[["outcome.level.high"]] = object@level1
  cat(paste("Ngrams used:", object@ngrams,"\n"))
  summary_list[["ngram"]] = object@ngrams
  cat(paste("Total number of ngrams in dataset:", total_tokens,"\n"))
  summary_list[["total.ngrams"]] = total_tokens
  cat(paste("Number of unique ngrams in dataset to serve as predictors (p):", ncol(object@x),"\n"))
  summary_list[["unique.ngrams"]] = ncol(object@x)

  cat(paste("Number of predictive ngrams in final model:", nrow(object@cat0raw)+nrow(object@cat1raw),"\n"))
  summary_list[["total.predictive.ngrams"]] = nrow(object@cat0raw)+nrow(object@cat1raw)
  cat(paste0("    Number of ngrams predicting '",object@level0,"': ",nrow(object@cat0raw),"\n"))
  summary_list[["predictive.ngrams.level.low"]] = nrow(object@cat0raw)
  cat(paste0("    Number of ngrams predicting '",object@level1,"': ",nrow(object@cat1raw),"\n\n"))
  summary_list[["predictive.ngrams.level.high"]] = nrow(object@cat1raw)
  cat(paste0("Cross-validated ",lossName," at '",object@lambda,"' = ",print_cvm,"\n\n"))
  if (!is.na(object@p_value)) {
    cat(paste0("Estimated p-value = ",signif(object@p_value,3)," (st.err. = ", signif(object@st_err_p,3),")\n"))
    summary_list[["estimated.p.value"]] = object@p_value
    summary_list[["standard.error.p.value"]] = object@st_err_p
    cat(paste0("   (p-value and error are based on ",object@permutationK," permutations; minimum possible p = ",signif(object@minimum_p,3),")\n\n"))
    summary_list[["permutations.k"]] = object@permutationK
    summary_list[["minimum.possible.p.value"]] = object@minimum_p
  }
  else {
    summary_list[["estimated.p.value"]] = NA
    summary_list[["standard.error.p.value"]] = NA
    summary_list[["permutations.k"]] = NA
    summary_list[["minimum.possible.p.value"]] = NA
  }
  cat("Various model evaluation metrics:\n")
  cat("   (Caution: these were obtained by using the cross-validated model to predict outcomes based on the original dataset)\n\n")
  if (object@type == "binary" & nrow(object@cat0raw)+nrow(object@cat1raw) > 0) {
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
  else if (object@type == "continuous" & nrow(object@cat0raw)+nrow(object@cat1raw) > 0) {
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
  else if (nrow(object@cat0raw)+nrow(object@cat1raw) == 0) {
    cat("**No model evaluation metrics are provided, because no predictive engrams survived the LASSO regularization.**")
  }

  invisible(summary_list)
}
