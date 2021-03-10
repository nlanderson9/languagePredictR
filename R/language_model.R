#' @title langModel Class
#'
#' @slot data_text The text input to create the corpus/model
#' @slot data_outcome The outcome variabe input to create the model
#' @slot type Model type, "binary" or "continuous"
#' @slot text The name of the column in the original_dataframe containing the data_text
#' @slot outcome The name of the column in the original_dataframe containing the data_outcome
#' @slot tokens The list of tokens in the language corpus
#' @slot x The document-frequency matrix
#' @slot y The dependent (outcome) variable
#' @slot cv The final model
#' @slot lambda The lambda value used
#' @slot level0 The bottom/first level of a binary variable, or the lowest value of a continuous variable
#' @slot level1 The top/second level of a binary variable, or the highest value of a continuous variable
#' @slot cat0raw The predictors (word engrams) predicting the level0 outcome, with their model weights
#' @slot cat1raw The predictors (word engrams) predicting the level1 outcome, with their model weights
#' @slot p_value The p-value estimated via permutation test
#'
#' @export langModel
#' @exportClass langModel

langModel = setClass("langModel", slots = c("data_text", "data_outcome", "type", "text", "outcome", "tokens", "x", "y", "cv", "lambda", "level0", "level1", "cat0raw", "cat1raw", "p_value"))


#' @title Create Language Model
#'
#' @description This function creates a regression model using input text as predictors, and a specified variable as the outcome.
#'
#' @param inputDataframe A dataframe containing a column with text data (character strings) and an outcome variable (numeric or two-level factor)
#' @param outcomeVariableColumnName A string consisting of the column name for the outcome variable in \code{inputDataframe}
#' @param outcomeVariableType A string consisting of the type of outcome variable being used - options are "binary" or "continuous"
#' @param textColumnName A string consisting of the column name for the text data in \code{inputDataframe}
#' @param ngrams A string defining the ngrams to serve as predictors in the model. Defaults to "1". For more information, see the \code{okens_ngrams} function in the \code{quanteda} package
#' @param dfmWeightScheme A string defining the weight scheme you wish to use for constructing a document-frequency matrix. Default is "count". For more information, see the \code{dfm_weight} function in the \code{quanteda} package
#' @param lossMeasure A string defining the loss measure to use. Must be one of the options given by \code{cv.glmnet}. Default is "deviance".
#' @param lambda A string defining the lambda value to be used. Default is "lambda.min". For more information, see the \code{cv.glmnet} function in the \code{glmnet} package
#' @param parallelCores An integer defining the number of cores to use in parallel processing for model creation. Defaults to NULL (no parallel processing).
#' @param permutePValue If TRUE, a permutation test is run to estimate a p-value for the model (i.e. whether the language provided significantly predicts the outcome variable). Warning: this can take a while depending on the size of the dataset and number of permutations!
#' @param permutationK The number of permutations to run in a permutation test. Only used if \code{permutePValue = TRUE}. Defaults to 1000.
#' @param permuteByGroup A string consisting of the column name defining a grouping variable in the dataset (usually a participant number). This means that when permutations are randomized, they will permute items on a group level rather than trial level. Default is NULL (no group variable considered).
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
#' @importFromt doParallel registerDoParallel
#' @importFrom methods setClass new
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
#'                                      outcomeVariableColumnName = "valence",
#'                                      outcomeVariableType = "binary",
#'                                      textColumnName = "cleanText")
#'
#' # Using language to predict 1-10 scale ratings,
#' # but using both unigrams and bigrams, as well as a proportion weighting scheme
#' movie_model_rating = language_model(movie_review_data1,
#'                                     outcomeVariableColumnName = "rating",
#'                                     outcomeVariableType = "continuous",
#'                                     textColumnName = "cleanText",
#'                                     ngrams = "1:2",
#'                                     dfmWeightScheme = "prop")
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

language_model = function(inputDataframe, outcomeVariableColumnName, outcomeVariableType, textColumnName, ngrams="1", dfmWeightScheme="count", lossMeasure="deviance", lambda="lambda.min", parallelCores=NULL, permutePValue=FALSE, permutationK = 1000, permuteByGroup=NULL, progressBar=TRUE) {

  weights=words=NULL

  td = inputDataframe

  if (!is.data.frame(td)) {
    stop("The inputDataframe argument must be a dataframe.")
  }

  if (outcomeVariableType == "binary") {
    if (!is.factor(td[[outcomeVariableColumnName]])) {
      stop("Your binary outcome variable must be a factor.")
    }
    else {
      if (nlevels(td[[outcomeVariableColumnName]]) != 2) {
        stop("Your binary outcome variable must have exactly 2 levels.")
      }
      level0 = levels(td[[outcomeVariableColumnName]])[1]
      level1 = levels(td[[outcomeVariableColumnName]])[2]
      td$cat = as.numeric(td[[outcomeVariableColumnName]]) - 1
    }
  }
  else if (outcomeVariableType == "continuous") {
    if (!is.numeric(td[[outcomeVariableColumnName]])) {
      stop("Your continuous outcome variable must be numeric.")
    }
    level0 = paste("low-", td[[outcomeVariableColumnName]], sep="")
    level1 = paste("high-", td[[outcomeVariableColumnName]], sep="")
    td$cat = td[[outcomeVariableColumnName]]
  }
  else {
    stop("The outcomeVariableType argument must be either 'binary' or 'continuous'.")
  }

  if (!is.character(td[[textColumnName]])) {
    stop("The textColumnName argument must specify a column of character strings in your dataframe.")
  }

  if (suppressWarnings(is.na(as.numeric(ngrams[1])))) {
    splits = strsplit(ngrams, ":")[[1]]
    if (suppressWarnings(is.na(as.numeric(splits[1]))) | suppressWarnings(is.na(as.numeric(splits[2])))) {
      stop("The argument ngrams is formatted according to the guidance given in the 'tokens' function of the 'quanteda' package. Your ngrams value is not valid under those specifications.")
    }
  }

  if (!(dfmWeightScheme %in% c("count", "prop", "propmax", "logcount", "boolean", "augmented", "logave"))) {
    stop("Your dfmWeightScheme argument should include one of the valid 'scheme' options for the 'quanteda' function 'dfm_weight'.\nThese include:\n'count'\n'prop'\n'propmax'\n'logcount'\n'boolean'\n'augmented'\n'logave'")
  }

  if (!(lambda %in% c("lambda.min", "lambda.1se"))) {
    stop("Your lambda argument should be either 'lambda.min' (for the value of lambda that results in the minimum cross-validation error) or 'lambda.1se' (for the value of lambda 1 standard error above lambda.min).")
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
    test_similarity = rename(count(inputDataframe, !!sym(outcomeVariableColumnName), !!sym(permuteByGroup)), Freq = n)
    if (nrow(test_similarity) != length(unique(inputDataframe[[permuteByGroup]]))) {
      stop("Your groups defined by the `permuteByGroup` argument have heterogeneous outcomes (i.e. a given group member does not have a single outcome type).")
    }
  }


  m1dat<-subset(td, !is.na(cat))

  #***************CREATE THE DFM*************************
  corpus1<-corpus(m1dat[[textColumnName]])

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
  familytype = ifelse(outcomeVariableType == "binary", "binomial", "gaussian")

  if(progressBar){
    show_progress = 1
  }
  else {
    show_progress = 0
  }


  if(is.null(parallelCores)) {
    cv1<-cv.glmnet(x,y,family=familytype,type.measure=lossMeasure,nfolds=10,standardize=F,
                   intercept=T,alpha=1, trace.it=show_progress)
  }
  else {
    cv1<-cv.glmnet(x,y,family=familytype,type.measure=lossMeasure,nfolds=10,standardize=F,
                   intercept=T,alpha=1, parallel=TRUE, trace.it=show_progress)
  }

  if (permutePValue) {
    prediction = predict(cv1, s=lambda, newx=x)
    original_auc = suppressMessages(roc(y, as.numeric(prediction)))
    original_auc = original_auc$auc

    if (!is.null(permuteByGroup)) {
      permutation_data = m1dat[,c(textColumnName, outcomeVariableColumnName, permuteByGroup)]
      correspondences = m1dat[!duplicated(m1dat[[permuteByGroup]]),c(outcomeVariableColumnName, permuteByGroup)]
      colnames(correspondences) = c("permuted_outcome", permuteByGroup)
    }
    aucs = c()
    if(progressBar){
      pb = progress_bar$new(total = permutationK, format = "[:bar]Permutations - :percent (eta :eta) (elapsed :elapsed) (:current / :total)")
    }
    for (i in 1:permutationK) {
      if(progressBar){
        pb$tick()
      }
      if(!is.null(permuteByGroup)) {
        permuted_correspondences = transform(correspondences, permuted_outcome = sample(permuted_outcome))
        permuted_data = permutation_data %>% left_join(permuted_correspondences, by = c(permuteByGroup = permuteByGroup))
        if(outcomeVariableType=="binary"){
          permuted_data$permuted_outcome = as.factor(permuted_data$permuted_outcome)
        }
        else if (outcomeVariableType=="continuous") {
          permuted_data$permuted_outcome = as.numeric(permuted_data$permuted_outcome)
        }
        permuted_y = permuted_data$permuted_outcome
      }
      else {
        permuted_y = sample(y)
      }

      if(is.null(parallelCores)) {
        cv_permute<-cv.glmnet(x,permuted_y,family=familytype,type.measure=lossMeasure,nfolds=10,standardize=F,
                       intercept=T,alpha=1, trace.it=show_progress)
      }
      else {
        cv_permute<-cv.glmnet(x,permuted_y,family=familytype,type.measure=lossMeasure,nfolds=10,standardize=F,
                       intercept=T,alpha=1, parallel=TRUE, trace.it=show_progress)
      }
      prediction_permute = predict(cv_permute, s=lambda, newx=x)
      permute_auc = suppressMessages(roc(permuted_y, as.numeric(prediction_permute)))
      aucs = c(aucs, permute_auc$auc)
    }
    p_value = mean(aucs >= original_auc)
  }
  else {
    p_value = NA
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


  output = new("langModel", data_text=inputDataframe[[textColumnName]], data_outcome=inputDataframe[[outcomeVariableColumnName]], type=outcomeVariableType, text=textColumnName, outcome=outcomeVariableColumnName, tokens=tokens1, x=x, y=y, cv=cv1, lambda=lambda, level0=level0, level1=level1, cat1raw=cat1raw, cat0raw=cat0raw, p_value=p_value)

  return(output)
}


