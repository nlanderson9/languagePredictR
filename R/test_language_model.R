#' testAssessment Class
#'
#' @slot type Model type, "binary" or "continuous"
#' @slot model_labels Label names and basic info for each model in the assessment
#' @slot auc_polygon_df Dataframe for plotting the AUC polygon
#' @slot roc_ci_df Dataframe for plotting the ROC confidence intervals
#' @slot roc_curve_df Dataframe for plotting the ROC curves
#' @slot auc_ci_labels_df Dataframe for AUC labels
#' @slot auc_tests Dataframe of significance tests for AUCs
#' @slot cat_data Dataframe of predictors (word engrams) with their model weights
#'
#' @export testAssessment
#' @exportClass testAssessment
#'
testAssessment = setClass("testAssessment", slots = c("type", "model_labels", "auc_polygon_df", "roc_ci_df", "roc_curve_df", "auc_ci_labels_df", "auc_tests", "cat_data"))


#' @title Test Language Model
#'
#' @description This function tests a model created by the \code{\link{language_model}} function on a new dataset
#'
#' @param inputDataframe A dataframe containing a column with text data (character strings) and an outcome variable (numeric or two-level factor)
#' @param outcomeVariableColumnName A string consisting of the column name for the outcome variable in \code{inputDataframe}
#' @param textColumnName A string consisting of the column name for the text data in \code{inputDataframe}
#' @param trainedModel A trained model created by the \code{\link{language_model}} function
#' @param ngrams A string defining the ngrams to serve as predictors in the model. Defaults to "1". For more information, see the \code{okens_ngrams} function in the \code{quanteda} package
#' @param dfmWeightScheme A string defining the weight scheme you wish to use for constructing a document-frequency matrix. Default is "count". For more information, see the \code{dfm_weight} function in the \code{quanteda} package
#' @param lambda A string defining the lambda value to be used. Default is "lambda.min". For more information, see the \code{cv.glmnet} function in the \code{glmnet} package
#'
#' @return An object of the type "testAssessment"
#'
#' @seealso \code{\link{language_model}} and \code{\link{assess_models}}
#'
#' @import quanteda
#' @import glmnet
#' @import pROC
#' @importFrom methods setClass new
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
#'                                      outcomeVariableColumnName = "valence",
#'                                      outcomeVariableType = "binary",
#'                                      textColumnName = "cleanText")
#'
#' # Test the model on the \code{movie_review_data2} dataset
#' movie_model_rating = test_language_model(movie_review_data2,
#'                                     outcomeVariableColumnName = "valence",
#'                                     textColumnName = "cleanText",
#'                                     trainedModel = movie_model_valence)
#' }
#'
#' @details
#' This function is effectively a special instance of the \code{\link{assess_models}} function. Instead of being provided with two independently-specified models, the two assessed models are the training model and the testing model.
#' This allows for assessing how well a trained language model generalizes to other inputs - this function allows for comparisons between the models using many of the same functions that can be used with \code{\link{assess_models}}.

test_language_model = function(inputDataframe, outcomeVariableColumnName, textColumnName, trainedModel, ngrams="1", dfmWeightScheme="count",lambda="lambda.min") {
  td = inputDataframe

  if (trainedModel@type == "binary") {
    if (!is.factor(td[[outcomeVariableColumnName]])) {
      stop("Binary outcome variable must be type 'factor'")
    }
    else {
      if (nlevels(td[[outcomeVariableColumnName]]) != 2) {
        stop("Binary outcome variable must have exactly 2 levels")
      }
      level0 = levels(td[[outcomeVariableColumnName]])[1]
      level1 = levels(td[[outcomeVariableColumnName]])[2]
      td$cat = as.numeric(td[[outcomeVariableColumnName]]) - 1
    }
  }
  else if (trainedModel@type == "continuous") {
    if (!is.numeric(td[[outcomeVariableColumnName]])) {
      stop("Continuous outcome variable must be be type 'numeric'")
    }
    level0 = paste("low-", td[[outcomeVariableColumnName]], sep="")
    level1 = paste("high-", td[[outcomeVariableColumnName]], sep="")
    td$cat = td[[outcomeVariableColumnName]]
  }

  if (suppressWarnings(is.na(as.numeric(ngrams[1])))) {
    splits = strsplit(ngrams, ":")[[1]]
    if (suppressWarnings(is.na(as.numeric(splits[1]))) | suppressWarnings(is.na(as.numeric(splits[2])))) {
      stop("The argument `ngrams` is formatted according to the guidance given in the 'tokens' function of the 'quanteda' package. Your ngrams value is not valid under those specifications.")
    }
  }

  if (!(dfmWeightScheme %in% c("count", "prop", "propmax", "logcount", "boolean", "augmented", "logave"))) {
    stop("Your `dfmWeightScheme` argument should include one of the valid 'scheme' options for the 'quanteda' function 'dfm_weight'.\nThese include:\n'count'\n'prop'\n'propmax'\n'logcount'\n'boolean'\n'augmented'\n'logave'")
  }

  if (!(lambda %in% c("lambda.min", "lambda.1se"))) {
    stop("Your `lambda` argument should be either 'lambda.min' (for the value of lambda that results in the minimum cross-validation error) or 'lambda.1se' (for the value of lambda 1 standard error above lambda.min).")
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

  train_dict = as.dictionary(data.frame(word=dimnames(trainedModel@x)[[2]], sentiment=dimnames(trainedModel@x)[[2]]))

  dfm1<-dfm(tokens1, dictionary=train_dict) %>% dfm_weight(scheme=dfmWeightScheme)
  #possible schemes = c("count", "prop", "propmax", "logcount", "boolean")


  x<-as.matrix(dfm1)

  #the dependent variable for fitting
  y<-m1dat$cat








  if (trainedModel@type == "binary") {

    results_matrix = data.frame(matrix(nrow=0, ncol=8))
    colnames(results_matrix) = c("model", "auc", "auc_ci", "predictive_acc", "high_predictor_1", "high_predictor_2", "low_predictor_1", "low_predictor_2")

    model_labels = data.frame(matrix(ncol=7,nrow=0))
    colnames(model_labels) = c("name","LASSO_model", "auc", "ci_lower", "ci_upper", "cat0title", "cat1title")

    roc_plot_data = data.frame(matrix(ncol=2,nrow=0))
    colnames(roc_plot_data) = c("specificities", "sensitivities")

    roc_ci_plot_data = data.frame(matrix(ncol=4, nrow=0))
    colnames(roc_ci_plot_data) = c("percent2p5", "percent50", "percent97p5", "sensitivities")

    roc_list = list()

    cat_data = data.frame(matrix(ncol=4,nrow=0))
    colnames(cat_data) = c("words", "weights", "class", "model")

    for (i in 1:2) {
      if (i == 1) {
        predictor<- as.numeric(predict(trainedModel@cv,newx=trainedModel@x,s=trainedModel@lambda))
        response = trainedModel@y
        model_name = as.character(substitute(trainedModel))
      }
      else if (i == 2) {
        predictor<- as.numeric(predict(trainedModel@cv,newx=x,s=trainedModel@lambda))
        response = y
        model_name = as.character(substitute(inputDataframe))
      }
      roc_data <- suppressMessages(roc(response,predictor, ci=TRUE))
      roc_list[[model_name]] = roc_data
      roc_ci_data = ci.sp(roc_data, sensitivities=seq(0,1,.01))

      if (trainedModel@type == "binary") {
        cat1title = paste('Words predicting\n"',trainedModel@level1, '" responses', sep="")
        cat0title = paste('Words predicting\n"',trainedModel@level0, '" responses', sep="")
      }
      else {
        cat1title = paste('Words predicting\n"high-',trainedModel@outcome, '" responses', sep="")
        cat0title = paste('Words predicting\n"low-',trainedModel@outcome, '" responses', sep="")
      }

      cat0data = trainedModel@cat0raw
      cat1data = trainedModel@cat1raw
      cat0data$class = "cat0"
      cat1data$class = "cat1"
      cat_data_temp = rbind(cat0data, cat1data)
      cat_data_temp$model = model_name
      cat_data = rbind(cat_data, cat_data_temp)



      roc_data_formatted = data.frame(specificities=roc_data$specificities, sensitivities=roc_data$sensitivities)
      roc_data_formatted$model = model_name
      roc_ci_data_formatted = as.data.frame(roc_ci_data)
      roc_ci_data_formatted$sensitivities = as.numeric(rownames(roc_ci_data_formatted))
      colnames(roc_ci_data_formatted) = c("percent2p5", "percent50", "percent97p5", "sensitivities")
      roc_ci_data_formatted$model = model_name
      roc_plot_data = rbind(roc_plot_data, roc_data_formatted)
      roc_ci_plot_data = rbind(roc_ci_plot_data, roc_ci_data_formatted)



      temp_frame = data.frame(name=model_name, LASSO_model=TRUE, auc = roc_data$auc, ci_lower = roc_data$ci[1], ci_upper = roc_data$ci[3], cat0title = cat0title, cat1title=cat1title)
      model_labels = rbind(model_labels, temp_frame)
    }

    roc_plot_data$model = factor(roc_plot_data$model, levels = model_labels$name)
    roc_ci_plot_data$model = factor(roc_ci_plot_data$model, levels = model_labels$name)
    lowest_auc_model = model_labels$name[which.min(model_labels$auc)]

    box_df = data.frame(polygon.x = c(0,0,1,1,0), polygon.y = c(0,1,1,0,0))

    auc_df = data.frame(matrix(ncol=3,nrow=0))
    colnames(auc_df) = c("sensitivities", "specificities", "model")
    for (j in 1:nrow(model_labels)) {
      auc_df_add = subset(roc_plot_data, model == model_labels$name[j])
      auc_extra = data.frame(sensitivities = c(1,0,0), specificities = c(0,0,1), model=rep(model_labels$name[j],3))
      auc_df = rbind(auc_df, auc_df_add, auc_extra)
    }

    auc_labels = data.frame(matrix(ncol=4,nrow=0))
    colnames(auc_labels) = c("model", "label_text", "x", "y")
    for (j in 1:nrow(model_labels)) {
      model = model_labels$name[j]
      label_text = paste("AUC: ",round(model_labels$auc[j],3), "\nCI: (", round(model_labels$ci_lower[j],3), "-", round(model_labels$ci_upper[j],3),")",sep="")
      temp_frame = data.frame(model=model, label_text=label_text, x=.4, y=.6-(.1*j))
      auc_labels = rbind(auc_labels, temp_frame)
    }



    roc_plot_data_all = CreateAllFacet(roc_plot_data, "model")
    roc_ci_plot_data_all = CreateAllFacet(roc_ci_plot_data, "model")

    auc_df$facet = auc_df$model
    auc_df_add = subset(roc_plot_data, model == lowest_auc_model)
    auc_df_add$facet = "all"
    auc_extra = data.frame(sensitivities = c(1,0,0), specificities = c(0,0,1), model=rep(model_labels$name[j],3), facet=rep("all",3))
    auc_df = rbind(auc_df, auc_df_add, auc_extra)

    roc_plot_data_all$facet = factor(roc_plot_data_all$facet, levels = c(model_labels$name, "all"))
    roc_ci_plot_data_all$facet = factor(roc_ci_plot_data_all$facet, levels = c(model_labels$name, "all"))
    auc_df$facet = factor(auc_df$facet, levels = c(model_labels$name, "all"))

    auc_labels$facet = "all"
    auc_labels_new = auc_labels
    auc_labels_new$facet = auc_labels_new$model
    auc_labels_new$y=max(auc_labels_new$y)
    auc_labels = rbind(auc_labels_new, auc_labels)
    auc_labels$facet = factor(auc_labels$facet, levels = c(model_labels$name, "all"))






    auc_tests = data.frame(matrix(ncol=5,nrow=0))
    colnames(auc_tests) = c("model1", "model2", "model1_auc", "model2_auc", "p_value")


    for (i in 1:length(roc_list)) {
      for (j in (i+1):length(roc_list)) {
        if (j > length(roc_list)) {
          next
        }
        model1 = names(roc_list)[i]
        model2 = names(roc_list)[j]
        if (model1 == model2) {
          next
        }
        model1_auc = roc_list[[i]]$auc
        model2_auc = roc_list[[j]]$auc
        p_value = roc.test(roc_list[[i]], roc_list[[j]])
        temp_frame = data.frame(model1=model1, model2=model2, model1_auc=model1_auc, model2_auc=model2_auc, p_value=p_value$p.value)
        auc_tests = rbind(auc_tests, temp_frame)
      }
    }

    auc_tests$sig_TF = ifelse(auc_tests$p_value < .05, 1, 0)
    auc_tests$sig_TF = factor(auc_tests$sig_TF, levels = c(0,1))
    auc_tests$font = ifelse(auc_tests$sig_TF == 0, "plain", "bold")
    auc_tests$size = ifelse(auc_tests$sig_TF == 0, 0, 1)
    auc_tests$size = as.factor(auc_tests$size)
    auc_tests$width = ifelse(auc_tests$sig_TF == 0, 1, .95)
    auc_tests$height = ifelse(auc_tests$sig_TF == 0, 1, .95)
    auc_tests$sig = ifelse(auc_tests$p_value >= .05, "NS", ifelse(auc_tests$p_value >= .01, "*", ifelse(auc_tests$p_value >= .001, "**", "***")))
    auc_tests$model1 = factor(auc_tests$model1, levels = model_labels$name)
    auc_tests$model2 = factor(auc_tests$model2, levels = rev(model_labels$name))





    output = new("testAssessment", type="binary", model_labels=model_labels, auc_polygon_df=auc_df, roc_ci_df=roc_ci_plot_data_all, roc_curve_df=roc_plot_data_all, auc_ci_labels_df=auc_labels, auc_tests=auc_tests, cat_data=cat_data)
    return(output)
  }
}
