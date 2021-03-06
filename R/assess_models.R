#' modelAssessment Class
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
#' @export modelAssessment
#' @exportClass modelAssessment
#'
modelAssessment = setClass("modelAssessment", slots = c("type", "model_labels", "auc_polygon_df", "roc_ci_df", "roc_curve_df", "auc_ci_labels_df", "auc_tests", "cat_data"))


#' @title Create Model Assessment
#'
#' @description This function is deprecated; all dependent functions (e.g. \code{plot_roc()} or \code{plot_predictor_words}) now take individual models as arguments. This function assesses one or more models created by the \code{\link{language_model}} function.
#'
#' @param ... Models generated by the \code{\link{language_model}} function, and/or two-column dataframes with a predictor variable and an outcome variable
#'
#' @return An object of the type "modelAssessment"
#'
#' @import pROC
#' @importFrom methods setClass new
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
#'                                      textColumnName = "cleanText")
#'
#' # Using language to predict "Positive" vs. "Negative" reviews
#' # Only for mild reviews (ratings of 4 or 7)
#' movie_model_mild = language_model(mild_movie_review_data,
#'                                      outcomeVariableColumnName = "valence",
#'                                      outcomeVariableType = "binary",
#'                                      textColumnName = "cleanText")
#'
#' # Create the model assessment
#' # movie_assessment = assess_models(movie_model_strong, movie_model_mild)
#' }
#'
#' @details
#' The primary purpose of this function is to be used with other functions included in this package, such as plot_roc() or predictor_word_plots()
#' All necessary calculations are performed by this function, so output plots and analyses can be performed quickly and modified as needed
#' This function can be used to assess models generated by the \code{\link{language_model}}, as well as simple predictors that could be compared with language models.


assess_models = function(...) {
  dots = list(...)
  dots_names = match.call(expand.dots = FALSE)

  model_type = c()
  for (i in 1:length(dots)) {
    input = dots[[i]]
    if (class(input) == "langModel") {
      model_type = c(model_type, input@type)
    }
    else if (is.data.frame(input)) {
      if (ncol(input) != 2) {
        stop("Your dataframe '", as.character(dots_names$...[[i]]), "' should have 2 columns: a column of predictor variables, and a column of outcome variables.")
      }
      column_names = colnames(input)
      if (is.factor(input[[column_names[2]]]) & nlevels(input[[column_names[2]]]) == 2) {
        model_type = c(model_type, "binary")
      }
      else if (is.numeric(input[[column_names[2]]])) {
        model_type = c(model_type, "continuous")
      }
      else {
        stop(paste("The second (outcome) column of your dataframe '", as.character(dots_names$...[[i]]), "' must be a 2-level factor or numeric variable.", sep=""))
      }
    }
    else {
      stop(paste("Your argument '", as.character(dots_names$...[[i]]), "'must be either a langModel generated by the create_lasso_regression() function, or a two-column dataframe.", sep=""))
    }
  }

  if (length(unique(model_type)) != 1) {
    stop("All models being assessed must predict the same type of variable (binary outcomes or continuous outcomes).")
  }
  else {
    model_type = model_type[1]
  }




  if (model_type == "binary") {

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

    for (i in 1:length(dots)) {
      input = dots[[i]]
      if (class(input) == "langModel") {
        LASSO_model = TRUE

        predictor<- as.numeric(predict(input@cv,newx=input@x,s=input@lambda))

        roc_data <- suppressMessages(roc(input@y,predictor, ci=TRUE))
        roc_list[[as.character(dots_names$...[[i]])]] = roc_data
        roc_ci_data = ci.sp(roc_data, sensitivities=seq(0,1,.01))

        if (input@type == "binary") {
          cat1title = paste('Words predicting\n"',input@level1, '" responses', sep="")
          cat0title = paste('Words predicting\n"',input@level0, '" responses', sep="")
        }
        else {
          cat1title = paste('Words predicting\n"high-',input@outcome, '" responses', sep="")
          cat0title = paste('Words predicting\n"low-',input@outcome, '" responses', sep="")
        }

        cat0data = input@cat0raw
        cat1data = input@cat1raw
        if (nrow(cat0data) == 0) {
          cat0data = data.frame(words="", weights=0)
        }
        if (nrow(cat1data) == 0) {
          cat1data = data.frame(words="", weights=0)
        }
        cat0data$class = "cat0"
        cat1data$class = "cat1"

        cat_data_temp = rbind(cat0data, cat1data)
        cat_data_temp$model = as.character(dots_names$...[[i]])
        cat_data = rbind(cat_data, cat_data_temp)

      }
      else {
        LASSO_model = FALSE

        column_names = colnames(input)
        input1_x = input[[column_names[1]]]
        response = input[[column_names[2]]]
        predictor_title1 = paste("'",column_names[1],"'", sep="")
        response_title1 = paste("'",column_names[2],"'", sep="")

        glm1<-glm(response~input1_x,family="binomial")
        predictor = as.numeric(predict(glm1,newx=input1_x))

        roc_data <- suppressMessages(roc(response,predictor, ci=TRUE))
        roc_list[[as.character(dots_names$...[[i]])]] = roc_data
        roc_ci_data = ci.sp(roc_data, sensitivities=seq(0,1,.01))

        cat1title = ""
        cat0title = ""

        cat_data_temp = data.frame(words="", weights = 0, class=NA, model = as.character(dots_names$...[[i]]))
        cat_data = rbind(cat_data, cat_data_temp)
      }

      roc_data_formatted = data.frame(specificities=roc_data$specificities, sensitivities=roc_data$sensitivities)
      roc_data_formatted$model = as.character(dots_names$...[[i]])
      roc_ci_data_formatted = as.data.frame(roc_ci_data)
      roc_ci_data_formatted$sensitivities = as.numeric(rownames(roc_ci_data_formatted))
      colnames(roc_ci_data_formatted) = c("percent2p5", "percent50", "percent97p5", "sensitivities")
      roc_ci_data_formatted$model = as.character(dots_names$...[[i]])
      roc_plot_data = rbind(roc_plot_data, roc_data_formatted)
      roc_ci_plot_data = rbind(roc_ci_plot_data, roc_ci_data_formatted)









      temp_frame = data.frame(name=as.character(dots_names$...[[i]]), LASSO_model=LASSO_model, auc = roc_data$auc, ci_lower = roc_data$ci[1], ci_upper = roc_data$ci[3], cat0title = cat0title, cat1title=cat1title)
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


    output = new("modelAssessment", type="binary", model_labels=model_labels, auc_polygon_df=auc_df, roc_ci_df=roc_ci_plot_data_all, roc_curve_df=roc_plot_data_all, auc_ci_labels_df=auc_labels, auc_tests=auc_tests, cat_data=cat_data)

    return(output)
  }
}
