#' Creates 'neat' p-values for printing
#' @param rawpValue The p value to be 'neatened,' a numeric value
#' @noRd

neat_p_value = function(rawpValue) {
  if (rawpValue >= 0.05) {
    result = paste("p > 0.05 (p = ", round(rawpValue,3), ")", sep="")
  }
  else if (rawpValue < 0.05 & rawpValue >= 0.01) {
    result = paste("(*) p < 0.05 (p = ", round(rawpValue,3), ")", sep="")
  }
  else if (rawpValue < 0.01 & rawpValue >= 0.001) {
    result = paste("(**) p < 0.01 (p = ", round(rawpValue,4), ")", sep="")
  }
  else if (rawpValue < 0.001 & rawpValue >= 0.0001) {
    result = paste("(**) p < 0.001 (p = ", round(rawpValue,5), ")", sep="")
  }
  else {
    if (round(rawpValue,6) > 0) {
      rawpValue = round(rawpValue,6)
    }
    else {
      rawpValue = formatC(rawpValue, format="e")
    }
    result = paste("(***) p < 0.0001 (p = ", rawpValue, ")", sep="")
  }
  return(result)
}


#' Creates part-of-speech comparisons for the \code{check_POS_predictors} function
#' @param test The part of speech being tested
#' @param inputLASSOModel the model to be tested, output by the \code{create_lasso_regression_model} function
#'
#' @importFrom grDevices rgb
#' @importFrom graphics text
#' @importFrom stats coef glm predict
#' @import pROC
#'
#' @noRd

generate_POS_comparison = function(test, inputLASSOModel) {
  familytype = ifelse(inputLASSOModel@type == "binary", "binomial", "gaussian")
  predictor = inputLASSOModel@original_dataframe[[test]]
  outcome = inputLASSOModel@y

  test_frame<-data.frame(pred=predictor, y=outcome)
  glm1<-glm(y~pred, data=test_frame,family=familytype)
  results_output1 = coef(summary(glm1))
  p_value_test = neat_p_value(results_output1[2,4])
  magnitude_test = results_output1[2,1]

  if (results_output1[2,4] < 0.05){
    if (magnitude_test > 0) {
      test_outcome = paste(test, " DOES predict '",inputLASSOModel@outcome,"'\n(",test," is greater for '", inputLASSOModel@level1,"' outcomes than '", inputLASSOModel@level0, "' outcomes)\n", sep="")
    }
    else if (magnitude_test < 0) {
      test_outcome = paste(test, " DOES predict '",inputLASSOModel@outcome,"'\n(",test," is greater for '", inputLASSOModel@level0,"' outcomes than '", inputLASSOModel@level1, "' outcomes)\n", sep="")
    }

  }
  else {
    test_outcome = paste(test, "DOES NOT predict '",inputLASSOModel@outcome,"'\n", sep="")
  }

  cat(paste(test_outcome, p_value_test, "\n", sep=""))

  if (results_output1[2,4] < 0.05){
    glm1_predict = predict(glm1,newx=predictor)
    classifier_out<-predict(inputLASSOModel@cv,newx=inputLASSOModel@x,s=inputLASSOModel@lambda)

    color1 = rgb(red=0.0902, green=0.2235, blue=0.9412, alpha=1.0)
    shade1 = rgb(red=0.0902, green=0.2235, blue=0.9412, alpha=0.3)
    color2 = rgb(red=0.2510, green=0.4431, blue=0.1333, alpha=1.0)
    shade2 = rgb(red=0.2510, green=0.4431, blue=0.1333, alpha=0.3)

    pROC_obj1 <- suppressMessages(roc(outcome,as.numeric(classifier_out),
                                      smooth = FALSE,
                                      # arguments for ci
                                      ci=TRUE,
                                      # arguments for plot
                                      plot=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                                      print.auc=TRUE,
                                      print.auc.x=0.5, print.auc.y=0.4, print.auc.col=color1, col=color1))

    sens.sp1 = ci.sp(pROC_obj1, sensitivities=seq(0,1,.01))

    pROC_obj2 <- suppressMessages(roc(outcome,as.numeric(glm1_predict),
                                      smooth = FALSE,
                                      # arguments for ci
                                      ci=TRUE,
                                      # arguments for plot
                                      plot=TRUE, grid=TRUE,
                                      print.auc=TRUE,
                                      print.auc.x=0.5, print.auc.y=0.3, print.auc.col=color2, col=color2, add=TRUE))

    sens.sp2 = ci.sp(pROC_obj2, sensitivities=seq(0,1,.01))

    roc_test = roc.test(pROC_obj1, pROC_obj2)
    p_value_roc = neat_p_value(roc_test$p.value)

    plot(sens.sp1, type="shape", col=shade1, no.roc=TRUE)
    plot(sens.sp2, type="shape", col=shade2, no.roc=TRUE)
    suppressMessages(plot(pROC_obj1, add=TRUE, col=color1))
    suppressMessages(plot(pROC_obj2, add=TRUE, col=color2))
    text(0.5, 1.08, paste("Response language predicting '", inputLASSOModel@outcome, "'", sep=""), col=color1)
    text(0.5, 1.04, paste(test, " predicting '", inputLASSOModel@outcome, "'", sep=""), col=color2)


    if (roc_test$p.value >= 0.05){
      compare_outcome = paste("Response language does not predict the outcome variable better than ",test,"\n", sep="")
    }
    else {
      if (auc(pROC_obj1) < auc(pROC_obj2)) {
        compare_outcome = paste(test," predicts the outcome variable better than response language\n",sep="")
      }
      else {
        compare_outcome = paste("Response language predicts the outcome variable better than ",test,"\n",sep="")
      }

    }

    cat(compare_outcome, p_value_roc, "\n\n", sep="")
  }
}


#' Creates data for a summary facet for the \code{assess_models} function (to be used by the \code{plot_roc} function)
#' @param df the dataframe to be used
#' @param col the column used for faceting
#' @noRd

CreateAllFacet <- function(df, col){
  df$facet <- df[[col]]
  temp <- df
  temp$facet <- "all"
  merged <-rbind(temp, df)

  # ensure the facet value is a factor
  merged[[col]] <- as.factor(merged[[col]])

  return(merged)
}

#' Allows for spacing between legend items in a vertical ggplot graph
#' @param data The data
#' @param params The parameters
#' @param size The size
#'
#' @export
#'
#' @noRd

`%||%` = function (x, y)
{
  if (is.null(x))
    y
  else x
}

#' Allows for spacing between legend items in a vertical ggplot graph
#' @param data The data
#' @param params The parameters
#' @param size The size
#'
#' @import scales
#'
#' @noRd

draw_key_polygon4 = function(data, params, size) {
  if (is.null(data$size)) {
    data$size <- 0.5
  }
  lwd <- min(data$size, min(size)/4)
  grid::rectGrob(width = grid::unit(.6, "npc"),
                 height = grid::unit(.6, "npc"),
                 gp = grid::gpar(col = data$colour %||% NA,
                                 fill = scales::alpha(data$fill %||% "grey20", data$alpha),
                                 lty = data$linetype %||% 1,
                                 lwd = lwd * .pt,
                                 linejoin = params$linejoin %||% "mitre",
                                 lineend = if (identical(params$linejoin, "round")) "round" else "square")
  )
}

#' Allows for spacing between legend items in a vertical ggplot graph
#' @param data The data
#' @param params The parameters
#' @param size The size
#'
#' @import scales
#'
#' @noRd

draw_key_path4 = function (data, params, size) {
  if (is.null(data$linetype)) {
    data$linetype <- 0
  }
  else {
    data$linetype[is.na(data$linetype)] <- 0
  }
  grid::segmentsGrob(x0 = unit(.32, "npc"),
                     y0 = grid::unit(.5, "npc"),
                     x1 = grid::unit(.68, "npc"),
                     y1 = grid::unit(.5, "npc"),
                     gp = grid::gpar(col = scales::alpha(data$colour %||% data$fill %||% "black", data$alpha),
                                     fill = scales::alpha(params$arrow.fill %||% data$colour %||% data$fill %||% "black", data$alpha),
                                     lwd = (data$size %||% 0.5) * .pt, lty = data$linetype %||% 1, lineend = "butt"),
                     arrow = params$arrow)
}
