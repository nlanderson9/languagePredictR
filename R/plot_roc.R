#' @title Plot ROC curves
#'
#' @description This function plots ROC curves from the results of the \code{\link{assess_models}} function
#'
#' @param modelAssessment The output of the \code{\link{assess_models}} function
#' @param individual_plot If TRUE, graphs individual ROC curves for each model. Defaults to TRUE.
#' @param combined_plot If TRUE, and modelAssessment contains multiple models, graphs a plot with all ROC curves overlapping. Defaults to TRUE.
#' @param facet_plot If TRUE, and modelAssessment contains multiple models, graphs a faceted plot with all ROC curves included. Defaults to TRUE.
#' @param facet_summary If TRUE, and modelAssessment contains multiple models, the facet_plot will include a plot with all ROC curves overlapping. Defaults to TRUE.
#' @param colors A vector of colors to use for each model's ROC curve.
#' @param model_names A vector of strings to use as titles/names for each model.
#' @param plot_auc_polygon If TRUE, the area below with ROC curve with the lowest AUC will be shaded in. Defaults to TRUE.
#' @param plot_ci If TRUE, a confidence band will be plotted around each ROC curve. Defaults to TRUE.
#' @param line_size A numeric representing the width of the ROC curve line. Defaults to 1.
#' @param print_auc If TRUE, the value of the AUC will be printed on the plot. Defaults to TRUE.
#' @param print_ci If TRUE, the range of the confidence interval will be printed on the plot. Defaults to TRUE.
#' @param print_auc_ci_font_size The font size for printed values for the AUC and confidence interval. Defaults to 4.
#' @param print_auc_ci_x A vector of x (horizontal) positions determining where on the plot the AUC and confidence interval values will be printed.
#' @param print_auc_ci_y A vector of y (vertical) positions determining where on the plot the AUC and confidence interval values will be printed.
#' @param plot_legend If TRUE, a legend will be printed on all plots.
#' @param plot_title The title of the plot
#' @param facet_n_row The number of rows used to plot the facet_plot. Defaults to NULL.
#' @param facet_n_col The number of columns used to plot the facet_plot. Defaults to 2.
#'
#' @return Nothing (this function plots a series of graphs)
#'
#' @seealso \code{\link{assess_models}}
#'
#' @import ggplot2
#' @importFrom scales hue_pal
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
#' # Plot ROC curves
#' plot_roc(movie_assessment)
#' }

plot_roc = function(modelAssessment, individual_plot=TRUE, combined_plot=TRUE, facet_plot=TRUE, facet_summary=TRUE, colors, model_names, plot_auc_polygon=TRUE, plot_ci=TRUE, line_size=1, print_auc=TRUE, print_ci=TRUE, print_auc_ci_font_size=4, print_auc_ci_x, print_auc_ci_y, plot_legend=TRUE, plot_title, facet_n_row=NULL, facet_n_col=2) {

  facet=model=polygon.x=polygon.y=specificities=sensitivities=percent2p5=percent97p5=label_text=x=y=NULL

  if (!(class(modelAssessment) %in% c("modelAssessment", "testAssessment"))) {
    stop("The input for this function must be an output from the `assess_models` function.")
  }

  if (modelAssessment@type != "binary") {
    stop("This function can only be used on assessments of models predicting a binary outcome.")
  }

  model_labels = modelAssessment@model_labels
  if (nrow(model_labels) == 1) {
    combined_plot=FALSE
    facet_plot=FALSE
  }

  if(!individual_plot & !combined_plot & !facet_plot) {
    stop("Given that all of the following arguments are FALSE, no plots will be printed: `individual_plot`, `combined_plot`, `facet_plot`")
  }



  box_df = data.frame(polygon.x = c(0,0,1,1,0), polygon.y = c(0,1,1,0,0))

  if(individual_plot) {
    for (i in 1:nrow(model_labels)) {
      this_model = model_labels$name[i]
      auc_polygon_df = subset(modelAssessment@auc_polygon_df, facet != "all")
      auc_polygon_df = subset(auc_polygon_df, model == this_model)
      roc_ci_df = subset(modelAssessment@roc_ci_df, facet != "all")
      roc_ci_df = subset(roc_ci_df, model == this_model)
      roc_curve_df = subset(modelAssessment@roc_curve_df, facet != "all")
      roc_curve_df = subset(roc_curve_df, model == this_model)
      auc_ci_labels_df = subset(modelAssessment@auc_ci_labels_df, facet != "all")
      auc_ci_labels_df = subset(auc_ci_labels_df, model == this_model)

      lowest_auc_model = model_labels$name[which.min(model_labels$auc)]

      p = ggplot() +
        geom_path(data=box_df, aes(x=polygon.x, y=polygon.y))

      if(plot_auc_polygon) {
        p = p + geom_polygon(data=auc_polygon_df, aes(x=specificities, y=sensitivities), alpha=.3)
      }

      if(!missing(colors)) {
        if (nrow(model_labels) != length(colors)) {
          stop("The length of the argument `colors` must be equal to the number of models in your assessment.")
        }
        this_color = colors[i]
      }
      else {
        this_color = hue_pal()(nrow(model_labels))[i]
      }


      if(plot_ci) {
        p = p + geom_ribbon(data=roc_ci_df, aes(xmin=percent2p5, xmax=percent97p5, y=sensitivities), fill=this_color, color=this_color, alpha=.4, size=.4)
      }

      p = p + geom_line(data=roc_curve_df, aes(x=specificities, y=sensitivities), color=this_color, size=line_size)

      p = p + geom_segment(aes(x=1, xend=0, y=0, yend=1), linetype="dashed")

      if(!missing(print_auc_ci_x)) {
        if (!print_auc & !print_ci) {
          warning("The argument `print_auc_ci_x` will not be used if the AUC and CI values are not being printed.")
        }
        if (length(print_auc_ci_x) > 1) {
          auc_ci_labels_df$x = print_auc_ci_x[1]
        }
        else {
          auc_ci_labels_df$x = print_auc_ci_x
        }
      }

      if(!missing(print_auc_ci_y)) {
        if (!print_auc & !print_ci) {
          warning("The argument `print_auc_ci_y` will not be used if the AUC and CI values are not being printed.")
        }
        if (length(print_auc_ci_y) > 1) {
          auc_ci_labels_df$y = print_auc_ci_y[1]
        }
        else {
          auc_ci_labels_df$y = print_auc_ci_y
        }
      }

      if(print_auc & print_ci) {
        p = p + geom_text(data=auc_ci_labels_df, aes(label=label_text, x=x, y=y), color=this_color, size=print_auc_ci_font_size, hjust=0, show.legend = FALSE)
      }
      if (!print_ci & print_auc) {
        temp_obj = unlist(strsplit(auc_ci_labels_df$label_text, split="\n"))
        n <- length(temp_obj)
        auc_ci_labels_df$label_text = c(temp_obj[seq(n) %% 2 == 1])
        p = p + geom_text(data=auc_ci_labels_df, aes(label=label_text, x=x, y=y), color=this_color, size=print_auc_ci_font_size, hjust=0, show.legend = FALSE)
      }
      if (!print_auc & print_ci) {
        temp_obj = unlist(strsplit(auc_ci_labels_df$label_text, split="\n"))
        n <- length(temp_obj)
        auc_ci_labels_df$label_text = c(rev(temp_obj[seq(n) %% 2 == 0]))
        p = p + geom_text(data=auc_ci_labels_df, aes(label=label_text, x=x, y=y), color=this_color, size=print_auc_ci_font_size, hjust=0, show.legend = FALSE)
      }


      p = p +
        scale_x_reverse() +
        labs(x="Sensitivity", y="Specificity", color="Model", fill="Model") +
        theme_bw() +
        coord_fixed() +
        theme(legend.position = "none")


      if(!missing(plot_title)) {
        if (nrow(model_labels) != length(plot_title)) {
          stop("If `individual_plots` is TRUE, the length of the argument `plot_title` must be equal to the number of models in your assessment.")
        }
        p = p + ggtitle(plot_title[i])
      }
      else if(!missing(model_names)) {
        if (nrow(model_labels) != length(model_names)) {
          stop("The length of the argument `model_names` must be equal to the number of models in your assessment.")
        }
        p = p + ggtitle(model_names[i])
      }
      else {
        p = p + ggtitle(model_labels$name[i])
      }


      plot(p)
    }

  }

  if(combined_plot) {

    auc_polygon_df = subset(modelAssessment@auc_polygon_df, facet != "all")
    roc_ci_df = subset(modelAssessment@roc_ci_df, facet != "all")
    roc_curve_df = subset(modelAssessment@roc_curve_df, facet != "all")
    auc_ci_labels_df = subset(modelAssessment@auc_ci_labels_df, facet == "all")

    lowest_auc_model = model_labels$name[which.min(model_labels$auc)]

    p = ggplot() +
      geom_path(data=box_df, aes(x=polygon.x, y=polygon.y))

    if(plot_auc_polygon) {
      p = p + geom_polygon(data=subset(auc_polygon_df, model == lowest_auc_model), aes(x=specificities, y=sensitivities), alpha=.3)
    }

    if(plot_ci) {
      p = p + geom_ribbon(data=roc_ci_df, aes(xmin=percent2p5, xmax=percent97p5, y=sensitivities, fill=model, color=model), alpha=.4, size=.4)
    }

    p = p + geom_line(data=roc_curve_df, aes(x=specificities, y=sensitivities, color=model), size=line_size)

    p = p + geom_segment(aes(x=1, xend=0, y=0, yend=1), linetype="dashed")

    if(!missing(print_auc_ci_x)) {
      if (!print_auc & !print_ci) {
        warning("The argument `print_auc_ci_x` will not be used if the AUC and CI values are not being printed.")
      }
      if (nrow(model_labels) != length(print_auc_ci_x)) {
        stop("The length of the argument `print_auc_ci_x` must be equal to the number of models in your assessment.")
      }

      auc_ci_labels_df$x = print_auc_ci_x

    }

    if(!missing(print_auc_ci_y)) {
      if (!print_auc & !print_ci) {
        warning("The argument `print_auc_ci_y` will not be used if the AUC and CI values are not being printed.")
      }
      if (nrow(model_labels) != length(print_auc_ci_y)) {
        stop("The length of the argument `print_auc_ci_y` must be equal to the number of models in your assessment.")
      }

      auc_ci_labels_df$y = print_auc_ci_y
    }

    if(print_auc & print_ci) {
      p = p + geom_text(data=auc_ci_labels_df, aes(label=label_text, x=x, y=y, color=model), size=print_auc_ci_font_size, hjust=0, show.legend = FALSE)
    }
    if (!print_ci & print_auc) {
      temp_obj = unlist(strsplit(auc_ci_labels_df$label_text, split="\n"))
      n <- length(temp_obj)
      auc_ci_labels_df$label_text = c(temp_obj[seq(n) %% 2 == 1])
      p = p + geom_text(data=auc_ci_labels_df, aes(label=label_text, x=x, y=y, color=model), size=print_auc_ci_font_size, hjust=0, show.legend = FALSE)
    }
    if (!print_auc & print_ci) {
      temp_obj = unlist(strsplit(auc_ci_labels_df$label_text, split="\n"))
      n <- length(temp_obj)
      auc_ci_labels_df$label_text = c(rev(temp_obj[seq(n) %% 2 == 0]))
      p = p + geom_text(data=auc_ci_labels_df, aes(label=label_text, x=x, y=y, color=model), size=print_auc_ci_font_size, hjust=0, show.legend = FALSE)
    }


    p = p +
      scale_x_reverse() +
      labs(x="Sensitivity", y="Specificity", color="Model", fill="Model") +
      theme_bw() +
      coord_fixed()

    if(!plot_legend) {
      p = p + theme(legend.position = "none")
    }

    if(!missing(plot_title)) {
      p = p + ggtitle(plot_title)
    }


    if(!missing(colors) & !missing(model_names)) {
      if (nrow(model_labels) != length(colors)) {
        stop("The length of the argument `colors` must be equal to the number of models in your assessment.")
      }
      if (nrow(model_labels) != length(model_names)) {
        stop("The length of the argument `model_names` must be equal to the number of models in your assessment.")
      }

      p = p + scale_fill_manual(values=colors, labels=model_names)
      p = p + scale_color_manual(values=colors, labels=model_names)
    }

    if(!missing(colors) & missing(model_names)) {
      if (nrow(model_labels) != length(colors)) {
        stop("The length of the argument `colors` must be equal to the number of models in your assessment.")
      }

      p = p + scale_fill_manual(values=colors)
      p = p + scale_color_manual(values=colors)
    }

    if(!missing(model_names) & missing(colors)) {
      if (nrow(model_labels) != length(model_names)) {
        stop("The length of the argument `model_names` must be equal to the number of models in your assessment.")
      }
      p = p + scale_fill_discrete(labels=model_names)
      p = p + scale_color_discrete(labels=model_names)
    }


    plot(p)
  }

  if(facet_plot) {

    if (facet_summary) {
      auc_polygon_df = modelAssessment@auc_polygon_df
      roc_ci_df = modelAssessment@roc_ci_df
      roc_curve_df = modelAssessment@roc_curve_df
      auc_ci_labels_df = modelAssessment@auc_ci_labels_df
    }
    else {
      auc_polygon_df = subset(modelAssessment@auc_polygon_df, facet != "all")
      roc_ci_df = subset(modelAssessment@roc_ci_df, facet != "all")
      roc_curve_df = subset(modelAssessment@roc_curve_df, facet != "all")
      auc_ci_labels_df = subset(modelAssessment@auc_ci_labels_df, facet != "all")
    }

    lowest_auc_model = model_labels$name[which.min(model_labels$auc)]

    q = ggplot(data=roc_curve_df) +
      geom_path(data=box_df, aes(x=polygon.x, y=polygon.y))

    if(plot_auc_polygon) {
      q = q + geom_polygon(data=auc_polygon_df, aes(x=specificities, y=sensitivities), alpha=.3)
    }

    if(plot_ci) {
      q = q + geom_ribbon(data=roc_ci_df, aes(xmin=percent2p5, xmax=percent97p5, y=sensitivities, fill=model, color=model), alpha=.4, size=.4)
    }

    q = q + geom_line(data=roc_curve_df, aes(x=specificities, y=sensitivities, color=model), size=line_size)

    q = q + geom_segment(aes(x=1, xend=0, y=0, yend=1), linetype="dashed")

    if(!missing(print_auc_ci_x)) {
      if (!print_auc & !print_ci) {
        warning("The argument `print_auc_ci_x` will not be used if the AUC and CI values are not being printed.")
      }
      if (facet_summary & nrow(model_labels) != length(print_auc_ci_x)) {
        stop("The length of the argument `print_auc_ci_x` must be equal to the number of models in your assessment.")
      }
      else if (!facet_summary & length(print_auc_ci_x) != 1) {
        warning("When `facet_summary` is FALSE, only the first value of `print_auc_ci_x` will be used for faceted graphs.")
      }

      auc_ci_labels_df$x = print_auc_ci_x

    }

    if(!missing(print_auc_ci_y)) {
      if (!print_auc & !print_ci) {
        warning("The argument `print_auc_ci_y` will not be used if the AUC and CI values are not being printed.")
      }
      if (facet_summary & nrow(model_labels) != length(print_auc_ci_y)) {
        stop("The length of the argument `print_auc_ci_y` must be equal to the number of models in your assessment.")
      }
      else if (!facet_summary & length(print_auc_ci_y) != 1) {
        warning("When `facet_summary` is FALSE, only the first value of `print_auc_ci_y` will be used for faceted graphs.")
      }

      auc_ci_labels_df$y = print_auc_ci_y
    }

    if(print_auc & print_ci) {
      q = q + geom_text(data=auc_ci_labels_df, aes(label=label_text, x=x, y=y, color=model), size=print_auc_ci_font_size, hjust=0, show.legend = FALSE)
    }
    if (!print_ci & print_auc) {
      temp_obj = unlist(strsplit(auc_ci_labels_df$label_text, split="\n"))
      n <- length(temp_obj)
      auc_ci_labels_df$label_text = c(temp_obj[seq(n) %% 2 == 1])
      q = q + geom_text(data=auc_ci_labels_df, aes(label=label_text, x=x, y=y, color=model), size=print_auc_ci_font_size, hjust=0, show.legend = FALSE)
    }
    if (!print_auc & print_ci) {
      temp_obj = unlist(strsplit(auc_ci_labels_df$label_text, split="\n"))
      n <- length(temp_obj)
      auc_ci_labels_df$label_text = c(rev(temp_obj[seq(n) %% 2 == 0]))
      q = q + geom_text(data=auc_ci_labels_df, aes(label=label_text, x=x, y=y, color=model), size=print_auc_ci_font_size, hjust=0, show.legend = FALSE)
    }

    q = q +
      facet_wrap(~facet, ncol=facet_n_col, nrow=facet_n_row) +
      # facet_wrap(~facet) +
      scale_x_reverse() +
      labs(x="Sensitivity", y="Specificity", color="Model", fill="Model") +
      theme_bw() +
      coord_fixed()

    if(!plot_legend) {
      q = q + theme(legend.position = "none")
    }

    if(!missing(plot_title)) {
      q = q + ggtitle(plot_title)
    }


    if(!missing(colors) & !missing(model_names)) {
      if (nrow(model_labels) != length(colors)) {
        stop("The length of the argument `colors` must be equal to the number of models in your assessment.")
      }
      if (nrow(model_labels) != length(model_names)) {
        stop("The length of the argument `model_names` must be equal to the number of models in your assessment.")
      }

      q = q + scale_fill_manual(values=colors, labels=model_names)
      q = q + scale_color_manual(values=colors, labels=model_names)
    }

    if(!missing(colors) & missing(model_names)) {
      if (nrow(model_labels) != length(colors)) {
        stop("The length of the argument `colors` must be equal to the number of models in your assessment.")
      }

      q = q + scale_fill_manual(values=colors)
      q = q + scale_color_manual(values=colors)
    }

    if(!missing(model_names) & missing(colors)) {
      if (nrow(model_labels) != length(model_names)) {
        stop("The length of the argument `model_names` must be equal to the number of models in your assessment.")
      }
      q = q + scale_fill_discrete(labels=model_names)
      q = q + scale_color_discrete(labels=model_names)
    }


    plot(q)
  }
}
