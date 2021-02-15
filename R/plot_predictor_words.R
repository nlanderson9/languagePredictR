#' @title Plot Predictor Words
#'
#' @description This function plots predictive words from the results of the \code{\link{assess_models}} function
#'
#' @param modelAssessment The output of the \code{\link{assess_models}} function
#' @param topX The number of most-predictive words to plot
#' @param colors A two-element vector containing the colors of the plotted bars. Defaults to c("blue", "orange")
#' @param plot_titles A vector of titles for the plots
#' @param model_names A vector of names for the individual models
#' @param xaxis_range A maximum value for the x-axis
#' @param standard_xaxis If TRUE, the x-axis on all graphs will be the same. If FALSE, it will adjust to fit each individual graph. Defaults to TRUE.
#' @param flip_graphs Flips the graphs horizontally. Defaults to FALSE (low-value outcome variable on the left, high-value outcome variable on the right)
#' @param print_individual If TRUE, prints an individual graph for each model. Defaults to TRUE.
#' @param print_summary If TRUE, prints a summary graph with all models. Defaults to TRUE.
#'
#' @return Nothing (this function plots a series of graphs)
#'
#' @seealso \code{\link{assess_models}}
#'
#' @import ggplot2
#' @import egg
#' @importFrom grid gpar
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#'
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
#' plot_predictor_words(movie_assessment)
#' #Test!

plot_predictor_words = function(modelAssessment, topX, colors=c("blue", "orange"), plot_titles, model_names, xaxis_range, standard_xaxis=TRUE, flip_graphs=FALSE, print_individual=TRUE, print_summary=TRUE) {

  LASSO_model=model=weights=words=NULL

  if(class(modelAssessment) != "modelAssessment") {
    if (class(modelAssessment) == "testAssessment") {
      stop("The `predictor_word_plots` function cannot be used with outputs from the `test_lasso_regression_model` function; the tested data uses the same input words as the results of the training model.")
    }
    else {
      stop("The `modelAssessment` argument should be an output from the `assess_models` function.")
    }
  }


  this_plotlist = list()

  model_labels = modelAssessment@model_labels
  model_labels = subset(model_labels, LASSO_model)

  if(standard_xaxis & missing(xaxis_range)) {
    xaxis_standard = max(abs(modelAssessment@cat_data$weights))
  }

  if (!missing(model_names)) {
    if (length(model_names) != nrow(model_labels)) {
      stop("The length of the argument `model_names` must be equal to the number of models in your assessment.")
    }
  }

  for (i in 1:nrow(model_labels)) {

    if(!missing(plot_titles)) {
      if (length(plot_titles) == 2) {
        cat0title = plot_titles[1]
        cat1title = plot_titles[2]
      }
      else if (length(plot_titles) == nrow(model_labels)*2) {
        cat0title = plot_titles[(i*2)-1]
        cat1title = plot_titles[(i*2)]
      }
      else {
        stop(paste("The `plot_titles` argument must either be length 2 (one title for all positive graphs, one title for all negative graphs) or a length equal to twice the number of language models (2*", nrow(model_labels), "=", nrow(model_labels)*2, ")", sep=""))
      }
    }
    else {
      cat0title = model_labels$cat0title[i]
      cat1title = model_labels$cat1title[i]
    }

    if (length(colors) == 2) {
      color1 = colors[1]
      color2 = colors[2]
    }
    else if (length(colors) == nrow(model_labels)*2) {
      color1 = colors[(i*2)-1]
      color2 = colors[(i*2)]
    }
    else {
      stop(paste("The `colors` argument must either be length 2 (one color for all positive graphs, one color for all negative graphs) or a length equal to twice the number of language models (2*", nrow(model_labels), "=", nrow(model_labels)*2, ")", sep=""))
    }

    cat0raw = subset(modelAssessment@cat_data, class=="cat0" & model == model_labels$name[i])
    cat1raw = subset(modelAssessment@cat_data, class=="cat1" & model == model_labels$name[i])


    cat1raw = cat1raw %>% arrange(desc(weights))
    cat0raw = cat0raw %>% arrange(weights)
    cat1raw$words = factor(cat1raw$words, levels=rev(cat1raw$words))
    cat0raw$words = factor(cat0raw$words, levels=rev(cat0raw$words))


    if (!missing(topX)) {
      if (nrow(cat1raw) > topX) {
        cat1raw = cat1raw[1:topX,]
      }
      if (nrow(cat0raw) > topX) {
        cat0raw = cat0raw[1:topX,]
      }
    }

    if (missing(xaxis_range)) {
      if (max(abs(cat0raw$weights)) >= max(cat1raw$weights)) {
        set_xaxis_range = ceiling(max(abs(cat0raw$weights))/.5) * .5
      }
      else {
        set_xaxis_range = ceiling(max(cat1raw$weights)/.5) * .5
      }
    }
    else {
      set_xaxis_range = xaxis_range
    }

    if(standard_xaxis & missing(xaxis_range)) {
      set_xaxis_range = xaxis_standard
    }

    if(flip_graphs) {
      hjust1 = 0
      hjust2 = 1
    }
    else {
      hjust1 = 1
      hjust2 = 0
    }


    # a = ggplot(cat1raw, aes(x=words, y=weights)) +
    #   geom_col(fill=color2, size=.5,color='black') +
    #   guides(fill=guide_legend('none'))+
    #   theme_bw(base_size = 18) +
    #   geom_hline(yintercept = 0 )+
    #   labs(y = "coefficients")+
    #   ylim(0,set_xaxis_range)+
    #   coord_flip() +
    #   ggtitle(cat1title) +
    #   theme(
    #     plot.title = element_text(size = 15, face = "bold", hjust = hjust1),
    #     panel.grid.minor = element_blank(),
    #     panel.grid.major.y = element_blank(),
    #     axis.title.y = element_blank())
    #
    # b = ggplot(cat0raw, aes(x=words, y=weights)) +
    #   geom_col(fill=color1, size=.5,color='black') +
    #   guides(fill=guide_legend('none'))+
    #   theme_bw(base_size = 18) +
    #   geom_hline(yintercept = 0 )+
    #   labs(y = "coefficients")+
    #   ylim(-set_xaxis_range,0)+
    #   coord_flip()+
    #   scale_x_discrete(position = "top") +
    #   ggtitle(cat0title) +
    #   theme(
    #     plot.title = element_text(size = 15, face = "bold", hjust = hjust2),
    #     panel.grid.minor = element_blank(),
    #     panel.grid.major.y = element_blank(),
    #     axis.title.y = element_blank())

    a = ggplot(cat1raw, aes(x=weights, y=words)) +
      geom_col(fill=color2, size=.5,color='black', orientation="y") +
      guides(fill=guide_legend('none'))+
      theme_bw(base_size = 18) +
      geom_vline(xintercept = 0 )+
      labs(x = "coefficients")+
      xlim(0,set_xaxis_range)+
      # coord_fixed(ratio = (nrow(cat1raw)/100)) +
      ggtitle(cat1title) +
      theme(
        plot.title = element_text(size = 15, face = "bold", hjust = hjust1),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=15))

    b = ggplot(cat0raw, aes(x=weights, y=words)) +
      geom_col(fill=color1, size=.5,color='black', orientation="y") +
      guides(fill=guide_legend('none'))+
      theme_bw(base_size = 18) +
      geom_vline(xintercept = 0 )+
      labs(x = "coefficients")+
      xlim(-set_xaxis_range,0)+
      # coord_fixed(ratio = (nrow(cat0raw)/100)) +
      scale_y_discrete(position="right") +
      ggtitle(cat0title) +
      theme(
        plot.title = element_text(size = 15, face = "bold", hjust = hjust2),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=15))

    this_plotlist[[paste(i,"A",sep="")]] = ggplot() + theme_void()
    this_plotlist[[paste(i,"B",sep="")]] = ggplot() + theme_void()

    if(flip_graphs) {
      this_plotlist[[paste(i,"C",sep="")]] = a
      this_plotlist[[paste(i,"D",sep="")]] = b
    }
    else {
      this_plotlist[[paste(i,"C",sep="")]] = b
      this_plotlist[[paste(i,"D",sep="")]] = a
    }

  }

  if(print_individual) {
    for (i in 1:nrow(model_labels)) {
      if (!missing(model_names)) {
        this_label = model_names[i]
      }
      else {
        this_label = model_labels$name[i]
      }
      egg::ggarrange(this_plotlist[[paste(i,"A",sep="")]], this_plotlist[[paste(i,"B",sep="")]], this_plotlist[[paste(i,"C",sep="")]], this_plotlist[[paste(i,"D",sep="")]], ncol=2, nrow=2, labels = c("", "", this_label, ""), heights = c(.05,.95), label.args = list(vjust=-.5, gp=gpar(fontsize=15, fontface="italic")))
    }
  }


  if(print_summary & nrow(model_labels) > 1) {

    label_vector = c()
    height_vector = c()
    for(i in 1:nrow(model_labels)) {
      if(!missing(model_names)) {
        label_vector = c(label_vector, "", "", model_names[i], "")
      }
      else {
        label_vector = c(label_vector, "", "", model_labels$name[i], "")
      }

      height_vector = c(height_vector, .1, .9)
    }
    egg::ggarrange(plots=this_plotlist, ncol=2, nrow=nrow(model_labels)*2, labels = label_vector, heights = height_vector, label.args = list(vjust=-.5, gp=gpar(fontsize=15, fontface="italic")))
  }
}
