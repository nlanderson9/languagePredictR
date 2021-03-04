#' @title Wrapped by word_network function
#'
#' @param input_node_edge_table The input dataframe
#' @param model The (optional) language model from language_model
#' @param topX The top X words to include
#' @param directed Whether the network is directed
#' @param removeVerticesBelowDegree Number of minimum edges a node must have to include
#' @param edgeColor Edge color
#' @param edgeAlpha Edge alpha
#' @param edgeCurve Degree of edge curve
#' @param modelNodeColors Colors of nodes
#' @param modelNodeSizeRange Size of nodes
#' @param nodeLabelSize Label size
#' @param nodeLabelColor Label color
#' @param plotTitle Plot title
#' @param cat Level (0, 1, or 2 (both))
#'
#' @noRd
#'
#' @importFrom igraph graph.data.frame E<- V<- E V degree delete.vertices plot.igraph layout_ with_fr
#' @importFrom rlang .data
#' @importFrom grDevices adjustcolor
#' @importFrom graphics par

make_word_network = function(input_node_edge_table, model=NULL, topX=100, directed=FALSE, removeVerticesBelowDegree = 2, edgeColor="darkgray", edgeAlpha = .5, edgeCurve = .15, modelNodeColors = c("lightblue", "orange"), modelNodeSizeRange = c(5,10), nodeLabelSize=1, nodeLabelColor="black",  plotTitle="", cat=NULL) {

  cooc_count = outcome = NULL

  # Sort input text by number of pair occurrences
  input_sorted = input_node_edge_table %>%
    arrange(desc(cooc_count))

  # Keep only the topX items
  resultGraph =   input_sorted[1:topX,]

  # Create the graph object
  graphNetwork = graph.data.frame(resultGraph, directed = directed)

  # Set edge colors
  E(graphNetwork)$color = adjustcolor(edgeColor, alpha.f = edgeAlpha)

  # Rescale the weights to plot edges nicely
  E(graphNetwork)$width = scales::rescale(E(graphNetwork)$weight, to = c(1, 10))

  # Set curve of edges, if desired
  if (edgeCurve > 0) {
    E(graphNetwork)$curved = edgeCurve
  }

  # If the input includes a model, set color & node size based on model results
  # (i.e. if the node is a predictive word in the model, color it and resize the node based on the weight in the model)
  if (cat == 0) {
    cat = model@cat0raw
    cat$color = modelNodeColors[1]
    cat$weights_rescaled = scales::rescale(abs(cat$weights), to = modelNodeSizeRange)
  }
  else if (cat == 1) {
    cat = model@cat1raw
    cat$color = modelNodeColors[2]
    cat$weights_rescaled = scales::rescale(cat$weights, to = modelNodeSizeRange)
  }
  else if (cat == 2) {
    cat0 = model@cat0raw
    cat0$color = modelNodeColors[1]
    cat0$weights = abs(cat0$weights)
    cat1 = model@cat1raw
    cat1$color = modelNodeColors[2]
    cat = bind_rows(cat0,cat1)
    cat$weights_rescaled = scales::rescale(cat$weights, to = modelNodeSizeRange)
  }

  if(!is.null(cat)) {
    for (i in 1:length(V(graphNetwork))) {
      label = V(graphNetwork)$name[i]
      if (label %in% as.character(cat$words)) {
        index = which(cat$words == label)
        V(graphNetwork)$size[i] = cat$weights_rescaled[index]
        V(graphNetwork)$color[i] = cat$color[index]
      }
      else {
        V(graphNetwork)$size[i] = 1
        V(graphNetwork)$color[i] = "gray"
      }
    }
  }

  # Remove nodes that have few connections
  verticesToRemove = V(graphNetwork)[degree(graphNetwork) < removeVerticesBelowDegree]
  graphNetwork = delete.vertices(graphNetwork, verticesToRemove)

  # If the plot has a title, add a margin at the top
  if (plotTitle=="") {
    par(mai=c(0,0,0,0))
  }
  else {
    par(mai=c(0,0,.5,0))
  }

  layout = layout_(graphNetwork, with_fr())

  # Final Plot
  plot(
    graphNetwork,
    layout = layout,
    main = plotTitle,
    vertex.label.family = "sans",
    vertex.shape = "circle",
    vertex.frame.color = adjustcolor(edgeColor, alpha.f = edgeAlpha), # have node borders match the edges
    vertex.label.color = nodeLabelColor,     # Color of node names
    vertex.label.font = 2,            # Font of node names
    vertex.label = V(graphNetwork)$name,
    vertex.label.cex = nodeLabelSize # font size of node names
  )
}

#' @title Plot Word Network
#' @description Plots a word network of adjacent words.
#'
#' @param input An input dataframe, typically the output from the \code{node_edge} function
#' @param model Optional - if \code{node_edge} used a model as input, the same model can be provided here for extra functionality
#' @param topX The number of word pairs to include in the graphed network. Chosen word pairs are selected from those with the greatest number of co-occurrences. Defaults to 100.
#' @param directed Determines if the network is directed (direction of edges matters) or not. Defaults to FALSE (the output from \code{node_edge} does not yield directional edge information, so only change this if using your own dataframe).
#' @param removeVerticesBelowDegree An integer which determines the minimum number of edges a node must have to be included. Default is 2.
#' @param edgeColor The color of the edges. Default is "darkgray".
#' @param edgeAlpha The alpha of the edges. Default is 0.5.
#' @param edgeCurve If greater than 0, edges will be curved with a radius corresponding to the value. Default is 0.15. A value of 0 yields straight edges.
#' @param modelNodeColors The color shading for nodes that are predictive words in the provided model. Must be a vector of two values. Defaults to c("lightblue", "orange").
#' @param modelNodeSizeRange The sizing for nodes that are predictive words in the provided model. Must be a vector of two values (the minimum plotted size and maximum plotted size). Defaults to c(5, 10).
#' @param nodeLabelSize The size of the text for node labels. Defaults to 1.
#' @param nodeLabelColor The color of the node labels. Defaults to "black".
#' @param plotTitle The title of the plot(s). If a model is used, it must be a vector of three strings. If not, it must be a single string.
#'
#' @return Nothing; a word_network is plotted
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' movie_review_data1$cleanText = clean_text(movie_review_data1$text)
#'
#' # Using language to predict "Positive" vs. "Negative" reviews
#' movie_model_valence = language_model(movie_review_data1,
#'                                      outcomeVariableColumnName = "valence",
#'                                      outcomeVariableType = "binary",
#'                                      textColumnName = "cleanText")
#'
#' node_edge_table = node_edge(movie_model_valence)
#' word_network(node_edge_table)
#' }

word_network = function(input, model=NULL, topX=100, directed=FALSE, removeVerticesBelowDegree = 2, edgeColor="darkgray", edgeAlpha = .5, edgeCurve = .15, modelNodeColors = c("lightblue", "orange"), modelNodeSizeRange = c(5,10), nodeLabelSize=1, nodeLabelColor="black", plotTitle=NULL) {

  outcome=NULL

  if ("outcome" %in% colnames(input)) {
    if (length(table(input$outcome)) > 1 & is.null(model)) {
      stop("If the input table has multiple outcome datasets included, the `model` argument must be included. If you don't want to include a model, ensure that your table contains a single `outcome` variable.")
    }
    if (is.null(plotTitle)) {
      title0 = paste(model@level0, "text")
      title1 = paste(model@level1, "text")
      title2 = paste(model@level0, "&", model@level1, "text")
    }
    else if (!plotTitle) {
      title0 = ""
      title1 = ""
      title2 = ""
    }
    else {
      if (length(plotTitle) != 3) {
        stop("The `plotTitle` argument must include titles for all 3 plots resulting from your model (both outcomes + combined).")
      }
      title0 = plotTitle[1]
      title1 = plotTitle[2]
      title2 = plotTitle[3]
    }

    make_word_network(subset(input, outcome==model@level0), model=model, topX=topX, cat=0, plotTitle=title0, directed=directed, removeVerticesBelowDegree=removeVerticesBelowDegree, edgeColor=edgeColor, edgeAlpha=edgeAlpha, edgeCurve=edgeCurve, modelNodeColors=modelNodeColors, modelNodeSizeRange=modelNodeSizeRange, nodeLabelSize=nodeLabelSize, nodeLabelColor=nodeLabelColor)
    make_word_network(subset(input, outcome==model@level1), model=model, topX=topX, cat=1, plotTitle=title1, directed=directed, removeVerticesBelowDegree=removeVerticesBelowDegree, edgeColor=edgeColor, edgeAlpha=edgeAlpha, edgeCurve=edgeCurve, modelNodeColors=modelNodeColors, modelNodeSizeRange=modelNodeSizeRange, nodeLabelSize=nodeLabelSize, nodeLabelColor=nodeLabelColor)
    make_word_network(subset(input, outcome=="all_outcomes"), model=model, topX=topX, cat=2, plotTitle=title2, directed=directed, removeVerticesBelowDegree=removeVerticesBelowDegree, edgeColor=edgeColor, edgeAlpha=edgeAlpha, edgeCurve=edgeCurve, modelNodeColors=modelNodeColors, modelNodeSizeRange=modelNodeSizeRange, nodeLabelSize=nodeLabelSize, nodeLabelColor=nodeLabelColor)
  }
  else {
    if (is.null(plotTitle) | !plotTitle) {
      plot_title = ""
    }
    else {
      if(length(plotTitle) != 1) {
        stop("The `plotTitle` argument can only be a length of 1.")
      }
      plot_title = plotTitle
    }
    make_word_network(input, model=NULL, topX=topX, cat=NULL, plotTitle=plot_title, directed=directed, removeVerticesBelowDegree=removeVerticesBelowDegree, edgeColor=edgeColor, edgeAlpha=edgeAlpha, edgeCurve=edgeCurve, modelNodeColors=modelNodeColors, modelNodeSizeRange=modelNodeSizeRange, nodeLabelSize=nodeLabelSize, nodeLabelColor=nodeLabelColor)
  }
}
