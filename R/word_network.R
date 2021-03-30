#' @title Wrapped by plot_word_network function
#'
#' @param input_node_edge_table The input dataframe
#' @param model The (optional) language model from language_model
#' @param topX The top X words to include
#' @param directed Whether the network is directed
#' @param removeVerticesBelowDegree Number of minimum edges a node must have to include
#' @param clusterType Type of clustering (node, edge, or none)
#' @param clusterNodeMethod If node clustering, which method
#' @param plotUnclusteredNetwork Plot the standard network with no clustering
#' @param plotClusteredNetwork Plot the network with clustering displayed
#' @param plotIndividualClusters Plot each cluster as an individual network
#' @param plotIndividualClusterFacet Plot a single graph with each cluster as a pane
#' @param plotClusterLegend Plot the legend for cluster colors
#' @param edgeColor Edge color
#' @param edgeAlpha Edge alpha
#' @param edgeCurve Degree of edge curve
#' @param modelNodeColors Colors of nodes
#' @param modelNodeSizeRange Size of nodes
#' @param nodeLabelSize Label size
#' @param nodeLabelColor Label color
#' @param plotTitle Plot title
#' @param layout igraph layout to reuse
#' @param cat Level (0, 1, or 2 (both))
#'
#' @noRd
#'
#' @importFrom igraph graph.data.frame E<- V<- E V degree delete.vertices delete.edges plot.igraph layout_ with_fr communities cluster_edge_betweenness cluster_fast_greedy cluster_infomap cluster_label_prop cluster_leading_eigen cluster_louvain cluster_optimal cluster_spinglass cluster_walktrap
#' @importFrom linkcomm getLinkCommunities
#' @importFrom rlang .data
#' @importFrom grDevices adjustcolor
#' @importFrom graphics par

make_word_network = function(input_node_edge_table, model=NULL, topX=100, directed=FALSE, removeVerticesBelowDegree = 2, clusterType="none", clusterNodeMethod="infomap", plotUnclusteredNetwork=TRUE, plotClusteredNetwork=TRUE, plotIndividualClusters=TRUE, plotIndividualClusterFacet=TRUE, plotClusterLegend=TRUE, edgeColor="darkgray", edgeAlpha = .5, edgeCurve = .15, modelNodeColors = c("lightblue", "orange"), modelNodeSizeRange = c(5,10), nodeLabelSize=1, nodeLabelColor="black",  plotTitle="", layout=NULL, cat=NULL) {

  cooc_count = outcome = NULL

  if (!(clusterType %in% c("none", "edge", "node"))) {
    stop("The `clusterType` argument must be one of 'none', 'edge', or 'node'.")
  }

  if (!(clusterNodeMethod %in% c("edge_betweenness", "fast_greedy", "infomap", "label_prop", "leading_eigen", "louvain", "optimal", "spinglass", "walktrap"))) {
    stop('The `clusterNodeMethod` argument must be one of:\n"edge_betweenness",\n"fast_greedy",\n"infomap",\n"label_prop",\n"leading_eigen",\n"louvain",\n"optimal",\n"spinglass",\n"walktrap"')
  }

  if (clusterType=="none") {
    plotClusteredNetwork = FALSE
    plotIndividualClusters = FALSE
    plotIndividualClusterFacet = FALSE
  }

  # Sort input text by number of pair occurrences
  input_sorted = input_node_edge_table %>%
    arrange(desc(cooc_count))

  # Keep only the topX items
  resultGraph = input_sorted[1:topX,]

  if(clusterType=="edge") {

    all_inputs = c(resultGraph$first, resultGraph$second)
    all_inputs_table = table(all_inputs)
    inputs_1 = all_inputs_table[all_inputs_table < removeVerticesBelowDegree]
    resultGraph = subset(resultGraph, !(first %in% names(inputs_1)) & !(second %in% names(inputs_1)))

    clustering_data = getLinkCommunities(as.matrix(subset(resultGraph, select = c(first, second, weight))), plot=FALSE, verbose=FALSE)
    clustering_table = clustering_data$edges
    num_clusters = max(as.numeric(clustering_table$cluster))
    clustering_table = clustering_table %>% rowwise() %>% mutate(color1 = hue_pal()(num_clusters)[as.numeric(cluster)])
    resultGraph = resultGraph %>% left_join(clustering_table, by=c("first" = "node1", "second" = "node2"))
    no_clusters_present = ifelse(sum(is.na(resultGraph$cluster)) > 0, TRUE, FALSE)
    resultGraph$linetype = ifelse(is.na(resultGraph$color1), 2, 1)
    resultGraph$color1 = ifelse(is.na(resultGraph$color1), "#000000", resultGraph$color1)
    resultGraph$cluster = ifelse(is.na(resultGraph$cluster), "No cluster", resultGraph$cluster)
    cluster_levels = seq(from=1, to=num_clusters, by=1)
    cluster_colors = hue_pal()(num_clusters)
    cluster_lty = rep(1, num_clusters)
    if(no_clusters_present) {
      cluster_levels = c(as.character(cluster_levels), "No cluster")
      cluster_colors = c(cluster_colors, "#000000")
      cluster_lty = c(cluster_lty, 2)
    }
    resultGraph$cluster = factor(resultGraph$cluster, levels=cluster_levels)
  }


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

  if (clusterType != "edge") {
    # Remove nodes that have few connections
    verticesToRemove = V(graphNetwork)[degree(graphNetwork) < removeVerticesBelowDegree]
    graphNetwork = delete.vertices(graphNetwork, verticesToRemove)
  }

  if (clusterType == "node"){
    if (clusterNodeMethod=="edge_betweenness") {
      result = cluster_edge_betweenness(graphNetwork)
    }
    else if (clusterNodeMethod=="fast_greedy") {
      result = cluster_fast_greedy(graphNetwork)
    }
    else if (clusterNodeMethod=="infomap") {
      result = cluster_infomap(graphNetwork)
    }
    else if (clusterNodeMethod=="label_prop") {
      result = cluster_label_prop(graphNetwork)
    }
    else if (clusterNodeMethod=="leading_eigen") {
      result = cluster_leading_eigen(graphNetwork)
    }
    else if (clusterNodeMethod=="louvain") {
      result = cluster_louvain(graphNetwork)
    }
    else if (clusterNodeMethod=="optimal") {
      result = cluster_optimal(graphNetwork)
    }
    else if (clusterNodeMethod=="spinglass") {
      result = cluster_spinglass(graphNetwork)
    }
    else if (clusterNodeMethod=="walktrap") {
      result = cluster_walktrap(graphNetwork)
    }
    result_frame = data.frame(membership = result$membership)
    num_clusters = max(as.numeric(result$membership))
    result_frame = result_frame %>% rowwise() %>% mutate(color2 = hue_pal()(num_clusters)[as.numeric(membership)])
    V(graphNetwork)$label.color = result_frame$color2
    V(graphNetwork)$cluster = result_frame$membership
    cluster_levels = seq(from=1, to=num_clusters, by=1)
    cluster_colors = rainbow(num_clusters, alpha=.3)
  }


  # If the plot has a title, add a margin at the top
  if (plotTitle=="") {
    par(mai=c(0,0,0,0))
  }
  else {
    par(mai=c(0,0,.5,0))
  }

  if(is.null(layout)) {
    layout = layout_(graphNetwork, with_fr())
  }

  # layout(matrix(c(1,1), ncol=1, byrow = TRUE))
  par(mfrow=c(1,1))
  if(plotUnclusteredNetwork) {
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
      vertex.label.cex = nodeLabelSize, # font size of node names
    )
  }
  if (plotClusteredNetwork) {
    if (clusterType=="edge") {
      E(graphNetwork)$color = E(graphNetwork)$color1
      E(graphNetwork)$lty = E(graphNetwork)$linetype
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
      if(plotClusterLegend) {
        legend('topright', legend=cluster_levels, col=cluster_colors, lty=cluster_lty, lwd=3)
      }
    }
    else if (clusterType == "node") {
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
        vertex.label.cex = nodeLabelSize, # font size of node names
        mark.groups = communities(result)
      )
      if(plotClusterLegend) {
        legend('topright', legend=cluster_levels, fill=cluster_colors)
      }
    }

  }


  if (plotIndividualClusters) {

    par(mfrow=c(1,1))

    if (clusterType == "edge") {
      if (!plotClusteredNetwork) {
        E(graphNetwork)$color = E(graphNetwork)$color1
        E(graphNetwork)$lty = E(graphNetwork)$linetype
      }

      for (i in 1:num_clusters) {
        remove_other_edges = E(graphNetwork)[E(graphNetwork)$cluster != as.character(i)]
        graphNetwork_cluster = delete.edges(graphNetwork, remove_other_edges)
        remove_unused_vertices = V(graphNetwork_cluster)[degree(graphNetwork_cluster) < 1]
        graphNetwork_cluster = delete.vertices(graphNetwork_cluster, remove_unused_vertices)

        if (length(V(graphNetwork_cluster)) > 1) {
          layout_frame = data.frame(rownumber=seq(1,length(V(graphNetwork)$name)), name=V(graphNetwork)$name)
          nodes_to_keep = subset(clustering_data$nodeclusters, cluster==i)
          layout_frame = subset(layout_frame, name %in% nodes_to_keep$node)
          layout_cluster = layout[layout_frame$rownumber,]
        }
        else {
          layout_cluster = layout_(graphNetwork_cluster, nicely())
        }

        plot(
          graphNetwork_cluster,
          layout = layout_cluster,
          # main = paste("Cluster", i),
          vertex.label.family = "sans",
          vertex.shape = "circle",
          vertex.frame.color = adjustcolor(edgeColor, alpha.f = edgeAlpha), # have node borders match the edges
          vertex.label.color = nodeLabelColor,     # Color of node names
          vertex.label.font = 2,            # Font of node names
          vertex.label = V(graphNetwork_cluster)$name,
          vertex.label.cex = nodeLabelSize # font size of node names
        )
        title(paste("Cluster", i),cex.main=3,col.main=hue_pal()(num_clusters)[i])
      }
    }
    else if (clusterType == "node") {
      for (i in 1:num_clusters) {
        remove_other_vertices = V(graphNetwork)[V(graphNetwork)$cluster != i]
        graphNetwork_cluster = delete.vertices(graphNetwork, remove_other_vertices)

        if (length(V(graphNetwork_cluster)) > 1) {
          layout_frame = data.frame(rownumber=seq(1,length(V(graphNetwork)$cluster)), cluster=V(graphNetwork)$cluster)
          layout_frame = subset(layout_frame, cluster==i)
          layout_cluster = layout[layout_frame$rownumber,]
        }
        else {
          layout_cluster = layout_(graphNetwork_cluster, nicely())
        }

        plot(
          graphNetwork_cluster,
          layout = layout_cluster,
          # main = ,
          vertex.label.family = "sans",
          vertex.shape = "circle",
          vertex.frame.color = adjustcolor(edgeColor, alpha.f = edgeAlpha), # have node borders match the edges
          vertex.label.color = nodeLabelColor,     # Color of node names
          vertex.label.font = 2,            # Font of node names
          vertex.label = V(graphNetwork_cluster)$name,
          vertex.label.cex = nodeLabelSize, # font size of node names
          mark.groups = communities(result)[i],
          mark.col =  rainbow(num_clusters, alpha = 0.3)[i],
          mark.border = rainbow(num_clusters, alpha = 1)[i]
        )
        title(paste("Cluster", i),cex.main=3,col.main=rainbow(num_clusters, alpha=1)[i])
      }
    }
  }


  if (plotIndividualClusterFacet) {

    layout_matrix = matrix(seq(from=1, to=num_clusters), ncol=3, byrow = TRUE)
    if (num_clusters %% 3 == 1) {
      layout_matrix[nrow(layout_matrix),2] = 0
      layout_matrix[nrow(layout_matrix),3] = 0
    }
    else if (num_clusters %% 3 == 2) {
      layout_matrix[nrow(layout_matrix),3] = 0
    }

    layout(layout_matrix)

    if (clusterType == "edge") {
      if (!plotClusteredNetwork) {
        E(graphNetwork)$color = E(graphNetwork)$color1
        E(graphNetwork)$lty = E(graphNetwork)$linetype
      }

      for (i in 1:num_clusters) {
        remove_other_edges = E(graphNetwork)[E(graphNetwork)$cluster != as.character(i)]
        graphNetwork_cluster = delete.edges(graphNetwork, remove_other_edges)
        remove_unused_vertices = V(graphNetwork_cluster)[degree(graphNetwork_cluster) < 1]
        graphNetwork_cluster = delete.vertices(graphNetwork_cluster, remove_unused_vertices)

        if (length(V(graphNetwork_cluster)) > 1) {
          layout_frame = data.frame(rownumber=seq(1,length(V(graphNetwork)$name)), name=V(graphNetwork)$name)
          nodes_to_keep = subset(clustering_data$nodeclusters, cluster==i)
          layout_frame = subset(layout_frame, name %in% nodes_to_keep$node)
          layout_cluster = layout[layout_frame$rownumber,]
        }
        else {
          layout_cluster = layout_(graphNetwork_cluster, nicely())
        }

        plot(
          graphNetwork_cluster,
          layout = layout_cluster,
          # main = paste("Cluster", i),
          vertex.label.family = "sans",
          vertex.shape = "circle",
          vertex.frame.color = adjustcolor(edgeColor, alpha.f = edgeAlpha), # have node borders match the edges
          vertex.label.color = nodeLabelColor,     # Color of node names
          vertex.label.font = 2,            # Font of node names
          vertex.label = V(graphNetwork_cluster)$name,
          vertex.label.cex = nodeLabelSize # font size of node names
        )
        title(paste("Cluster", i),cex.main=3,col.main=hue_pal()(num_clusters)[i])
      }
    }
    else if (clusterType == "node") {
      for (i in 1:num_clusters) {
        remove_other_vertices = V(graphNetwork)[V(graphNetwork)$cluster != i]
        graphNetwork_cluster = delete.vertices(graphNetwork, remove_other_vertices)

        if (length(V(graphNetwork_cluster)) > 1) {
          layout_frame = data.frame(rownumber=seq(1,length(V(graphNetwork)$cluster)), cluster=V(graphNetwork)$cluster)
          layout_frame = subset(layout_frame, cluster==i)
          layout_cluster = layout[layout_frame$rownumber,]
        }
        else {
          layout_cluster = layout_(graphNetwork_cluster, nicely())
        }

        plot(
          graphNetwork_cluster,
          layout = layout_cluster,
          # main = ,
          vertex.label.family = "sans",
          vertex.shape = "circle",
          vertex.frame.color = adjustcolor(edgeColor, alpha.f = edgeAlpha), # have node borders match the edges
          vertex.label.color = nodeLabelColor,     # Color of node names
          vertex.label.font = 2,            # Font of node names
          vertex.label = V(graphNetwork_cluster)$name,
          vertex.label.cex = nodeLabelSize, # font size of node names
          mark.groups = communities(result)[i],
          mark.col =  rainbow(num_clusters, alpha = 0.3)[i],
          mark.border = rainbow(num_clusters, alpha = 1)[i]
        )
        title(paste("Cluster", i),cex.main=3,col.main=rainbow(num_clusters, alpha=1)[i])
      }
    }
  }
  if (clusterType == "none") {
    return(list(network_object = graphNetwork, layout_object = layout, parameters = list(edgeColor=edgeColor, edgeAlpha=edgeAlpha, nodeLabelColor=nodeLabelColor, nodeLabelSize=nodeLabelSize)))
  }
  else if (clusterType == "node") {
    return(list(network_object = graphNetwork, layout_object = layout, cluster_object = result, parameters = list(edgeColor=edgeColor, edgeAlpha=edgeAlpha, nodeLabelColor=nodeLabelColor, nodeLabelSize=nodeLabelSize)))
  }
  else if (clusterType == "edge") {
    return(list(network_object = graphNetwork, layout_object = layout, cluster_object = clustering_data, parameters = list(edgeColor=edgeColor, edgeAlpha=edgeAlpha, nodeLabelColor=nodeLabelColor, nodeLabelSize=nodeLabelSize)))
  }
}

#' @title Plot Word Network
#' @description Plots a word network of adjacent words.
#'
#' @param input An input dataframe, typically the output from the \code{node_edge} function
#' @param model Optional - if \code{node_edge} used a model as input, the same model can be provided here for extra functionality
#' @param topX The number of word pairs to include in the graphed network. Chosen word pairs are selected from those with the greatest number of co-occurrences. Defaults to 100.
#' @param graphIndividual If TRUE, individual graphs are produced for both outcomes of the model. Default is TRUE.
#' @param graphCombined If TRUE, a network is graphed based on the entire language corpus. Default is FALSE.
#' @param directed Determines if the network is directed (direction of edges matters) or not. Defaults to FALSE (the output from \code{node_edge} does not yield directional edge information, so only change this if using your own dataframe).
#' @param removeVerticesBelowDegree An integer which determines the minimum number of edges a node must have to be included. Default is 2.
#' @param clusterType The type of clustering to perform. "node" clusters by nodes (using the method defined by \code{clusterNodeMethod}), "edge" clusters by edges (using the \code{lincomm} package. Defaults to "none".
#' @param clusterNodeMethod If clustering by "node", this determines the method used. Options are the same clustering options given in the \code{igraph} package.
#' @param plotUnclusteredNetwork If TRUE, the network is plotted with no clustering displayed. Defaults to TRUE.
#' @param plotClusteredNetwork If TRUE, the network is plotted with clustering displayed (shaded regions for "node" clustering, colored edges for "edge" clustering). Defaults to TRUE.
#' @param plotIndividualClusters If TRUE, each cluster is plotted in a separate graph. Defaults to TRUE.
#' @param plotIndividualClusterFacet If TRUE, each cluster is plotted in a faceted section of a single graph. Defaults to TRUE.
#' @param plotClusterLegend If TRUE, plots a legend with cluster numbers and corresponding colors. Defaults to TRUE.
#' @param edgeColor The color of the edges. Default is "darkgray".
#' @param edgeAlpha The alpha of the edges. Default is 0.5.
#' @param edgeCurve If greater than 0, edges will be curved with a radius corresponding to the value. Default is 0.15. A value of 0 yields straight edges.
#' @param modelNodeColors The color shading for nodes that are predictive words in the provided model. Must be a vector of two values. Defaults to c("lightblue", "orange").
#' @param modelNodeSizeRange The sizing for nodes that are predictive words in the provided model. Must be a vector of two values (the minimum plotted size and maximum plotted size). Defaults to c(5, 10).
#' @param nodeLabelSize The size of the text for node labels. Defaults to 1.
#' @param nodeLabelColor The color of the node labels. Defaults to "black".
#' @param plotTitle The title of the plot(s). If a model is used, it must be a vector of three strings. If not, it must be a single string.
#' @param layout An \code{igraph} layout object, helpful if you want to re-graph the same network multiple times with the same layout
#'
#' @return A list containing the \code{igraph} network object, \code{igraph} layout, and clustering data (if applicable) - this can be used with the \code{plot_cluster} function, or graphed manually with the \code{igraph} package
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
#' plot_word_network(node_edge_table)
#' }

plot_word_network = function(input, model=NULL, topX=100, graphIndividual=TRUE, graphCombined=FALSE, directed=FALSE, removeVerticesBelowDegree = 2, clusterType="none", clusterNodeMethod="infomap", plotUnclusteredNetwork=TRUE, plotClusteredNetwork=TRUE, plotIndividualClusters=TRUE, plotIndividualClusterFacet=TRUE, plotClusterLegend=TRUE, edgeColor="darkgray", edgeAlpha = .5, edgeCurve = .15, modelNodeColors = c("lightblue", "orange"), modelNodeSizeRange = c(5,10), nodeLabelSize=1, nodeLabelColor="black", plotTitle=NULL, layout=NULL) {

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
    else {
      if (is.character(plotTitle)) {
        if (length(plotTitle) != 3) {
          stop("The `plotTitle` argument must include titles for all 3 plots resulting from your model (both outcomes + combined).")
        }
        title0 = plotTitle[1]
        title1 = plotTitle[2]
        title2 = plotTitle[3]
      }
      else {
        title0 = ""
        title1 = ""
        title2 = ""
      }
    }

    if(graphIndividual){
      output1 = make_word_network(subset(input, outcome==model@level0), model=model, topX=topX, cat=0, plotTitle=title0, directed=directed, removeVerticesBelowDegree=removeVerticesBelowDegree, clusterType=clusterType, clusterNodeMethod=clusterNodeMethod, plotUnclusteredNetwork=plotUnclusteredNetwork, plotClusteredNetwork=plotClusteredNetwork, plotIndividualClusters=plotIndividualClusters, plotIndividualClusterFacet=plotIndividualClusterFacet, plotClusterLegend=plotClusterLegend, edgeColor=edgeColor, edgeAlpha=edgeAlpha, edgeCurve=edgeCurve, modelNodeColors=modelNodeColors, modelNodeSizeRange=modelNodeSizeRange, nodeLabelSize=nodeLabelSize, nodeLabelColor=nodeLabelColor, layout=layout[[1]])
      output2 = make_word_network(subset(input, outcome==model@level1), model=model, topX=topX, cat=1, plotTitle=title1, directed=directed, removeVerticesBelowDegree=removeVerticesBelowDegree, clusterType=clusterType, clusterNodeMethod=clusterNodeMethod, plotUnclusteredNetwork=plotUnclusteredNetwork, plotClusteredNetwork=plotClusteredNetwork, plotIndividualClusters=plotIndividualClusters, plotIndividualClusterFacet=plotIndividualClusterFacet, plotClusterLegend=plotClusterLegend, edgeColor=edgeColor, edgeAlpha=edgeAlpha, edgeCurve=edgeCurve, modelNodeColors=modelNodeColors, modelNodeSizeRange=modelNodeSizeRange, nodeLabelSize=nodeLabelSize, nodeLabelColor=nodeLabelColor, layout=layout[[2]])
    }
    if(graphCombined & graphIndividual){
      output3 = make_word_network(subset(input, outcome=="all_outcomes"), model=model, topX=topX, cat=2, plotTitle=title2, directed=directed, removeVerticesBelowDegree=removeVerticesBelowDegree, clusterType=clusterType, clusterNodeMethod=clusterNodeMethod, plotUnclusteredNetwork=plotUnclusteredNetwork, plotClusteredNetwork=plotClusteredNetwork, plotIndividualClusters=plotIndividualClusters, plotIndividualClusterFacet=plotIndividualClusterFacet, plotClusterLegend=plotClusterLegend, edgeColor=edgeColor, edgeAlpha=edgeAlpha, edgeCurve=edgeCurve, modelNodeColors=modelNodeColors, modelNodeSizeRange=modelNodeSizeRange, nodeLabelSize=nodeLabelSize, nodeLabelColor=nodeLabelColor, layout=layout[[3]])
    }
    else if(graphCombined & !graphIndividual){
      output3 = make_word_network(subset(input, outcome=="all_outcomes"), model=model, topX=topX, cat=2, plotTitle=title2, directed=directed, removeVerticesBelowDegree=removeVerticesBelowDegree, clusterType=clusterType, clusterNodeMethod=clusterNodeMethod, plotUnclusteredNetwork=plotUnclusteredNetwork, plotClusteredNetwork=plotClusteredNetwork, plotIndividualClusters=plotIndividualClusters, plotIndividualClusterFacet=plotIndividualClusterFacet, plotClusterLegend=plotClusterLegend, edgeColor=edgeColor, edgeAlpha=edgeAlpha, edgeCurve=edgeCurve, modelNodeColors=modelNodeColors, modelNodeSizeRange=modelNodeSizeRange, nodeLabelSize=nodeLabelSize, nodeLabelColor=nodeLabelColor, layout=layout[[1]])
    }
    if(graphIndividual & graphCombined) {
      invisible(list(output1, output2, output3))
    }
    else if (graphIndividual) {
      invisible(list(output1, output2))
    }
    else if (graphCombined) {
      invisible(output3)
    }
  }
  else if (!is.null(model)) {
    if (is.null(plotTitle)) {
      plot_title = ""
    }
    else if (!is.character(plotTitle)) {
      plot_title = ""
    }
    else {
      if(length(plotTitle) != 1) {
        stop("The `plotTitle` argument can only be a length of 1.")
      }
      plot_title = plotTitle
    }
    if (model@type == "continuous") {
      output = make_word_network(input, model=model, topX=topX, cat=2, plotTitle=plot_title, directed=directed, removeVerticesBelowDegree=removeVerticesBelowDegree, clusterType=clusterType, clusterNodeMethod=clusterNodeMethod, plotUnclusteredNetwork=plotUnclusteredNetwork, plotClusteredNetwork=plotClusteredNetwork, plotIndividualClusters=plotIndividualClusters, plotIndividualClusterFacet=plotIndividualClusterFacet, plotClusterLegend=plotClusterLegend, edgeColor=edgeColor, edgeAlpha=edgeAlpha, edgeCurve=edgeCurve, modelNodeColors=modelNodeColors, modelNodeSizeRange=modelNodeSizeRange, nodeLabelSize=nodeLabelSize, nodeLabelColor=nodeLabelColor, layout=layout)
      invisible(output)
    }
    else {
      warning("You did not provide a model for the creation of the input data - the `model` argument will be ignored.")
      output = make_word_network(input, model=NULL, topX=topX, cat=NULL, plotTitle=plot_title, directed=directed, removeVerticesBelowDegree=removeVerticesBelowDegree, clusterType=clusterType, clusterNodeMethod=clusterNodeMethod, plotUnclusteredNetwork=plotUnclusteredNetwork, plotClusteredNetwork=plotClusteredNetwork, plotIndividualClusters=plotIndividualClusters, plotIndividualClusterFacet=plotIndividualClusterFacet, plotClusterLegend=plotClusterLegend, edgeColor=edgeColor, edgeAlpha=edgeAlpha, edgeCurve=edgeCurve, modelNodeColors=modelNodeColors, modelNodeSizeRange=modelNodeSizeRange, nodeLabelSize=nodeLabelSize, nodeLabelColor=nodeLabelColor, layout=layout)
      invisible(output)
    }
  }
  else {
    if (is.null(plotTitle)) {
      plot_title = ""
    }
    else if (!is.character(plotTitle)) {
      plot_title = ""
    }
    else {
      if(length(plotTitle) != 1) {
        stop("The `plotTitle` argument can only be a length of 1.")
      }
      plot_title = plotTitle
    }
    output = make_word_network(input, model=NULL, topX=topX, cat=NULL, plotTitle=plot_title, directed=directed, removeVerticesBelowDegree=removeVerticesBelowDegree, clusterType=clusterType, clusterNodeMethod=clusterNodeMethod, plotUnclusteredNetwork=plotUnclusteredNetwork, plotClusteredNetwork=plotClusteredNetwork, plotIndividualClusters=plotIndividualClusters, plotIndividualClusterFacet=plotIndividualClusterFacet, plotClusterLegend=plotClusterLegend, edgeColor=edgeColor, edgeAlpha=edgeAlpha, edgeCurve=edgeCurve, modelNodeColors=modelNodeColors, modelNodeSizeRange=modelNodeSizeRange, nodeLabelSize=nodeLabelSize, nodeLabelColor=nodeLabelColor, layout=layout)
    invisible(output)
  }
}

#' @title Plot Individual Cluster
#' @description Plots individual clusters from the \code{plot_word_network} function. Can be helpful if these need to be plotted separately for any reason.
#'
#' @param network_input The output of the \code{plot_word_network} function
#'
#' @importFrom igraph graph.data.frame E<- V<- E V degree delete.vertices delete.edges plot.igraph layout_ with_fr nicely communities
#' @importFrom rlang .data
#' @importFrom grDevices adjustcolor
#' @importFrom graphics par
#'
#' @return
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
#' network_output = plot_word_network(node_edge_table, clusterType="node")
#' plot_cluster(network_output[[1]], cluster_number=1)
#' }

plot_cluster = function(network_input, cluster_number) {
  graphNetwork = network_input$network_object
  layout = network_input$layout_object
  if (!is.null(network_input$cluster_object$membership)) {
    clusterType = "node"
    result = network_input$cluster_object
    num_clusters = max(result$membership)
  }
  else {
    clusterType = "edge"
    clustering_data = network_input$cluster_object
    num_clusters = max(as.numeric(clustering_data$edges$cluster))
  }
  parameters = network_input$parameters
  edgeColor = parameters$edgeColor
  edgeAlpha = parameters$edgeAlpha
  nodeLabelColor = parameters$nodeLabelColor
  nodeLabelSize = parameters$nodeLabelSize

  i = cluster_number

  if (clusterType == "edge") {

    E(graphNetwork)$color = E(graphNetwork)$color1
    E(graphNetwork)$lty = E(graphNetwork)$linetype


    remove_other_edges = E(graphNetwork)[E(graphNetwork)$cluster != as.character(i)]
    graphNetwork_cluster = delete.edges(graphNetwork, remove_other_edges)
    remove_unused_vertices = V(graphNetwork_cluster)[degree(graphNetwork_cluster) < 1]
    graphNetwork_cluster = delete.vertices(graphNetwork_cluster, remove_unused_vertices)

    if (length(V(graphNetwork_cluster)) > 1) {
      layout_frame = data.frame(rownumber=seq(1,length(V(graphNetwork)$name)), name=V(graphNetwork)$name)
      nodes_to_keep = subset(clustering_data$nodeclusters, cluster==i)
      layout_frame = subset(layout_frame, name %in% nodes_to_keep$node)
      layout_cluster = layout[layout_frame$rownumber,]
    }
    else {
      layout_cluster = layout_(graphNetwork_cluster, nicely())
    }

    plot(
      graphNetwork_cluster,
      layout = layout_cluster,
      # main = paste("Cluster", i),
      vertex.label.family = "sans",
      vertex.shape = "circle",
      vertex.frame.color = adjustcolor(edgeColor, alpha.f = edgeAlpha), # have node borders match the edges
      vertex.label.color = nodeLabelColor,     # Color of node names
      vertex.label.font = 2,            # Font of node names
      vertex.label = V(graphNetwork_cluster)$name,
      vertex.label.cex = nodeLabelSize # font size of node names
    )
    title(paste("Cluster", i),cex.main=1,col.main=hue_pal()(num_clusters)[i])

  }
  else if (clusterType == "node") {

    remove_other_vertices = V(graphNetwork)[V(graphNetwork)$cluster != i]
    graphNetwork_cluster = delete.vertices(graphNetwork, remove_other_vertices)

    if (length(V(graphNetwork_cluster)) > 1) {
      layout_frame = data.frame(rownumber=seq(1,length(V(graphNetwork)$cluster)), cluster=V(graphNetwork)$cluster)
      layout_frame = subset(layout_frame, cluster==i)
      layout_cluster = layout[layout_frame$rownumber,]
    }
    else {
      layout_cluster = layout_(graphNetwork_cluster, nicely())
    }

    plot(
      graphNetwork_cluster,
      layout = layout_cluster,
      # main = ,
      vertex.label.family = "sans",
      vertex.shape = "circle",
      vertex.frame.color = adjustcolor(edgeColor, alpha.f = edgeAlpha), # have node borders match the edges
      vertex.label.color = nodeLabelColor,     # Color of node names
      vertex.label.font = 2,            # Font of node names
      vertex.label = V(graphNetwork_cluster)$name,
      vertex.label.cex = nodeLabelSize, # font size of node names
      mark.groups = communities(result)[i],
      mark.col =  rainbow(num_clusters, alpha = 0.3)[i],
      mark.border = rainbow(num_clusters, alpha = 1)[i]
    )
    title(paste("Cluster", i),cex.main=1,col.main=rainbow(num_clusters, alpha=1)[i])
  }
}
