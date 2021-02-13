#' @title Overview Plots
#'
#' @description This function creates a set of plots showing basic information about a corpus of text data.
#'
#' @param inputDataframe A dataframe containing a column with text data (character strings)
#' @param textColumnName A string consisting of the name of the column in \code{inputDataframe} which contains text data
#' @param participantColumnName (Optional argument) A string consisting of the name of the column in \code{inputDataframe} which contains participant IDs
#'
#' @return Nothing (this function plots a series of graphs)
#'
#' @import ggplot2
#' @import quanteda
#' @importFrom stats sd
#' @importFrom tibble enframe
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' overview_plots(movie_review_data1, "text")
#'}
#'
#' @details
#' If a \code{participantColumnName} is not provided, three graphs will be produced:
#' -A pie chart with the total number of words in the provided corpus, divided into "Unique" words (those only used once), and "Repeated" words (those used at least twice).
#' -A density plot with the length of each individual response in the corpus (in words)
#' -A bar plot of the 25 most common words in the corpus, arranged by frequency
#'
#' If a \code{participantColumnName} is provided, an additional two graphs will be produced:
#' -The average number of words per response, plotted by participant (the overall average will be displayed as a vertical line)
#' -The total number of words produced by each participant, compared to the total number of unique words produced by each participant


overview_plots = function(inputDataframe, textColumnName, participantColumnName) {
  if (!is.data.frame(inputDataframe)) {
    stop("The inputDataframe argument must be a dataframe.")
  }
  if (!is.character(inputDataframe[[textColumnName]])) {
    stop("The textColumnName argument must specify a column of character strings in your dataframe.")
  }
  if (!missing(participantColumnName)) {
    if (!is.factor(inputDataframe[[participantColumnName]])) {
      inputDataframe[[participantColumnName]] = as.factor(inputDataframe[[participantColumnName]])
    }
  }

  alltokens = sum(ntoken(inputDataframe[[textColumnName]]))
  uniquetokens = ntype(paste(unlist(inputDataframe[[textColumnName]]), collapse = " "))

  tokendata = data.frame(Unique = uniquetokens, Repeated = alltokens-uniquetokens)
  tokendata = data.frame(Value = c(uniquetokens, alltokens-uniquetokens), Group = c("Unique", "Repeated"))

  plot(ggplot(tokendata, aes(x="", y=.data$Value, fill=.data$Group)) +
         geom_bar(stat="identity", width=1, color= "white") +
         coord_polar("y", start=0) +
         theme_void() +
         theme(legend.position = "none") +
         # geom_text(aes(y = ypos, label = Group), color = "white", size = 6)
         geom_text(aes(y = .data$Value/2 + c(0, cumsum(.data$Value)[-length(.data$Value)]),
                       label = paste(.data$Group,":\n",prettyNum(.data$Value, big.mark=",", scientific=FALSE)," words", sep="")), size=5))

  response_lengths = ntoken(inputDataframe[[textColumnName]])
  inputDataframe$response_lengths = response_lengths
  plot(ggplot(inputDataframe, aes(x=response_lengths)) +
         geom_density(fill="black") +
         xlab("Response lengths (in words)") +
         theme_bw())

  corpus1 = corpus(inputDataframe[[textColumnName]])

  tokens1<- corpus1 %>% quanteda::tokens(remove_punct = T, remove_numbers = T,
                                         remove_symbols = T,ngrams=1,concatenator = " ") %>% tokens_tolower()

  dfm1<-dfm(tokens1) %>% dfm_weight(scheme='count')


  savetxtstt = textstat_frequency(dfm1)
  savetxtstt = savetxtstt[1:25,]
  alldfmtokens = sum(ntoken(dfm1))
  savetxtstt$prop = savetxtstt$frequency / alldfmtokens
  savetxtstt$feature = factor(savetxtstt$feature, levels = rev(c(savetxtstt$feature)))

  plot(ggplot(savetxtstt, aes(x=.data$feature, y=.data$prop)) +
         geom_bar(stat="identity") +
         geom_label(aes(label=.data$feature), nudge_y = .001, hjust = 0) +
         geom_hline(yintercept = 0) +
         scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, ceiling(savetxtstt$prop[1]*100)/100 + .03)) +
         coord_flip() +
         ylab("Frequency of 25 Most Common Words in Dataset") +
         theme_bw() +
         theme(axis.text.y = element_blank(),
               axis.ticks.y = element_blank(),
               axis.title.y = element_blank(),
               panel.grid.major.y = element_blank(),
               panel.grid.minor.y = element_blank()))

  if (!missing(participantColumnName)) {
    avg_response_length = mean(inputDataframe$response_lengths)
    sd_response_length = sd(inputDataframe$response_lengths)

    uniques = inputDataframe %>%
      group_by(!!as.name(participantColumnName)) %>%
      group_map( ~ paste(unlist(.x[[textColumnName]]), collapse = " "))

    uniques = enframe(uniques)
    uniques$name = unique(inputDataframe[[participantColumnName]])
    uniques$count = lapply(uniques$value, .data$ntype)
    uniques$totalcount = lapply(uniques$value, .data$ntoken)
    fixlist = function(x) x[[1]]
    uniques$count = lapply(uniques$count, FUN=fixlist)
    uniques$totalcount = lapply(uniques$totalcount, FUN=fixlist)

    inputDataframe$participant_unique_count = NA
    inputDataframe$participant_total_count = NA
    for (i in 1:nrow(inputDataframe)) {
      inputDataframe$participant_unique_count[i] = uniques$count[which(uniques$name == inputDataframe[[participantColumnName]][i])]
      inputDataframe$participant_total_count[i] = uniques$totalcount[which(uniques$name == inputDataframe[[participantColumnName]][i])]
    }

    inputDataframe$participant_unique_count = as.numeric(inputDataframe$participant_unique_count)
    inputDataframe$participant_total_count = as.numeric(inputDataframe$participant_total_count)

    inputDataframe[[participantColumnName]] = factor(inputDataframe[[participantColumnName]], levels = rev(levels(inputDataframe[[participantColumnName]])))

    plot(ggplot(inputDataframe) +
           geom_jitter(aes_string(x=participantColumnName, y=response_lengths), width=.1) +
           geom_hline(yintercept = avg_response_length, color = "black") +
           xlab("Participant Number") +
           ylab("Number of Words Per Response") +
           coord_flip() +
           theme_bw())

    uniques$count = unlist(uniques$count)
    uniques$totalcount = unlist(uniques$totalcount)
    uniques$value = unlist(uniques$value)
    uniques1="NA"
    uniques1 = uniques %>%
      select(-.data$value) %>%
      pivot_longer(!.data$name, names_to = "category", values_to = "count")

    uniques1$category = factor(uniques1$category, levels = c("totalcount", "count"))
    uniques1$name = factor(uniques1$name, levels = rev(levels(uniques1$name)))

    plot(ggplot(uniques1, aes(x=.data$name, y=.data$count, color=.data$category, group=.data$name)) +
           geom_line(color="gray") +
           geom_point() +
           scale_color_manual(values = c("black", "red"), labels = c("Total\nwords\nproduced\n", "\nTotal\nunique\nwords\nproduced")) +
           xlab("Participant Number") +
           coord_flip() +
           theme_bw())
  }
}
