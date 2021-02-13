#' @title Idiosyncratic Response Words
#'
#' @description This function identifies response words that are idiosyncratic (i.e. appear multiple times in a single response, and not in any other responses). It can also be used to remove these words.
#'
#' @param inputDataframe A dataframe containing a column with text data (character strings)
#' @param mode This defines the mode of operation. Options include "output", "remove", or "both". See Details below.
#' @param textColumnName A string consisting of the name of the column in \code{inputDataframe} which contains text data
#' @param participantColumnName (Optional argument) A string consisting of the name of the column in \code{inputDataframe} which contains participant IDs
#'
#' @return A dataframe (\code{mode="output"}), a character string or vector of character strings (\code{mode="remove"}), or a two-object list containing both results (\code{mode="both"})
#'
#' @seealso \code{\link{idiosync_participant_words}}
#'
#' @importFrom tm removeWords
#' @importFrom rlang .data
#'
#'
#' @export
#'
#' @examples
#' myStrings = c("I like going to the park. The park is one of my favorite places to visit.",
#'               "Today is really rainy, but I'm a fan of this kind of weather to be honest.",
#'               "Yesterday, a bright red car with shiny red wheels drove past the house.")
#' mydataframe = data.frame(text=myStrings, stringsAsFactors = FALSE)
#' idiosync_output = idiosync_response_words(mydataframe, textColumnName = "text", mode = "output")
#' idiosync_output
#' # response_number     feature       frequency
#' # 1                   park          2
#' # 3                   red           2
#'
#' idiosync_removed = idiosync_response_words(mydataframe, textColumnName = "text", mode = "remove")
#' idiosync_removed
#' # "I like going to the. The is one of my favorite places to visit."
#' # "Today is really rainy, but I'm a fan of this kind of weather to be honest."
#' # "Yesterday, a bright car with shiny wheels drove past the house."
#'
#' @details
#' This function has three modes:
#' In the "output" mode, a dataframe is produced with three columns: the response with idiosyncratic words, the words, and how frequently they appear in that response. If a \code{participantColumnName} is provided, a fourth column with participant IDs is included.
#' In the "remove" mode, a character string (or vector of character strings) is produced, where all of the idiosyncratic words are removed.
#' In the "both" mode, both of the above results will be produced (i.e. a list containing a dataframe of idiosyncratic words, as well as the text with those words removed)

idiosync_response_words = function(inputDataframe, mode, textColumnName, participantColumnName) {

  rank=docfreq=group=participant=response_number=feature=frequency=NULL

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

  if(!missing(participantColumnName)) {
    response_frequent_words = data.frame(matrix(nrow=0,ncol=4))
    colnames(response_frequent_words) = c("participant", "response_number", "feature", "frequency")
    participants = unique(inputDataframe[[participantColumnName]])
    for (i in 1:length(participants)) {
      participant_data = subset(inputDataframe, eval(parse(text=participantColumnName)) == participants[i])
      for (j in 1:nrow(participant_data)) {
        response_data = participant_data[j,]
        corpus = corpus(response_data[[textColumnName]])
        tokens = corpus %>% quanteda::tokens(remove_punct = T, remove_numbers = T,
                                             remove_symbols = T,ngrams=1,concatenator = " ") %>% tokens_tolower()
        dfm<-dfm(tokens) %>% dfm_weight(scheme='count')
        savetxtstt_response = textstat_frequency(dfm)
        if (nrow(savetxtstt_response) == 0) {
          next
        }
        savetxtstt_response$participant = participants[i]
        savetxtstt_response$response_number = j
        savetxtstt_response = subset(savetxtstt_response, select = -c(rank,docfreq,group))
        savetxtstt_response = subset(savetxtstt_response, select = c(participant, response_number, feature, frequency))
        response_frequent_words = rbind(response_frequent_words, savetxtstt_response)
      }
    }
  }
  else {
    response_frequent_words = data.frame(matrix(nrow=0,ncol=3))
    colnames(response_frequent_words) = c("response_number", "feature", "frequency")
    for (j in 1:nrow(inputDataframe)) {
      response_data = inputDataframe[j,]
      if (is.data.frame(response_data)) {
        corpus = corpus(response_data[[textColumnName]])
      }
      else {
        corpus = corpus(response_data)
      }
      tokens = corpus %>% quanteda::tokens(remove_punct = T, remove_numbers = T,
                                           remove_symbols = T) %>% tokens_tolower()
      dfm<-dfm(tokens) %>% dfm_weight(scheme='count')
      savetxtstt_response = textstat_frequency(dfm)
      if (nrow(savetxtstt_response) == 0) {
        next
      }
      savetxtstt_response$response_number = j
      savetxtstt_response = subset(savetxtstt_response, select = -c(rank,docfreq,group))
      savetxtstt_response = subset(savetxtstt_response, select = c(response_number, feature, frequency))
      response_frequent_words = rbind(response_frequent_words, savetxtstt_response)
    }
  }

  response_frequent_words <- response_frequent_words %>%
    group_by(feature) %>%
    filter(n()<2)
  response_frequent_words = subset(response_frequent_words, frequency > 1)

  idiosync_free_data = removeWords(inputDataframe[[textColumnName]], response_frequent_words$feature)
  idiosync_free_data = gsub("\\s+", " ", idiosync_free_data)
  idiosync_free_data = gsub("^\\s+|\\s+$", "", idiosync_free_data)
  idiosync_free_data = gsub("\\s+\\.", ".", idiosync_free_data)
  idiosync_free_data = gsub("\\s+\\,", ",", idiosync_free_data)
  idiosync_free_data = gsub("\\s+\\:", ":", idiosync_free_data)
  idiosync_free_data = gsub("\\s+\\;", ";", idiosync_free_data)
  idiosync_free_data = gsub("\\s+\\?", "?", idiosync_free_data)
  idiosync_free_data = gsub("\\s+\\!", "!", idiosync_free_data)

  if (mode == "output") {
    return(response_frequent_words)
  }
  if (mode == "remove") {
    return(idiosync_free_data)
  }
  if (mode == "both") {
    return(list(response_frequent_words, idiosync_free_data))
  }
}


#' @title Idiosyncratic Participant Words
#'
#' @description This function identifies participants' words that are idiosyncratic (i.e. used multiple times by a single participant, and never by any other participant). It can also be used to remove these words.
#'
#' @param inputDataframe A dataframe containing a column with text data (character strings) and participant IDs
#' @param mode This defines the mode of operation. Options include "output", "remove", or "both". See Details below.
#' @param textColumnName A string consisting of the name of the column in \code{inputDataframe} which contains text data
#' @param participantColumnName A string consisting of the name of the column in \code{inputDataframe} which contains participant IDs
#'
#' @return A dataframe (\code{mode="output"}), a character string or vector of character strings (\code{mode="remove"}), or a two-object list containing both results (\code{mode="both"})
#'
#' @seealso \code{\link{idiosync_response_words}}
#'
#' @importFrom tm removeWords
#' @importFrom rlang .data
#'
#'
#' @export
#'
#' @examples
#' myStrings = c("Last week while I was walking in the park, I saw a firetruck go by. It was red.",
#'               "My dog loves to go on walks in the park every day of the week.",
#'               "Where I live, it snows all winter long. It's so cold outside.",
#'               "My kids love to play in the snow. They love to collect snow to build snowmen.",
#'               "When I was younger, I used to visit my grandmother every week.",
#'               "In the summertime, we would get together with my grandmother to bake cookies.")
#' mydataframe = data.frame(text=myStrings, participant=c(1,1,2,2,3,3), stringsAsFactors = FALSE)
#' idiosync_output = idiosync_participant_words(mydataframe, "output", "text", "participant")
#' idiosync_output
#' # participant     feature       frequency
#' # 1                park          2
#' # 1                go            2
#' # 2                love          2
#' # 2                snow          2
#' # 3                grandmother   2
#'
#' idiosync_removed = idiosync_participant_words(mydataframe, "remove", "text", "participant")
#' idiosync_removed
#' # "Last week while I was walking in the, I saw a firetruck by. It was red."
#' # "My dog loves to on walks in the every day of the week."
#' # "Where I live, it snows all winter long. It's so cold outside."
#' # "My kids to play in the. They to collect to build snowmen."
#' # "When I was younger, I used to visit my every week."
#' # "In the summertime, we would get together with my to bake cookies."
#'
#'
#' @details
#' This function has three modes:
#' In the "output" mode, a dataframe is produced with three columns: the participant who produced the idiosyncratic words, the words, and how frequently they are used by the participant.
#' In the "remove" mode, a character string (or vector of character strings) is produced, where all of the idiosyncratic words are removed.
#' In the "both" mode, both of the above results will be produced (i.e. a list containing a dataframe of idiosyncratic words, as well as the text with those words removed)

idiosync_participant_words = function(inputDataframe, mode, textColumnName, participantColumnName) {

  group=participant=feature=frequency=NULL

  if (!is.data.frame(inputDataframe)) {
    stop("The inputDataframe argument must be a dataframe.")
  }
  if (!is.character(inputDataframe[[textColumnName]])) {
    stop("The textColumnName argument must specify a column of character strings in your dataframe.")
  }
  if (!is.factor(inputDataframe[[participantColumnName]])) {
    inputDataframe[[participantColumnName]] = as.factor(inputDataframe[[participantColumnName]])
  }

  participant_frequent_words = data.frame(matrix(nrow=0,ncol=3))
  colnames(participant_frequent_words) = c("participant", "feature", "frequency")
  participants = unique(inputDataframe[[participantColumnName]])
  for (i in 1:length(participants)) {
    participant_data = subset(inputDataframe, eval(parse(text=participantColumnName)) == participants[i])
    corpus = corpus(participant_data[[textColumnName]])
    tokens = corpus %>% quanteda::tokens(remove_punct = T, remove_numbers = T,
                                         remove_symbols = T) %>% tokens_tolower()
    dfm<-dfm(tokens) %>% dfm_weight(scheme='count')
    savetxtstt_participant = textstat_frequency(dfm)
    if (nrow(savetxtstt_participant) == 0) {
      next
    }
    savetxtstt_participant$participant = participants[i]
    savetxtstt_participant = subset(savetxtstt_participant, select = -c(rank,docfreq,group))
    savetxtstt_participant = subset(savetxtstt_participant, select = c(participant,feature,frequency))
    participant_frequent_words = rbind(participant_frequent_words, savetxtstt_participant)
  }

  participant_frequent_words <- participant_frequent_words %>%
    group_by(feature) %>%
    filter(n()<2)
  participant_frequent_words = subset(participant_frequent_words, frequency > 1)

  idiosync_free_data = removeWords(inputDataframe[[textColumnName]], participant_frequent_words$feature)
  idiosync_free_data = gsub("\\s+", " ", idiosync_free_data)
  idiosync_free_data = gsub("^\\s+|\\s+$", "", idiosync_free_data)
  idiosync_free_data = gsub("\\s+\\.", ".", idiosync_free_data)
  idiosync_free_data = gsub("\\s+\\,", ",", idiosync_free_data)
  idiosync_free_data = gsub("\\s+\\:", ":", idiosync_free_data)
  idiosync_free_data = gsub("\\s+\\;", ";", idiosync_free_data)
  idiosync_free_data = gsub("\\s+\\?", "?", idiosync_free_data)
  idiosync_free_data = gsub("\\s+\\!", "!", idiosync_free_data)

  if (mode == "output") {
    return(participant_frequent_words)
  }
  if (mode == "remove") {
    return(idiosync_free_data)
  }
  if (mode == "both") {
    return(list(participant_frequent_words, idiosync_free_data))
  }
}
