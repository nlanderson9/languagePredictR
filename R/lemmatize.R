#' @title Lemmatize Text
#'
#' @description This function performs lemmatization on input text by reducing words to their base units.
#'
#' @param inputText A character string or vector of character strings
#' @param method Either 'direct' (which uses a predefined list of words and their lemmas) or 'treetagger' (which uses the software \code{TreeTagger}, implemented through the \code{koRpus} package)
#' @param treetaggerDirectory the filepath to the location of your installation of the \code{treetagger} library (See Details below)
#' @param progressBar Show a progress bar. Defaults to TRUE.
#'
#' @return A dataframe with lemmatized text, as well as columns with information about parts of speech
#'
#' @seealso the \code{\link[koRpus]{treetag}} function from the \code{koRpus} package, as well as the treetagger documentation: \url{https://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/}
#'
#' @import koRpus.lang.en
#' @importFrom koRpus treetag
#' @importFrom quanteda corpus tokens tokens_replace
#' @importFrom pbapply pblapply
#' @importFrom rlang .data
#'
#'
#' @export
#'
#' @examples
#' myStrings = c("I walked in the park with both of my dogs.",
#' "The largest geese ran very fast.")
#' \dontrun{
#' lemmatized_data = lemmatize(myStrings, "~/path/to/TreeTagger")
#' lemmatized_data$lemma_text}
#' # "I walk in the park with both of my dog."
#' # "The large goose run very fast."
#'
#' @details
#' This function is essentially a wrapper for the \code{\link[koRpus]{treetag}} function from the \code{[koRpus]} package.
#' In turn, koRpus implements the TreeTagger software package (available here: \url{https://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/}).
#' The software must be downloaded and installed on your local computer in order to use the \code{lemmatize} function.
#' Once installed, the \code{treetaggerDirectory} argument should consist of the path where the software was installed.
#'
#' This function performs "lemmatization," which is one form of reducing words to their most basic units. It is more thorough than "stemming," which only removes suffixes.
#' E.g. for the words "walked" and "dogs," both lemmatization and stemming would reduce the words to "walk" and "dog."
#' However, stemming would ignore "ran" and "geese," while lemmatization would properly render these "run" and "goose."

lemmatize = function(inputText, method="direct", treetaggerDirectory, progressBar=TRUE) {
  if (!is.character(inputText)) {
    stop("The inputText argument needs to be a character string.")
  }

  if(!(method %in% c("direct", "treetagger"))) {
    stop("The argument `method` must either be 'direct' or 'treetagger'.")
  }

  if (!dir.exists(treetaggerDirectory)) {
    stop("You must provide a valid directory path pointing to your installation of TreeTagger. If you haven't installed TreeTagger yet, you can find instructions here: https://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/")
  }

  if (!is.logical(progressBar)) {
    stop("The progressBar argument can only be TRUE or FALSE.")
  }
  if (method == "treetagger") {
    output_dataframe = data.frame(original_text = inputText, stringsAsFactors = FALSE)
    if (progressBar) {
      tagged = pblapply(inputText, function(x) {suppressWarnings(treetag(x, treetagger = "manual", format = "obj", TT.tknz=TRUE, lang="en", encoding="UTF-8", TT.options = list(path=treetaggerDirectory, preset="en")))})
    }
    else {
      tagged = lapply(inputText, function(x) {suppressWarnings(treetag(x, treetagger = "manual", format = "obj", TT.tknz=TRUE, lang="en", encoding="UTF-8", TT.options = list(path=treetaggerDirectory, preset="en")))})
    }
    POS = lapply(tagged, function(x) {x@tokens$wclass})

    names(tagged) = seq(1,length(tagged), b=1)
    correct_lemma = lapply(seq_along(tagged), function(i) {ifelse(tagged[[i]]@tokens$lemma=="<unknown>", tagged[[i]]@tokens$token, tagged[[i]]@tokens$lemma)})
    correct_lemma_strings = lapply(correct_lemma, function(x) {paste(x, collapse = " ")})
    output_dataframe$lemma_text = as.character(unlist(correct_lemma_strings))

    output_dataframe$word_count = unlist(lapply(inputText, function(x) {sapply(strsplit(x, " "), length)}))
    output_dataframe$noun_count = unlist(lapply(POS, function(x) {table(x)["noun"]}))
    output_dataframe$noun_proportion = round(output_dataframe$noun_count / output_dataframe$word_count, 3)
    output_dataframe$verb_count = unlist(lapply(POS, function(x) {table(x)["verb"]}))
    output_dataframe$verb_proportion = round(output_dataframe$verb_count / output_dataframe$word_count, 3)
    output_dataframe$adjective_count = unlist(lapply(POS, function(x) {table(x)["adjective"]}))
    output_dataframe$adjective_proportion = round(output_dataframe$adjective_count / output_dataframe$word_count, 3)

    output_dataframe$lemma_text = gsub("\\s+", " ", output_dataframe$lemma_text)
    output_dataframe$lemma_text = gsub("^\\s+|\\s+$", "", output_dataframe$lemma_text)
    output_dataframe$lemma_text = gsub("\\s+\\.", ".", output_dataframe$lemma_text)
    output_dataframe$lemma_text = gsub("\\s+\\,", ",", output_dataframe$lemma_text)
    output_dataframe$lemma_text = gsub("\\s+\\:", ":", output_dataframe$lemma_text)
    output_dataframe$lemma_text = gsub("\\s+\\;", ";", output_dataframe$lemma_text)
    output_dataframe$lemma_text = gsub("\\s+\\?", "?", output_dataframe$lemma_text)
    output_dataframe$lemma_text = gsub("\\s+\\!", "!", output_dataframe$lemma_text)

    return(output_dataframe)
  }
  else if (method == "direct") {
    lemma_data = data("lemma_data")
    lemma_text = inputText %>% corpus() %>% quanteda::tokens() %>% tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% as.list()
    lemma_text = lapply(token_text, function(x) paste(x, collapse = " "))

    return(lemma_text)
  }

}
