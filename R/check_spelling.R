#' @title Check Spelling
#'
#' @description This function performs spell-checking on input text. It can provide a list of errors and probable correct spellings, as well as returning the input text with all errors corrected.
#'
#' @param inputText A character string or vector of character strings
#' @param mode This defines the mode of operation. Options include "output", "replace", or "both". See Details below.
#' @param customSpellingList (Optional argument) If provided, the function will use this list to correct spelling errors. Must be in the same format as the result of "output" mode, and only works in "replace" mode.
#'
#' @return A dataframe (\code{mode="output"}), a character string or vector of character strings (\code{mode="replace"}), or a two-object list containing both results (\code{mode="both"})
#'
#'
#' @import ggplot2
#' @import dplyr
#' @import stringr
#' @importFrom hunspell hunspell hunspell_suggest
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' myString = "I went to the stroe and bought some egggs for a good porce!"
#'
#' spell_check_results = check_spelling(myString, mode = "output")
#' spell_check_results
#' # error    freq    suggested_correction
#' # egggs    1       eggs
#' # stroe    1       store
#' # porce    1       pore
#'
#' spell_correction_results = check_spelling(myString, mode = "replace")
#' spell_correction_results
#' # "I went to the store and bought some eggs for a good pore!"
#'
#' error = c("egggs", "stroe", "porce")
#' suggested_correction = c("eggs", "store", "price")
#' my_corrections = data.frame(error=error, suggested_correction = suggested_correction)
#' correction_results = check_spelling(myString, mode = "replace", customSpellingList = my_corrections)
#' correction_results
#' # "I went to the store and bought some eggs for a good price!"
#'
#' @details
#' This function has three modes:
#' In the "output" mode, a dataframe is produced with three columns: the spelling errors present in the input text, how frequently they appear, and the most likely correct spelling for each word. A frequency graph is also plotted.
#' In the "replace" mode, a character string (or vector of character strings) is produced, where all of the spelling errors identified are replaced by their most likely correct spelling.
#' When \code{customSpellingList = TRUE}, the "replace" mode will only correct words in the provided list
#' In the "both" mode, both of the above results will be produced (i.e. a list containing a dataframe of errors and suggestions, as well as the text with corrected spellings)
#'
#' As a warning, this function is particularly slow, and make take a significantly long time on a sizeable vector of character strings.


check_spelling = function(inputText, mode, customSpellingList) {
  if (!is.character(inputText)) {
    stop("The inputText argument needs to be a character string.")
  }

  if (!(mode %in% c("output", "replace", "both"))) {
    stop("The mode argument can only have a value of 'output', 'replace', or 'both'.")
  }

  if (!missing(customSpellingList) & mode != "replace") {
    stop("You can only provide a customSpellingList argument if mode='replace'.")
  }

  if (!missing(customSpellingList)) {
    if (!("error" %in% colnames(customSpellingList))) {
      stop("Your customSpellingList must have an 'error' column with misspelled words you wish to correct.")
    }
    if (!("suggested_correction" %in% colnames(customSpellingList))) {
      stop("Your customSpellingList must have a 'suggested_correction' column with corrections for misspelled words.")
    }
  }

  if (mode == "output" | mode == "both" | (mode == "replace" & missing(customSpellingList))) {
    full_text = paste(unlist(inputText), collapse = " ")
    bad = hunspell(full_text)

    spelling = data.frame(table(bad[[1]]))
    colnames(spelling) = c("error", "freq")

    correct = hunspell_suggest(as.character(spelling$error))

    correct = as.data.frame(sapply(correct, "[", 1))
    spelling$suggested_correction = as.character(correct[[1]])
    spelling$suggested_correction = coalesce(spelling$suggested_correction, spelling$error)
  }

  if (!missing(customSpellingList)){
    spelling = customSpellingList
  }

  if (mode == "replace" | mode == "both") {
    spelling_for_replace = spelling
    spelling_for_replace$suggested_correction = coalesce(spelling_for_replace$suggested_correction, spelling_for_replace$error)
    errors = str_c("\\b", as.character(spelling_for_replace$error), "\\b", collapse="|")
    correct_spelling = function(error) {
      spelling_for_replace$suggested_correction[which(spelling_for_replace$error == error)]
    }
    correction_output = str_replace_all(inputText, errors, correct_spelling)

    correction_output = gsub("\\s+", " ", correction_output)
    correction_output = gsub("^\\s+|\\s+$", "", correction_output)
  }

  if (mode == "output" | mode == "both") {
    if (nrow(spelling) > 30) {
      length_spelling = nrow(spelling)
      spelling_plot = spelling %>% slice_max(.data$freq, n = 30)
      spelling_title = "Top 30 most misspelled words"
    }
    else {
      spelling_plot = spelling
      spelling_title = "Misspelled words"
    }
    spelling_plot = spelling_plot %>% arrange(desc(.data$error))
    spelling_plot = spelling_plot %>% arrange(.data$freq)
    spelling_plot$error = factor(spelling_plot$error, levels = spelling_plot$error)

    plot(ggplot(spelling_plot, aes(x=.data$error, y=.data$freq)) +
           geom_bar(stat="identity") +
           geom_hline(yintercept = 0) +
           coord_flip() +
           ylab("Freq of Misspelled Words") +
           ggtitle(spelling_title) +
           theme_bw() +
           theme(panel.grid.major.y = element_blank(),
                 panel.grid.minor.y = element_blank()))
  }
  if (mode == "output") {
    return(spelling)
  }
  if (mode == "replace") {
    return(correction_output)
  }
  if (mode == "both") {
    return(list(spelling, correction_output))
  }
}
