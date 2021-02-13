#' @title Clean Input Text
#'
#' @description This function cleans text by: \cr
#' -Setting all text to lowercase \cr
#' -Removing non-ASCII characters \cr
#' -Expanding contractions ("don't" --> "do not") \cr
#' -Removing punctuation \cr
#' -Removing symbols (if replaceSymbol is FALSE) \cr
#' -Removing numbers (if replaceNumber is FALSE)
#'
#' @param inputText A character string or vector of character strings
#' @param replaceSymbol If TRUE, symbols are replaced with their equivalent (e.g. "@" becomes "at"). Defaults to FALSE.
#' @param replaceNumber If TRUE, numbers are replaced with their equivalent (e.g. "20" becomes "twenty", "3rd" becomes "third"). Defaults to FALSE.
#'
#' @return A character string (or vector of character strings) with cleaned text.
#'
#' @import textclean
#'
#' @export
#'
#' @examples
#' myString = "He gave his last $10 to Sally's sister because she's nice."
#'
#' cleanText = clean_text(myString)
#' # "he gave his last to sally sister because she is nice"
#'
#' cleanText = clean_text(myString, replaceNumber = TRUE)
#' # "he gave his last ten to sally sister because she is nice"
#'
#' cleanText = clean_text(myString, replaceSymbol = TRUE)
#' # "he gave his last dollar to sally sister because she is nice"



clean_text = function(inputText, replaceSymbol=FALSE, replaceNumber=FALSE) {
  if (!is.character(inputText)) {
    stop("The inputText argument needs to be a character string.")
  }

  new_text = replace_contraction(inputText)
  new_text = replace_non_ascii(new_text)
  if (replaceSymbol) {
    new_text = replace_symbol(new_text)
  }

  new_text = gsub("[[:punct:]]", " ", new_text)

  if (replaceNumber) {
    new_text = replace_ordinal(new_text)
    new_text = replace_number(new_text)
  }
  else {
    new_text = gsub("[0-9](st|nd|rd|th)", " ", new_text)
    new_text = gsub("[0-9]", "", new_text)
  }
  new_text = gsub("\\s+", " ", new_text)
  new_text = gsub("^\\s+|\\s+$", "", new_text)

  # messing with the text itself can sometimes leave isolated instances of "s" (probably from possessives)
  new_text = gsub(" s ", " ", new_text)
  new_text = gsub("^s ", "", new_text)
  new_text = gsub(" s$", "", new_text)
  new_text = tolower(new_text)
  return(new_text)
}
