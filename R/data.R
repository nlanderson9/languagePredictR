#' Movie Reviews from IMDB
#'
#' A dataset containing the text and ratings of 2000 movie reviews from IMDB. 1000 are negative (rating 1-4) and 1000 are positive (rating 7-10).
#'
#' @docType data
#'
#' @format A data frame with 2000 rows and 3 variables:
#' \describe{
#'   \item{text}{text of the user's movie review}
#'   \item{valence}{valence of the user's movie review, \code{Positive} (rating 1-4) or \code{Negative} (rating 7-10)}
#'   \item{rating}{user's rating of the movie, on a scale of 1-10}
#' }
#' @source \url{http://ai.stanford.edu/~amaas/data/sentiment/}
"movie_review_data1"


#' Movie Reviews from IMDB
#'
#' A dataset containing the text and ratings of 2000 movie reviews from IMDB. 1000 are negative (rating 1-4) and 1000 are positive (rating 7-10).
#'
#' @docType data
#'
#' @format A data frame with 2000 rows and 3 variables:
#' \describe{
#'   \item{text}{text of the user's movie review}
#'   \item{valence}{valence of the user's movie review, \code{Positive} (rating 1-4) or \code{Negative} (rating 7-10)}
#'   \item{rating}{user's rating of the movie, on a scale of 1-10}
#' }
#' @source \url{http://ai.stanford.edu/~amaas/data/sentiment/}
"movie_review_data2"


#' Strong Movie Reviews from IMDB
#'
#' A dataset containing the text and ratings of 2000 movie reviews from IMDB. 1000 are strongly negative (rating of 1) and 1000 are strongly positive (rating of 10).
#'
#' @docType data
#'
#' @format A data frame with 2000 rows and 3 variables:
#' \describe{
#'   \item{text}{text of the user's movie review}
#'   \item{valence}{valence of the user's movie review, \code{Positive} (rating of 1) or \code{Negative} (rating of 10)}
#'   \item{rating}{user's rating of the movie, on a scale of 1-10}
#' }
#'
#' @source \url{http://ai.stanford.edu/~amaas/data/sentiment/}
"strong_movie_review_data"


#' Mild Movie Reviews from IMDB
#'
#' A dataset containing the text and ratings of 2000 movie reviews from IMDB. 1000 are mildly negative (rating of 4) and 1000 are mildly positive (rating of 7).
#'
#' @docType data
#'
#' @format A data frame with 2000 rows and 3 variables:
#' \describe{
#'   \item{text}{text of the user's movie review}
#'   \item{valence}{valence of the user's movie review, \code{Positive} (rating of 4) or \code{Negative} (rating of 7)}
#'   \item{rating}{user's rating of the movie, on a scale of 1-10}
#' }
#' @source \url{http://ai.stanford.edu/~amaas/data/sentiment/}
"mild_movie_review_data"


#' Lemma Data
#'
#' A dataset containing inflected and base forms of text, used for the \code{lemmatize} function.
#'
#' @docType data
#'
#' @format A data frame with 47,366 rows and 2 variables:
#' \describe{
#'   \item{inflected_form}{the original, inflected word (e.g. 'dogs' or 'walked')}
#'   \item{lemma}{the base form of the word (e.g. 'dog' or 'walk')}
#' }
#' @source \url{https://github.com/tm4ss/tm4ss.github.io/blob/master/resources/baseform_en.tsv}
"lemma_data"
