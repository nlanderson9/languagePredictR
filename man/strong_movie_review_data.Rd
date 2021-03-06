% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{strong_movie_review_data}
\alias{strong_movie_review_data}
\title{Strong Movie Reviews from IMDB}
\format{
A data frame with 2000 rows and 3 variables:
\describe{
  \item{text}{text of the user's movie review}
  \item{valence}{valence of the user's movie review, \code{Positive} (rating of 1) or \code{Negative} (rating of 10)}
  \item{rating}{user's rating of the movie, on a scale of 1-10}
}
}
\source{
\url{http://ai.stanford.edu/~amaas/data/sentiment/}
}
\usage{
strong_movie_review_data
}
\description{
A dataset containing the text and ratings of 2000 movie reviews from IMDB. 1000 are strongly negative (rating of 1) and 1000 are strongly positive (rating of 10).
}
\keyword{datasets}
