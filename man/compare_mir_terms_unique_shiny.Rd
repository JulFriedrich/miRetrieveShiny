% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_mir_terms_shiny.R
\name{compare_mir_terms_unique_shiny}
\alias{compare_mir_terms_unique_shiny}
\title{Compare unique terms for miR in data frame}
\usage{
compare_mir_terms_unique_shiny(
  df,
  mir,
  top = 20,
  token = "words",
  normalize = TRUE
)
}
\arguments{
\item{df}{Data frame with two topics and miRNA names.}

\item{mir}{String. miRNA name of interest.}

\item{top}{Integer. Number of top terms to plot.}

\item{token}{String. Either \code{"words"} or \code{"2-grams"}. Specifies
type of tokenization}

\item{normalize}{Boolean. Specifies if count shall be normalized.}

\item{return}{Bar plot with top terms compared.}
}
\description{
Compare unique terms for miR in data frame.
}
