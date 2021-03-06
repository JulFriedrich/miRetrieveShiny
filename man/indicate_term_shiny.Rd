% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indicate_terms_shiny.R
\name{indicate_term_shiny}
\alias{indicate_term_shiny}
\title{Indicate terms in data frame}
\usage{
indicate_term_shiny(df, term = NULL, threshold = 1, case = FALSE)
}
\arguments{
\item{df}{Data frame.}

\item{term}{Character vector. Terms to screen abstracts for.}

\item{threshold}{Integer. Specifies how often the term(s) must be
contained to be recognized.}

\item{case}{Boolean. Specifies if terms should be case sensitive or not.}
}
\value{
Data frame subset for terms.
}
\description{
Indicate terms in data frame for Shiny. If term is not present in
an abstract, the abstract is automatically discarded.
}
