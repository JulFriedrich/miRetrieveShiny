#' Indicate terms in data frame
#'
#' Indicate terms in data frame for Shiny. If term is not present in
#' an abstract, the abstract is automatically discarded.
#'
#' @param df Data frame.
#' @param term Character vector. Terms to screen abstracts for.
#' @param threshold Integer. Specifies how often the term(s) must be
#' contained to be recognized.
#' @param case Boolean. Specifies if terms should be case sensitive or not.
#'
#' @return Data frame subset for terms.
#'
#' @export

indicate_term_shiny <- function(df, term = NULL, threshold = 1, case = FALSE) {

    if(is.null(term)) {
        return(df)
    } else {
        df_ind <- miRetrieve::indicate_term(df = df,
                                            threshold = threshold,
                                            term = term,
                                            case = case,
                                            discard = TRUE) %>%
            # Selektiert die ersten 6 Spalten und dadurch keine "Term_" Spalten.
            # Fühlt sich zwar nicht so gut an, aber eigentlich sollte das mit Shiny,
            # wo die Tables nur durch mich manipuliert werden, kein Problem sein. Andernfalls
            # ist es wohl am einfachsten, die Funktion miRetrieve::indicate_term einfach
            # abzuändern, dass keine Columns hinzugefügt werden bzw. diese nur für die
            # Term scores hinzugefügt werden
            dplyr::select(seq(1,6))

        return(df_ind)
    }
}

#' Count miRNAs of data frame with indicated terms
#'
#' Count top 5 miRNAs of data frame with indicated terms for Shiny.
#'
#' @param df Data frame.
#' @param term Character vector. Terms to screen abstracts for.
#' @param threshold Integer. Specifies how often the term(s) must be
#' contained to be recognized.
#' @param case Boolean. Specifies if terms should be case sensitive or not.
#'
#' @return Plot with top 5 miRNAs for term subset
#'
#' @export
count_mir_term_indicated_shiny <- function(df, term, threshold = 1, case = FALSE) {

    topic <- get_topic_name_shiny(df)

    df_ <- indicate_term_shiny(df = df,
                               threshold =threshold,
                               term = term,
                               case = case)

    if(nrow(df_) == 0) {
        return(paste0('No miRNAs associated with "', paste(term, collapse = '", "'),'" and/or these settings in ', topic))
    }

    title <- paste0('Top miRNAs mentioned with "',
                        stringr::str_c(term, collapse = '", "'), '" in ', topic)

    plot <- miRetrieve::plot_mir_count(df_, top = 5, title = title)

    return(plot)
}
