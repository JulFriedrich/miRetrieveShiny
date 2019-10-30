#' Plot miRNA-terms association
#'
#' Plot miRNA-terms association. Less arguments than in miRetrieve,
#' short cut to tokenization.
#'
#' @param df Data frame.
#' @param mir String. Specifies which miR to plot terms for.
#' @param top Integer. Number of top terms to plot.
#' @param token String. Either 'words' or '2-grams'.
#' @param normalize Boolean. Specifies if count shall be normalized.
#'
#' @return Plot.
#'
#' @export
plot_mir_terms_shiny <- function(df, mir, top = 20, token = "words", normalize = TRUE) {
    topic <- get_topic_name_shiny(df)

    if(token == "words") {
        title <- paste0("Top terms for ", mir, " in ", topic)

        plot <- miRetrieve::plot_mir_terms(df = df,
                               mir = mir,
                               top = top,
                               token = "words",
                               stopwords = stopwords_miretrieve,
                               title = title, normalize = normalize)
    } else if(token == "2-grams") {
        title <- paste0("Top 2-grams for ", mir, " in ", topic)
        plot <- miRetrieve::plot_mir_terms(df = df,
                               mir = mir,
                               top = top,
                               token = "ngrams",
                               n = 2,
                               stopwords = stopwords_miretrieve,
                               title = title, normalize = normalize)
    }

    return(plot)
}
