#' Count miRNA names in table
#'
#' Count miRNA names in table. In contrast to miRetrieve,
#' the second column is renamed in miRetrieveShiny.
#'
#' @param df Data frame with miRNA names.
#'
#' @return Data frame with counts.
#'
#' @export
count_mir_table_shiny <- function(df) {
    table <- miRetrieve::count_mir(df) %>%
        dplyr::rename("Mentioned in # of abstracts" = 2)

    return(table)
}

#' Plot top miRNA names
#'
#' Plot top miRNA names. In contrast to miRetrieve,
#' the title is prefixed with the topic of the data frame.
#'
#' @param df Data frame with miRNA names.
#' @param top Integer. Number of top miRNA counts to plot.
#'
#' @return Plot with top miRNA counts.
#'
#' @export
count_mir_plot_shiny <- function(df, top = 10) {

    topic_name <- get_topic_name_shiny(df)

    title <- paste0("Most frequently mentioned miRNAs in ", topic_name)

    plot <- miRetrieve::plot_mir_count(df, top = top, title = title)

    return(plot)
}
