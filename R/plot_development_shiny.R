#' Plot development of miRNAs in abstracts
#'
#' Plot development of miRNAs in abstracts.
#'
#' @param df Data frame with miRNAs and publication years.
#' @param mir String or character vector. Specifies which miRNAs to plot.
#' @param start_year Integer. Start year.
#' @param end_year Integer. End year.
#'
#' @return Line plot.
#'
#' @export
plot_mir_development_shiny <- function(df, mir, start_year, end_year) {

    topic <- get_topic_name_shiny(df)

    title <- paste0("Development of ", paste(mir, collapse = ", "), " in ", topic)

    plot <- miRetrieve::plot_mir_development(df = df,
                                     mir = mir,
                                     start = start_year,
                                     end = end_year,
                                     title = title)

    return(plot)
}
