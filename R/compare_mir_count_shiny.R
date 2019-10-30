#' Compare count of top unique miRNAs per topic
#'
#' Compare count of top unique miRNAs per topic.
#'
#' @param df Data frame with two topic names
#' @param top Integer. Specifies of how many top unique miRNAs the count
#' shall be plotted.
#' @param normalize Boolean. Specifies if count shall be normalized to the
#' relative amount of abstracts to all abstracts per topic, or if the absolute
#' count shall be used.
#'
#' @return Plot with count of top unique miRNAs.
#'
#' @export

compare_mir_count_unique_shiny <- function(df, top, normalize) {
    topic_names <- get_topic_name_shiny(df)

    title <- paste0("Top unique miRNAs in ", topic_names[1],
                    " and ", topic_names[2])

    plot <- miRetrieve::compare_mir_count_unique(df = df,
                                                 top = top,
                                                 normalize = normalize,
                                                 title = title)

    return(plot)
}

#' Compare count of shared miRNAs in two topics
#'
#' Compare count of shared miRNAs in two topics.
#'
#' @param df Data frame with two topic names.
#' @param mir String or character vector. Specifies which miRNA count to
#' compare.
#' @param normalize Boolean. Specifies if count shall be normalized to the
#' relative amount of abstracts to all abstracts per topic, or if the absolute
#' count shall be used.
#' @param type String. Either `"standard"` or `"log2"`. Specifies if count
#' shall be compared on a standard scale or on a log2-scale.
#'
#' @return Plot with comparison of shared miRNA count.
#'
#' @export
compare_mir_count_shared_shiny <- function(df, mir, normalize, type) {

    topic_names <- get_topic_name_shiny(df)

    title <- paste0("Comparison of miRNA count in ", topic_names[1],
                    " and ", topic_names[2])

    if(type == "standard") {
        plot <- miRetrieve::compare_mir_count(df = df,
                                              mir = mir,
                                              normalize = normalize,
                                              title = title)
    } else if (type == "log2") {
        plot <- miRetrieve::compare_mir_count_log2(df = df,
                                              mir = mir,
                                              normalize = normalize,
                                              title = title)$plot
    }
    return(plot)
}
