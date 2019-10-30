#' Plot top likely miRNA biomarker
#'
#' Plot top likely miRNA biomarker. Biomarker abstracts are identified
#' with calculate_score and a threshold, rest of abstracts are discarded.
#' miRNAs are counted.
#'
#' @param df Data frame.
#' @param threshold Integer. Threshold to distinguish biomarker abstracts.
#'
#' @return Plot with top most likely biomarkers.
#'
#' @export
plot_mir_biomarker_shiny <- function(df, threshold = 1){
    df_biomarker <- miRetrieve::calculate_score_biomarker(df = df,
                                              threshold = threshold,
                                              discard = TRUE)

    topic <- get_topic_name_shiny(df)

    title_biomarker <- paste0("Likely biomarker in ", topic)

    plot <- miRetrieve::plot_mir_count(df_biomarker, title = title_biomarker)

    return(plot)
}
