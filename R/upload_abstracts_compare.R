#' Upload second data set to compare
#'
#' Upload a second data set to compare. This data set is automatically joined to
#' `df_present`.
#'
#' @param file_compare String. File path to upload.
#' @param df_present Data frame with extracted miRNAs and topic name. This is the
#' reference data frame that the newly uploaded file is joined to.
#' @param topic_compare String. Name of topic for the new data frame.
#' @param threshold Integer. Specifies how often a miRNA must be in an
#' abstract to be extracted.
#' @param subset_research Boolean. Specifies if only abstracts of original research
#' articles shall be kept.
#' @param extract_letters Boolean. Specifies if miRNA shall be extracted with
#' trailing letters (e.g. miR-23a).
#'
#' @return Data frame with miRNAs extracted and two topics
#'
#' @export

upload_table_compare_shiny <- function(file_compare,
                                 df_present,
                                 topic_compare = "Topic_compare",
                                 threshold = 1,
                                 subset_research = TRUE,
                                 extract_letters = FALSE) {

    df_compare <- upload_abstracts_error_shiny(medline_file = file_compare,
                                          threshold = threshold,
                                          topic = topic_compare,
                                          subset_research = subset_research,
                                          extract_letters = extract_letters)

    if(is.character(df_compare)) {
        return(df_compare)
    } else {
        df_combined <- miRetrieve::combine_df(df_present, df_compare)
        return(df_combined)
    }
}

#' Get common miRNA names in ordered fashion
#'
#' Get common miRNA names from a data frame with a reference vector consisting
#' of miRNA names. The reference vector should have miRNA names in an ordered
#' fashion and should be a product of `get_mir_names_shiny()`.
#'
#' @param df Data frame with two topics and miRNA names. Should be a result of
#' `upload_table_compare()`.
#' @param mir_names_topic_ordered Character vector. Containing reference miRNA
#' names. Product of `get_mir_names_shiny()`.
#'
#' @return Character vector of shared miRNA names in two topics for analysis.
#'
#' @export

common_mir_names_shiny <- function(df, mir_names_topic_ordered) {
    shared_mirna <- miRetrieve:::get_all_shared_mir(df)
    shared_mirna <- mir_names_topic_ordered[mir_names_topic_ordered %in% shared_mirna]

    return(shared_mirna)
}
