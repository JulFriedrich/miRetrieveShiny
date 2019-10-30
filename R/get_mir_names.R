#' Get miRNA names from a data frame
#'
#' Get miRNA names as a vector from a data frame. miRNA names
#' are sorted in order of their count, e.g. the miRNA with the highest
#' frequency is at the beginning.
#'
#' @param df Data frame with miRNA names.
#' @param col.mir Symbol. Column containing miRNA names.
#'
#' @return Character vector.
#'
#' @export


get_mir_names_shiny <- function(df, col.mir = miRNA) {
    mirna_names <- miRetrieve::count_mir(df) %>%
        dplyr::select({{col.mir}}) %>%
        dplyr::pull() %>%
        unique()

    return(mirna_names)
}

#' Get topic name of data frame.
#'
#' Get topic name of Topic in data frame.
#'
#' @param df Data frame with topic names.
#' @param col.topic Symbol. Column containing topic name.
#'
#' @return String with the topic name
#'
#' @export
get_topic_name_shiny <- function(df, col.topic = Topic) {
    topic_name <- df %>%
        dplyr::select({{col.topic}}) %>%
        dplyr::pull() %>%
        unique()

    return(topic_name)
}

