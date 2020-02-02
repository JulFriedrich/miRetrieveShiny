#' Extract miRNA names for target operations to compare
#'
#' Extract miRNA names for target operations to compare, sorted by frequency
#'
#' @param df_abstracts Data frame containing PMIDs and abstracts of two topics.
#' @param df_target Data frame with miRNA targets based on miRTarBase 8.0,
#' usually created by `create_target_df_shiny()`.
#' @param mir_names_topic_ordered Character vector. Containing reference miRNA
#' names. Product of `get_mir_names_shiny()`.
#'
#' @return Character vector with miRNA names from `df_abstracts` with targets in
#' miRTarBase 8.0, sorted by count frequeny in `df_abstracts`.
#'
#' @export

get_target_mir_names_compare_shiny <- function(df_abstracts,
                                               df_target,
                                               mir_names_topic_ordered) {

    # Get miRNA names from df_abstracts
    mir_names_abstracts <- common_mir_names_shiny(df_abstracts,
                                                  mir_names_topic_ordered)

    # Extract only miRNA stem; this is necessary when miRNAs with
    # trailing letters had been extracted
    mir_names_abstracts <- stringr::str_extract(mir_names_abstracts,
                                                pattern = "miR-\\d+")

    # Get miRNA names from target data frame
    mir_names_target <- df_target$miRNA

    # Filter for miRNA names in target data frame, sorted by count frequency
    # based on df_abstract
    mir_names_sorted <- mir_names_abstracts[mir_names_abstracts %in% mir_names_target]

    return(mir_names_sorted)
}

#' Get names of shared targets between two topics
#'
#' Get names of shared targets between two topics.
#'
#' @param df_target_compare Data frame with targets for two topics,
#' based on miRTarBase 8.0
#' @param target_name_ref Character vector. Reference target character vector
#' with target names sorted by reference. Product of
#' `get_target_target_names_shiny()`.
#'
#' @return Character vector with shared target names for two topics, sorted by
#' frequency based on reference character vector.
#'
#' @export

get_shared_target_names_shiny <- function(df_target_compare, target_name_ref) {

    # Get shared target names
    target_shared <- df_target_compare %>%
        # Select columns of interest
        select(Target, Topic) %>%
        distinct() %>%
        add_count(Target) %>%
        # Select only targets that are mentioned in two topics
        filter(n == 2) %>%
        select(Target) %>%
        distinct() %>%
        pull()

    # Get shared target names, sorted by frequency based on `target_name_ref`
    shared_names_ref <- target_name_ref[target_name_ref %in% target_shared]

    return(shared_names_ref)
}
