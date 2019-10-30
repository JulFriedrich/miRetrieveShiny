#' Helper - Sort miRNAs of two topic by average relative count
#'
#' Sort relative miRNA count of two topics by average of their
#' relative count. Mainly created to sort the miRNAs that are often
#' reported in both topics to the top for the Shiny application.
#'
#' @param df Data frame after extracting miRNAs
#' @param col.mir Symbol. Column containing miRNA names
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#' @param col.topic Symbol. Column containing topic names.
#'
#' @return miRNA vector sorted with relative count.
#'
#' @export
sort_mir_perc_shiny <- function(df,
                                col.mir = miRNA,
                                col.pmid = PMID,
                                col.topic = Topic) {

    # Split data frame into smaller data frames per group
    df_split <- df %>%
        dplyr::group_split({{col.topic}})


    # Get shared miRNAs

    mir_list <- purrr::map(df_split, ~ get_all_mir(df = .x,
                                                   col.mir = {{col.mir}}))

    shared_mir <- base::intersect(mir_list[[1]], mir_list[[2]])


    # Get relative miRNA count per group
    df_combined <- purrr::map_df(df_split, ~ miRetrieve::count_mir_perc(df = .x,
                                                            col.mir = {{col.mir}},
                                                            col.pmid = {{col.pmid}}))

    # Get average of relative miRNA count over groups
    df_combined <- df_combined %>%
        dplyr::filter({{col.mir}} %in% shared_mir) %>%
        dplyr::group_by({{col.mir}}) %>%
        dplyr::summarize(average_perc = mean(perc, na.rm = TRUE)) %>%
        dplyr::arrange(desc(average_perc))


    # Get sorted miRNA vector
    mirna_perc_sorted <- df_combined %>%
        dplyr::pull({{col.mir}}) %>%
        unique()

    return(mirna_perc_sorted)
}
