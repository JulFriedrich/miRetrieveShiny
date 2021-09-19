#' Create target data frame
#'
#' Create target data frame for Shiny. Joins data frame
#' with abstracts with miRTarBase 8.0 on its PMIDs and
#' selects PMIDs, miRNAs as taken from miRTarBase, Targets, and
#' Topic.
#'
#' These are not the same miRNAs as extracted while uploading the abstracts!
#' The miRNAs are taken from miRTarBase, where only the miRNA stem (e.g. "miR-200"
#' instead of "hsa-miR-200b-5p") is extracted.
#'
#' @param df_abstracts Data frame containing PMIDs and abstracts.
#'
#' @return Target data frame. Data frame with four columns, namely
#' PMID, miRNA (as taken from miRTarBase), Target, and Topic
#'
#' @export

create_target_df_shiny <- function(df_abstracts) {

    df_targets <- df_abstracts %>%
        # Join with miRTarBase 8.0
        dplyr::left_join(mirtarbase) %>%
        # Drop NA
        tidyr::drop_na(miRNA_tarbase) %>%
        # Select columns of interest
        dplyr::select(PMID, miRNA = miRNA_tarbase, Target, Topic) %>%
        # Select distinct combinations of PMID, miRNA, and Target
        dplyr::distinct(PMID, miRNA, Target, Topic)

    return(df_targets)
}

#' Extract miRNA names for target operations, sorted by frequency
#'
#' Extract miRNA names for target operations, sorted by frequency
#'
#' @param df_abstracts Data frame containing PMIDs and abstracts.
#' @param df_target Data frame with miRNA targets based on miRTarBase 8.0,
#' usually created by `create_target_df_shiny()`.
#'
#' @return Character vector with miRNA names from `df_abstracts` with targets in
#' miRTarBase 8.0, sorted by count frequeny in `df_abstracts`.
#'
#' @export

get_target_mir_names_shiny <- function(df_abstracts, df_target) {

    # Get miRNA names from df_abstracts
    mir_names_abstracts <- get_mir_names_shiny(df_abstracts)

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

#' Get names of miRNA targets based on target df
#'
#' Get names of miRNA targets based on target df, sorted by count
#' frequency.
#'
#' @param df_target Target data frame.
#'
#' @return Character vector with targets names sorted by count frequency.
#'
#' @export

get_target_target_names_shiny <- function(df_target) {

    target_names <- df_target %>%
        dplyr::count(Target) %>%
        dplyr::arrange(desc(n)) %>%
        pull(Target)

    return(target_names)
}

#' Add miRTarBase caption to plot
#'
#' Add miRTarBase caption to plot.
#'
#' @param plot Ggplot2 Object. Plot to add caption to.
#'
#' @return Plot with miRTarBase caption added.
#'
#' @export

add_mirtarbase_caption_shiny <- function(plot) {
    plot +
        ggplot2::labs(caption = "Source: miRTarBase 8.0
        miRTarBase 2020: updates to the experimentally validated microRNA–target interaction database.
                      Huang HY, Lin YC, Li J, Huang KY, et al., 2020, Nucleic Acids Research.") +
        ggplot2::theme(plot.caption = ggplot2::element_text(size = 6))
}

#' Plot top validated targets
#'
#' Plot top validated targets as a bar plot.
#'
#' @param df_target Target data frame.
#' @param top Integer. Number of top targets to plot.
#'
#' @return Bar plot with top targets.
#'
#' @export

plot_target_count_shiny <- function(df_target, top) {

    # Create plot title
    title <- paste0("Top validated miRNA targets in ", unique(df_target$Topic))

    # Plot target count
    plot <- miRetrieve::plot_target_count(df = df_target,
                                  top = top,
                                  title = title) +
        # Adjust title of y-axis
        ggplot2::ylab("Validated in at least # studies")

    # Add miRTarBase caption
    add_mirtarbase_caption_shiny(plot)

}


#' Plot target-miR scatter plot based on miRNAs
#'
#' Plot target-miR scatter plot based on miRNAs
#'
#' @param df_target Target data frame, created by `create_target_df_shiny()`.
#' @param mir Character vector. miRNAs to plot.
#'
#' @return Scatter plot with target-miR interactions based on miRNAs.
#'
#' @export

plot_target_mir_scatter_mir_shiny <- function(df_target, mir) {

    if(is.null(mir)) {
        title <- "Top miRNA-target interactions"
    } else {
        title <- paste0("miRNA-target interactions for ", paste(mir, collapse = ", "))
    }

    plot <- miRetrieve::plot_target_mir_scatter(df = df_target,
                                        mir = mir,
                                        title = title)

    add_mirtarbase_caption_shiny(plot)

}


#' Plot target-miR scatter plot based on targets
#'
#' Plot target-miR scatter plot based on targets
#'
#' @param df_target Target data frame, created by `create_target_df_shiny()`.
#' @param mir Character vector. Targets to plot.
#'
#' @return Scatter plot with target-miR interactions based on targets.
#'
#' @export

plot_target_mir_scatter_target_shiny <- function(df_target, target) {

    if(is.null(target)) {
        title <- "Top miRNA-target interactions"
    } else {
        title <- paste0("miRNA-target interactions for ", paste(target, collapse = ", "))
    }

    plot <- miRetrieve::plot_target_mir_scatter(df = df_target,
                                        target = target,
                                        title = title)

    add_mirtarbase_caption_shiny(plot)

}
