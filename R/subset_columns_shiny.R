#' Subset data frame for abstracts of research articles
#'
#' Subset data frame for abstracts of research articles only. Special function
#' for miRetrieve Shiny.
#'
#' Optionally, subset data frame for abstracts of research articles only.
#' At the same time, abstracts from other article types such as *Review*,
#' *Letter*, etc. are dropped.
#' Most importantly, columns calles "Type" and "Language" are dropped
#' from the data frame after reading it into R.
#' Special Shiny function.
#'
#' @param df Data frame containing article types.
#' @param col.lang Symbol. Column containing Language type.
#' @param col.type Symbol. Column containing articles types.
#' @param subset Boolean. If `subset = TRUE`, subset for abstracts of
#' original research articles.
#'
#' @return Data frame containing abstracts of research articles only.
#'
#' @seealso [subset_review()], [subset_year()]
#'
#' @family subset functions
#'
#' @importFrom magrittr %>%
#'
#' @export
subset_columns_research_shiny <- function(df,
                                    col.lang = Language,
                                    col.type = Type,
                                    subset = TRUE) {
    if(subset == TRUE) {
        #Get PMIDs of non-original research articles
        pmid_review <- df %>%
            tidyr::unnest({{col.type}}) %>%
            dplyr::filter({{col.type}} == "Review") %>%
            dplyr::select(PMID) %>%
            dplyr::pull()

        pmid_letter <- df %>%
            tidyr::unnest({{col.type}}) %>%
            dplyr::filter({{col.type}} == "Letter") %>%
            dplyr::select(PMID) %>%
            dplyr::pull()

        pmid_retracted <- df %>%
            tidyr::unnest({{col.type}}) %>%
            dplyr::filter({{col.type}} == "Retracted") %>%
            dplyr::select(PMID) %>%
            dplyr::pull()

        pmid_combined <- c(pmid_review,
                           pmid_letter,
                           pmid_retracted) %>%
            unique()

        #Filter for original research Articles
        df <- df %>%
            dplyr::filter(!PMID %in% pmid_combined) %>% #filter out PMIDs with reviews, letters, retracted
            tidyr::unnest({{col.type}}) %>%
            dplyr::filter({{col.type}} == "Journal Article") #keep original Journal Articles
    }

    df <- df %>%
        dplyr::select(-{{col.type}}, -{{col.lang}})


    return(df)
}
