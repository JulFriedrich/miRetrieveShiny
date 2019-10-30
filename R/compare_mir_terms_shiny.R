#' Compare unique terms for miR in data frame
#'
#' Compare unique terms for miR in data frame.
#'
#' @param df Data frame with two topics and miRNA names.
#' @param mir String. miRNA name of interest.
#' @param top Integer. Number of top terms to plot.
#' @param token String. Either `"words"` or `"2-grams"`. Specifies
#' type of tokenization
#' @param normalize Boolean. Specifies if count shall be normalized.
#'
#' @param return Bar plot with top terms compared.
#'
#' @export

compare_mir_terms_unique_shiny <- function(df, mir, top = 20, token = "words", normalize = TRUE) {
    if(token == "words") {
        plot <- miRetrieve::compare_mir_terms_unique(df = df,
                                             mir = mir,
                                             token = "words",
                                             top = top,
                                             normalize = normalize)
    } else if (token == "2-grams") {
        plot <- miRetrieve::compare_mir_terms_unique(df = df,
                                                     mir = mir,
                                                     token = "ngrams",
                                                     n = 2,
                                                     top = top,
                                                     normalize = normalize)
    }
    return(plot)
}


#' Compare shared terms for miR in data frame
#'
#' Compare shared terms for miR in data frame.
#'
#' @param df Data frame with two topics and miRNA names.
#' @param mir String. miRNA name of interest.
#' @param top Integer. Number of top terms to plot.
#' @param token String. Either `"words"` or `"2-grams"`. Specifies
#' type of tokenization.
#' @param type String. Either `"standard"`, `"log2"`, or `"scatter"`.
#' Specifies scale/plot type.
#' @param normalize Boolean. Specifies if count shall be normalized.
#'
#' @param return Bar plot with top terms compared.
#'
#' @export

compare_mir_terms_shared_shiny <- function(df, mir, top = 20,
                                           token = "words", type = "standard",
                                           normalize = TRUE) {
    topic_names <- get_topic_name_shiny(df)

    title <- paste0("Comparison of miRNA-term association for ", mir, " in ",
                    topic_names[1], " and ", topic_names[2])

    if(type == "standard") {
        if(token == "words") {
            plot <- miRetrieve::compare_mir_terms(df = df,
                              mir = mir,
                              top = top,
                              title = title,
                              normalize = normalize)
        } else if (token == "2-grams") {
            plot <- miRetrieve::compare_mir_terms(df = df,
                                      mir = mir,
                                      top = top,
                                      token = "ngrams",
                                      n = 2,
                                      title = title,
                                      normalize = normalize)
        }
    } else if (type == "log2") {
        if(token == "words") {
            plot <- miRetrieve::compare_mir_terms_log2(df = df,
                                      mir = mir,
                                      top = top,
                                      title = title,
                                      normalize = normalize)
            if(is.character(plot)) {
                return("No shared terms for this combination.")
            } else {
                return(plot$plot)
            }
        } else if (token == "2-grams") {
            plot <- miRetrieve::compare_mir_terms_log2(df = df,
                                      mir = mir,
                                      top = top,
                                      token = "ngrams",
                                      n = 2,
                                      title = title,
                                      normalize = normalize)
            if(is.character(plot)) {
                return("No shared terms for this combination.")
            } else {
                return(plot$plot)
            }
        }
    } else if (type == "scatter") {
        normalize <- NULL
        if(token == "words") {
            plot <- miRetrieve::compare_mir_terms_scatter(df = df,
                                                       mir = mir,
                                                       top = top,
                                                       title = title)
        } else if (token == "2-grams") {
            plot <- miRetrieve::compare_mir_terms_scatter(df = df,
                                                       mir = mir,
                                                       top = top,
                                                       token = "ngrams",
                                                       n = 2,
                                                       title = title)
            }
    }
    return(plot)
}

#' Compare miRNA-terms association between two abstracts.
#'
#' Compare miRNA-terms association between two abstracts.
#'
#' @param df Data frame with two topics and miRNA names.
#' @param mir String. miRNA name of interest.
#' @param unique_shared String. Either `"unique"` or `"shared"`. Sets if unique
#' or shared miR-term associations shall be displayed.
#' @param top Integer. Number of top terms to plot.
#' @param token String. Either `"words"` or `"2-grams"`. Specifies
#' type of tokenization.
#' @param type String. Either `"standard"`, `"log2"`, or `"scatter"`.
#' Specifies scale/plot type.
#' @param normalize Boolean. Specifies if count shall be normalized.
#'
#' @param return Bar plot with top terms, either unique miRNA-term or shared
#' miRNA-term associations.
#'
#' @export

compare_mir_terms_shiny <- function(df, mir, unique_shared = "unique",
                                    top = 20, token = "words",
                                    type = "standard", normalize = TRUE) {

    if(is.null(type)) {
        type <- "standard"
    }

    if(unique_shared == "unique") {
        plot <- compare_mir_terms_unique_shiny(df = df,
                                       mir = mir,
                                       top = top,
                                       token = token,
                                       normalize = normalize)
    } else if (unique_shared == "shared") {
        plot <- compare_mir_terms_shared_shiny(df = df,
                                       mir = mir,
                                       top = 20,
                                       token = token,
                                       type = type,
                                       normalize = normalize)
    }
    return(plot)
}
