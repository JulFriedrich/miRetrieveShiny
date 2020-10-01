#' Upload abstracts for Shiny
#'
#' Upload abstracts for Shiny. Unlike in miRetrieve,
#' columns "Type" and "Language" are dropped in miRetrieve Shiny after
#' upload.
#'
#' @param pubmed_file PubMed-file.
#' @param threshold. Integer. Specifies how often a miRNA must be mentioned
#' in Abstracts to be extracted.
#' @param topic String. Specifies topic name.
#' @param subset_research Boolen. If TRUE, only abstracts of original research
#' articles are kept.
#' @param extract_letters Boolean. If TRUE, letters are extracted from miRNA
#' names (e.g. miR-23a).
#'
#' @return Uploaded data frame.
#'
#' @export
upload_abstracts_shiny <- function(pubmed_file,
                                   threshold = 1,
                                   topic = NULL,
                                   subset_research = TRUE,
                                   extract_letters = FALSE) {
    if(is.null(topic)) {
        topic <- "Topic"
    }

    df <- miRetrieve::read_pubmed(pubmed_file = pubmed_file,
                                          topic = topic)

    if(subset_research) {
        df <- miRetrieve::subset_research(df)
    }

    df <- miRetrieve::extract_mir_df(df,
                                     threshold = threshold,
                                     extract_letters = extract_letters)

    df <- df %>%
        dplyr::select(-Type, -Language)

    return(df)
}

#' Check if file type for upload ends in .txt
#'
#' Check if file type for upload ends in .txt.
#'
#' @param file_name File name of file to upload.
#'
#' @return Boolean. If file name ends in .txt, it returns `TRUE`.
#'
#' @export

check_file_type_txt_shiny <- function(file_name) {
    file_ending <- substring(file_name,
                             nchar(file_name)-3,
                             nchar(file_name))

    if(file_ending == ".txt") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' Set topic for data frame
#'
#' Set topic for data frame. No idea if this works, but Shiny does not want to
#' accept my ideas.
#' If topic_input == "", then topic = topic_set. If topic_input != "", then
#' topic = topic_input.
#'
#' @param topic_input Source of topic string.
#' @param topic_set String. What the topic shall be named if topic_input == "".
#'
#' @return String with topic name.
#'
#' @export
set_topic_shiny <- function(topic_input, topic_set) {
    if(topic_input == "" | is.null(topic_input)) {
        topic <- topic_set
    } else {
        topic <- topic_input
    }
    return(topic)
}

#' Upload abstracts and catch errors
#'
#' Catch error messages when uploading abstracts and return a character value.
#'
#' @param ... Arguments passed to `upload_abstracts_shiny()`.
#'
#' @return Either tibble if the functions works, or string with error message
#' if a wrong file was used.
#'
#' @export
upload_abstracts_error_shiny <- function(...) {
    return_value <- tryCatch({upload_abstracts_shiny(...)},
                             warning = function(cond) {
                                 return("It seems that the uploaded file does
                                        not adhere to the PubMed format. Please
                                        upload a PubMed file. For more information,
                                        see 'Help'.")
                             },
                             error = function(cond) {
                                 return("It seems that the chosen file does
                                        not adhere to the PubMed format. Please
                                        upload a PubMed file. For more information,
                                        see 'Help'.")
                             })

    return(return_value)
}
