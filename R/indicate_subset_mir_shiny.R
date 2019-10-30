#' Indicate miRNA in data frame
#'
#' Indicates presence of miRNA in abstract with "Yes"/"No" column. Different
#' column name as opposed to miRetrieve.
#'
#' @param df Data frame.
#' @param indicate.mir String or character vector. miRNAs to indicate.
#' @param col.mir Symbol. Column containing miRNA names.
#'
#' @return Data frame.
#'
#' @export
indicate_mir_shiny <- function(df, indicate.mir = NULL, col.mir = miRNA) {

    if(is.null(indicate.mir)) {
        return(df)
    } else {
        for (miR in indicate.mir) {

            col_name <- miR

            df <- df %>%
                dplyr::mutate({{col_name}} := ifelse({{col.mir}} == miR, "Yes", "No"))
        }
        return(df)
    }
}

#' Subet data frame for miRNA
#'
#' Subet data frame for miRNA
#'
#' @param df Data frame.
#' @param mir String or character vector. miRNAs to subset for.
#' @param col.mir Symbol. Column containing miRNA names.
#'
#' @return Data frame.
#'
#' @export
subset_mir_shiny <- function(df, mir = NULL, col.mir = miRNA) {

    if(is.null(mir)) {
        return(df)
    } else {
        df <- df %>%
            dplyr::filter({{col.mir}} %in% mir)
        return(df)
    }
}
