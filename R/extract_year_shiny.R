#' Get max publication year
#'
#' Get highest publication year in a data frame
#'
#' @param df Data frame to extract publication year from.
#' @param col.year Symbol. Column containing publication year.
#'
#' @return integer
#'
#' @export
#'
#' @importFrom magrittr %>%

get_max_year_shiny <- function(df, col.year = Year) {
    max_year <- df %>%
        dplyr::select({{col.year}}) %>%
        dplyr::pull() %>%
        max()

    return(max_year)
}

#' Get min publication year
#'
#' Get lowest publication year in a data frame
#'
#' @param df Data frame to extract publication year from.
#' @param col.year Symbol. Column containing publication year.
#'
#' @return integer
#'
#' @export
#'
#' @importFrom magrittr %>%

get_min_year_shiny <- function(df, col.year = Year) {
    min_year <- df %>%
        dplyr::select({{col.year}}) %>%
        dplyr::pull() %>%
        min()

    return(min_year)
}
