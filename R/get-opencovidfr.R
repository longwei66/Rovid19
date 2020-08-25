#' getOpenCovidFr
#'
#' A function to get covid-19 cases from opencovid france
#'
#' @return
#' @export
#' @import dplyr
#' @import data.table
#' @return a list of two objects, cases_cum, cases_daily for France
#'
#' @examples
#' \dontrun{
#'  getOpenCovidFr()
#' }
getOpenCovidFr <- function(
  france_com_url = "https://www.data.gouv.fr/fr/datasets/r/0b66ca39-1623-4d9c-83ad-5434b7f9e2a4"
){

  france_com_cases_cum <- read.csv(file = france_com_url, stringsAsFactors = FALSE) %>%
    dplyr::filter(source_nom == "Ministère des Solidarités et de la Santé" &
             granularite == "pays") %>%
    dplyr::select(date, cas_confirmes) %>%
    dplyr::mutate(date = as.Date(date), country = "France") %>%
    dplyr::rename( cases = cas_confirmes) %>%
    data.table::as.data.table()

  france_com_cases_daily <- france_com_cases_cum %>%
    dplyr::arrange( cases ) %>%
    dplyr::mutate(
      daily_cases = c(cases[1], (cases - lag(cases))[-1])
    ) %>%
    dplyr::ungroup() %>%
    data.table::as.data.table()

  return(
    list(
      cases_cum = france_com_cases_cum
      , cases_daily = france_com_cases_daily
      )
    )
}
