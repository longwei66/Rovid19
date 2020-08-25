#' getJHUcases
#'
#' A function to get covid-19 cases from github
#'
#' @return
#' @export
#' @import dplyr
#' @import tidyr
#' @import readr
#' @import data.table
#' @return a list of three objects, cases_cum, cases_daily, deaths_cum per
#' countries
#'
#' @examples
#' \dontrun{
#'  getJHUcases()
#' }
getJHUcases <- function(
  jhu_data_cases_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  , jhu_data_deaths_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
){
  www_cases_cum <- readr::read_csv(jhu_data_cases_url) %>%
    tidyr::gather(date, cases, 5:ncol(.)) %>%
    dplyr::mutate(date = as.Date(date, "%m/%d/%y"))  %>%
    dplyr::group_by(country = `Country/Region`, date) %>%
    dplyr::summarise(cases = sum(cases)) %>%
    data.table::as.data.table()

  www_cases_daily <- www_cases_cum %>%
    dplyr::group_by(country) %>%
    dplyr::arrange( cases ) %>%
    dplyr::mutate(
      daily_cases = c(cases[1], (cases - lag(cases))[-1])
    ) %>%
    dplyr::ungroup() %>%
    data.table::as.data.table()

  www_deaths_cum <- read_csv(jhu_data_deaths_url) %>%
    tidyr::gather(date, cases, 5:ncol(.)) %>%
    dplyr::mutate(date = as.Date(date, "%m/%d/%y")) %>%
    dplyr::group_by(country = `Country/Region`, date) %>%
    dplyr::summarise(cases = sum(cases))


  return(
    list(
      cases_cum = www_cases_cum
      , cases_daily = www_cases_daily
      , deaths_cum = www_deaths_cum
      )
    )
}
