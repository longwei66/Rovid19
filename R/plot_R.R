#' plot_R
#'
#' @param my_country
#' @param start_date
#' @param last_date
#' @param mydata
#'
#' @return
#' @export
#' @import dplyr
#' @import EpiEstim
#'
#' @examples
plot_R <- function(
  my_country
  , start_date
  , last_date
  , mydata
  ) {

  mydata <- mydata %>%
    dplyr::filter(date <= last_date) %>%
    dplyr::filter(date >= start_date) %>%
    dplyr::filter(country == my_country) %>%
    dplyr::filter(!is.na(daily_cases)) %>%
    dplyr::select(date, daily_cases) %>%
    dplyr::rename(
      dates=date
      , I=daily_cases
      )

  estimate_R_obj <- EpiEstim::estimate_R(
    mydata
    , method="uncertain_si"
    , config = make_config(
      list(
        mean_si = 7.5, std_mean_si = 2.0
        , min_mean_si = 1, max_mean_si = 8.4
        , std_si = 3.4, std_std_si = 1.0
        , min_std_si = 0.5, max_std_si = 4.0
        , n1 = 1000, n2 = 1000
        )
      )
    )

  plot(estimate_R_obj, "R") + labs(title=paste("Instantaneous effective R - ", my_country))
}
