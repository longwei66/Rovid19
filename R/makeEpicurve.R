#' makeEpicurve
#'
#' a function to plot epidemic curves for a set of countries
#'
#' @param my_data the cases data, daily
#' @param my_ncol the number of columns to be dsiplayed
#'
#' @return a ggplot object
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
makeEpicurve <- function(
  my_data = NULL
  , my_ncol = 3
  ){
  my_data %>%
    dplyr::select(-cases) %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      aes(x=date, y=daily_cases, fill = country)
      , stat = "identity"
      ) +
    ggplot2::facet_wrap(
      facets = . ~ country
      , ncol = my_ncol
      , scales = "free_y"
    ) +
    ggplot2::labs(
      y = "Incident cases"
      , title= "COVID-19 incident daily cases"
      ) +
    ggplot2::theme(legend.position="top")
}
