#' lambda_plot
#'
#' a function to plot lambda function showing epidemics intensity
#'
#' @param my_country Name of the country as chr
#' @param mydata daily incident cases
#' @param my_date starting date for the plot
#'
#' @return a plot
#' @export
#' @import earlyR
#' @import dplyr
#' @import tidyr
#'
#' @examples
lambda_plot <- function(
  my_country
  , mydata
  , my_date
  , si_mean = 5.0
  , si_sd = 3.4
) {

  # alt_si_mean <- 7.5
  # alt_si_sd <- 3.4
  # last_date <- "2020-08-24"
  # r0_date <- "2020-03-01"


  mydata %>%
    dplyr::filter(date <= my_date,
           !is.na(daily_cases),
           country == my_country) %>%
    dplyr::select(date, daily_cases) %>%
    tidyr::uncount(daily_cases) %>%
    dplyr::pull(date) -> local_case_dates

  local_case_dates %>%
    incidence(., last_date=my_date) -> local_cases

  res_obj <- earlyR::get_R(local_cases, si_mean = si_mean, si_sd = si_sd)
  res_obj$local_case_dates <- local_case_dates
  res_obj$city <- my_country
  res_obj$last_date <- my_date
  res_obj$si_mean <- si_mean
  res_obj$si_sd <- si_sd



  plot(
    res_obj, "lambdas"
    , scale = length(res_obj$local_case_dates) + 1
    , bty="n")
  title(
    sub=paste("\nEstimated", expression(lambda), "for",
              res_obj$region,
              "(assuming serial interval mean =",
              res_obj$si_mean,
              ", sd =",
              res_obj$si_sd, ")"))
  abline(v = res_obj$local_case_dates, lwd = 3, col = "grey")
  abline(v = res_obj$last_date, col = "blue", lty = 2, lwd = 2)
  points(res_obj$local_case_dates, seq_along(res_obj$local_case_dates), pch = 20, cex = 3)
}
