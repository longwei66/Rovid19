#' plotFTlogTrendCases
#'
#' A function to get covid-19 cases from github
#'
#' Total Number of Covid-19 cases per country, log scale, from 100+ plus
#' Original code from https://twitter.com/jburnmurdoch
#' Stories, stats & scatterplots for FinancialTimes
#' john.burn-murdoch(at)ft.com
#'
#' @return a list containing the data and the ggplot object
#' @import dplyr
#' @import ggplot2
#' @importFrom shadowtext geom_shadowtext
#' @export
#' @seealso \url{https://gist.githubusercontent.com/johnburnmurdoch/34bd7470dca92e470fd5f12a488923ce/raw/c7a66c8a718299ecfdb9b7c922daca5d33eb8aa0/coronavirus_cases_trajectories.R}

#'
#' @examples
#' \dontrun{
#'  plotFTlogTrendCases(jhu_data = mydata)
#' }
plotFTlogTrendCases <- function(
  jhu_data = NULL
  , start_case = 100 #600
  ){

  if(is.null(jhu_data)){
    stop("juh_data cannot be null")
  }


  countries_colors <- c(
    "China" = "#EB5E8D"
    , "United Kingdom" = "#ce3140"
    , "US" = "#EB5E8D"
    , "Italy" = "black"
    , "France" = "#208fce"
    , "Germany" = "#c2b7af"
    , "Hong Kong" = "#1E8FCC"
    , "Iran" = "#9dbf57"
    , "Japan" = "#208fce"
    , "Singapore" = "#1E8FCC"
    , "Korea, South" = "#208fce"
    , "Belgium" = "#c2b7af"
    , "Netherlands" = "#c2b7af"
    , "Norway" = "#c2b7af"
    , "Spain" = "#c2b7af"
    , "Sweden" = "#c2b7af"
    , "Switzerland" = "#c2b7af"
    , "33% daily rise" = "#D9CCC3"
  )

  my_df <- jhu_data %>%
    dplyr::filter(
      country != "Others" &
        country != "Cruise Ship"
    ) %>%
    dplyr::bind_rows(
      dplyr::tibble(
        country = "Republic of Korea"
        , date = as.Date("2020-03-11")
        , cases = 7755
        )
    ) %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(days_since_100 = as.numeric(date-min(date[cases >= start_case]))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.finite(days_since_100)) %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(new_cases = cases-cases[days_since_100 == 0]) %>%
    dplyr::filter(sum(cases >= start_case) >= 5) %>%
    dplyr::filter(cases >= start_case) %>%
    dplyr::bind_rows(
      dplyr::tibble(country = "33% daily rise", days_since_100 = 0:38) %>%
        dplyr::mutate(cases = start_case*1.33^days_since_100)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      country = country %>% str_replace_all("( SAR)|( \\(.+)|(Republic of )", "")
    ) %>%
    dplyr::filter(country %in% c(
      "France","Italy", "China", "United Kingdom"
      , "Korea, South", "Japan", "US", "Iran", "Spain", "Germany"
      , "33% daily rise"
    ))


  my_df %>%
    ggplot2::ggplot(aes(days_since_100, cases, col = country)) +
    ggplot2::geom_hline(yintercept = 100) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_line(size = 0.8) +
    ggplot2::geom_point(pch = 21, size = 1) +
    ggplot2::scale_y_log10(
      expand = expand_scale(add = c(0,0.1))
      , breaks=c(
        100, 200, 500, 1000, 2000, 5000, 10000,
        20000, 40000, 80000, 160000, 300000, 500000, 800000,
        1000000, 1200000,1500000, 2000000, 3000000, 5000000)
    ) +
    ggplot2::scale_x_continuous(expand = expand_scale(add = c(0,1))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = margin(3,15,3,3,"mm")
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::scale_colour_manual(values = countries_colors) +
    shadowtext::geom_shadowtext(
      aes(label = paste0(" ",country))
      , hjust=0, vjust = 0
      , data = . %>% group_by(country) %>% top_n(1, days_since_100)
      , bg.color = "white") +
    ggplot2::labs(
      x = paste0("Number of days since ",start_case,"th case"),
      y = "", subtitle = "Total number of cases") -> g



  return(
    list(
      data = my_df
        , chart = g
      )
    )
}




#' plotFTlogTrendDeaths
#'
#' A function to get covid-19 cases from github
#'
#' Total Number of Covid-19 cases per country, log scale, from 100+ plus
#' Original code from https://twitter.com/jburnmurdoch
#' Stories, stats & scatterplots for FinancialTimes
#' john.burn-murdoch(at)ft.com
#'
#' @return a list containing the data and the ggplot object
#' @import dplyr
#' @import ggplot2
#' @importFrom shadowtext geom_shadowtext
#' @export
#' @seealso \url{https://gist.githubusercontent.com/johnburnmurdoch/34bd7470dca92e470fd5f12a488923ce/raw/c7a66c8a718299ecfdb9b7c922daca5d33eb8aa0/coronavirus_cases_trajectories.R}

#'
#' @examples
#' \dontrun{
#'  plotFTlogTrendCases(jhu_data = mydata)
#' }
plotFTlogTrendDeaths <- function(
  jhu_data = NULL
  , start_case = 10 #600
){

  if(is.null(jhu_data)){
    stop("juh_data cannot be null")
  }

  countries_colors <- c(
    "United Kingdom" = "#ce3140"
    , "US" = "#EB5E8D"
    , "Italy" = "black"
    , "France" = "#208fce"
    , "Germany" = "#c2b7af"
    , "Hong Kong" = "#1E8FCC"
    , "Iran" = "#9dbf57"
    , "Japan" = "#208fce"
    , "Singapore" = "#1E8FCC"
    , "Korea, South" = "#208fce"
    , "Belgium" = "#c2b7af"
    , "Netherlands" = "#c2b7af"
    , "Norway" = "#c2b7af"
    , "Spain" = "#c2b7af"
    , "Sweden" = "#c2b7af"
    , "Switzerland" = "#c2b7af"
    , "33% daily rise" = "#D9CCC3"
    , "25% daily rise" = "#D9CCC3"
    , "15% daily rise" = "#D9CCC3"
  )

  my_df <- jhu_data %>%
    dplyr::filter(country != "Others" & country != "Mainland China" & country != "Cruise Ship") %>%
    dplyr::bind_rows(
      dplyr::tibble(country = "Republic of Korea", date = as.Date("2020-03-11"), cases = 7755)
    ) %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(days_since_100 = as.numeric(date-min(date[cases >= start_case]))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.finite(days_since_100)) %>%
    dplyr::group_by(country) %>%
    #mutate(new_cases = cases-cases[days_since_100 == 0]) %>%
    dplyr::filter(sum(cases >= start_case) >= 5) %>%
    dplyr::filter(cases >= start_case) %>%
    dplyr::bind_rows(
      dplyr::tibble(country = "33% daily rise", days_since_100 = 0:33) %>%
        dplyr::mutate(cases = start_case*1.33^days_since_100)
    ) %>%
    dplyr::bind_rows(
      dplyr::tibble(country = "25% daily rise", days_since_100 = 0:33) %>%
        dplyr::mutate(cases = start_case*1.25^days_since_100)
    ) %>%
    dplyr::bind_rows(
      dplyr::tibble(country = "15% daily rise", days_since_100 = 0:33) %>%
        dplyr::mutate(cases = start_case*1.15^days_since_100)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      country = country %>% str_replace_all("( SAR)|( \\(.+)|(Republic of )", "")
    ) %>%
    dplyr::filter(country %in% c(
      "France","Italy"
      , "Korea, South", "Japan", "US", "Iran", "Spain", "Germany"
      , "33% daily rise" , "25% daily rise", "15% daily rise"
    ))


  my_df %>%
    ggplot2::ggplot(aes(days_since_100, cases, col = country)) +
    ggplot2::geom_line(size = 0.8) +
    ggplot2::geom_point(pch = 21, size = 1) +
    ggplot2::scale_y_log10(
      expand = expand_scale(
        add = c(0,0.1))
      , breaks=c(
        100, 200, 500, 1000, 2000, 5000, 10000,
        20000, 50000, 100000, 200000)

    ) +
    # scale_y_continuous(expand = expand_scale(add = c(0,100))) +
    ggplot2::scale_x_continuous(expand = expand_scale(add = c(0,1))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = margin(3,15,3,3,"mm")
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::scale_colour_manual(values = countries_colors) +
    shadowtext::geom_shadowtext(
      aes(label = paste0(" ",country))
      , hjust=0, vjust = 0
      , data = . %>% group_by(country) %>% top_n(1, days_since_100)
      , bg.color = "white"
      ) +
    ggplot2::labs(
      x = paste0("Number of days since ",start_case,"th death")
      , y = "", subtitle = "Total number of cases"
      )  -> g



  return(
    list(
      data = my_df
      , chart = g
    )
  )
}



