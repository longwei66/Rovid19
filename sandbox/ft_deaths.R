## Total Number of Covid-19 cases per country, log scale, from 100+ plus
## Original code from https://twitter.com/jburnmurdoch
## Stories, stats & scatterplots for @FinancialTimes  john.burn-murdoch@ft.com | #dataviz
## https://gist.githubusercontent.com/johnburnmurdoch/34bd7470dca92e470fd5f12a488923ce/raw/c7a66c8a718299ecfdb9b7c922daca5d33eb8aa0/coronavirus_cases_trajectories.R
library(tidyverse)
library(shadowtext)
library(plotly)

# unique(my_df$country)
# [1] "Belgium"        "China"          "France"         "Germany"        "Iran"           "Italy"          "Japan"          "Korea, South"  
# [9] "Netherlands"    "Norway"         "Spain"          "Sweden"         "Switzerland"    "United Kingdom" "US"             "33% daily rise"

start_case <- 10 #600

my_df <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
    gather(date, cases, 5:ncol(.)) %>%
    mutate(date = as.Date(date, "%m/%d/%y")) %>%
    group_by(country = `Country/Region`, date) %>%
    summarise(cases = sum(cases)) %>%
    filter(country != "Others" & country != "Mainland China" & country != "Cruise Ship") %>%
    bind_rows(
        tibble(country = "Republic of Korea", date = as.Date("2020-03-11"), cases = 7755)
    ) %>%
    group_by(country) %>%
    mutate(days_since_100 = as.numeric(date-min(date[cases >= start_case]))) %>%
    ungroup() %>%
    filter(is.finite(days_since_100)) %>% 
    group_by(country) %>%
    mutate(new_cases = cases-cases[days_since_100 == 0]) %>%
    filter(sum(cases >= start_case) >= 5) %>%
    filter(cases >= start_case) %>% 
    bind_rows(
        tibble(country = "33% daily rise", days_since_100 = 0:33) %>%
            mutate(cases = start_case*1.33^days_since_100)
    ) %>%
    bind_rows(
        tibble(country = "25% daily rise", days_since_100 = 0:33) %>%
            mutate(cases = start_case*1.25^days_since_100)
    ) %>%
    bind_rows(
        tibble(country = "15% daily rise", days_since_100 = 0:33) %>%
            mutate(cases = start_case*1.15^days_since_100)
    ) %>%
    ungroup() %>%
    mutate(
        country = country %>% str_replace_all("( SAR)|( \\(.+)|(Republic of )", "")
    ) %>%
    filter(country %in% c(
        "France","Italy"
        , "Korea, South", "Japan", "US", "Iran", "Spain", "Germany"
        , "33% daily rise" , "25% daily rise", "15% daily rise"
    ))

# my_df[ my_df$country == "France" & my_df$date == as.Date("2020-03-15"),]$cases <- 5423
# my_df[ my_df$country == "France" & my_df$date == as.Date("2020-03-15"),]$new_cases <- 5423
today <- data.frame(list(country = "France",date = as.Date("2020-03-27"), cases = 1995, days_since_100 = 20, new_cases = 1995))
my_df <- rbind(my_df, today)    


my_df %>%
    # filter(days_since_100 <= 10) %>%
    ggplot(aes(days_since_100, cases, col = country)) +
    #geom_hline(yintercept = 10) +
    #geom_vline(xintercept = 0) +
    geom_line(size = 0.8) +
    geom_point(pch = 21, size = 1) +
    scale_y_log10(expand = expand_scale(add = c(0,0.1)), breaks=c(100, 200, 500, 1000, 2000, 5000, 10000)) +
    # scale_y_continuous(expand = expand_scale(add = c(0,100))) +
    scale_x_continuous(expand = expand_scale(add = c(0,1))) +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin = margin(3,15,3,3,"mm")
    ) +
    coord_cartesian(clip = "off") +
    scale_colour_manual(values = c(
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
        )) +
    geom_shadowtext(aes(label = paste0(" ",country)), hjust=0, vjust = 0, data = . %>% group_by(country) %>% top_n(1, days_since_100), bg.color = "white") +
    labs(x = "Number of days since 10th death", y = "", subtitle = "Total number of cases")  -> g

ggplotly(g)




