dcd_survival <- copy(dcd_all_clean)
dcd_survival[ , age := as.numeric((date_deces - date_naissance) / 365 )]
dcd_survival[ , year := year(date_deces)]

dcd_survival[ , age_r := round( age * 10 ) / 10]


make_s_curve <- function(y){
  dcd_survival[ year == y &
                  ! is.na(age) &
                  age < 120 &
                  age > 0] %>%
    group_by(age_r) %>%
    summarize(n = n()) %>%
    mutate( t = cumsum(n)) -> s_data

  t_death <- sum(s_data$n)

  s_data %>%
    mutate( t_p = (1 - t / t_death ) * 100) %>%
    mutate( year = y )-> s_data
  return(as.data.table(s_data))
}

survival_all <- rbindlist(lapply(c(1980,1990,2000,2016:2020), make_s_curve)) %>% mutate(year = as.factor(year))

g <- ggplot(data = survival_all)
g <- g + geom_point(aes( x = age_r, y = t_p, col = year), size = 0.5)
g <- g  + guides(color = guide_legend(override.aes = list(size = 5)))
g <- g + ggtitle(label = "France survival curves") +
  xlab("Age of death") +
  ylab("% of population surviving to a given age") +
  ylim(c(0,100)) +
  xlim(c(0,120)) + ggthemes::theme_fivethirtyeight()
g


