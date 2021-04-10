dcd_survival <- copy(dcd_all_clean)
dcd_survival[ , age := as.numeric((date_deces - date_naissance) / 365 )]
dcd_survival[ , year := year(date_deces)]

dcd_survival[ , age_r := round( age * 10 ) / 10]
dcd_survival[ , age_r := round( age * 10 ) / 10]
dcd_survival <- dcd_survival[ ! is.na(age) &
               age < 120 &
               age >= 0 ]
dcd_survival[ , age_range := cut(x = age_r, breaks = 0:24*5) ]



make_s_curve <- function(y){
  dcd_survival[ year == y ] %>%
    group_by(age_r) %>%
    summarize(n = n()) %>%
    mutate( t = cumsum(n)) -> s_data

  t_death <- sum(s_data$n)

  s_data %>%
    mutate( t_p = (1 - t / t_death ) * 100) %>%
    mutate( year = y )-> s_data
  return(as.data.table(s_data))
}

survival_all <- rbindlist(lapply(c(
  #1980,1990,2000,
  2003,2019:2020), make_s_curve)) %>% mutate(year = as.factor(year))

g <- ggplot(data = survival_all)
g <- g + geom_point(aes( x = age_r, y = t_p, col = year), size = 0.5)
g <- g  + guides(color = guide_legend(override.aes = list(size = 5)))
g <- g + ggtitle(label = "France survival curves") +
  xlab("Age of death") +
  ylab("% of population surviving to a given age") +
  ylim(c(0,100)) +
  xlim(c(0,120)) + ggthemes::theme_fivethirtyeight()
g


make_pyramid <- function(y){
  dcd_survival[ year == y ] %>%
    group_by(sexe,age_range) %>%
    summarize(n = n())  %>%
    mutate( year = y )-> s_data

  return(as.data.table(s_data))
}
survival_all_pyramid <- rbindlist(lapply(c(1980,1990,2000,2002,2003,2016:2020), make_pyramid)) %>%
  mutate(year = as.factor(year)) %>%
  mutate(sexe = as.factor(sexe))


cas <- survival_all_pyramid[ year %in% c(2002,2003,2020,2019,2018) &  !is.na(age_range) ]
## Death by age range / genre in 2019
gg <-  ggplot(cas[ year == 2019 ]) +
  aes( x=age_range, fill=sexe ) +
  geom_bar(data = subset( cas, sexe == 1),aes(y=n*(-1)),stat="identity") + # les valeurs deviennent négatives
  geom_bar(data = subset(cas, sexe == 2),aes(y=n),stat="identity") +
  scale_fill_manual(values = c("blue","pink")) + # Un peu de couleur
  #scale_y_discrete(limits=c(-100,0,100,200),labels=c(100,0,100,200)) + # étiquettes pour l'axe des x, à modifier selon vos données.
  coord_flip() + ggtitle(label = "Death per genre and age category in 2020")
plot(gg)

## Death by age range / genre in 2020
gg <-  ggplot(cas[ year == 2020 ]) +
  aes( x=age_range, fill=sexe ) +
  geom_bar(data = subset( cas, sexe == 1),aes(y=n*(-1)),stat="identity") + # les valeurs deviennent négatives
  geom_bar(data = subset(cas, sexe == 2),aes(y=n),stat="identity") +
  scale_fill_manual(values = c("blue","pink")) + # Un peu de couleur
  #scale_y_discrete(limits=c(-100,0,100,200),labels=c(100,0,100,200)) + # étiquettes pour l'axe des x, à modifier selon vos données.
  coord_flip() + ggtitle(label = "Death per genre and age category in 2020")
plot(gg)
## Death by age range / genre in 2003
gg <-  ggplot(cas[ year == 2003 ]) +
  aes( x=age_range, fill=sexe ) +
  geom_bar(data = subset( cas, sexe == 1),aes(y=n*(-1)),stat="identity") + # les valeurs deviennent négatives
  geom_bar(data = subset(cas, sexe == 2),aes(y=n),stat="identity") +
  scale_fill_manual(values = c("blue","pink")) + # Un peu de couleur
  #scale_y_discrete(limits=c(-100,0,100,200),labels=c(100,0,100,200)) + # étiquettes pour l'axe des x, à modifier selon vos données.
  coord_flip() + ggtitle(label = "Death per genre and age category in 2020")
plot(gg)


## Death by age range / genre in 2020 vs. 2019 & 2003
gg <-  ggplot(cas[ year %in% c(2020,2019)]) +
  aes( x = age_range, fill = year, col = sexe ) +
  geom_bar(data = subset( cas[ year %in% c(2020,2019)], sexe == 1 ),aes(y=n),position="dodge",stat="identity", alpha = 0.5) + # les valeurs deviennent négatives
  geom_bar(data = subset( cas[ year %in% c(2020,2019)], sexe == 2 ),aes(y=n*(-1)),position="dodge",stat="identity", alpha = 0.5) + # les valeurs deviennent négatives
  scale_color_manual(values = c("lightblue","pink")) + # Un peu de couleur
  scale_fill_manual(values = c("grey","lightblue")) + # Un peu de couleur
  coord_flip() + ggtitle(label = "Death per genre and age category in 2020") + ggthemes::theme_few()
plot(gg)


## Death by age range / genre in 2020 vs. 2019 & 2003
gg <-  ggplot(cas[ year %in% c(2019,2003)]) +
  aes( x = age_range, fill = year, col = sexe ) +
  geom_bar(data = subset( cas[ year %in% c(2019,2003)], sexe == 1 ),aes(y=n),position="dodge",stat="identity", alpha = 0.5) + # les valeurs deviennent négatives
  geom_bar(data = subset( cas[ year %in% c(2019,2003)], sexe == 2 ),aes(y=n*(-1)),position="dodge",stat="identity", alpha = 0.5) + # les valeurs deviennent négatives
  scale_color_manual(values = c("lightblue","pink")) + # Un peu de couleur
  scale_fill_manual(values = c("lightblue","grey")) + # Un peu de couleur
  coord_flip() + ggtitle(label = "Death per genre and age category in 2020 vs. 2003") + ggthemes::theme_few()
plot(gg)


## Death by age range / genre in 2020 vs. 2019 & 2003
gg <-  ggplot(cas[ year %in% c(2018,2002)]) +
  aes( x = age_range, fill = year, col = sexe ) +
  geom_bar(data = subset( cas[ year %in% c(2018,2002)], sexe == 1 ),aes(y=n),position="dodge",stat="identity", alpha = 0.5) + # les valeurs deviennent négatives
  geom_bar(data = subset( cas[ year %in% c(2018,2002)], sexe == 2 ),aes(y=n*(-1)),position="dodge",stat="identity", alpha = 0.5) + # les valeurs deviennent négatives
  scale_color_manual(values = c("lightblue","pink")) + # Un peu de couleur
  scale_fill_manual(values = c("lightblue","grey")) + # Un peu de couleur
  coord_flip() + ggtitle(label = "Death per genre and age category in 2018 vs. 2002") + ggthemes::theme_few()
plot(gg)
