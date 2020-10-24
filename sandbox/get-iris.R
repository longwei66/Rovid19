## Best ressource :
## https://rcarto.github.io/carto_avec_r/chapitre1.html#le-package-sf

## donwload as zip and then unzip
# iris-sp <- "https://www.data.gouv.fr/fr/datasets/r/6d440be0-b94e-4e8c-9102-de0bc711806b"

# https://cran.r-project.org/web/packages/rmapshaper/vignettes/rmapshaper.html
library(rmapshaper)
library(sf)
library(ggplot2)

## The iris France shapefile
url <- "/media/lagrange/homes/lagrange_barthelemy/_DATA_STORE/geo/france/iris/CONTOURS-IRIS_2-1__SHP__FRA_2020-01-01/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2020-01-00139/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2019/CONTOURS-IRIS.shp"
iris_sp <- sf::st_read(url)

## The iris metadata
url <- "/media/lagrange/homes/lagrange_barthelemy/_DATA_STORE/geo/france/iris/reference_IRIS_geo2020.csv"
iris_mapping <- data.table::fread(file = url)
iris_mapping <- iris_mapping[ , .(CODE_IRIS,REG,DEP)]

iris_sp <-merge(x = iris_sp, y = iris_mapping, by.x = "CODE_IRIS", by.y = "CODE_IRIS")
save(iris_sp, file = "/media/lagrange/homes/lagrange_barthelemy/_DATA_STORE/geo/france/clean/iris_sp.Rda")


## Plot (take long on the full map)
plot(sf::st_geometry(iris_sp))
plot(sf::st_geometry(iris_sp[1:1000,]))

plot(sf::st_geometry(iris_sp[ iris_sp$REG == "11",]))
plot(sf::st_geometry(iris_sp[ iris_sp$DEP == "19",]))

ggplot2::ggplot(data = iris_sp[ iris_sp$REG %in% c("11","12"),]) + ggplot2::geom_sf()


## Convert to spatial data frame to be used in ggplot
iris_sdf <- as_Spatial(iris_sp)

## Save intermediary
save(iris_sdf, file = "/media/lagrange/homes/lagrange_barthelemy/_DATA_STORE/geo/france/clean/iris_sdf.Rda")

iris_f_fortified <- broom::tidy(iris_sdf, region = "CODE_IRIS")

save(iris_f_fortified, file = "/media/lagrange/homes/lagrange_barthelemy/_DATA_STORE/geo/france/clean/iris_f_fortified.Rda")


## Simplified version
iris_sdf_simp <- ms_simplify(iris_sdf)
iris_simp <- as_Spatial(iris_simp)
save(iris_simp, file = "/media/lagrange/homes/lagrange_barthelemy/_DATA_STORE/geo/france/clean/iris_simplifiy.Rda")
plot(iris_simp)






