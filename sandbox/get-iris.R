## donwload as zip and then unzip
# iris-sp <- "https://www.data.gouv.fr/fr/datasets/r/6d440be0-b94e-4e8c-9102-de0bc711806b"

# https://cran.r-project.org/web/packages/rmapshaper/vignettes/rmapshaper.html
library(rmapshaper)
library(sf)
#iris_sp <- st_read("/media/lagrange/homes/lagrange_barthelemy/_DATA_STORE/geo/france/iris/iris-2013-01-01.shp")


url <- "/media/lagrange/homes/lagrange_barthelemy/_DATA_STORE/geo/france/iris/CONTOURS-IRIS_2-1__SHP__FRA_2020-01-01/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2020-01-00139/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2019/CONTOURS-IRIS.shp"
iris_sp <- st_read(url)

iris_simp <- ms_simplify(iris_sp)
iris_simp <- as_Spatial(iris_simp)
iris_f <- as_Spatial(iris_sp)
plot(iris_simp)
save(iris_simp, file = "/media/lagrange/homes/lagrange_barthelemy/_DATA_STORE/geo/france/clean/iris_simplifiy.Rda")
save(iris_f, file = "/media/lagrange/homes/lagrange_barthelemy/_DATA_STORE/geo/france/clean/iris_f.Rda")
iris_f_fortified <- broom::tidy(iris_f, region = "CODE_IRIS")
save(iris_f_fortified, file = "/media/lagrange/homes/lagrange_barthelemy/_DATA_STORE/geo/france/clean/iris_f_fortified.Rda")

