#' getFranceOfficialTests
#'
#' A function to get France official Tests data
#'
#' @export
#' @import dplyr
#' @import data.table
#' @return a list of two objects
#'
#' @examples
#' \dontrun{
#'  getFranceOfficialEmmergency()
#' }
getFranceOfficialTests <- function(
  sidep_dep_url = "https://www.data.gouv.fr/en/datasets/r/19a91d64-3cd3-42fc-9943-d635491a4d76"
  , sidep_dep_codebook_url = "https://www.data.gouv.fr/en/datasets/r/a8b5931a-3aa7-4aec-a81b-8b3de628cf63"
  , sidep_reg_url = "https://www.data.gouv.fr/en/datasets/r/ad09241e-52fa-4be8-8298-e5760b43cae2"
  , sidep_total_url = "https://www.data.gouv.fr/en/datasets/r/57d44bd6-c9fd-424f-9a72-7834454f9e3c"
  #, regions_url = "https://data.opendatasoft.com/explore/dataset/contours-geographiques-simplifies-des-nouvelles-regions-metropole@public/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B"
  , regions_url = "https://data.opendatasoft.com/explore/dataset/georef-france-region@datanova/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B"
  , region_departements_url = "https://www.data.gouv.fr/en/datasets/r/987227fb-dcb2-429e-96af-8979f97c9c84"
  , sidep_sg_iris_url = "https://www.data.gouv.fr/fr/datasets/r/44d4c265-24c3-4720-9144-f3e4a5213422"
  , sidep_sg_epci_url = "https://www.data.gouv.fr/fr/datasets/r/34dcc90c-aec9-48ee-9fd3-a972b44202c0"
  , sidep_sg_com_url = "https://www.data.gouv.fr/fr/datasets/r/c2e2e844-9671-4f81-8c81-1b79f7687de3"
){

  # ===========================================================================
  # Geographical information
  # ===========================================================================
  message("------ get regions and department metadata")
  france_region_departements <- data.table::as.data.table(
    read.csv(file = region_departements_url, sep = ",")
  )
  if(
    ! identical(
      names(france_region_departements),
      c("num_dep", "dep_name","region_name"))
  ) {
    stop("error in region_departements_url")
  } else {
    message("ok region_departements_url")
  }
  regions <- as.data.table(read.csv(
    file= regions_url
    , sep = ";"))
  if(
    ! identical(
      names(regions),
      #c("geo_point", "geo_shape", "Région", "New.Code" )
      c(
        "Geo.Point", "Geo.Shape", "Année", "Code.Officiel.Région",
        "Code.Officiel.Courant.Région", "Nom.Officiel.Région",
        "Nom.Officiel.Région.Majuscule", "Nom.Officiel.Région.Minuscule",
        "Code.Iso.3166.3.Zone", "Type", "viewport", "Est.une.CTU",  "SIREN"
        )
      )
  ) {
    stop("error in regions_url")
  } else {
    message("ok regions_url")
  }

  # regions <- regions %>%
  #   mutate( code_insee = New.Code , region_name = Région) %>%
  #   select(code_insee, region_name)
  regions <- regions %>%
    mutate( code_insee = Code.Officiel.Région , region_name = Nom.Officiel.Région) %>%
    select(code_insee, region_name)


  message("------ get sidep tests per department codebook")
  france_official_sidep_dep <- data.table::as.data.table(
    read.csv(file = sidep_dep_url, sep = ";"))
  if(
    ! identical(
      names(france_official_sidep_dep),
      c("dep","jour","P","cl_age90","pop"))
  ) {
    stop("error in sidep_dep_url")
  } else {
    message("ok sidep_dep_url")
  }
  france_official_sidep_dep %>%
    mutate(date = as.Date(jour)) %>%
    mutate(cl_age90 = as.factor(cl_age90)) %>%
    select(-jour) %>%
    as.data.table()-> france_official_sidep_dep
  france_official_sidep_dep <- data.table::as.data.table(merge(
    france_official_sidep_dep
    , france_region_departements
    , by.x = "dep"
    , by.y = "num_dep"
  ))

  message("------ get sidep tests per regions")
  france_official_sidep_reg <- data.table::as.data.table(
    read.csv(file = sidep_reg_url, sep = ";"))
  if(
    ! identical(
      names(france_official_sidep_reg),
      c("reg","jour","P_f","P_h","P","pop_f","pop_h","cl_age90","pop" ))
  ) {
    stop("error in sidep_reg_url")
  } else {
    message("ok sidep_reg_url")
  }
  france_official_sidep_reg %>%
    mutate(date = as.Date(jour)) %>%
    mutate(cl_age90 = as.factor(cl_age90)) %>%
    select(-jour) %>%
    filter( reg != "ZZ" ) %>% # ZZ is empty data
    mutate( reg = as.numeric(reg)) %>%
    as.data.table()-> france_official_sidep_reg
  france_official_sidep_reg <- data.table::as.data.table(merge(
    france_official_sidep_reg
    , regions
    , by.x = "reg"
    , by.y = "code_insee"
  ))

  message("------ get sidep tests per country")
  france_official_sidep_total <- data.table::as.data.table(
    read.csv(file = sidep_total_url, sep = ";"))
  if(
    ! identical(
      names(france_official_sidep_total),
      c("fra","jour","P_f","P_h","P","pop_f","pop_h","cl_age90","pop"))
  ) {
    stop("error in sidep_total_url")
  } else {
    message("ok sidep_total_url")
  }

  france_official_sidep_total %>%
    mutate(date = as.Date(jour)) %>%
    mutate(cl_age90 = as.factor(cl_age90)) %>%
    select(-jour) %>%
    as.data.table()-> france_official_sidep_total


  message("------ get sidep incidence per iris code area")
  france_official_sidep_sg_iris <- data.table::as.data.table(
    read.csv(
      file = sidep_sg_iris_url
      , sep = ","
    )
  )
  if(
    ncol(france_official_sidep_sg_iris) != 6
  ) {
    stop("error in sidep_sg_iris_url")
  } else {
    message("ok sidep_sg_iris_url")
    france_official_sidep_sg_iris <- france_official_sidep_sg_iris %>%
      mutate(date = as.Date(x = gsub(pattern = ".*([0-9]{4}-[0-9]{2}-[0-9]{2})$",replacement = "\\1", x = semaine_glissante))) %>%
      mutate(week = lubridate::isoweek(date)) %>%
      mutate( ti_class_low = as.numeric(gsub(pattern = "^\\[([0-9]*);.*", replacement = "\\1", x = ti_classe))) %>%
      mutate( ti_class_high = gsub(pattern = "^\\[[0-9]*;(.*)(\\[|\\])$", replacement = "\\1", x = ti_classe)) %>%
      mutate(ti_class_high = replace(ti_class_high, ti_class_high=="Max", "1000")) %>%
      mutate(ti_class_high = as.numeric(ti_class_high)) %>%
      mutate(ti_class_low = as.numeric(ti_class_low)) %>%
      mutate( ti_class_mean = (ti_class_high + ti_class_low) / 2)
  }
  ## Fix temporaire du au changement d'encodage du fichier source
  # Start Fix
  # france_official_sidep_sg_iris <- read.csv(file = sidep_sg_iris_url, sep = ";", skip = 1)
  # if(
  #   ncol(france_official_sidep_sg_iris) != 9
  # ) {
  #   stop("error in sidep_sg_iris_url")
  # } else {
  #   message("ok sidep_sg_iris_url")
  #   names(france_official_sidep_sg_iris) <- c(
  #     "iris2019","semaine_glissante","clage_65",
  #     "ti_classe", "ti_classe2",
  #     "td_classe", "td_classe2",
  #     "tp_classe", "tp_classe2"
  #   )
  #   france_official_sidep_sg_iris$ti_classe <- paste0(france_official_sidep_sg_iris$ti_classe,";",france_official_sidep_sg_iris$ti_classe2)
  #   france_official_sidep_sg_iris$td_classe <- paste0(france_official_sidep_sg_iris$td_classe,";",france_official_sidep_sg_iris$td_classe2)
  #   france_official_sidep_sg_iris$tp_classe <- paste0(france_official_sidep_sg_iris$tp_classe,";",france_official_sidep_sg_iris$tp_classe2)
  #   france_official_sidep_sg_iris$ti_classe2 <- NULL
  #   france_official_sidep_sg_iris$td_classe2 <- NULL
  #   france_official_sidep_sg_iris$tp_classe2 <- NULL
  #   france_official_sidep_sg_iris <- data.table::as.data.table(france_official_sidep_sg_iris)
  # }
  ## End fix



  #, sidep_sg_epci_url = "https://www.data.gouv.fr/fr/datasets/r/34dcc90c-aec9-48ee-9fd3-a972b44202c0"
  #, sidep_sg_com_url = "https://www.data.gouv.fr/fr/datasets/r/c2e2e844-9671-4f81-8c81-1b79f7687de3"

  return(
    list(
      #tests_dep_codebook = france_official_tests_dep_codebook
      #, tests_dep = france_official_tests_dep
      #, tests_reg = france_official_tests_reg
      #, tests_total = france_official_tests_total
      sidep_dep = france_official_sidep_dep
      , sidep_reg = france_official_sidep_reg
      , sidep_total = france_official_sidep_total
      , sidep_sg_iris = france_official_sidep_sg_iris
      #, sidep_sg_epci = france_official_sidep_sg_epci
      #, sidep_sg_com = france_official_sidep_sg_com
    )
  )
}
