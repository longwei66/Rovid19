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
  tests_dep_url = "https://www.data.gouv.fr/fr/datasets/r/b4ea7b4b-b7d1-4885-a099-71852291ff20"
  , tests_dep_codebook_url = "https://www.data.gouv.fr/fr/datasets/r/971c5cbd-cd80-4492-b2b3-c3deff8c1f5e"
  , sidep_dep_url = "https://www.data.gouv.fr/en/datasets/r/19a91d64-3cd3-42fc-9943-d635491a4d76"
  , sidep_dep_codebook_url = "https://www.data.gouv.fr/en/datasets/r/a8b5931a-3aa7-4aec-a81b-8b3de628cf63"
  , sidep_reg_url = "https://www.data.gouv.fr/en/datasets/r/ad09241e-52fa-4be8-8298-e5760b43cae2"
  , sidep_total_url = "https://www.data.gouv.fr/en/datasets/r/57d44bd6-c9fd-424f-9a72-7834454f9e3c"
  , regions_url = "https://data.opendatasoft.com/explore/dataset/contours-geographiques-tres-simplifies-des-regions-2019@ofgl-opendatamef/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true&csv_separator=%3B"
  , region_departements_url = "https://www.data.gouv.fr/en/datasets/r/987227fb-dcb2-429e-96af-8979f97c9c84"
){


  france_region_departements <- data.table::as.data.table(
    read.csv(file = region_departements_url, sep = ",")
  )

  regions <- as.data.table(read.csv(
    file= regions_url
    , sep = ";"))
  regions <- regions %>%
    mutate( code_insee = Code.INSEE, region_name = Nom.région) %>%
    select(code_insee, region_name)

  france_official_tests_dep_codebook <- data.table::as.data.table(
    read.csv(file = france_official_tests_dep_codebook_url
             , sep = ";", header = T, stringsAsFactors = F)
    )


  france_official_tests_dep <- data.table::as.data.table(
    read.csv(file = tests_dep_url, sep = ";")
    )
  france_official_tests_dep %>%
    rename(
      department = dep
      , date = jour
    ) %>%
    mutate(
      date = as.Date(date)
    )-> france_official_tests_dep

  france_official_tests_dep <- data.table::as.data.table(merge(
    france_official_tests_dep
    , france_region_departements
    , by.x = "department"
    , by.y = "num_dep"
  ))

  france_official_tests_dep %>%
    group_by(region_name,date,clage_covid) %>%
    summarise(
      nb_test = sum(nb_test, na.rm=TRUE)
      , nb_pos = sum(nb_pos, na.rm = TRUE)
      , nb_test_h = sum(nb_test_h, na.rm = TRUE)
      , nb_pos_h = sum(nb_pos_h, na.rm = TRUE)
      , nb_test_f = sum(nb_test_f, na.rm = TRUE)
      , nb_pos_f = sum(nb_pos_f, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    data.table::as.data.table()-> france_official_tests_reg

  france_official_tests_dep %>%
    group_by(date,clage_covid) %>%
    summarise(
      nb_test = sum(nb_test, na.rm=TRUE)
      , nb_pos = sum(nb_pos, na.rm = TRUE)
      , nb_test_h = sum(nb_test_h, na.rm = TRUE)
      , nb_pos_h = sum(nb_pos_h, na.rm = TRUE)
      , nb_test_f = sum(nb_test_f, na.rm = TRUE)
      , nb_pos_f = sum(nb_pos_f, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    data.table::as.data.table()-> france_official_tests_total


  france_official_sidep_dep <- data.table::as.data.table(
    read.csv(file = sidep_dep_url, sep = ";"))

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


  france_official_sidep_reg <- data.table::as.data.table(
    read.csv(file = sidep_reg_url, sep = ";"))

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


  france_official_sidep_total <- data.table::as.data.table(
    read.csv(file = sidep_total_url, sep = ";"))

  france_official_sidep_total %>%
    mutate(date = as.Date(jour)) %>%
    mutate(cl_age90 = as.factor(cl_age90)) %>%
    select(-jour) %>%
    as.data.table()-> france_official_sidep_total



  return(
    list(
      tests_dep_codebook = france_official_tests_dep_codebook
      , tests_dep = france_official_tests_dep
      , tests_reg = france_official_tests_reg
      , tests_total = france_official_tests_total
      , sidep_dep = france_official_sidep_dep
      , sidep_reg = france_official_sidep_reg
      , sidep_total = france_official_sidep_total
    )
  )
}
