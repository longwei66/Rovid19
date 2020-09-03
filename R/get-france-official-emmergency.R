#' getFranceOfficialEmmergency
#'
#' A function to get France official emmergency data
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
getFranceOfficialEmmergency <- function(
  emergency_dep_url = "https://www.data.gouv.fr/fr/datasets/r/eceb9fb4-3ebc-4da3-828d-f5939712600a"
  , emergency_dep_codebook_url = "https://www.data.gouv.fr/fr/datasets/r/2265d880-5260-4b34-8827-b3d76180032e"
  , emergency_reg_url = "https://www.data.gouv.fr/fr/datasets/r/d2af5160-a21d-47b7-8f30-3c20dade63b1"
  , emergency_reg_codebook_url = "https://www.data.gouv.fr/fr/datasets/r/02c99d99-96a0-4953-8e30-04fd660530f6"
  , emergency_total_url = "https://www.data.gouv.fr/fr/datasets/r/219427ba-7e90-4eb1-9ac7-4de2e7e2112c"
  , emergency_total_codebook_url = "https://www.data.gouv.fr/fr/datasets/r/0289e4fe-bb67-42b9-a320-cd22c98d82c3"
  , age_categories_url = "https://www.data.gouv.fr/fr/datasets/r/cbed61a8-64e8-4c8c-8e97-65d3a31a99b6"
  , regions_url = "https://data.opendatasoft.com/explore/dataset/contours-geographiques-tres-simplifies-des-regions-2019@ofgl-opendatamef/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true&csv_separator=%3B"
  , region_departements_url = "https://www.data.gouv.fr/en/datasets/r/987227fb-dcb2-429e-96af-8979f97c9c84"
){

  france_official_emergency_dep_codebook <- data.table::as.data.table(
    read.csv(
      file = emergency_dep_codebook_url
      , sep = ";", header = T, stringsAsFactors = F)
  )
  mynames <- as.character(france_official_emergency_dep_codebook[1])
  setnames(france_official_emergency_dep_codebook, unlist(mynames))

  france_official_emergency_reg_codebook <- data.table::as.data.table(
    read.csv(
      file = emergency_reg_codebook_url,
      sep = ";", header = T, stringsAsFactors = F)
  )
  mynames <- as.character(france_official_emergency_reg_codebook[1])
  setnames(france_official_emergency_reg_codebook, unlist(mynames))


  france_age_categories <- data.table::as.data.table(
    read.csv(
      file = age_categories_url, sep = ";"
      , header = T, stringsAsFactors = F)
  )
  names(france_age_categories) <- c("category_code", "category_description")

  france_region_departements <- data.table::as.data.table(
    read.csv(file = region_departements_url, sep = ",")
  )

  france_official_emergency_dep <- data.table::as.data.table(
    read.csv(file = emergency_dep_url, sep = ";")
  )
  france_official_emergency_dep %>%
    mutate(date = as.Date(date_de_passage)) %>%
    as.data.table()-> france_official_emergency_dep

  france_official_emergency_dep <- data.table::as.data.table(merge(
    france_official_emergency_dep
    , france_region_departements
    , by.x = "dep"
    , by.y = "num_dep"
  ))


  france_official_emergency_reg <- data.table::as.data.table(
    read.csv(file = emergency_reg_url, sep = ";")
  )

  regions <- as.data.table(read.csv(
    file= regions_url
    , sep = ";"))
  regions <- regions %>%
    mutate( code_insee = Code.INSEE, region_name = Nom.région) %>%
    select(code_insee, region_name)

  france_official_emergency_reg <- merge(
    x = france_official_emergency_reg
    , y = regions
    , by.x = "reg"
    , by.y = "code_insee")

  france_official_emergency_reg[ , date := as.Date(date_de_passage)]

  # Since July 2nd 2020 the data is not updated
  # we compute agregated data from regions instead
  # france_official_emergency_total <- read.csv(
  #   file = emergency_total_url, sep = ","
  #   , stringsAsFactors = TRUE
  # ) %>%
  #   mutate(
  #     date = as.Date(date_de_passage)
  #   ) %>%
  #   data.table::as.data.table()

  france_official_emergency_total <- france_official_emergency_reg %>%
    select(-region_name,-reg, -date) %>%
    group_by(date_de_passage,sursaud_cl_age_corona) %>%
    summarise_all(sum, na.rm = TRUE) %>%
    ungroup() %>%
    data.table::as.data.table()
  france_official_emergency_total[ , reg := "France"]
  france_official_emergency_total[ , date := as.Date(date_de_passage)]



  return(
    list(
      emergency_dep_codebook = france_official_emergency_dep_codebook
      , emergency_reg_codebook = france_official_emergency_reg_codebook
      , age_categories = france_age_categories
      , emergency_dep = france_official_emergency_dep
      , emergency_reg = france_official_emergency_reg
      , emergency_total = france_official_emergency_total
    )
  )
}
