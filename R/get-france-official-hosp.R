#' getFranceOfficialHosp
#'
#' A function to get France official hospitalization data
#'
#' @return
#' @export
#' @import dplyr
#' @import data.table
#' @return a list of two objects, cases_cum, cases_daily for France
#'
#' @examples
#' \dontrun{
#'  getFranceOfficialHosp()
#' }
getFranceOfficialHosp <- function(
  hospital_dep_url = "https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7"
  , hospital_dep_codebook_url = "https://www.data.gouv.fr/fr/datasets/r/3f0f1885-25f4-4102-bbab-edec5a58e34a"
  , france_region_departements_url = "https://www.data.gouv.fr/en/datasets/r/987227fb-dcb2-429e-96af-8979f97c9c84"

){

  hospital_dep_codebook <- data.table::as.data.table(
    read.csv(file = hospital_dep_codebook_url, sep = ";")
    )

  hospital_dep_codebook %>%
    rename(Feature = Colonne) %>%
    mutate(Feature = c( "departement", "sex", "date", "hospitalized_now", "icu_now", "returned_home_now", "death_cum")) -> france_official_hospital_dep_codebook


  france_official_hospital_dep <- data.table::as.data.table(
    read.csv(file = hospital_dep_url, sep = ";")
    )

  france_region_departements <- data.table::as.data.table(
    read.csv(file = france_region_departements_url, sep = ",")
    )

  france_official_hospital_dep %>%
    rename(
      department = dep
      , sex = sexe
      , date = jour
      , hospitalized_now = hosp
      , icu_now = rea
      , back_home_now = rad
      , death_cum = dc
    ) %>%
    mutate(
      date = as.Date(date)
    ) %>%
    dplyr::mutate( sex = as.factor(sex)) -> france_official_hospital_dep

  france_official_hospital_dep <- data.table::as.data.table(merge(
    france_official_hospital_dep
    , france_region_departements
    , by.x = "department"
    , by.y = "num_dep"
  ))


  france_official_hospital_dep %>%
    dplyr::group_by(region_name,sex,date) %>%
    dplyr::summarise(
      hospitalized_now = sum(hospitalized_now, na.rm=TRUE)
      , icu_now = sum(icu_now, na.rm = TRUE)
      , back_home_now = sum(back_home_now, na.rm = TRUE)
      , death_cum = sum(death_cum, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate( sex = as.factor(sex)) %>%
    data.table::as.data.table()-> france_official_reg

  france_official_hospital_dep %>%
    dplyr::group_by(sex,date) %>%
    dplyr::summarise(
      hospitalized_now = sum(hospitalized_now, na.rm=TRUE)
      , icu_now = sum(icu_now, na.rm = TRUE)
      , back_home_now = sum(back_home_now, na.rm = TRUE)
      , death_cum = sum(death_cum, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate( sex = as.factor(sex)) %>%
    data.table::as.data.table()-> france_official_hospital_total



  return(
    list(
      hospital_dep_codebook = hospital_dep_codebook
      , hospital_dep = france_official_hospital_dep
      , hospital_reg = france_official_reg
      , hospital_total = france_official_hospital_total
      )
    )
}
