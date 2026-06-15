#' Load Pittsburgh 311 request data
#'
#' Loads 311 service request data from WPRDC. Returns a tibble with
#' formatted date columns and selected fields.
#'
#' @return A tibble of 311 service requests
#' @export
#' @importFrom readr read_csv cols
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate across select
#' @importFrom lubridate ymd_hms
#'
#' @examples
#' \dontrun{
#' requests <- load_311()
#' }
load_311 <- function() {
  url <- "https://data.wprdc.org/datastore/dump/5202679a-d243-402e-b82a-63189995a942"

  read_csv(url, col_types = cols(.default = "c")) |>
    clean_names() |>
    mutate(
      across(c(created_date_et, last_modified_date_et, closed_date_et), ~ ymd_hms(.x)),
      across(c(latitude, longitude), as.numeric)
    ) |>
    select(
      case_number,
      status,
      case_owner,
      request_type = subject,
      request_type_code = subject_code,
      create_date = created_date_et,
      closed_date = closed_date_et,
      last_modified_date = last_modified_date_et,
      origin,
      street,
      city,
      neighborhood,
      census_tract,
      council_district,
      ward,
      police_zone,
      latitude,
      longitude,
      geo_accuracy,
      unique_id
    )
}
