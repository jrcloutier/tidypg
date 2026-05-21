#' Load condemned properties data
#'
#' Loads condemned properties data from WPRDC. Returns a tibble with
#' formatted date column.
#'
#' @return A tibble of condemned properties with formatted dates
#' @export
#' @importFrom dplyr mutate across select
#' @importFrom lubridate ymd
#'
#' @examples
#' \dontrun{
#' condemned <- load_condemned_props()
#' }
load_condemned_props <- function() {
  resource_id <- "0a963f26-eb4b-4325-bbbc-3ddf6a871410"

  load_wprdc_resource(resource_id) |>
    mutate(
      across(c(create_date), ~ ymd(.x))
    ) |>
    select(
      record_number,
      parid = parcel_id, 
      prop_owner = owner,
      status = property_type,
      creation_date = create_date,
      last_inspection_result, 
      last_inspection_score,
      inspection_status,
      latitude,
      longitude
    )
}
