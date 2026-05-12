#' Load Pittsburgh PLI permit data
#'
#' Loads permit records from the Pittsburgh Bureau of Building Inspection via
#' WPRDC. Returns a tibble of permit records with cleaned column names and
#' formatted date and numeric fields.
#'
#' @return A tibble with one row per permit record
#' @export
#' @importFrom dplyr mutate filter across rename
#' @importFrom lubridate ymd
#' @importFrom stringr str_to_upper str_remove_all str_replace_all str_squish
#'
#' @examples
#' \dontrun{
#' permits <- load_permits()
#' }
load_permits <- function() {
  resource_id <- "f4d1177a-f597-4c32-8cbf-7885f56253f6"

  dat <- load_wprdc_resource(resource_id)

  dat <- dat |>
    mutate(
      issue_date = ymd(issue_date),
      total_project_value = as.numeric(total_project_value),
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude),
      across(
        c(permit_type, owner_name, contractor_name, work_type,
          commercial_or_residential, neighborhood, status),
        str_to_upper
      ),
      contractor_name = contractor_name |>
        str_remove_all("(,?\\s*\\b(LLC|INC\\.?|INS\\.?|INCORPORATED|CO\\.?|CORP\\.?|LTD\\.?|LP\\.?))+\\s*$") |>
        str_remove_all(",\\s*$") |>
        str_replace_all("\\b([A-Z]) & ([A-Z])\\b", "\\1&\\2") |>
        str_replace_all(" & ", " AND ") |>
        str_squish(),
      address = address |>
        str_remove_all("[.,]") |>
        str_remove_all("(?<!\\d)-|-(?!\\d)") |>
        str_to_upper() |>
        str_squish()
    ) |>
    rename(parid = parcel_num) |>
    filter(!is.na(parid) & parid != "")

  dat
}
