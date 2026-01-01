#' Load appeals
#'
#' Loads completed property assessment appeals from WPRDC. Returns a tibble
#' with formatted variables, standardized municipality information, and
#' selected relevant columns.
#'
#' @return A tibble of completed appeals with formatted dates, numeric values,
#'   and parent municipality columns
#' @export
#' @importFrom dplyr mutate across select
#' @importFrom lubridate ymd dmy
#' @importFrom stringr str_squish str_to_upper
#'
#' @examples
#' \dontrun{
#' completed <- load_appeals()
#' }
load_appeals <- function() {
  resource_id <- "8a7607fb-c93e-4d7a-9b23-528b5c25b1de"

  dat <- load_wprdc_resource(resource_id)

  # Format variables
  dat <- dat |>
    mutate(
      across(
        c(
          pre_appeal_land, pre_appeal_bldg, pre_appeal_total, post_appeal_land,
          post_appeal_bldg, post_appeal_total, hearing_change_amount,
          current_land_value, current_bldg_value, current_value_vs_pre_appeal
        ),
        as.numeric
      ),
      across(c(elapsed_days, tax_year), as.integer),
      across(c(hearing_date, dispo_date), ~ dmy(.x)),
      across(c(as_of_date), ~ ymd(.x)),
      school_district = str_squish(str_to_upper(school_district))
    )

  # Add parent municipality
  dat <- add_parent_muni(dat, muni_col = "muni_name")

  # Select and rename columns
  dat |>
    select(
      parid = parcel_id, tax_year, class, tax_status, muni_code, muni,
      parent_muni, parent_muni_code, school_code:as_of_date
    )
}
