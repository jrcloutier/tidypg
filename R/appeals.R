#' Load completed assessment appeals
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
#' completed <- load_completed_appeals()
#' }
load_completed_appeals <- function() {
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


#' Load filed assessment appeals
#'
#' Loads filed property assessment appeals from WPRDC. Combines data from
#' multiple sources and returns a tibble with formatted variables, standardized
#' municipality information, and selected relevant columns.
#'
#' @return A tibble of filed appeals with formatted dates, numeric values,
#'   and parent municipality columns
#' @export
#' @importFrom readr read_csv cols
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate across select bind_rows
#' @importFrom lubridate ymd
#' @importFrom stringr str_squish str_to_upper
#'
#' @examples
#' \dontrun{
#' filed <- load_filed_appeals()
#' }
load_filed_appeals <- function() {
  urls <- c(
    "https://data.wprdc.org/dataset/filed-property-assessment-appeals/resource/12e00874-bdca-46c2-89ab-bd0e9272b3cb/download/12e00874-bdca-46c2-89ab-bd0e9272b3cb.csv",
    "https://data.wprdc.org/dataset/filed-property-assessment-appeals/resource/c74b5a54-7448-4aa2-82f9-e6663fc10412/download/c74b5a54-7448-4aa2-82f9-e6663fc10412.csv"
  )

  # Read and bind all CSVs
  raw <- urls |>
    lapply(function(u) read_csv(u, col_types = cols(.default = "c")) |> clean_names()) |>
    bind_rows()

  # Format variables
  dat <- raw |>
    mutate(
      across(c(prev_taxyr_mkt_value, cur_mkt_value), as.numeric),
      across(c(tax_year), as.integer),
      across(c(as_of), ~ ymd(.x)),
      school_district = str_squish(str_to_upper(school_district_name))
    )

  # Add parent municipality
  dat <- add_parent_muni(dat, muni_col = "municipality")

  # Select and rename columns
  dat |>
    select(
      id,
      parid = parcel_id,
      tax_year:on_behalf_of,
      hearing_status_code = hrstatus,
      hearing_status,
      owner_name,
      school_district,
      school_code = school_district_code,
      parent_muni,
      parent_muni_code,
      muni,
      muni_code,
      prev_taxyr_mkt_value,
      cur_mkt_value,
      as_of
    )
}
