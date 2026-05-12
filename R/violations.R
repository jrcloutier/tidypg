#' Load Pittsburgh code violations data
#'
#' Loads code violations data from WPRDC. Returns a list with two tibbles:
#' casefiles (aggregated by case) and violations (individual violation records).
#' The function removes records with missing parcel IDs, removes duplicates,
#' and structures the data into two related datasets.
#'
#' @return A list with four tibbles:
#'   \describe{
#'     \item{casefiles}{Aggregated data by casefile with case summary information}
#'     \item{casefile_entries}{Individual casefile entry records linked to casefiles}
#'     \item{court_cases}{One row per docket number with latest court date and decision}
#'     \item{court_hearings}{Individual court hearing records per docket}
#'   }
#' @export
#' @importFrom dplyr mutate filter group_by across slice_max ungroup arrange summarise first last n_distinct if_else select left_join
#' @importFrom lubridate ymd
#' @importFrom tidyr separate_longer_delim
#'
#' @examples
#' \dontrun{
#' violations_data <- load_violations()
#' casefiles <- violations_data$casefiles
#' violations <- violations_data$violations
#' }
load_violations <- function() {
  resource_id <- "70c06278-92c5-4040-ab28-17671866f81c"

  dat <- load_wprdc_resource(resource_id)

  # Format variables
  dat <- dat |>
    mutate(
      investigation_date = ymd(investigation_date)
    )
  
  # Remove records with missing parcel ID
  dat <- dat |>
    filter(!is.na(parcel_id) & parcel_id != "")

  # Remove duplicate records
  dat <- dat |>
    group_by(
      across(
        c(
          casefile_number, 
          parcel_id, 
          status, 
          investigation_date,
          violation_description,
          violation_code_section,
          violation_spec_instructions,
          investigation_outcome, 
          investigation_findings
        )
      )
    ) |>
    slice_max(id, n = 1, with_ties = FALSE) |> 
    ungroup()
  
  # Split into two datasets: casefiles and violations
  
  casefiles <- dat |>
    arrange(casefile_number, investigation_date) |>
    group_by(casefile = casefile_number) |>
    summarise(
      parid = first(parcel_id),
      case_start_date = min(investigation_date, na.rm = TRUE),
      last_inspection_date = max(investigation_date, na.rm = TRUE),
      last_inspection_outcome = last(investigation_outcome[!is.na(investigation_date)]),
      n_inspects = n_distinct(investigation_date[!is.na(investigation_date)]),
      n_violations = n_distinct(violation_code_section[!is.na(violation_code_section)]),
      has_court_case = any(!is.na(docket_number) & docket_number != ""),
      .groups = "drop"
    )

  casefile_entries <- dat |>
    arrange(casefile_number, investigation_date) |>
    mutate(rowid = row_number()) |>
    select(
      rowid, 
      casefile = casefile_number, 
      parid = parcel_id,
      inspect_date = investigation_date,
      inspect_outcome = investigation_outcome,
      inspect_finding = investigation_findings,
      violation_desc = violation_description,
      violation_code = violation_code_section,
      violation_instructions = violation_spec_instructions
    )
  
  court_dat <- dat |>
    filter(!is.na(docket_number) & docket_number != "") |>
    separate_longer_delim(docket_number, delim = ",") |>
    mutate(
      docket_number = trimws(docket_number),
      court_date = ymd(court_date)
    ) |>
    group_by(casefile_number, docket_number) |>
    mutate(
      docket_id = paste(casefile_number, format(min(court_date, na.rm = TRUE), "%Y-%m-%d"), sep = "_")
    ) |>
    ungroup()

  casefiles <- casefiles |>
    left_join(
      court_dat |>
        group_by(casefile_number) |>
        summarise(docket_ids = paste(unique(docket_id), collapse = ", "), .groups = "drop"),
      by = c("casefile" = "casefile_number")
    )

  court_hearings <- court_dat |>
    arrange(docket_id, court_date) |>
    mutate(
      court_time = format(strptime(toupper(trimws(court_time)), "%I:%M%p"), "%H:%M"),
      court_decision = toupper(court_decision)
    ) |>
    select(
      docket_id,
      casefile = casefile_number,
      parid = parcel_id,
      court_date,
      court_time,
      court_decision
    )

  court_cases <- court_dat |>
    arrange(docket_id, court_date) |>
    group_by(docket_id) |>
    summarise(
      casefile = first(casefile_number),
      parid = first(parcel_id),
      latest_court_date = max(court_date, na.rm = TRUE),
      latest_court_decision = last(court_decision[!is.na(court_date)]),
      docket_number = first(docket_number),
      .groups = "drop"
    )

  return(list(
    casefiles = casefiles,
    casefile_entries = casefile_entries,
    court_cases = court_cases,
    court_hearings = court_hearings
  ))
}