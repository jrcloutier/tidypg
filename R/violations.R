#' Load Pittsburgh code violations data
#'
#' Loads code violations data from WPRDC. Returns a list of four tibbles:
#' pli_casefiles, pli_inspections, pli_violations, and pli_hearings.
#' The function removes records with missing parcel IDs, removes duplicates,
#' and structures the data into related datasets.
#'
#' @return A list with four tibbles:
#'   \describe{
#'     \item{pli_casefiles}{Aggregated data by casefile with case summary information, including casefile_status}
#'     \item{pli_inspections}{One row per unique inspection event per casefile (not all will have a date)}
#'     \item{pli_violations}{One row per unique violation per casefile}
#'     \item{pli_hearings}{Individual court hearing records per docket, including docket number for case lookup}
#'   }
#' @export
#' @importFrom dplyr mutate filter group_by across slice_max ungroup arrange summarise first last n_distinct select distinct
#' @importFrom lubridate ymd
#' @importFrom tidyr separate_longer_delim
#'
#' @examples
#' \dontrun{
#' violations_data <- load_violations()
#' pli_casefiles <- violations_data$pli_casefiles
#' pli_violations <- violations_data$pli_violations
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
  
  pli_casefiles <- dat |>
    arrange(casefile_number, investigation_date) |>
    group_by(casefile = casefile_number) |>
    summarise(
      parid = first(parcel_id),
      case_start_date = min(investigation_date, na.rm = TRUE),
      last_inspection_date = max(investigation_date, na.rm = TRUE),
      last_inspection_outcome = last(investigation_outcome[!is.na(investigation_date)]),
      n_inspects = n_distinct(investigation_date[!is.na(investigation_date)]),
      n_violations = n_distinct(violation_code_section[!is.na(violation_code_section)]),
      violation_codes = paste(sort(unique(violation_code_section[!is.na(violation_code_section) & violation_code_section != ""])), collapse = "; "),
      casefile_status = last(status),
      has_court_case = any(!is.na(docket_number) & docket_number != ""),
      .groups = "drop"
    )

  pli_inspections <- dat |>
    filter(!is.na(investigation_outcome) | !is.na(investigation_findings)) |>
    distinct(casefile_number, parcel_id, investigation_date, investigation_outcome, investigation_findings) |>
    arrange(casefile_number, investigation_date) |>
    mutate(rowid = row_number()) |>
    select(
      rowid,
      casefile = casefile_number,
      parid = parcel_id,
      inspect_date = investigation_date,
      inspect_outcome = investigation_outcome,
      inspect_finding = investigation_findings
    )

  pli_violations <- dat |>
    filter(!is.na(violation_description) | !is.na(violation_code_section)) |>
    distinct(casefile_number, parcel_id, violation_description, violation_code_section, violation_spec_instructions) |>
    arrange(casefile_number) |>
    mutate(rowid = row_number()) |>
    select(
      rowid,
      casefile = casefile_number,
      parid = parcel_id,
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
    )

  pli_hearings <- court_dat |>
    arrange(docket_number, court_date) |>
    mutate(
      court_time = format(strptime(toupper(trimws(court_time)), "%I:%M%p"), "%H:%M"),
      court_decision = toupper(court_decision)
    ) |>
    select(
      docket_number,
      casefile = casefile_number,
      parid = parcel_id,
      court_date,
      court_time,
      court_decision
    )

  return(list(
    pli_casefiles = pli_casefiles,
    pli_inspections = pli_inspections,
    pli_violations = pli_violations,
    pli_hearings = pli_hearings
  ))
}
