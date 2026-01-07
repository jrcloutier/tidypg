#' Load Pittsburgh code violations data
#'
#' Loads code violations data from WPRDC. Returns a list with two tibbles:
#' casefiles (aggregated by case) and violations (individual violation records).
#' The function removes records with missing parcel IDs, removes duplicates,
#' and structures the data into two related datasets.
#'
#' @return A list with two tibbles:
#'   \describe{
#'     \item{casefiles}{Aggregated data by casefile with case summary information}
#'     \item{violations}{Individual violation records linked to casefiles}
#'   }
#' @export
#' @importFrom dplyr mutate filter group_by across slice_max ungroup arrange summarise first last n_distinct if_else select
#' @importFrom lubridate ymd
#'
#' @examples
#' \dontrun{
#' violations_data <- load_pgh_violations()
#' casefiles <- violations_data$casefiles
#' violations <- violations_data$violations
#' }
load_pgh_violations <- function() {
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
      case_start_date = min(investigation_date),
      last_inspection_date = max(investigation_date),
      last_inspection_outcome = last(investigation_outcome),  # already sorted by date
      n_inspects = n_distinct(investigation_date),
      n_violations = n_distinct(violation_code_section),
      has_criminal_complaint = any(investigation_outcome == "ISSUE CRIMINAL COMPLAINT"),
      additional_parids = if_else(
        n_distinct(parcel_id) > 1,
        paste(setdiff(unique(parcel_id), first(parcel_id)), collapse = ", "),
        NA_character_
      ),
      .groups = "drop"
    )

  violations <- dat |>
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
  
  return(list(
    casefiles = casefiles,
    violations = violations
  ))
}