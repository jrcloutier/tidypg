#' Load data from WPRDC datastore API
#'
#' Generic function to load any dataset from the Western Pennsylvania Regional
#' Data Center (WPRDC) using the CKAN datastore API. Automatically handles
#' pagination to retrieve all records. Returns a tibble with cleaned column names.
#'
#' @param resource_id The WPRDC resource ID (found in the dataset URL)
#' @param limit Number of records to fetch per request (default: 32000, API maximum)
#' @return A tibble with cleaned column names
#' @export
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows as_tibble
#' @importFrom janitor clean_names
#'
#' @examples
#' \dontrun{
#' # Load a custom WPRDC dataset
#' data <- load_wprdc_resource("your-resource-id-here")
#' }
load_wprdc_resource <- function(resource_id, limit = 32000) {
  base_url <- "https://data.wprdc.org/api/3/action/datastore_search"

  all_records <- list()
  offset <- 0

  repeat {
    # Build query URL with parameters
    response <- GET(
      base_url,
      query = list(
        resource_id = resource_id,
        limit = limit,
        offset = offset
      )
    )

    # Parse JSON response
    result <- content(response, as = "text", encoding = "UTF-8") |>
      fromJSON()

    # Check if request was successful
    if (!result$success) {
      stop("API request failed: ", result$error$message)
    }

    # Extract records
    records <- result$result$records

    # Break if no more records
    if (length(records) == 0 || nrow(records) == 0) {
      break
    }

    all_records[[length(all_records) + 1]] <- records

    # Break if we got fewer records than requested (last page)
    if (nrow(records) < limit) {
      break
    }

    offset <- offset + limit
  }

  # Combine all records and clean names
  bind_rows(all_records) |>
    as_tibble() |>
    clean_names()
}
