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
#' @importFrom httr2 request req_body_json req_perform resp_body_json resp_status
#' @importFrom dplyr bind_rows as_tibble
#' @importFrom janitor clean_names
#'
#' @examples
#' \dontrun{
#' # Load a custom WPRDC dataset
#' data <- load_wprdc_resource("your-resource-id-here")
#' }
load_wprdc_resource <- function(resource_id, limit = 32000) {
  base_url <- "https://data.wprdc.org/api/action/datastore_search"

  all_records <- list()
  offset <- 0

  repeat {
    response <- request(base_url) |>
      req_body_json(list(
        resource_id = resource_id,
        limit = limit,
        offset = offset
      )) |>
      req_perform()

    result <- resp_body_json(response, simplifyVector = TRUE)

    if (!isTRUE(result$success)) {
      status <- resp_status(response)
      error_msg <- if (is.list(result$error)) {
        msg <- result$error$message
        if (is.null(msg)) msg <- result$error[["__type"]]
        if (is.null(msg)) msg <- toString(names(result$error))
        msg
      } else {
        as.character(result$error)
      }
      stop("API request failed [HTTP ", status, "]: ", error_msg)
    }

    records <- result$result$records

    if (length(records) == 0 || nrow(records) == 0) {
      break
    }

    all_records[[length(all_records) + 1]] <- records

    if (nrow(records) < limit) {
      break
    }

    offset <- offset + limit
  }

  bind_rows(all_records) |>
    as_tibble() |>
    clean_names()
}
