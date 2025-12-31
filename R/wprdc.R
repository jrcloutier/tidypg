#' Load data from WPRDC datastore API
#'
#' Generic function to load any dataset from the Western Pennsylvania Regional
#' Data Center (WPRDC) using the datastore dump endpoint. Returns a tibble with
#' cleaned column names.
#'
#' @param resource_id The WPRDC resource ID (found in the dataset URL)
#' @return A tibble with cleaned column names (all columns are character type)
#' @export
#' @importFrom readr read_csv cols
#' @importFrom janitor clean_names
#'
#' @examples
#' \dontrun{
#' # Load a custom WPRDC dataset
#' data <- load_wprdc_resource("your-resource-id-here")
#' }
load_wprdc_resource <- function(resource_id) {
  url <- paste0("https://data.wprdc.org/datastore/dump/", resource_id)
  read_csv(url, col_types = cols(.default = "c")) |>
    clean_names()
}
