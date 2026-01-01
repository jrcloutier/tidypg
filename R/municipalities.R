#' Add parent municipality columns
#'
#' Adds parent_muni and parent_muni_code columns based on ward patterns
#' in municipality names. Handles Pittsburgh, McKeesport, Clairton, and
#' Duquesne ward subdivisions, which appear in Allegheny County data as
#' "WARD - CITY" format.
#'
#' @param df A dataframe with municipality column and muni_code column
#' @param muni_col Name of the municipality column (default: "muni_name")
#' @return The dataframe with muni, parent_muni, and parent_muni_code columns added
#' @export
#' @importFrom dplyr mutate case_when
#' @importFrom stringr str_squish str_to_upper str_detect
#'
#' @examples
#' \dontrun{
#' # Add parent municipality to a dataframe
#' df <- add_parent_muni(df, muni_col = "muni_name")
#'
#' # Works with different column names
#' df <- add_parent_muni(df, muni_col = "municipality")
#' }
add_parent_muni <- function(df, muni_col = "muni_name") {
  df |>
    mutate(
      muni_code = as.character(muni_code),
      muni = str_squish(str_to_upper(.data[[muni_col]])),
      parent_muni = case_when(
        str_detect(muni, "- PITTSBURGH") ~ "PITTSBURGH",
        str_detect(muni, "- MCKEESPORT") ~ "MCKEESPORT",
        str_detect(muni, "- CLAIRTON") ~ "CLAIRTON",
        str_detect(muni, "- DUQUESNE") ~ "DUQUESNE",
        TRUE ~ muni
      ),
      parent_muni_code = case_when(
        parent_muni == "PITTSBURGH" ~ "100",
        parent_muni == "MCKEESPORT" ~ "400",
        parent_muni == "CLAIRTON" ~ "200",
        parent_muni == "DUQUESNE" ~ "300",
        TRUE ~ muni_code
      )
    )
}
