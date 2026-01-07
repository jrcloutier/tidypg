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
  dat <- add_parent_muni(dat, muni_name = "muni_name")

  # Select and rename columns
  dat |>
    select(
      parid = parcel_id, tax_year, class, tax_status, muni_code, muni,
      parent_muni, parent_muni_code, school_code:as_of_date
    )
}

#' Load assessments
#'
#' Loads current property assessments from WPRDC. Returns a tibble
#' with formatted variables, standardized municipality information, and
#' selected relevant columns.
#'
#' @return A tibble of current property assessments with formatted dates, numeric values,
#'   and parent municipality columns
#' @export
#' @importFrom dplyr mutate across select if_else
#' @importFrom lubridate ymd dmy mdy
#' @importFrom stringr str_squish str_to_upper str_replace_all
#' @importFrom tidyr unite
#'
#' @examples
#' \dontrun{
#' completed <- load_assessments()
#' }

load_assessments <- function() {
  # Define resource ID for assessments dataset
  resource_id <- "9a1c60bd-f9f7-4aba-aeb7-af8c3aaa44e5"

  # Load data
  dat <- load_wprdc_resource(resource_id)

  # Format variables
  dat <- dat |>
    mutate(
      # Convert date columns to standard date format (they come as mm-dd-yyyy strings)
      record_date = mdy(recorddate),
      sale_date = mdy(saledate),
      prev_sale_date_1 = mdy(prevsaledate),
      prev_sale_date_2 = mdy(prevsaledate2),
      # asofdate might be numeric or in different format, try ymd first
      as_of_date = if_else(
        !is.na(suppressWarnings(as.numeric(asofdate))),
        as.Date(as.numeric(asofdate), origin = "1899-12-30"),
        ymd(asofdate)
      ),
      # Convert specified columns to numeric
      across(
        c(
          countyland, 
          countybuilding, 
          countyexemptbldg,
          countytotal, 
          localland, 
          localbuilding, 
          localtotal, 
          fairmarketland,
          fairmarketbuilding, 
          fairmarkettotal, 
          lotarea, 
          saleprice, 
          prevsaleprice,
          prevsaleprice2, 
          yearblt, 
          taxyear
        ),
        as.numeric
      ),
      across(
        c(
          totalrooms, 
          bedrooms, 
          fullbaths, 
          halfbaths, 
          fireplaces, 
          cardnumber,
          finishedlivingarea, 
          condition, 
          stories, 
        ),
        as.integer
      ),
      # Convert specified columns to boolean
      homestead_flag = if_else(!is.na(homesteadflag), TRUE, FALSE),
      farmstead_flag = if_else(!is.na(farmsteadflag), TRUE, FALSE),
      green_flag = if_else(!is.na(cleangreen), TRUE, FALSE),
      abatement_flag = if_else(!is.na(abatementflag), TRUE, FALSE)
    )

  # Normalize city names 
  dat <- mutate(dat, prop_city = normalize_city(propertycity))

  # Merge property address 
  dat <- dat |>
    # Unite street number
  unite(
    prop_num, 
    propertyhousenum, 
    propertyfraction, 
    remove = TRUE, 
    na.rm = TRUE, 
    sep = ""
    ) %>%
  # Unite street address
  unite(
    prop_str, 
    prop_num, 
    propertyaddress, 
    remove = TRUE, 
    na.rm = TRUE,
    sep = " "
    ) %>%
  # Unite full address
  unite(
    prop_addr, 
    prop_str, 
    propertyunit, 
    prop_city, 
    propertystate, 
    propertyzip, 
    remove = FALSE, 
    na.rm = TRUE, 
    sep = " "
  )

  # Add parent municipality
  dat <- add_parent_muni(dat, muni_name = "munidesc", muni_code = "municode")

  # Select and rename columns
  dat <- dat |>
    select(
      parid,
      prop_addr, 
      prop_str,
      prop_unit = propertyunit,
      prop_city,
      prop_state = propertystate,
      prop_zip = propertyzip,
      prop_class = classdesc,
      prop_type = usedesc,
      muni_code = municode,
      parent_muni,
      muni,
      school_code = schoolcode,
      school_district = schooldesc,
      neighborhood = neighdesc,
      homestead_flag,
      farmstead_flag,
      green_flag,
      abatement_flag,
      record_date, 
      sale_date,
      sale_price = saleprice,
      sale_desc = saledesc,
      deed_book = deedbook, 
      deed_page = deedpage, 
      tax_year = taxyear, 
      tax_code = taxcode,
      tax_desc = taxdesc,
      county_total = countytotal, 
      county_bldg = countybuilding, 
      county_land = countyland, 
      county_exempt_bldg = countyexemptbldg, 
      local_total = localtotal, 
      local_bldg = localbuilding, 
      local_land = localland, 
      fair_market_total = fairmarkettotal, 
      fair_market_bldg = fairmarketbuilding, 
      fair_market_land = fairmarketland,
      year_built = yearblt, 
      lot_area = lotarea,
      living_area = finishedlivingarea,
      grade, 
      grade_desc = gradedesc, 
      condition, 
      condition_desc = conditiondesc, 
      cdu,
      cdu_desc = cdudesc,
      rooms = totalrooms,
      bedrooms,
      full_baths = fullbaths,
      half_baths = halfbaths,
      heating_cooling = heatingcoolingdesc,
      as_of_date 
    ) |>
    mutate(
      # Remove extra spaces from character strings
      across(where(is.character), str_squish),
      # Generate URLs to online property assessment records
      assessment_url = paste0(
        "https://realestate.alleghenycounty.us/GeneralInfo?ID=", 
        parid
      )
    )
}
