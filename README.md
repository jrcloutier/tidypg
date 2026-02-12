# tidypg

An R package for loading and tidying Allegheny County property data from the [Western Pennsylvania Regional Data Center (WPRDC)](https://www.wprdc.org/). Each loader pulls data from the WPRDC API, formats variables, standardizes municipality names, and returns a clean tibble ready for analysis.

## Installation

Install from GitHub using the `remotes` package:

```r
# install.packages("remotes")
remotes::install_github("jrcloutier/tidypg")
```

## Usage

```r
library(tidypg)

# Load current property assessments
assessments <- load_assessments()

# Load completed assessment appeals
appeals <- load_appeals()

# Load completed appeals from CSV (backup when the API is stale)
appeals <- load_appeals_backup()

# Load condemned properties
condemned <- load_condemned_props()

# Load Pittsburgh PLI code violations
violations <- load_pgh_violations()

# Load any WPRDC resource by ID
custom_data <- load_wprdc_resource("your-resource-id-here")

# Add parent municipality columns to your own data
df <- add_parent_muni(df, muni_name = "muni_name")

# Normalize city names (remove abbreviations, punctuation, etc.)
normalize_city(c("MT LEBANON TWP", "N. VERSAILLES BORO"))
```

## Functions

### Data loaders

- `load_assessments()` - Load current property assessments with values, sale history, and property characteristics
- `load_appeals()` - Load completed assessment appeals via the WPRDC API
- `load_appeals_backup()` - Load completed assessment appeals from a WPRDC CSV file (use when the API is not returning up-to-date records)
- `load_condemned_props()` - Load condemned properties
- `load_pgh_violations()` - Load Pittsburgh PLI code violations; returns a list with `casefiles` (aggregated by case) and `violations` (individual records)
- `load_wprdc_resource(resource_id)` - Generic loader for any WPRDC datastore resource with automatic pagination

### Helpers

- `add_parent_muni(df, muni_name, muni_code)` - Add parent municipality columns that roll up Pittsburgh, McKeesport, Clairton, and Duquesne wards to their parent city
- `normalize_city(x)` - Standardize city name strings (uppercase, remove abbreviations like TWP/BORO, fix common patterns)

## License

MIT
