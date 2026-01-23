# About

A collection of helper functions for loading Allegheny County property assessment data from the Western Pennsylvania Regional Data Center (WPRDC) APIs.

## Installation

Install from GitHub using the `remotes` package:

```r
# Install remotes if you don't have it
install.packages("remotes")

# Install tidypg
remotes::install_github("yourusername/tidypg")
```

## Usage

```r
library(tidypg)

# Load condemned properties
condemned <- load_condemned_props()

# Load completed assessment appeals
completed <- load_completed_appeals()

# Load filed assessment appeals
filed <- load_filed_appeals()

# Load any WPRDC resource by ID
custom_data <- load_wprdc_resource("your-resource-id-here")

# Add parent municipality columns to your own data
df <- add_parent_muni(df, muni_col = "municipality")
```

## Functions

- `load_wprdc_resource(resource_id)` - Generic loader for any WPRDC datastore resource
- `load_condemned_props()` - Load condemned properties dataset
- `load_completed_appeals()` - Load completed assessment appeals
- `load_filed_appeals()` - Load filed assessment appeals
- `add_parent_muni(df, muni_col)` - Add parent municipality columns for Pittsburgh, McKeesport, Clairton, and Duquesne wards

## Data Sources

All data comes from the [Western Pennsylvania Regional Data Center](https://www.wprdc.org/).

## License

MIT
