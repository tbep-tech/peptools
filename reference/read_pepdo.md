# Import dissolved oxygen data

Import dissolved oxygen data

## Usage

``` r
read_pepdo(
  site = c("01304562", "01304200", "01304650"),
  nms = c("Peconic River", "Orient Harbor", "Shelter Island"),
  startDate = "2013-01-01",
  endDate = "2025-12-31"
)
```

## Arguments

- site:

  chr string of site numbers for USGS stations

- nms:

  chr string vector of names to reassign to site numbers

- startDate:

  chr string of start date in YYYY-MM-DD

- endDate:

  chr string of end date in YYYY-MM-DD, data for the entire day are
  included

## Value

data.frame

## Details

Raw data are downloaded from the USGS Water Data API using the
dataRetrieval R package, this function is a simple wrapper to the
[`read_waterdata_continuous`](https://rdrr.io/pkg/dataRetrieval/man/read_waterdata_continuous.html)
function. Start and end dates are for full days in Eastern Standard Time
(no daylight savings), consistent with the `DateTime` column in the
output.

Note that downloading the station data with the default arguments may
take several minutes. Sites are 01304562 for Peconic River, 01304200 for
Orient Harbor, 01304650 for Shelter Island.

The USGS Water Data API limits the number of requests per hour for users
without an API key. A key can be obtained from
<https://api.waterdata.usgs.gov/signup/> and set with
`Sys.setenv(API_USGS_PAT = "your_key")` if the limit is exceeded.

## Examples

``` r
if (FALSE) { # \dontrun{
dodat <- read_pepdo(site = c('01304562', '01304200', '01304650'),
  nms = c('Peconic River', 'Orient Harbor', 'Shelter Island'),
  startDate = '2020-06-01', endDate = '2021-06-30')
dodat
} # }
```
