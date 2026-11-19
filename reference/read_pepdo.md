# Import dissolved oxygen data

Import dissolved oxygen data

## Usage

``` r
read_pepdo(
  site = c("01304562", "01304200", "01304650"),
  nms = c("Peconic River", "Orient Harbor", "Shelter Island"),
  startDate = "2013-01-01",
  endDate = "2024-12-31"
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

  chr string of end date in YYYY-MM-DD

## Value

data.frame

## Details

Raw data are downloaded using the USGS dataRetrieval R package, this
function is a simple wrapper to the
[`readNWISuv`](https://rdrr.io/pkg/dataRetrieval/man/readNWISuv.html)
function.

Note that downloading the station data with the default arguments may
take a few minutes. Sites are 01304562 for Peconic River, 01304200 for
Orient Harbor, 01304650 for Shelter Island.

## Examples

``` r
if (FALSE) { # \dontrun{
dodat <- read_pepdo(site = c('01304562', '01304200', '01304650'), 
  nms = c('Peconic River', 'Orient Harbor', 'Shelter Island'), 
  StartDate = '2020-06-01', endDate = '2021-06-30')
dodat
} # }
```
