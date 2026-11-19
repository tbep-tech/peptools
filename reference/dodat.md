# Dissolved oxygen data for USGS stations

Dissolved oxygen data for USGS stations

## Usage

``` r
dodat
```

## Format

A `data.frame` object with columns for site name (`site`), date and time
in Eastern Standard Time (`DateTime`), and dissolved oxygen in mg/L
(`do_mgl`)

## Details

Data are from the USGS Water Data API, see
[`read_pepdo`](https://tbep-tech.github.io/peptools/reference/read_pepdo.md).

## Examples

``` r
if (FALSE) { # \dontrun{

# 01304562 is Peconic River, 01304200 is Orient Harbor, 01304650 is Shelter Island
dodat <- read_pepdo(site = c('01304562', '01304200', '01304650'),
  nms = c('Peconic River', 'Orient Harbor', 'Shelter Island'),
  startDate = '2013-01-01', endDate = '2025-12-31')

save(dodat, file = 'data/dodat.RData', compress = 'xz')

} # }
```
