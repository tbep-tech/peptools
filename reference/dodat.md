# Dissolved oxygen data for USGS stations

Dissolved oxygen data for USGS stations

## Usage

``` r
dodat
```

## Format

A `data.frame` object

## Examples

``` r
if (FALSE) { # \dontrun{

# 01304562 is Peconic River, 01304200 is Orient Harbor, 01304650 is Shelter Island
dodat <- read_pepdo(site = c('01304562', '01304200', '01304650'), 
  nms = c('Peconic River', 'Orient Harbor', 'Shelter Island'))

save(dodat, file = 'data/dodat.RData', compress = 'xz')

} # }
```
