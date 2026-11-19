# Bathing beaches

Bathing beaches

## Usage

``` r
beaches
```

## Format

A `data.frame` object

## Examples

``` r
if (FALSE) { # \dontrun{

library(sf)
library(dplyr)

beaches <- read.csv('inst/extdata/beaches.csv') %>% 
  st_as_sf(
     coords = c('Longitude', 'Latitude'), 
     crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
  )

save(beaches, file = 'data/beaches.RData')

} # }
```
