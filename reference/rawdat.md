# Raw water quality data from Suffolk County

Raw water quality data from Suffolk County

## Usage

``` r
rawdat
```

## Format

A `data.frame` object

## Examples

``` r
if (FALSE) { # \dontrun{
path <- system.file("extdata", "currentdata.xlsx", package="peptools")
rawdat <- read_pepwq(path)
save(rawdat, file = 'data/rawdat.RData', compress = 'xz')
} # }
```
