# Bay segment targets

Bay segment targets

## Usage

``` r
peptargets
```

## Format

A `data.frame` object

## Examples

``` r
if (FALSE) { # \dontrun{
peptargets <- tibble::tibble(
  bay_segment = factor(c('1a', '1b', '2', '3'), 
    levels = c('1a', '1b', '2', '3')), 
  name = factor(c('1a', '1b', '2', '3'), 
    levels = c('1a', '1b', '2', '3')), 
  sd_thresh = c(6.5, 6.5, 6.5, 6.5),
  chla_thresh = c(5.5, 5.5, 5.5, 5.5), 
  tn_thresh = c(0.4, 0.4, 0.4, 0.4)
)
save(peptargets, file = 'data/peptargets.RData', compress= 'xz')
} # }
```
