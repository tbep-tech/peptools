# Count beach exceedances for enterococcus

Count beach exceedances for enterococcus

## Usage

``` r
anlz_entpep(entdat, thr = 104)
```

## Arguments

- entdat:

  result returned from
  [`read_pepent`](https://tbep-tech.github.io/peptools/reference/read_pepent.md)

- thr:

  numeric value defining threshold for exceedance

## Value

A `data.frame` with counts of exceedances per year for each beach

## Details

The exceedance threshold is set by default as 104 cfu/100 ml criterion.
This is simply based on counts in a year when any value at any station
was above the threshold for each 24 hour period in the record.

The `samples` column shows how many days of the year were sampled at
each beach and the `exceedances` column shows how many samples were
above the threshold.

## Examples

``` r
anlz_entpep(entdat)
#> # A tibble: 476 × 4
#>    Name               yr samples exceedances
#>    <chr>           <dbl>   <int>       <int>
#>  1 Alberts Landing  2010       3           0
#>  2 Alberts Landing  2011       3           0
#>  3 Alberts Landing  2012       4           0
#>  4 Alberts Landing  2013       3           0
#>  5 Alberts Landing  2014       4           0
#>  6 Alberts Landing  2015       3           0
#>  7 Alberts Landing  2016       5           0
#>  8 Alberts Landing  2017       3           0
#>  9 Alberts Landing  2018       6           1
#> 10 Alberts Landing  2019       3           0
#> # ℹ 466 more rows
```
