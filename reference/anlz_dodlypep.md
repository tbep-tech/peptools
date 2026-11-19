# Analyze daily DO values relative to threshold

Analyze daily DO values relative to threshold

## Usage

``` r
anlz_dodlypep(dodat, thr = 3, impute = TRUE)
```

## Arguments

- dodat:

  result returned from
  [`read_pepdo`](https://tbep-tech.github.io/peptools/reference/read_pepdo.md)

- thr:

  numeric indicating appropriate dissolved oxygen thresholds, usually 3
  mg/L for acute, 4.8 mg/L for chronic

- impute:

  logical indicating of missing dissolved oxygen values are imputed with
  the year, month, site average

## Value

data.frame

## Details

The [`dodat`](https://tbep-tech.github.io/peptools/reference/dodat.md)
data object can be used as input without downloading USGS data

If `impute = TRUE`, missing dissolved oxygen values in the complete
daily time series are imputed to the average for the year, month, site
combination. This is often necessary to create summary values that make
sense. For example, if a month has incomplete data, the maximum
`below_cumsum` value will not show 30 or 31 days even if every day in
the observed record is below the threshold.

## Examples

``` r
data(dodat)
dat <- anlz_dodlypep(dodat)
dat
#> # A tibble: 10,696 × 7
#>    site          Date          yr    mo do_mgl below below_cumsum
#>    <fct>         <date>     <dbl> <dbl>  <dbl> <dbl>        <dbl>
#>  1 Peconic River 2013-01-01  2013     1   11.0     0            0
#>  2 Peconic River 2013-01-02  2013     1   11.3     0            0
#>  3 Peconic River 2013-01-03  2013     1   11.6     0            0
#>  4 Peconic River 2013-01-04  2013     1   11.6     0            0
#>  5 Peconic River 2013-01-05  2013     1   11.7     0            0
#>  6 Peconic River 2013-01-06  2013     1   11.8     0            0
#>  7 Peconic River 2013-01-07  2013     1   12.0     0            0
#>  8 Peconic River 2013-01-08  2013     1   12.1     0            0
#>  9 Peconic River 2013-01-09  2013     1   12.0     0            0
#> 10 Peconic River 2013-01-10  2013     1   11.7     0            0
#> # ℹ 10,686 more rows
```
