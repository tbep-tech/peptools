# Analyze monthly DO values relative to threshold

Analyze monthly DO values relative to threshold

## Usage

``` r
anlz_domopep(dodat, thr = 3, impute = TRUE)
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

The date are summarized as three different values, where `do_mgl` is the
average of all daily DO averages across the month, `below_ave` is the
proportion of days in a month when concentrations in a given day fell
below the threshold (1 would mean all days had an instance of DO below
the threshold, 0 would mean none), and `below_maxrun` is the maximum
number of sequential days in a month when concentrations in a given day
fell below the threshold (30 or 31, depending on month, would indicate
all days in a month had an instance of DO below the threshold).

If `impute = TRUE`, missing dissolved oxygen values in the complete
daily time series are imputed to the average for the year, month, site
combination. This is often necessary to create summary values that make
sense. For example, if a month has incomplete data, the `below_ave`
summary may indicate a value of one if all daily averages in the
available data are below the threshold, whereas the `below_maxrun`
summary may indicate a maximum run of days not equal to the number of
days in the month.

## Examples

``` r
data(dodat)
dat <- anlz_domopep(dodat)
dat
#> # A tibble: 316 × 6
#>    site             yr mo    do_mgl below_ave below_maxrun
#>    <fct>         <dbl> <ord>  <dbl>     <dbl>        <dbl>
#>  1 Peconic River  2013 Jan    11.6      0                0
#>  2 Peconic River  2013 Feb    11.9      0                0
#>  3 Peconic River  2013 Mar    11.7      0                0
#>  4 Peconic River  2013 Apr     9.55     0                0
#>  5 Peconic River  2013 May     7.90     0                0
#>  6 Peconic River  2013 Jun     5.73     0.3              5
#>  7 Peconic River  2013 Jul     3.12     0.871           11
#>  8 Peconic River  2013 Aug     3.13     0.774           15
#>  9 Peconic River  2013 Sep     4.81     0.633            6
#> 10 Peconic River  2013 Oct     6.03     0.258            7
#> # ℹ 306 more rows
```
