# Estimate annual medians

Estimate annual medians by segment for chlorophyll, secchi, and total
nitrogen data

## Usage

``` r
anlz_medpep(dat)
```

## Arguments

- dat:

  `data.frame` formatted from
  [`read_pepwq`](https://tbep-tech.github.io/peptools/reference/read_pepwq.md)

## Value

Median estimates for chlorophyll, secchi, and total nitrogen

## Examples

``` r
# view median estimates
anlz_medpep(rawdat)
#> $ann
#> # A tibble: 1,332 × 5
#>    bay_segment    yr   val est    var  
#>    <fct>       <dbl> <dbl> <chr>  <chr>
#>  1 1a           2026  1.54 lwr.ci chla 
#>  2 1a           2026  1.78 medv   chla 
#>  3 1a           2026  7.81 upr.ci chla 
#>  4 1a           2025  3.32 lwr.ci chla 
#>  5 1a           2025  7.12 medv   chla 
#>  6 1a           2025 16.8  upr.ci chla 
#>  7 1a           2024  6.29 lwr.ci chla 
#>  8 1a           2024  8.39 medv   chla 
#>  9 1a           2024 11.9  upr.ci chla 
#> 10 1a           2023  6.6  lwr.ci chla 
#> # ℹ 1,322 more rows
#> 
#> $mos
#> # A tibble: 5,328 × 6
#>    bay_segment    yr    mo   val est   var  
#>    <fct>       <dbl> <dbl> <dbl> <chr> <chr>
#>  1 1a           2026    12 NA    medv  chla 
#>  2 1a           2026    11 NA    medv  chla 
#>  3 1a           2026    10 NA    medv  chla 
#>  4 1a           2026     9 NA    medv  chla 
#>  5 1a           2026     8 NA    medv  chla 
#>  6 1a           2026     7 NA    medv  chla 
#>  7 1a           2026     6 NA    medv  chla 
#>  8 1a           2026     5  8.06 medv  chla 
#>  9 1a           2026     4  1.72 medv  chla 
#> 10 1a           2026     3  1.54 medv  chla 
#> # ℹ 5,318 more rows
#> 
```
