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
#> # A tibble: 1,260 × 5
#>    bay_segment    yr   val est    var  
#>    <fct>       <dbl> <dbl> <chr>  <chr>
#>  1 1a           2024  6.29 lwr.ci chla 
#>  2 1a           2024  8.39 medv   chla 
#>  3 1a           2024 11.9  upr.ci chla 
#>  4 1a           2023  6.6  lwr.ci chla 
#>  5 1a           2023  8.53 medv   chla 
#>  6 1a           2023 10.8  upr.ci chla 
#>  7 1a           2022  4.16 lwr.ci chla 
#>  8 1a           2022  7.53 medv   chla 
#>  9 1a           2022  9.97 upr.ci chla 
#> 10 1a           2021  8.4  lwr.ci chla 
#> # ℹ 1,250 more rows
#> 
#> $mos
#> # A tibble: 5,040 × 6
#>    bay_segment    yr    mo   val est   var  
#>    <fct>       <dbl> <dbl> <dbl> <chr> <chr>
#>  1 1a           2024    12  8.82 medv  chla 
#>  2 1a           2024    11 30.5  medv  chla 
#>  3 1a           2024    10 15.0  medv  chla 
#>  4 1a           2024     9 24.5  medv  chla 
#>  5 1a           2024     8 24.9  medv  chla 
#>  6 1a           2024     7 11.8  medv  chla 
#>  7 1a           2024     6 11.6  medv  chla 
#>  8 1a           2024     5  4.79 medv  chla 
#>  9 1a           2024     4  3.50 medv  chla 
#> 10 1a           2024     3  2.24 medv  chla 
#> # ℹ 5,030 more rows
#> 
```
