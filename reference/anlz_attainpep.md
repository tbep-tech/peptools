# Get attainment categories

Get attainment categories for each year and bay segment using
chlorophyll and secchi depth

## Usage

``` r
anlz_attainpep(meddat, magdurout = FALSE, trgs = NULL)
```

## Arguments

- meddat:

  result returned from
  [`anlz_medpep`](https://tbep-tech.github.io/peptools/reference/anlz_medpep.md)

- magdurout:

  logical indicating if the separate magnitude and duration estimates
  are returned

- trgs:

  optional `data.frame` for annual bay segment water quality targets,
  defaults to
  [`peptargets`](https://tbep-tech.github.io/peptools/reference/peptargets.md)

## Value

A `data.frame` for each year and bay segment showing the attainment
category

## Examples

``` r
meddat <- anlz_medpep(rawdat)
anlz_attainpep(meddat)
#> # A tibble: 148 × 4
#>    bay_segment    yr chla_sd outcome
#>    <fct>       <dbl> <chr>   <chr>  
#>  1 1a           2026 2_3     red    
#>  2 1a           2025 2_3     red    
#>  3 1a           2024 3_3     red    
#>  4 1a           2023 3_3     red    
#>  5 1a           2022 2_2     red    
#>  6 1a           2021 3_2     red    
#>  7 1a           2020 2_NA    NA     
#>  8 1a           2019 2_3     red    
#>  9 1a           2018 2_3     red    
#> 10 1a           2017 3_3     red    
#> # ℹ 138 more rows
```
