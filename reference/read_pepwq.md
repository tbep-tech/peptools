# Import raw water quality data

Import raw water quality data

## Usage

``` r
read_pepwq(path)
```

## Arguments

- path:

  chr string of path for excel file

## Value

data.frame

## Details

Raw data from here
<https://gis.suffolkcountyny.gov/portal/home/item.html?id=5d4b53ec44204219a8da685f1859e096>

All data prior to 1990 are removed - some exist but the data are scarce.

## Examples

``` r
path <- system.file("extdata", "currentdata.xlsx", package="peptools")
dat <- read_pepwq(path)
dat
#> # A tibble: 40,564 × 11
#>    Date       BayStation name  value status    yr    mo StationName  bay_segment
#>    <date>     <chr>      <chr> <dbl> <chr>  <dbl> <dbl> <chr>        <fct>      
#>  1 1990-01-31 60113      chla    4.8 ""      1990     1 Little Peco… 2          
#>  2 1990-01-31 60113      sd      8   ""      1990     1 Little Peco… 2          
#>  3 1990-01-31 60114      chla    2.8 ""      1990     1 Paradise Po… 2          
#>  4 1990-01-31 60114      sd     12   ""      1990     1 Paradise Po… 2          
#>  5 1990-01-31 60115      chla    8.5 ""      1990     1 Orient Harb… 3          
#>  6 1990-01-31 60115      sd      7   ""      1990     1 Orient Harb… 3          
#>  7 1990-01-31 60116      chla    7.5 ""      1990     1 Gardiner's … 3          
#>  8 1990-01-31 60116      sd      7.5 ""      1990     1 Gardiner's … 3          
#>  9 1990-01-31 60118      chla    7.3 ""      1990     1 Northwest H… 2          
#> 10 1990-01-31 60118      sd      7   ""      1990     1 Northwest H… 2          
#> # ℹ 40,554 more rows
#> # ℹ 2 more variables: Longitude <dbl>, Latitude <dbl>
```
