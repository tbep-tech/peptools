# Plot annual water quality value and thresholds for a segment

Plot annual water quality values and thresholds for a bay segment

## Usage

``` r
show_thrpep(
  dat,
  bay_segment = c("1a", "1b", "2", "3"),
  param = c("chla", "sd", "tn"),
  ylim = NULL,
  trgs = NULL,
  yrrng = NULL,
  family = "",
  labelexp = TRUE,
  txtlab = TRUE
)
```

## Arguments

- dat:

  data frame of data returned by
  [`read_pepwq`](https://tbep-tech.github.io/peptools/reference/read_pepwq.md)

- bay_segment:

  chr string for the bay segment, one of "1a", "1b", "2", or "3"

- param:

  chr string indicating which water quality value and appropriate
  threshold to plot, one of "chla" for chlorophyll, "sd" for secchi
  depth, or "tn" for total nitrogen

- ylim:

  numeric vector of length two indicating range for the y-axis

- trgs:

  optional `data.frame` for annual bay segment water quality thresholds,
  defaults to
  [`peptargets`](https://tbep-tech.github.io/peptools/reference/peptargets.md)

- yrrng:

  numeric vector indicating min, max years to include

- family:

  optional chr string indicating font family for text labels

- labelexp:

  logical indicating if y axis and target labels are plotted as
  expressions, default `TRUE`

- txtlab:

  logical indicating if a text label for the target value is shown in
  the plot

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object

## Examples

``` r
show_thrpep(rawdat, bay_segment = '1a', param = 'chl')
#> Warning: Removed 4 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 4 rows containing missing values or values outside the scale range
#> (`geom_line()`).
```
