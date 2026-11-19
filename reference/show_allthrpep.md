# Plot annual water quality value and thresholds for all segments

Plot annual water quality values and thresholds for all bay segments

## Usage

``` r
show_allthrpep(
  dat,
  param = c("chla", "sd", "tn"),
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

- param:

  chr string indicating which water quality value and appropriate
  threshold to plot, one of "chla" for chlorophyll, "sd" for secchi
  depth, or "tn" for total nitrogen

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

## Details

This function is conceptually similar to
[`show_thrpep`](https://tbep-tech.github.io/peptools/reference/show_thrpep.md),
but results are shown as annual medians across all bay segments for the
selected parameter.

## Examples

``` r
show_allthrpep(rawdat, param = 'chl')
```
