# Plot monthly chlorophyll, secchi, or tn values values for a segment

Plot monthly chlorophyl, secchi, or tn values values for a bay segment

## Usage

``` r
show_boxpep(
  dat,
  param = c("chla", "sd", "tn"),
  yrsel = NULL,
  yrrng = NULL,
  ptsz = 0.5,
  bay_segment = c("1a", "1b", "2", "3"),
  trgs = NULL,
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

- yrsel:

  numeric for year to emphasize, shown as separate red points on the
  plot

- yrrng:

  numeric vector indicating min, max years to include

- ptsz:

  numeric indicating point size of observations not in `yrsel`

- bay_segment:

  chr string for the bay segment, one of "1a", "1b", "2", or "3"

- trgs:

  optional `data.frame` for annual bay segment water quality targets,
  defaults to
  [`peptargets`](https://tbep-tech.github.io/peptools/reference/peptargets.md)

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

Points not included in `yrsel` are plotted over the box plots using
[`position_jitter`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
Use `ptsz = -1` to suppress.

## Examples

``` r
show_boxpep(rawdat, bay_segment = '1a')
#> Warning: Removed 102 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).
#> Warning: Removed 102 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 9 rows containing missing values or values outside the scale range
#> (`geom_point()`).
```
