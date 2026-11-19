# Create a colorized table for water quality outcomes and exceedances by segment

Create a colorized table for water quality outcomes by segment that
includes the management action and chlorophyll, and secchi exceedances

## Usage

``` r
show_segmatrixpep(
  dat,
  txtsz = 3,
  trgs = NULL,
  yrrng = NULL,
  bay_segment = c("1a", "1b", "2", "3"),
  abbrev = FALSE,
  family = ""
)
```

## Arguments

- dat:

  `data.frame` formatted from
  [`read_pepwq`](https://tbep-tech.github.io/peptools/reference/read_pepwq.md)

- txtsz:

  numeric for size of text in the plot, applies only if `tab = FALSE`

- trgs:

  optional `data.frame` for annual bay segment water quality targets,
  defaults to
  [`peptargets`](https://tbep-tech.github.io/peptools/reference/peptargets.md)

- yrrng:

  numeric vector indicating min, max years to include

- bay_segment:

  chr string for bay segments to include, one to all of "1a", "1b", "2",
  or "3"

- abbrev:

  logical indicating if text labels in the plot are abbreviated as the
  first letter, applies only to center column

- family:

  optional chr string indicating font family for text labels

## Value

A static [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object is returned

## Details

This function provides a combined output for the
[`show_wqmatrixpep`](https://tbep-tech.github.io/peptools/reference/show_wqmatrixpep.md)
and
[`show_matrixpep`](https://tbep-tech.github.io/peptools/reference/show_matrixpep.md)
functions. Only one bay segment can be plotted for each function call.

## See also

[`show_wqmatrixpep`](https://tbep-tech.github.io/peptools/reference/show_wqmatrixpep.md),
[`show_matrixpep`](https://tbep-tech.github.io/peptools/reference/show_matrixpep.md)

## Examples

``` r
show_segmatrixpep(rawdat, bay_segment = '1a')
#> Warning: Removed 16 rows containing missing values or values outside the scale range
#> (`geom_text()`).
```
