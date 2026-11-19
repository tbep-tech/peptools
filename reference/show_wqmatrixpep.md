# Create a colorized table for chlorophyll or secchi exceedances

Create a colorized table for chlorophyll or secchi exceedances

## Usage

``` r
show_wqmatrixpep(
  dat,
  param = c("chla", "sd"),
  txtsz = 3,
  trgs = NULL,
  yrrng = NULL,
  alpha = 1,
  bay_segment = c("1a", "1b", "2", "3"),
  asreact = FALSE,
  nrows = 10,
  family = ""
)
```

## Arguments

- dat:

  `data.frame` formatted from
  [`read_pepwq`](https://tbep-tech.github.io/peptools/reference/read_pepwq.md)

- param:

  chr string for which parameter to plot, one of `"chla"` for
  chlorophyll or `"sd"` for secchi

- txtsz:

  numeric for size of text in the plot, applies only if `tab = FALSE`

- trgs:

  optional `data.frame` for annual bay segment water quality targets,
  defaults to
  [`peptargets`](https://tbep-tech.github.io/peptools/reference/peptargets.md)

- yrrng:

  numeric vector indicating min, max years to include

- alpha:

  numeric indicating color transparency

- bay_segment:

  chr string for bay segments to include, one to all of "1a", "1b", "2",
  or "3"

- asreact:

  logical indicating if a
  [`reactable`](https://glin.github.io/reactable/reference/reactable.html)
  object is returned

- nrows:

  if `asreact = TRUE`, a numeric specifying number of rows in the table

- family:

  optional chr string indicating font family for text labels

## Value

A static [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object is returned if `asreact = FALSE`, otherwise a
[`reactable`](https://glin.github.io/reactable/reference/reactable.html)
table is returned

## See also

[`show_matrixpep`](https://tbep-tech.github.io/peptools/reference/show_matrixpep.md),
[`show_segmatrixpep`](https://tbep-tech.github.io/peptools/reference/show_segmatrixpep.md)

## Examples

``` r
show_wqmatrixpep(rawdat)
#> Warning: Removed 4 rows containing missing values or values outside the scale range
#> (`geom_text()`).
```
