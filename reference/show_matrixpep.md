# Create a colorized table for indicator reporting

Create a colorized table for indicator reporting

## Usage

``` r
show_matrixpep(
  dat,
  txtsz = 3,
  trgs = NULL,
  yrrng = NULL,
  bay_segment = c("1a", "1b", "2", "3"),
  asreact = FALSE,
  nrows = 10,
  abbrev = FALSE,
  family = ""
)
```

## Arguments

- dat:

  data frame of water quality data returned by
  [`read_pepwq`](https://tbep-tech.github.io/peptools/reference/read_pepwq.md)

- txtsz:

  numeric for size of text in the plot, applies only if
  `asreact = FALSE`

- trgs:

  optional `data.frame` for annual bay segment water quality targets,
  defaults to
  [`peptargets`](https://tbep-tech.github.io/peptools/reference/peptargets.md)

- yrrng:

  numeric vector indicating min, max years to include

- bay_segment:

  chr string for bay segments to include, one to all of "1a", "1b", "2",
  or "3"

- asreact:

  logical indicating if a
  [`reactable`](https://glin.github.io/reactable/reference/reactable.html)
  object is returned

- nrows:

  if `asreact = TRUE`, a numeric specifying number of rows in the table

- abbrev:

  logical indicating if text labels in the plot are abbreviated as the
  first letter

- family:

  optional chr string indicating font family for text labels

## Value

A static [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object is returned if `asreact = FALSE`, otherwise a
[`reactable`](https://glin.github.io/reactable/reference/reactable.html)
table is returned

## Examples

``` r
show_matrixpep(rawdat)
#> Warning: Removed 6 rows containing missing values or values outside the scale range
#> (`geom_text()`).
```
