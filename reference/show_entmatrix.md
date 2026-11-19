# Create a colorized table for beach pathogen exceedances

Create a colorized table for beach pathogen exceedances

## Usage

``` r
show_entmatrix(entdat, txtsz = 2, thr = 104, yrrng = NULL, family = "")
```

## Arguments

- entdat:

  data frame of enterococcus data returned by
  [`read_pepent`](https://tbep-tech.github.io/peptools/reference/read_pepent.md)

- txtsz:

  numeric for size of text in the plot, applies only if
  `asreact = FALSE`

- thr:

  numeric value defining threshold for exceedance

- yrrng:

  numeric vector indicating min, max years to include

- family:

  optional chr string indicating font family for text labels

## Value

A static [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object is returned if `asreact = FALSE`, otherwise a
[`reactable`](https://glin.github.io/reactable/reference/reactable.html)
table is returned

## See also

[`anlz_entpep`](https://tbep-tech.github.io/peptools/reference/anlz_entpep.md)

## Examples

``` r
show_entmatrix(entdat)
```
