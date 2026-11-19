# Create reactable table from matrix data

Create reactable table from matrix data

## Usage

``` r
show_reactablepep(totab, colfun, nrows = 10)
```

## Arguments

- totab:

  A data frame in wide format of summarized results

- colfun:

  Function specifying how colors are treated in cell background

- nrows:

  numeric specifying number of rows in the table

## Value

A
[`reactable`](https://glin.github.io/reactable/reference/reactable.html)
table

## Details

This function is used internally within
[`show_matrixpep`](https://tbep-tech.github.io/peptools/reference/show_matrixpep.md)
