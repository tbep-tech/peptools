# Raw beach pathogen data from Suffolk County

Raw beach pathogen data from Suffolk County

## Usage

``` r
entdat
```

## Format

A `data.frame` object with columns for beach name (`Name`), station code
(`FieldNum`), sample date (`Date`), Enterococcus concentration as
cfu/100 mL (`value`), and whether the observation was above, below, or
at the reported value (`status` as `>`, `<`, or `=`)

## Details

Data are from the Suffolk County Department of Health Services Bathing
Beach Program, available at
<https://gis.suffolkcountyny.gov/portal/home/item.html?id=b5d1e5c88d8c4fb48a3732cd44cf4007>.
See
[`read_pepent`](https://tbep-tech.github.io/peptools/reference/read_pepent.md)
for details.

## Examples

``` r
if (FALSE) { # \dontrun{
entdat <- read_pepent()

save(entdat, file = 'data/entdat.RData', compress = 'xz')
} # }
```
