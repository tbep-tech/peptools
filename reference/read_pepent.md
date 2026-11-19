# Import raw enterococcus data

Import raw enterococcus data

## Usage

``` r
read_pepent(
  path = NULL,
  url =
    "https://gis.suffolkcountyny.gov/portal/sharing/rest/content/items/b5d1e5c88d8c4fb48a3732cd44cf4007/data"
)
```

## Arguments

- path:

  chr string of path to a local copy of the data (optional), see details

- url:

  chr string of url for downloading the data, used only if `path = NULL`

## Value

data.frame

## Details

Data are from the Suffolk County Department of Health Services (SCDHS)
Bathing Beach Program, available at
<https://gis.suffolkcountyny.gov/portal/home/item.html?id=b5d1e5c88d8c4fb48a3732cd44cf4007>.
The data are provided as a zip file that is downloaded to a temporary
directory if `path = NULL` (default). The zip file includes historical
data as a csv file, more recent data as one Excel file per year, and an
Excel file of beach sampling locations. The historical and recent data
files use different formats, both of which are handled by this function.

Alternatively, the `path` argument can point to a local copy of the zip
file, a folder with the extracted contents of the zip file, or a vector
of the individual data files. The sampling locations file must be
included if any of the data files use the recent format.

Records are matched to the beaches in the
[`beaches`](https://tbep-tech.github.io/peptools/reference/beaches.md)
data object by station code (`FieldNum`), not beach name. Beach names in
the source data have changed over time and are not unique between data
files (e.g., "Crescent Beach - Suffolk" is a Huntington beach in the
source data). Station codes for records in the recent format are
obtained from the sampling locations file using the EPA beach id. The
beach names in the output are those in
[`beaches`](https://tbep-tech.github.io/peptools/reference/beaches.md).
Only Enterococcus records are returned.

## Examples

``` r
if (FALSE) { # \dontrun{
entdat <- read_pepent()
} # }
head(entdat)
#>                    Name FieldNum       Date value status
#> 1 Alberts Landing Beach      EH7 2010-06-29     4      <
#> 2 Alberts Landing Beach      EH7 2010-07-27     4      <
#> 3 Alberts Landing Beach      EH7 2010-08-13    28      =
#> 4 Alberts Landing Beach      EH7 2011-06-22     4      <
#> 5 Alberts Landing Beach      EH7 2011-07-20     4      <
#> 6 Alberts Landing Beach      EH7 2011-08-17     4      <
```
