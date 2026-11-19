# Import raw enterococcus data

Import raw enterococcus data

## Usage

``` r
read_pepent(path = NULL)
```

## Arguments

- path:

  chr string of path for excel file (optional)

## Value

data.frame

## Details

Data are from the ArcGIS REST Services here
<https://gis.suffolkcountyny.gov/hosted/rest/services/Hosted/Beach_Water_Quality_Data/FeatureServer/>.

The API is queried by beach names in the
[`beaches`](https://tbep-tech.github.io/peptools/reference/beaches.md)
data object. The queries are done individually for each beach to not
exceed the 2000 record limit.

Data can also be imported from an Excel if the `path` argument is used
for the location to the file.

## Examples

``` r
if (FALSE) { # \dontrun{
entdat <- read_pepent()
} # }
head(entdat)
#>                    Name FieldNum       Date value status
#> 1 Alberts Landing Beach      EH7 2010-06-29     4      =
#> 2 Alberts Landing Beach      EH7 2010-07-27     4      =
#> 3 Alberts Landing Beach      EH7 2010-08-13    28      =
#> 4 Alberts Landing Beach      EH7 2011-06-22     4      =
#> 5 Alberts Landing Beach      EH7 2011-07-20     4      =
#> 6 Alberts Landing Beach      EH7 2011-08-17     4      =
```
