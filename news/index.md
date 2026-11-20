# Changelog

## peptools 2.0.0

- Interactive maps now use leaflet instead of mapview. You can pick from
  three ESRI basemaps: World Gray (the default), World Imagery and World
  Topo Map. The mapview, httr and jsonlite dependencies were removed.
- Plotting functions now work with ggplot2 4.0, which is now the minimum
  required version. The `family` argument defaults to `""`, and line
  widths use `linewidth`.
- [`read_pepent()`](https://tbep-tech.github.io/peptools/reference/read_pepent.md)
  was rewritten for the new Suffolk County beach data download, which
  handles both the old and new file formats. Samples are now matched to
  beaches by station code. This fixes a bug where Huntington’s Crescent
  Beach records were mixed in with the Shelter Island beach.
- [`read_pepdo()`](https://tbep-tech.github.io/peptools/reference/read_pepdo.md)
  now uses the new USGS Water Data API through
  [`dataRetrieval::read_waterdata_continuous()`](https://rdrr.io/pkg/dataRetrieval/man/read_waterdata_continuous.html).
  The old NWIS service is being retired. Requests are made one year at a
  time to stay within the API’s query limits.
- The package datasets were updated. `rawdat` has water quality data
  through June 2026, `entdat` has beach pathogen data from 2010 through
  2026, and `dodat` has dissolved oxygen data through 2025.
- In the vignettes, the maps show bay segment and beach names when you
  hover over them. The examples now run through 2025.
- The package has a hex logo.

## peptools 1.2.0

- `ylim` argument added to
  [`show_thrpep()`](https://tbep-tech.github.io/peptools/reference/show_thrpep.md)
- Fixed bug with
  [`read_pepent()`](https://tbep-tech.github.io/peptools/reference/read_pepent.md)
  where path was sent to
  [`read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html)
  if `NULL`
- [`read_pepent()`](https://tbep-tech.github.io/peptools/reference/read_pepent.md)
  fix to filter to enterococcus, enterococci
- `rawdat`, `dodat`, `entdat` updated with 2024 data

## peptools 1.1.0

- TN added to water quality import and plotting functions
- Bay segmentation updated for 1a, 1b, 2, and 3 segments
- `dodat` updated with 2022 data
- [`read_pepent()`](https://tbep-tech.github.io/peptools/reference/read_pepent.md)
  now accepts an optional `path` to import external data file
- `"crescent beach - shelter island"` station is automatically changed
  to `"crescent beach - suffolk"` on data import for
  [`read_pepent()`](https://tbep-tech.github.io/peptools/reference/read_pepent.md)
- [`read_pepdo()`](https://tbep-tech.github.io/peptools/reference/read_pepdo.md)
  now imports DO data from Shelter Island Sound site
- [`show_domatrix()`](https://tbep-tech.github.io/peptools/reference/show_domatrix.md)
  now plots one site at a time

## peptools v1.0.0

- First release
