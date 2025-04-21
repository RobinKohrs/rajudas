# Functions Good to know

## `geosphere_get_data`

-   Use the internal dataset `geosphere_bbox_austria` for the `bbox`-parameter for certain endpoints

**Beispiel**

> Alle monatlichen Sonnenscheindauer für Österreich herunterladen

``` r
# years -------------------------------------------------------------------
years = 1961:2023

walk(years, function(y) {
  op = makePath(here(glue("data_raw/geosphere/monthly/{y}.nc")))
  if(file.exists(op)){
    return()
  }
  start = glue("{y}-01-01")
  end = glue("{y}-12-31")

  rajudas::geosphere_get_data(
    ... = list(
      parameters = "SA",
      output_format = "netcdf",
      start = start,
      end = end,
      bbox = rajudas::geosphere_bbox_austria
    ),
    resource_id = "spartacus-v2-1m-1km",
    output_file = op
  )
})
```


> Regendaten am Stephansplatz für den Dezember 2023

```r
rajudas::geosphere_get_data(
  list(
    parameters = c("RR"),
    start = "2023-12-01",
    end = "2023-12-31",
    lat_lon = "48.20874412370146, 16.373161515740865"
  )type = "timeseries",
  mode = "historical",
  resource_id = "inca-v1-1h-1km"
)
```


