San Francisco Rentals
================
Matthew
2022-07-06

``` r
rent <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')
```

    ## Rows: 200796 Columns: 17
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (8): post_id, nhood, city, county, address, title, descr, details
    ## dbl (9): date, year, price, beds, baths, sqft, room_in_apt, lat, lon
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
permits <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/sf_permits.csv')
```

    ## Rows: 86103 Columns: 44
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (19): permit_type_definition, block, lot, street_number_suffix, street_...
    ## dbl  (16): permit_number, permit_type, street_number, unit, number_of_existi...
    ## lgl   (1): tidf_compliance
    ## dttm  (7): permit_creation_date, status_date, filed_date, issued_date, compl...
    ## date  (1): date
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
new_construction <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/new_construction.csv')
```

    ## Rows: 261 Columns: 10
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (2): county, source
    ## dbl (6): cartodb_id, year, totalproduction, sfproduction, mfproduction, mhpr...
    ## lgl (2): the_geom, the_geom_webmercator
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
