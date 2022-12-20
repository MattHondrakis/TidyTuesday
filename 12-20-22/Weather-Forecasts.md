Weather Forcasts
================
Matthew
2022-12-20

-   <a href="#data-cleaning" id="toc-data-cleaning">Data Cleaning</a>
    -   <a href="#data-view" id="toc-data-view">Data View</a>

# Data Cleaning

``` r
weather_forecasts <- weather_forecasts %>% 
  left_join(outlook_meanings)  
```

    ## Joining, by = "forecast_outlook"

The *weather forecasts* dataset contains abbreviated weather outlooks,
while the *outlook meanings* dataset contains non-abbreviated weather
outlooks. The *weather forecasts* dataset and the *outlook meanings*
dataset were joined together using the “forecast_outlook” column as a
common key. This resulted in a combined dataset that includes both the
abbreviated and non-abbreviated weather outlooks from the two datasets.

``` r
weather_forecasts %>% 
  count(meaning, sort = TRUE)
```

    ## # A tibble: 24 x 2
    ##    meaning            n
    ##    <chr>          <int>
    ##  1 Sunny         173769
    ##  2 Partly Cloudy 104907
    ##  3 Rain Showers   95671
    ##  4 Mostly Cloudy  80178
    ##  5 Thunderstorms  72927
    ##  6 <NA>           37875
    ##  7 Rain           24198
    ##  8 Snow           14240
    ##  9 Windy          13368
    ## 10 Snow Showers    7924
    ## # ... with 14 more rows

## Data View

``` r
weather_forecasts %>% 
  head() %>% 
  knitr::kable()
```

| date       | city    | state | high_or_low | forecast_hours_before | observed_temp | forecast_temp | observed_precip | forecast_outlook | possible_error | meaning |
|:-----------|:--------|:------|:------------|----------------------:|--------------:|--------------:|----------------:|:-----------------|:---------------|:--------|
| 2021-01-30 | ABILENE | TX    | high        |                    48 |            70 |            NA |               0 | NA               | none           | NA      |
| 2021-01-30 | ABILENE | TX    | high        |                    36 |            70 |            NA |               0 | NA               | none           | NA      |
| 2021-01-30 | ABILENE | TX    | high        |                    24 |            70 |            NA |               0 | NA               | none           | NA      |
| 2021-01-30 | ABILENE | TX    | high        |                    12 |            70 |            70 |               0 | DUST             | none           | Dust    |
| 2021-01-30 | ABILENE | TX    | low         |                    48 |            42 |            NA |               0 | NA               | none           | NA      |
| 2021-01-30 | ABILENE | TX    | low         |                    36 |            42 |            NA |               0 | NA               | none           | NA      |
