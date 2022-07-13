European Flights
================
Matthew
2022-07-13

``` r
flights <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')
```

    ## Rows: 688099 Columns: 14
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (6): MONTH_NUM, MONTH_MON, APT_ICAO, APT_NAME, STATE_NAME, Pivot Label
    ## dbl  (7): YEAR, FLT_DEP_1, FLT_ARR_1, FLT_TOT_1, FLT_DEP_IFR_2, FLT_ARR_IFR_...
    ## dttm (1): FLT_DATE
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
flights$MONTH_NUM <- as.numeric(flights$MONTH_NUM)

flights <- flights %>% 
  rename_with(tolower)
```

``` r
flights %>% 
  ggplot(aes(fct_reorder(month_mon, month_num))) + geom_bar() + labs(x = "")
```

![](European-Flights_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
flights %>% 
  ggplot(aes(year)) + geom_bar()
```

![](European-Flights_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->
