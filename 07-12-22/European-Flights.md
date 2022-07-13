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

``` r
flights %>% 
  count(state_name) %>% 
  ggplot(aes(n, fct_reorder(state_name,n))) + geom_col() +
  scale_x_continuous(breaks = seq(0, 200000, 25000), labels = comma)
```

![](European-Flights_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

``` r
flights %>% 
  filter(state_name == "Greece") %>% 
  count(apt_name, sort = TRUE) %>% 
  ggplot(aes(n, fct_reorder(apt_name, n))) + geom_col()
```

![](European-Flights_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
flights %>% 
  filter(state_name == "Greece") %>% 
  group_by(apt_name) %>% 
  summarize("in" = sum(flt_arr_1),
            out = sum(flt_dep_1)) %>% 
  pivot_longer(-apt_name) %>% 
  ggplot(aes(value, fct_reorder(apt_name, value, max), fill = name)) + geom_col(position = "dodge") +
  labs(y = "") + scale_x_continuous(labels = comma)
```

![](European-Flights_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
flights %>% 
  filter(state_name == "Greece") %>% 
  group_by(month_mon) %>% 
  summarize(diff = sum(flt_arr_1) - sum(flt_dep_1)) %>% 
  left_join(flights %>% select(contains("month"))) %>% 
  ggplot(aes(diff, fct_reorder(month_mon, month_num))) + geom_col()
```

    ## Joining, by = "month_mon"

![](European-Flights_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->