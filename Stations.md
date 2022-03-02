Stations
================
Matthew
2/28/2022

``` r
stations <- stations %>%
  select(where(~any(!is.na(.x)))) %>%
  rename_with(tolower) %>%
  select(-objectid) %>%
  mutate(open_date = as.Date(open_date))
```

``` r
stations %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + geom_histogram() + facet_wrap(~key, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Stations_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
stations %>%
  group_by(state) %>%
  count(fuel_type_code) %>%
  mutate(total = sum(n)) %>%
  filter(!is.na(n)) %>%
  ggplot(aes(n, y = fct_reorder(state, total), fill = fct_reorder(fuel_type_code, n))) +
  geom_col() + labs(y = "State", x = "Count", fill = "Fuel") +
  scale_fill_brewer(palette = "Dark2", direction = -1)
```

![](Stations_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

### Observing date 2021-01-07

``` r
stations %>%
  count(open_date, sort = TRUE) %>%
  filter(year(open_date) > 2010) %>%
  ggplot(aes(open_date, n)) + geom_line()
```

![](Stations_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
stations %>%
  filter(open_date == "2021-01-27") %>%
  count(state, sort = TRUE) %>%
  ggplot(aes(n, y = fct_reorder(state, n))) +
  geom_col() + labs(y = "State", x = "Count", fill = "Fuel")
```

![](Stations_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
stations %>%
  filter(!is.na(open_date)) %>%
  select(open_date, state) %>%
  mutate(key = ifelse(open_date == "2021-01-27", "Yes", "No")) %>%
  group_by(state,key) %>%
  count(state) %>%
  ggplot(aes(n, fct_reorder(state, n), fill = fct_rev(as.factor(key)))) + geom_col() +
  labs(fill = "2021-01-27", y = "") + scale_fill_hue(direction = -1)
```

![](Stations_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->
