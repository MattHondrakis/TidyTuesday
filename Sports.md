Sports
================
Matthew
3/29/2022

``` r
sports <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')
```

    ## Rows: 132327 Columns: 28
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (8): institution_name, city_txt, state_cd, zip_text, classification_nam...
    ## dbl (20): year, unitid, classification_code, ef_male_count, ef_female_count,...
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

# EDA

``` r
sports %>%
  keep(is.numeric) %>%
  select(-year) %>%
  gather() %>%
  ggplot(aes(value)) + geom_histogram() + facet_wrap(~key, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Sports_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
sports %>%
  keep(is.numeric) %>%
  summarize_all(~ mean(is.na(.))) %>%
  select_if(. > 0) %>%
  gather() %>%
  arrange(-value)
```

    ## # A tibble: 10 x 2
    ##    key                value
    ##    <chr>              <dbl>
    ##  1 partic_coed_men    0.994
    ##  2 partic_coed_women  0.994
    ##  3 partic_men         0.532
    ##  4 rev_men            0.532
    ##  5 exp_men            0.532
    ##  6 rev_women          0.479
    ##  7 partic_women       0.479
    ##  8 exp_women          0.479
    ##  9 total_rev_menwomen 0.342
    ## 10 total_exp_menwomen 0.342

``` r
sports %>%
  select(partic_coed_men) %>%
  count(partic_coed_men, sort = TRUE)
```

    ## # A tibble: 52 x 2
    ##    partic_coed_men      n
    ##              <dbl>  <int>
    ##  1              NA 131560
    ##  2               1     65
    ##  3               3     65
    ##  4               2     61
    ##  5               6     54
    ##  6               5     48
    ##  7               8     48
    ##  8               7     46
    ##  9               4     43
    ## 10              12     34
    ## # ... with 42 more rows

``` r
sports %>% 
  filter(!is.na(total_exp_menwomen)) %>% 
  group_by(state_cd) %>%
  summarize(n = sum(total_exp_menwomen)/1e9) %>%
  arrange(-n) %>% 
  head(15) %>%
  ggplot(aes(n, fct_reorder(state_cd,n))) + geom_col() +
  labs(y = "", x = "Total Expenditure (in millions)", title = "Total Spent by State") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](Sports_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
