Solar and Wind
================
Matthew
5/3/2022

``` r
capacity <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/capacity.csv')
```

    ## Rows: 49 Columns: 7
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (1): type
    ## dbl (6): year, standalone_prior, hybrid_prior, standalone_new, hybrid_new, t...
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
wind <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/wind.csv')
```

    ## Rows: 328 Columns: 3
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## dbl  (2): wind_mwh, wind_capacity
    ## date (1): date
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
solar <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/solar.csv')
```

    ## Rows: 328 Columns: 3
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## dbl  (2): solar_mwh, solar_capacity
    ## date (1): date
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
average_cost <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/average_cost.csv')
```

    ## Rows: 13 Columns: 4
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## dbl (4): year, gas_mwh, solar_mwh, wind_mwh
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

# EDA

``` r
(average_cost %>% 
  pivot_longer(-year) %>% 
  ggplot(aes(year, value, color = name)) + geom_line() +
  labs(y = "", title = "Actual Values") + scale_y_continuous(labels = scales::dollar)) +
(average_cost %>% 
  pivot_longer(-year) %>% 
  ggplot(aes(year, value, color = name)) + geom_smooth(method = "lm", se = FALSE) +
  labs(y = "", title = "Linear Trend") + scale_y_continuous(labels = scales::dollar) +
  theme(plot.title = element_text(hjust = 1))) +
  plot_layout(guides = 'collect') + 
  plot_annotation(title = "Price per Energy", 
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 18)))
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Solar-and-Wind_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
