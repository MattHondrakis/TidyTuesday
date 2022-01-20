Chocolate Bar Ratings
================
Matthew
1/18/2022

# EDA

## Plot of Numerics

``` r
chocolate %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + geom_histogram() + facet_wrap(~key, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 87 rows containing non-finite values (stat_bin).

![](Chocolate_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
chocolate %>%
  gather() %>%
  count(value, sort = TRUE)
```

    ## # A tibble: 5,433 x 2
    ##    value      n
    ##    <chr>  <int>
    ##  1 3       1546
    ##  2 U.S.A.  1169
    ##  3 70      1046
    ##  4 B,S,C    999
    ##  5 2        783
    ##  6 B,S      718
    ##  7 4        581
    ##  8 3.5      565
    ##  9 3.25     464
    ## 10 2.75     333
    ## # ... with 5,423 more rows

## Plot function

``` r
ratingplot <- function(x) {
  chocolate %>%
    group_by({{x}}) %>%
    summarize(n = mean(rating)) %>%
    arrange(-n) %>%
    head(10) %>%
    ggplot(aes(n, fct_reorder({{x}}, n))) + geom_col() + labs(x = "Average Rating")
}
ratingplot(company_manufacturer) + labs(y = "Manufacturer")
```

![](Chocolate_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ratingplot(company_location) + labs(y = "Location")
```

![](Chocolate_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
ratingplot(country_of_bean_origin) + labs(y = "Bean Origin")
```

![](Chocolate_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

## Rating by Year

``` r
chocolate %>%
  group_by(year) %>%
  summarize(n = mean(rating)) %>%
  ggplot(aes(year, n)) + geom_point() + geom_smooth() + ylim(ymin = 0, ymax = 5)
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Chocolate_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Rating by Cocoa Percent

``` r
chocolate %>%
  group_by(cocoa_percent) %>%
  summarize(n = mean(rating)) %>%
  ggplot(aes(cocoa_percent, n)) + geom_point() + geom_line() + 
  geom_smooth(se = FALSE) + labs(y = "Mean Rating", x = "Cocoa Percent")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Chocolate_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Rating by amount of ingredients

``` r
chocolate %>%
  drop_na() %>%
  group_by(ingredients_amount) %>%
  mutate(new = mean(rating)) %>%
  ggplot(aes(ingredients_amount, rating, group = ingredients_amount)) + geom_boxplot() + 
  geom_line(aes(x = ingredients_amount, y = new, group = 1), color = "blue") +
  geom_point(aes(x = ingredients_amount, y = new), color = "blue", size = 2) +
  labs(caption = "Blue line/dots represent the average per group")
```

![](Chocolate_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
chocolate %>%
  drop_na() %>%
  group_by(ingredients_amount) %>%
  summarize(Average = mean(rating)) %>%
  ggplot(aes(ingredients_amount, Average)) + geom_line() + labs(title = "Zoomed in")
```

![](Chocolate_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

### Ingredients Type

``` r
chocolate %>%
  drop_na() %>%
  group_by(ingredients_type) %>%
  summarize(Average = mean(rating)) %>%
  ggplot(aes(Average, fct_reorder(ingredients_type, Average), fill = Average)) + geom_col() +
  scale_fill_viridis_c(option = "H", direction = -1) +
  labs(title = "Average Rating of all Ingredients", 
       y = "Ingredients", x = "Average Rating")
```

![](Chocolate_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
chocolate %>%
  drop_na() %>%
  mutate(ingredients_type = fct_lump(ingredients_type, n = 5)) %>%
  group_by(ingredients_type) %>%
  summarize(Average = mean(rating)) %>%
  ggplot(aes(Average, fct_reorder(ingredients_type, Average))) + geom_col() + 
  labs(title = "Average Rating of Top 5 Ingredients", subtitle = "Ratings not in top 5 are collapsed as Other", 
       y = "Ingredients", x = "Average Rating")
```

![](Chocolate_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

## Type of Sugar

``` r
chocolate %>%
  drop_na() %>%
  mutate(sugar = str_detect(ingredients_type, "\\*"),
         sugar = case_when(sugar == FALSE ~ "Real Sugar",
                           sugar == TRUE ~ "Sweetener")) %>%
  ggplot(aes(sugar, rating)) + geom_boxplot() + 
  labs(y = "Rating", x = "Sugar", title = "Ratings separated by synthetic or real sugar",
       subtitle = "There is far more real sugar in the dataset, than sweetener substitutes", 
       caption = "Real Sugar: 2,367     Sweetener: 76")
```

![](Chocolate_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->