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
         sugar = ifelse(sugar == FALSE, "Real Sugar", "Sweetener")) %>%
  ggplot(aes(sugar, rating)) + geom_boxplot() + 
  labs(y = "Rating", x = "Sugar", title = "Ratings separated by synthetic or real sugar",
       subtitle = "There is far more real sugar in the dataset, than sweetener substitutes", 
       caption = "Real Sugar: 2,367     Sweetener: 76")
```

![](Chocolate_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
chocolate %>%
  drop_na() %>%
  mutate(sugar = str_detect(ingredients_type, "\\*"),
         sugar = ifelse(sugar == FALSE, "Real Sugar", "Sweetener")) %>%
  ggplot(aes(rating, color = sugar)) + geom_density()
```

![](Chocolate_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

## Company location and bean origin

``` r
chocolate <- chocolate %>%
  mutate(location_origin = ifelse(company_location == country_of_bean_origin, "Same","Different"))


chocolate %>%
  ggplot(aes(location_origin, rating, group = location_origin)) + geom_boxplot() + 
  stat_summary(fun = mean, geom ="line", color = "blue", group = 1) +
  stat_summary(fun = mean, geom ="point", color = "blue")
```

![](Chocolate_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
chocolate %>%
  ggplot(aes(rating, color = location_origin)) + geom_density()
```

![](Chocolate_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

## Ratings by country = bean origin

``` r
chocolate %>%
  group_by(company_location, location_origin) %>%
  summarize(rating = mean(rating),
            n = n()) %>%
  filter(n > 20) %>%
  arrange(-rating) %>%
  head(11) %>%
  ggplot(aes(rating, reorder(company_location, rating, order = TRUE), fill = location_origin)) + geom_col(position = "identity") +
  labs(y = "", x = "Average Rating", title = "Top 11 Countries", 
       subtitle = "Only countries that received a total of at least 20 ratings", fill = "",
       caption = "Color indicates whether the beans come from that country")
```

    ## `summarise()` has grouped output by 'company_location'. You can override using the `.groups` argument.

![](Chocolate_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Using ggbarplot for previous plot

``` r
chocolate %>%
  group_by(company_location, location_origin) %>%
  summarize(rating = mean(rating),
            n = n()) %>%
  filter(n > 20) %>%
  arrange(-rating) %>%
  head(11) %>%
  ggbarplot("company_location", "rating", rotate = TRUE, fill = "location_origin", sort.val = "asc", sort.by.groups = FALSE) %>%
  ggpar(xlab = "", ylab = "Average Rating", title = "Top 11 Countries", font.ytickslab = 11,
       subtitle = "Only countries that received a total of at least 20 ratings", legend.title = "",
       caption = "Color indicates whether the beans come from that country", palette = "npg")
```

    ## `summarise()` has grouped output by 'company_location'. You can override using the `.groups` argument.

![](Chocolate_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
chocolate %>%
  group_by(company_manufacturer, location_origin) %>%
  summarize(rating = mean(rating),
            n = n()) %>%
  filter(n > 10) %>%
  arrange(-rating) %>%
  head(10) %>%
  ggplot(aes(rating, reorder(company_manufacturer, rating, order = TRUE), fill = location_origin)) + 
  geom_col() +
  labs(y = "", x = "Average Rating", title = "Top 10 Companies", fill = "",
       subtitle = "Only companies that received a total of at least 10 ratings",
       caption = "Color indicates whether the company and the beans are from the same country")
```

    ## `summarise()` has grouped output by 'company_manufacturer'. You can override using the `.groups` argument.

![](Chocolate_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Ratings per country

``` r
chocolate %>%
  group_by(country_of_bean_origin) %>%
  summarize(n = n()) %>%
  arrange(-n) %>%
  head(20) %>%
  ggbarplot(x = "country_of_bean_origin", y = "n", fill = "n", sort.val = "asc", 
            rotate = TRUE, ggtheme = theme_minimal(),
            ylab = "Number of ratings") %>%
  ggpar(legend = "", main = "Number of Ratings per Country's Bean Origin", xlab = "", ylab = "", font.ytickslab = c(10, "bold"))
```

![](Chocolate_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Number of variety of ingredients

``` r
chocolate %>%
  drop_na() %>%
  group_by(company_location) %>%
  distinct(ingredients_type) %>%
  summarize(n = n()) %>%
  arrange(-n) %>%
  head(15) %>%
  ggbarplot(x = "company_location", y = "n", fill = "n", rotate = TRUE, sort.val = "asc") %>%
  ggpar(xlab = "", ylab = "Number of different ingredient combinations", legend = "", 
        title = "Number of Different Chocolate Variations by Country")
```

![](Chocolate_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
