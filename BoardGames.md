Boardgames
================
Matthew
1/25/2022

# EDA

## Check Data

``` r
games %>%
  select_if(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + geom_histogram() + facet_wrap(~key, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](BoardGames_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
games %>%
  count(mechanic, sort = TRUE)
```

    ## # A tibble: 8,292 x 2
    ##    mechanic                                           n
    ##    <chr>                                          <int>
    ##  1 <NA>                                            1590
    ##  2 ['Hand Management']                              428
    ##  3 ['Hexagon Grid']                                 389
    ##  4 ['Roll / Spin and Move']                         381
    ##  5 ['Dice Rolling']                                 365
    ##  6 ['Tile Placement']                               287
    ##  7 ['Dice Rolling', 'Hexagon Grid', 'Simulation']   264
    ##  8 ['Dice Rolling', 'Hexagon Grid']                 252
    ##  9 ['Set Collection']                               237
    ## 10 ['Hand Management', 'Set Collection']            180
    ## # ... with 8,282 more rows

## Facet numerics

### Playtime

``` r
games %>%
  group_by(name) %>%
  arrange(-average) %>%
  head(20) %>%
  pivot_longer(contains("playtime"), names_to = "key", values_to = "value") %>%
  ggplot(aes(average, fct_reorder(name, average), fill = factor(value))) + geom_col(color = "black") + facet_wrap(~key) +
  scale_fill_viridis_d(option = "magma") +
  labs(x = "Average Rating", y = "", title = "Top 20 Games") 
```

![](BoardGames_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Number of players

``` r
games %>%
  group_by(name) %>%
  arrange(-average) %>%
  head(20) %>%
  pivot_longer(contains("player"), names_to = "key", values_to = "value") %>%
  ggplot(aes(average, fct_reorder(name, average), fill = factor(value))) + geom_col(color = "black") + facet_wrap(~key) +
  scale_fill_viridis_d(option = "magma") +
  labs(x = "Average Rating", y = "", title = "Top 20 Games")
```

![](BoardGames_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Gplot column function for filling by numeric

``` r
Gplot <- function(x) {
  games %>%
    group_by(name) %>%
    arrange(-average) %>%
    head(20) %>%
    ggplot(aes(average, fct_reorder(name, average), fill = {{x}})) + geom_col(color = "black") +
    labs(x = "Average Rating", y = "", title = "Top 20 Games") 
}

Gplot(maxplayers)
```

![](BoardGames_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
Gplot(minplayers)
```

![](BoardGames_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

### Gplot2 point function of numerics and average

``` r
Gplot2 <- function(x) {
  games %>%
    filter({{x}} != 0) %>%
    ggplot(aes({{x}}, average)) + geom_point(alpha = 0.1) + scale_x_log10() + 
    geom_hline(yintercept = mean(games$average), lty = 3, size = 1.5, color = "red") +
    geom_smooth()
}

Gplot2(wanting)
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](BoardGames_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
Gplot2(wishing)
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](BoardGames_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
Gplot2(trading)
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](BoardGames_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
Gplot2(owned)
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](BoardGames_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

## Game Mechanics

``` r
games %>%
  unnest_tokens(word, mechanic) %>%
  group_by(word) %>%
  summarize(n = n(), mean = mean(average)) %>%
  arrange(-n) %>%
  filter(!word %in% c("game", "and", "player"), !is.na(word)) %>%
  head(30) %>%
  ggplot(aes(mean, fct_reorder(word, mean), fill = word)) + geom_col() + geom_vline(xintercept = mean(games$average), lty = 3) +
  theme(legend.position = "") + labs(y = "", x = "Average rating", title = "Average rating by most common game mechanics")
```

![](BoardGames_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Tidy mechanic column and furthur analysis

``` r
games %>%
  separate_rows(mechanic, sep = ",") %>%
  mutate(mechanic = str_replace_all(mechanic, "\\[|\\]", ""),
         mechanic = str_replace_all(mechanic, "\'", ""),
         mechanic = str_trim(mechanic, "both")) %>% 
  group_by(mechanic) %>%
  summarize(n = n(), average = mean(average)) %>%
  filter(n >100) %>%
  arrange(-average) %>%
  head(30) %>%
  ggplot(aes(average, fct_reorder(mechanic, average), fill = average)) + geom_col(color = "black") + 
  theme(legend.position = "") +
  geom_vline(xintercept = mean(games$average), lty = 3, color = "red") +
  labs(y = "", x = "", title = "Average rating by common mechanics", subtitle = "Highest 30 Mechanics",
       caption = "Red line indicates global average")
```

![](BoardGames_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
games %>%
  separate_rows(mechanic, sep = ",") %>%
  mutate(mechanic = str_replace_all(mechanic, "\\[|\\]", ""),
         mechanic = str_replace_all(mechanic, "\'", ""),
         mechanic = str_trim(mechanic, "both")) %>% 
  group_by(mechanic) %>%
  summarize(n = n(), average = mean(average)) %>%
  filter(n >100, !is.na(mechanic)) %>%
  arrange(average) %>%
  head(30) %>%
  ggplot(aes(average, fct_reorder(mechanic, average), fill = average)) + geom_col(color = "black") + 
  geom_vline(xintercept = mean(games$average), lty = 3, color = "red") +
  theme(legend.position = "") +
  labs(y = "", x = "", title = "Average rating by common mechanics", subtitle = "Lowest 30 Mechanics",
       caption = "Red line indicates global average")
```

![](BoardGames_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
games %>%
  separate_rows(mechanic, sep = ",") %>%
  mutate(mechanic = str_replace_all(mechanic, "\\[|\\]", ""),
         mechanic = str_replace_all(mechanic, "\'", ""),
         mechanic = str_trim(mechanic, "both")) %>% 
  group_by(mechanic) %>%
  summarize(n = n(), average = mean(average)) %>%
  filter(n >100, !is.na(mechanic)) %>%
  arrange(-average) %>%
  ggplot(aes(average, n)) + geom_text(aes(label = mechanic), check_overlap = TRUE)
```

![](BoardGames_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

## Game category

``` r
games %>%
  separate_rows(category, sep = ",") %>%
  mutate(category = str_replace_all(category, "\\[|\\]", ""),
         category = str_replace_all(category, "\'",""), 
         category = str_trim(category, "both")) %>%
  group_by(category) %>%
  drop_na() %>%
  summarize(n = n(), mean = mean(average)) %>%
  arrange(-mean) %>%
  filter(n > 25) %>%
  head(25) %>%
  ggplot(aes(mean, fct_reorder(category, mean), fill = mean)) + geom_col(color = "black") + 
  geom_vline(xintercept = mean(games$average), lty = 3, color = "red") +
  labs(y = "", x = "Average Rating", title = "Top 25 Categories", caption = "Only categories with more than 25 occurances") + 
  theme(legend.position = "")
```

![](BoardGames_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
games %>%
  separate_rows(category, sep = ",") %>%
  mutate(category = str_replace_all(category, "\\[|\\]", ""),
         category = str_replace_all(category, "\'",""), 
         category = str_trim(category, "both")) %>%
  group_by(category) %>%
  drop_na() %>%
  summarize(n = n(), mean = mean(average)) %>%
  arrange(mean) %>%
  filter(n > 10) %>%
  head(25) %>%
  ggplot(aes(mean, fct_reorder(category, mean), fill = mean)) + geom_col(color = "black") + 
  geom_vline(xintercept = mean(games$average), lty = 3, color = "red") +
  labs(y = "", x = "Average Rating", title = "Bottom 25 Categories", caption = "Only categories with more than 10 occurances") +
  theme(legend.position = "")
```

![](BoardGames_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

## Bayes Average and gplot function

``` r
gplot <- function(x,y) {
  games %>%
    filter({{x}} > 0) %>%
    group_by({{x}}) %>%
    summarize(y = mean({{y}})) %>%
    ggplot(aes({{x}}, y)) + geom_point()
}
gplot(wanting, bayes_average) + scale_x_log10()
```

![](BoardGames_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Gathered Bayes/Average and make another gplot function

``` r
gplot2 <- function(x) {
  games %>% 
    gather(key = key, value = value, bayes_average, average) %>%
    filter({{x}} > 0) %>%
    group_by({{x}}, key) %>%
    summarize(value = mean(value)) %>%
    ggplot(aes({{x}}, value)) + geom_point() + facet_wrap(~key)
}

gplot2(wanting) + scale_x_log10() + geom_smooth(method = "lm") + labs(title = "X is on a Log10 Scale")
```

    ## `summarise()` has grouped output by 'wanting'. You can override using the `.groups` argument.

    ## `geom_smooth()` using formula 'y ~ x'

![](BoardGames_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
gplot2(wanting) + scale_x_sqrt() + geom_smooth(method = "lm") + labs(title = "X is on a Sqrt Scale")
```

    ## `summarise()` has grouped output by 'wanting'. You can override using the `.groups` argument.
    ## `geom_smooth()` using formula 'y ~ x'

![](BoardGames_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

#### Quick lm models

``` r
lm(bayes_average ~ sqrt(wanting), games %>% filter(wanting >0)) %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = bayes_average ~ sqrt(wanting), data = games %>% 
    ##     filter(wanting > 0))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4285 -0.0707  0.0039  0.0635  1.3894 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   5.3569883  0.0016381  3270.3   <2e-16 ***
    ## sqrt(wanting) 0.0715120  0.0002412   296.5   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.162 on 19711 degrees of freedom
    ## Multiple R-squared:  0.8168, Adjusted R-squared:  0.8168 
    ## F-statistic: 8.79e+04 on 1 and 19711 DF,  p-value: < 2.2e-16

``` r
lm(average ~ log(wishing), games %>% filter(wishing >0)) %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = average ~ log(wishing), data = games %>% filter(wishing > 
    ##     0))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0085 -0.4565 -0.0583  0.4316  3.2637 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  5.191703   0.012817   405.1   <2e-16 ***
    ## log(wishing) 0.320822   0.003059   104.9   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7533 on 21520 degrees of freedom
    ## Multiple R-squared:  0.3382, Adjusted R-squared:  0.3382 
    ## F-statistic: 1.1e+04 on 1 and 21520 DF,  p-value: < 2.2e-16
