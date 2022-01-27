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
gplot(wanting, bayes_average) + scale_x_sqrt()
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
    ggplot(aes({{x}}, value)) + geom_point(alpha = 0.3) + facet_wrap(~key)
}

gplot2(wishing) + scale_x_log10() + geom_smooth(method = "lm") + labs(title = "X is on a Log10 Scale")
```

    ## `summarise()` has grouped output by 'wishing'. You can override using the `.groups` argument.

    ## `geom_smooth()` using formula 'y ~ x'

![](BoardGames_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
gplot2(wishing) + scale_x_sqrt() + geom_smooth(method = "lm") + labs(title = "X is on a Sqrt Scale")
```

    ## `summarise()` has grouped output by 'wishing'. You can override using the `.groups` argument.
    ## `geom_smooth()` using formula 'y ~ x'

![](BoardGames_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

#### Quick lm models

``` r
lm(bayes_average ~ sqrt(wishing) * sqrt(wanting) + log(minplayers), games %>% filter(wishing >0, minplayers >0)) %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = bayes_average ~ sqrt(wishing) * sqrt(wanting) + 
    ##     log(minplayers), data = games %>% filter(wishing > 0, minplayers > 
    ##     0))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4731 -0.0587  0.0030  0.0570  1.1481 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  5.334e+00  2.801e-03 1904.10   <2e-16 ***
    ## sqrt(wishing)                2.044e-02  3.658e-04   55.88   <2e-16 ***
    ## sqrt(wanting)                2.724e-02  7.252e-04   37.56   <2e-16 ***
    ## log(minplayers)              5.194e-02  2.996e-03   17.34   <2e-16 ***
    ## sqrt(wishing):sqrt(wanting) -8.792e-05  8.110e-06  -10.84   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1467 on 21474 degrees of freedom
    ## Multiple R-squared:  0.8403, Adjusted R-squared:  0.8403 
    ## F-statistic: 2.825e+04 on 4 and 21474 DF,  p-value: < 2.2e-16

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

##### Plots of tidy lm models

``` r
lm(bayes_average ~ sqrt(wishing) * sqrt(wanting), games %>% filter(wishing >0)) %>% 
  tidy(conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(estimate, term, color = term)) + geom_point() + geom_errorbar(aes(xmin = conf.low, xmax = conf.high))
```

![](BoardGames_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
lm(bayes_average ~ ., games %>% select_if(is.numeric) %>% 
  filter(wishing >0)) %>% 
  tidy(conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>% 
  drop_na() %>%
  ggplot(aes(estimate, fct_reorder(term, estimate), color = term)) + 
  geom_point() + geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +
  theme(legend.position = "") + labs(y = "", title = "Linear model using only numeric estimates")
```

![](BoardGames_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
library(tidymodels)
```

    ## Registered S3 method overwritten by 'tune':
    ##   method                   from   
    ##   required_pkgs.model_spec parsnip

    ## -- Attaching packages -------------------------------------- tidymodels 0.1.4 --

    ## v broom        0.7.10     v rsample      0.1.1 
    ## v dials        0.0.10     v tune         0.1.6 
    ## v infer        1.0.0      v workflows    0.2.4 
    ## v modeldata    0.1.1      v workflowsets 0.1.0 
    ## v parsnip      0.1.7      v yardstick    0.0.9 
    ## v recipes      0.1.17

    ## -- Conflicts ----------------------------------------- tidymodels_conflicts() --
    ## x scales::discard() masks purrr::discard()
    ## x dplyr::filter()   masks stats::filter()
    ## x recipes::fixed()  masks stringr::fixed()
    ## x dplyr::lag()      masks stats::lag()
    ## x yardstick::spec() masks readr::spec()
    ## x recipes::step()   masks stats::step()
    ## * Learn how to get started at https://www.tidymodels.org/start/

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-38. For overview type 'help("mgcv-package")'.

``` r
games_split <- initial_split(games, strata = bayes_average)
games_train <- training(games_split)
games_test <- testing(games_split)


model <- lm(bayes_average ~ sqrt(wishing) * sqrt(wanting) + minplayers + average, games_train)

model %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = bayes_average ~ sqrt(wishing) * sqrt(wanting) + 
    ##     minplayers + average, data = games_train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.56488 -0.06045  0.00263  0.05631  1.14250 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  5.199e+00  9.372e-03 554.779  < 2e-16 ***
    ## sqrt(wishing)                1.952e-02  4.047e-04  48.236  < 2e-16 ***
    ## sqrt(wanting)                2.453e-02  8.059e-04  30.437  < 2e-16 ***
    ## minplayers                   2.338e-02  1.617e-03  14.461  < 2e-16 ***
    ## average                      2.163e-02  1.403e-03  15.416  < 2e-16 ***
    ## sqrt(wishing):sqrt(wanting) -4.550e-05  8.994e-06  -5.059 4.26e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1386 on 16215 degrees of freedom
    ## Multiple R-squared:  0.8553, Adjusted R-squared:  0.8552 
    ## F-statistic: 1.917e+04 on 5 and 16215 DF,  p-value: < 2.2e-16

``` r
games_test %>%
  mutate(predictions = predict(model, games_test),
         residuals = bayes_average - predictions) %>%
  ggplot(aes(predictions, residuals)) + geom_point(alpha = 0.1) + 
  geom_hline(yintercept = 0, lty = 2, color = "blue", size = 1) +
  labs(title = "Residual plot for first lm")
```

![](BoardGames_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
games_test %>%
  mutate(predictions = predict(model, games_test)) %>%
  group_by(wishing) %>%
  summarize(bayes_average = mean(bayes_average),
            predictions = mean(predictions)) %>%
  ggplot(aes(wishing)) + geom_point(aes(y = bayes_average), alpha = 0.3) + 
  geom_point(aes(y = predictions), color = "blue", alpha = 0.3) + scale_x_sqrt()
```

![](BoardGames_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

``` r
games_test %>%
  mutate(predictions = predict(model, games_test)) %>%
  ggplot(aes(predictions, bayes_average)) + geom_point(alpha = 0.07) + 
  geom_abline(color = "blue", linetype = "dashed", size = 1) +
  xlim(5.5,8.5) + ylim(5.5,8.5)
```

![](BoardGames_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->

##### Gam model

``` r
model2 <- gam(bayes_average ~ s(wishing), family = gaussian, data = games_train)

model2 %>% summary()
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## bayes_average ~ s(wishing)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 5.684313   0.001076    5285   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##             edf Ref.df     F p-value    
    ## s(wishing) 8.94  8.999 10948  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.859   Deviance explained = 85.9%
    ## GCV = 0.018778  Scale est. = 0.018766  n = 16221

``` r
games_test %>%
  mutate(predictions = predict(model2, games_test),
         residuals = bayes_average - predictions) %>%
  ggplot(aes(predictions, residuals)) + geom_point(alpha = 0.1) + 
  geom_hline(yintercept = 0, lty = 2, color = "blue", size = 1) +
  labs(title = "Residual plot for first Gam")
```

![](BoardGames_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
games_test %>%
  mutate(predictions = predict(model2, games_test)) %>%
  group_by(wishing) %>%
  summarize(bayes_average = mean(bayes_average),
            predictions = mean(predictions)) %>%
  ggplot(aes(wishing)) + geom_point(aes(y = bayes_average), alpha = 0.3) + 
  geom_point(aes(y = predictions), color = "blue", alpha = 0.3) + scale_x_sqrt()
```

![](BoardGames_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
games_test %>%
  mutate(predictions = predict(model2, games_test)) %>%
  ggplot(aes(predictions, bayes_average)) + geom_point(alpha = 0.07) + 
  geom_abline(color = "blue", linetype = "dashed", size = 1) +
  xlim(5.5,8.5) + ylim(5.5,8.5)
```

![](BoardGames_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->

##### Both models

``` r
games_test %>%
  mutate(predictions = predict(model2, games_test),
         model = "2") %>%
  bind_rows(games_test %>%
              mutate(predictions = predict(model, games_test),
                     model = "1")) %>%
  group_by(wishing, model) %>%
  summarize(bayes_average = mean(bayes_average),
            predictions = mean(predictions)) %>%
  ggplot(aes(wishing)) + geom_point(aes(y = bayes_average), alpha = 0.3) + 
  geom_point(aes(y = predictions, color = model), alpha = 0.3) + scale_x_sqrt() +
  labs(title = "Both models fitted", subtitle = "Square root scale")
```

    ## `summarise()` has grouped output by 'wishing'. You can override using the `.groups` argument.

![](BoardGames_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
