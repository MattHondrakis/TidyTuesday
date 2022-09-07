Pell Grant
================
Matthew
2022-08-30

-   <a href="#clean-data" id="toc-clean-data">Clean Data</a>
-   <a href="#eda" id="toc-eda">EDA</a>
    -   <a href="#map" id="toc-map">Map</a>
    -   <a href="#top-pell-grant-recipients"
        id="toc-top-pell-grant-recipients">Top Pell Grant Recipients</a>
    -   <a href="#pell-grants-per-recipients"
        id="toc-pell-grants-per-recipients">Pell Grants per Recipients</a>
    -   <a href="#award-by-recipient" id="toc-award-by-recipient">Award by
        recipient</a>
-   <a href="#model" id="toc-model">Model</a>
    -   <a href="#model-prediction" id="toc-model-prediction">Model
        Prediction</a>
    -   <a href="#map-1" id="toc-map-1">Map</a>

# Clean Data

``` r
names(pell) <- names(pell) %>% tolower()

tbjoin <- as_tibble(cbind(state_name = state.name, state = state.abb))

pell <- pell %>% 
  left_join(tbjoin)
```

    ## Joining, by = "state"

``` r
pell %>% filter(is.na(state_name)) %>% count(state, sort = TRUE)
```

    ## # A tibble: 9 x 2
    ##   state     n
    ##   <chr> <int>
    ## 1 PR     1925
    ## 2 DC      318
    ## 3 GU       56
    ## 4 VI       28
    ## 5 AS       19
    ## 6 FM       19
    ## 7 MH       19
    ## 8 MP       19
    ## 9 PW       19

``` r
pell <- pell %>% 
  mutate(state_name = case_when(state == "PR" ~ "Puerto Rico",
                                state == "DC" ~ "District of Columbia",
                                state == "GU" ~ "Guam",
                                state == "VI" ~ "Virgin Islands",
                                state == "AS" ~ "American Samoa",
                                state == "FM" ~ "Federated States of Micronesia",
                                state == "MH" ~ "Marshall Islands",
                                state == "MP" ~ "Northern Mariana Islands",
                                state == "PW" ~ "Palau",
                                TRUE ~ as.character(state_name)))
```

# EDA

``` r
pell %>% 
  ggplot(aes(award)) + geom_histogram() +
  scale_x_log10()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Pell-Grants_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
pell %>% 
  group_by(state_name) %>% 
  mutate(median = median(award, na.rm = TRUE)) %>% 
  filter(median >= 1920043) %>% 
  ggplot(aes(award, fct_reorder(state_name, median))) + geom_boxplot() +
  scale_x_log10(label = dollar) + labs(y = "State", x = "Pell Grant Award")
```

![](Pell-Grants_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
pell %>% 
  group_by(year) %>% 
  summarize(award = median(award, na.rm = TRUE)) %>% 
  ggplot(aes(year, award)) + geom_line()
```

![](Pell-Grants_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

## Map

``` r
plot_usmap(data = pell %>% 
             filter(year == max(year)) %>% 
             group_by(state) %>% 
             summarize(award = median(award, na.rm = TRUE)), 
           values = "award") +
  theme(legend.position = "right")
```

![](Pell-Grants_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Top Pell Grant Recipients

``` r
pell %>% 
  group_by(name, state_name) %>% 
  summarize(award = max(award)) %>% 
  head(10) %>% 
  mutate(name = str_remove_all(name, "\""),
         name = ifelse(grepl("Art Institute", name), 
                       "Art Institute of Pittsburgh", name)) %>% 
  ggplot(aes(award, fct_reorder(name, award))) + 
  geom_col(color = "black", aes(fill = state_name)) +
  scale_x_log10(labels = dollar, breaks = c(1e3, 1e5, 1e7)) + 
  ggthemes::theme_fivethirtyeight() +
  labs(fill = "")
```

    ## `summarise()` has grouped output by 'name'. You can override using the
    ## `.groups` argument.

![](Pell-Grants_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Pell Grants per Recipients

``` r
pell %>% 
  mutate(per = award/recipient) %>% 
  arrange(-per) %>% 
  head(10) %>% 
  ggplot(aes(per, fct_reorder(name, per))) + 
  geom_col(color = "black", aes(fill = state)) + 
  scale_x_log10() + theme_fivethirtyeight() +
  labs(fill = "", x = "Average award per recipient", 
       y = "School", title = "Top 10 Schools with Highest Pell Grants per Recipient")
```

![](Pell-Grants_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Award by recipient

``` r
pell %>% 
  group_by(recipient, state) %>% 
  summarize(m = median(award)) %>% 
  ggplot(aes(recipient, m)) + geom_line(aes(color = state)) +
  theme(legend.position = "none") + geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10))
```

    ## `summarise()` has grouped output by 'recipient'. You can override using the
    ## `.groups` argument.
    ## `geom_smooth()` using formula 'y ~ x'

![](Pell-Grants_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Model

``` r
model <- lm(log(award + 1) ~ log(recipient + 1), pell) 

model %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = log(award + 1) ~ log(recipient + 1), data = pell)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.3877  -0.2284   0.0282   0.2630   1.2466 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        7.7939042  0.0036888    2113   <2e-16 ***
    ## log(recipient + 1) 1.0278338  0.0006061    1696   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3343 on 100468 degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## Multiple R-squared:  0.9662, Adjusted R-squared:  0.9662 
    ## F-statistic: 2.876e+06 on 1 and 100468 DF,  p-value: < 2.2e-16

``` r
autoplot(model, which = 1:6, ncol = 3, label.size = 2)
```

![](Pell-Grants_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
model %>% broom::augment() %>% 
  arrange(desc(abs(.std.resid))) %>%
  group_by(`log(award + 1)`) %>% 
  summarize(sum = sum(.std.resid)) %>% 
  ggplot(aes(`log(award + 1)`, sum)) + geom_point(aes(size = abs(sum)))
```

![](Pell-Grants_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

The overwhelming majority of the error comes from instances where the
award is 0.

``` r
pell %>% filter(award == 0)
```

    ## # A tibble: 12 x 7
    ##    state award recipient name                           session  year state_name
    ##    <chr> <dbl>     <dbl> <chr>                          <chr>   <dbl> <chr>     
    ##  1 OH        0        32 Cuyahoga Community College - ~ 1999-00  1999 Ohio      
    ##  2 PA        0         1 North Montco Technical Career~ 2000-01  2000 Pennsylva~
    ##  3 TX        0         1 Bilingual Education Institute  2000-01  2000 Texas     
    ##  4 TX        0         2 Texas Chiropractic College     2000-01  2000 Texas     
    ##  5 CA        0         3 Advanced Career Technologies ~ 2002-03  2002 California
    ##  6 MA        0         2 Western Mass Precision Instit~ 2002-03  2002 Massachus~
    ##  7 NM        0         1 Eastern New Mexico University~ 2002-03  2002 New Mexico
    ##  8 NY        0         2 Briarcliffe College - Patchog~ 2002-03  2002 New York  
    ##  9 ME        0         1 Mercy Hospital School of Radi~ 2004-05  2004 Maine     
    ## 10 CA        0         0 Claremont Graduate University  2009-10  2009 California
    ## 11 NY        0         0 Bank Street College of Educat~ 2009-10  2009 New York  
    ## 12 NY        0         0 Teachers College, Columbia Un~ 2009-10  2009 New York

## Model Prediction

``` r
pell %>% 
  filter(award > 0) %>% 
  mutate(overbelow = 
           ifelse(log(award) > predict(model, pell %>% filter(award > 0)), 1, 0)) %>% 
  group_by(state) %>% 
  summarize(avg = mean(overbelow == 1)) %>% 
  arrange(-avg)
```

    ## # A tibble: 59 x 2
    ##    state   avg
    ##    <chr> <dbl>
    ##  1 FM    0.842
    ##  2 GU    0.679
    ##  3 ID    0.675
    ##  4 VI    0.643
    ##  5 PR    0.637
    ##  6 NY    0.596
    ##  7 WV    0.590
    ##  8 KY    0.564
    ##  9 MS    0.562
    ## 10 SC    0.562
    ## # ... with 49 more rows

``` r
pell %>% 
  filter(award > 0) %>% 
  mutate(overbelow = 
           ifelse(log(award) > predict(model, pell %>% filter(award > 0)), 1, 0)) %>% 
  group_by(state) %>% 
  summarize(avg = mean(overbelow == 1)) %>% 
  ungroup() %>% 
  summarize(m = mean(avg > 0.5))
```

    ## # A tibble: 1 x 1
    ##       m
    ##   <dbl>
    ## 1 0.712

71% of states received more than the model predicted using the number of
recipients as a predictor.

## Map

``` r
pell %>% 
  filter(award > 0) %>% 
  mutate(overbelow = 
           ifelse(log(award) > predict(model, pell %>% filter(award > 0)), 1, 0)) %>% 
  group_by(state) %>% 
  summarize(avg = mean(overbelow == 1)) %>% 
  plot_usmap(data = ., values = "avg") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0.5)
```

![](Pell-Grants_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
pell %>% 
  filter(award == 0) %>% 
  select(state_name, recipient, name) %>% 
  mutate(predicted = exp(predict(model, .))) %>% 
  arrange(-recipient)
```

    ## # A tibble: 12 x 4
    ##    state_name    recipient name                                        predicted
    ##    <chr>             <dbl> <chr>                                           <dbl>
    ##  1 Ohio                 32 Cuyahoga Community College - District Offi~    88233.
    ##  2 California            3 Advanced Career Technologies Institute         10085.
    ##  3 Texas                 2 Texas Chiropractic College                      7503.
    ##  4 Massachusetts         2 Western Mass Precision Institute                7503.
    ##  5 New York              2 Briarcliffe College - Patchogue                 7503.
    ##  6 Pennsylvania          1 North Montco Technical Career Center            4946.
    ##  7 Texas                 1 Bilingual Education Institute                   4946.
    ##  8 New Mexico            1 Eastern New Mexico University - Rowsell         4946.
    ##  9 Maine                 1 Mercy Hospital School of Radiologic Techno~     4946.
    ## 10 California            0 Claremont Graduate University                   2426.
    ## 11 New York              0 Bank Street College of Education                2426.
    ## 12 New York              0 Teachers College, Columbia University           2426.
