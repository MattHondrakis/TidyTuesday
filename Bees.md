---
title: "Bee Colony"
author: "Matthew"
date: "1/11/2022"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
editor_options:
  chunk_output_type: console
---




# Explore


```r
beecolony %>%
  select(where(is.numeric), -year) %>%
  gather() %>%
  ggplot(aes(value)) + geom_histogram() + scale_x_log10(labels = scales::comma) + facet_wrap(~key, scales = "free", ncol = 3)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Bees_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
beecolony %>%
  arrange(-colony_added) %>%
  select(colony_added)
```

```
## # A tibble: 685 x 1
##    colony_added
##           <dbl>
##  1       250000
##  2       240000
##  3       240000
##  4       215000
##  5       210000
##  6       200000
##  7       200000
##  8       184000
##  9       176000
## 10       175000
## # ... with 675 more rows
```

## Colony_* Function and plots

```r
gplot <- function(x){
  colony %>%
    filter(!is.na({{x}})) %>%
    mutate(across(where(is.character), as.factor)) %>%
    distinct(months, year, state, {{x}}) %>%
    ggplot(aes({{x}}, reorder(state, {{x}}, order = TRUE))) + geom_col() + labs(y = "")
}

gplot(colony_n)
```

![](Bees_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


## Stressors

```r
levels <- c("Disesases", "Unknown", "Other", "Pesticides", "Other pests/parasites", "Varroa mites")

stressor %>%
  drop_na() %>%
  mutate(stressor = as.factor(stressor), stressor = fct_relevel(stressor, levels = levels)) %>%
  group_by(year, stressor) %>%
  summarize(n = mean(stress_pct)) %>%
  ggplot(aes(year, n, fill = stressor)) + geom_col(position = "dodge") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1))
```

```
## Warning: Outer names are only allowed for unnamed scalar atomic inputs
```

```
## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.
```

![](Bees_files/figure-html/unnamed-chunk-3-1.png)<!-- -->



```r
beecolony %>%
  mutate(months = as.factor(months), 
         months = fct_relevel(months, c("January-March", "April-June", 
                                        "July-September", "October-December"))) %>%
  group_by(months) %>%
  summarize(n = mean(`Varroa mites`)) %>%
  ggplot(aes(months, n, fill = months)) + geom_col() + scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(y = "", x = "", title = "Colonies Destroyed by Varroa mites", subtitle = "On average every year" ) +
  theme(legend.position = "none")
```

![](Bees_files/figure-html/unnamed-chunk-4-1.png)<!-- -->



```r
library(GGally)
```

```
## Warning: package 'GGally' was built under R version 4.0.5
```

```
## Registered S3 method overwritten by 'GGally':
##   method from   
##   +.gg   ggplot2
```

```r
colony %>%
  select(contains("colony"), -contains("pct")) %>%
  ggpairs()
```

```
## Warning: Removed 47 rows containing non-finite values (stat_density).
```

```
## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 72 rows containing missing values
```

```
## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 47 rows containing missing values
```

```
## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 83 rows containing missing values
```

```
## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 131 rows containing missing values
```

```
## Warning: Removed 72 rows containing missing values (geom_point).
```

```
## Warning: Removed 72 rows containing non-finite values (stat_density).
```

```
## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 72 rows containing missing values
```

```
## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 108 rows containing missing values
```

```
## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 156 rows containing missing values
```

```
## Warning: Removed 47 rows containing missing values (geom_point).
```

```
## Warning: Removed 72 rows containing missing values (geom_point).
```

```
## Warning: Removed 47 rows containing non-finite values (stat_density).
```

```
## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 83 rows containing missing values
```

```
## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 131 rows containing missing values
```

```
## Warning: Removed 83 rows containing missing values (geom_point).
```

```
## Warning: Removed 108 rows containing missing values (geom_point).
```

```
## Warning: Removed 83 rows containing missing values (geom_point).
```

```
## Warning: Removed 83 rows containing non-finite values (stat_density).
```

```
## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 145 rows containing missing values
```

```
## Warning: Removed 131 rows containing missing values (geom_point).
```

```
## Warning: Removed 156 rows containing missing values (geom_point).
```

```
## Warning: Removed 131 rows containing missing values (geom_point).
```

```
## Warning: Removed 145 rows containing missing values (geom_point).
```

```
## Warning: Removed 131 rows containing non-finite values (stat_density).
```

![](Bees_files/figure-html/unnamed-chunk-5-1.png)<!-- -->




```r
colony %>%
  drop_na() %>%
  ggplot(aes(colony_added, colony_lost)) + geom_point() + scale_x_log10(labels = scales::label_comma()) +
  scale_y_log10(labels = scales::label_comma()) + geom_smooth(method = "lm") + facet_wrap(~months) +
  labs(x = "Colonies added", y = "Colonies lost", title = "Colonies: Lost vs Added", subtitle = "Axes on a log10 scale")
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](Bees_files/figure-html/unnamed-chunk-6-1.png)<!-- -->





