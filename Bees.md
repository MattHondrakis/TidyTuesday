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
    distinct(months, year, state, {{x}}) %>%
    group_by(months, year, state) %>%
    ggplot(aes({{x}}, reorder(state, {{x}}, order = TRUE))) + geom_col() + labs(y = "")
}

gplot(colony_max)
```

![](Bees_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


## Stressors

```r
levels <- c("Disesases", "Unknown", "Other", "Pesticides", "Other pests/parasites", "Varroa mites")

stressor %>%
  drop_na() %>%
  mutate(stressor = as.factor(stressor), stressor = fct_relevel(stressor, levels = levels)) %>%
  group_by(year, stressor) %>%
  summarize(n = sum(stress_pct)) %>%
  ggplot(aes(year, n, fill = stressor)) + geom_col(position = "dodge")
```

```
## Warning: Outer names are only allowed for unnamed scalar atomic inputs
```

```
## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.
```

![](Bees_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


