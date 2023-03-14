European Drug Development
================
Matthew
2023-03-14

- <a href="#eda" id="toc-eda">EDA</a>

``` r
drugs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-14/drugs.csv')
```

# EDA

``` r
drugs %>% 
  count(category) %>% 
  knitr::kable()
```

| category   |    n |
|:-----------|-----:|
| human      | 1706 |
| veterinary |  282 |
