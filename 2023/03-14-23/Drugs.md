European Drug Development
================
Matthew
2023-03-14

- <a href="#eda" id="toc-eda">EDA</a>
  - <a href="#categoricals" id="toc-categoricals">Categoricals</a>

``` r
drugs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-14/drugs.csv')
```

# EDA

## Categoricals

``` r
drugs %>% 
  count(category) %>% 
  knitr::kable()
```

| category   |    n |
|:-----------|-----:|
| human      | 1706 |
| veterinary |  282 |

``` r
drugs %>% 
  count(authorisation_status, sort = TRUE) %>% 
  knitr::kable()
```

| authorisation_status |    n |
|:---------------------|-----:|
| authorised           | 1573 |
| withdrawn            |  357 |
| refused              |   57 |
| NA                   |    1 |
