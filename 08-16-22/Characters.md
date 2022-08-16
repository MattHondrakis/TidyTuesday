Open Psychometrics
================
Matthew
2022-08-16

``` r
characters <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/characters.csv')
```

    ## Rows: 889 Columns: 7
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (6): id, name, uni_id, uni_name, link, image_link
    ## dbl (1): notability
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(characters)
```

    ## # A tibble: 6 x 7
    ##   id    name           uni_id uni_name notability link                image_link
    ##   <chr> <chr>          <chr>  <chr>         <dbl> <chr>               <chr>     
    ## 1 F2    Monica Geller  F      Friends        79.7 https://openpsycho~ https://o~
    ## 2 F1    Rachel Green   F      Friends        76.7 https://openpsycho~ https://o~
    ## 3 F5    Chandler Bing  F      Friends        74.4 https://openpsycho~ https://o~
    ## 4 F4    Joey Tribbiani F      Friends        74.3 https://openpsycho~ https://o~
    ## 5 F3    Phoebe Buffay  F      Friends        72.6 https://openpsycho~ https://o~
    ## 6 F6    Ross Geller    F      Friends        51.6 https://openpsycho~ https://o~

``` r
skimr::skim(characters)
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | characters |
| Number of rows                                   | 889        |
| Number of columns                                | 7          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| character                                        | 6          |
| numeric                                          | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| id            |         0 |             1 |   2 |   7 |     0 |      889 |          0 |
| name          |         0 |             1 |   2 |  44 |     0 |      885 |          0 |
| uni_id        |         0 |             1 |   1 |   6 |     0 |      100 |          0 |
| uni_name      |         0 |             1 |   2 |  33 |     0 |      100 |          0 |
| link          |         0 |             1 |  56 |  61 |     0 |      889 |          0 |
| image_link    |         0 |             1 |  74 |  79 |     0 |      889 |          0 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |  mean |    sd |   p0 |  p25 |  p50 | p75 | p100 | hist  |
|:--------------|----------:|--------------:|------:|------:|-----:|-----:|-----:|----:|-----:|:------|
| notability    |         0 |             1 | 67.63 | 17.39 | 18.4 | 55.6 | 71.7 |  81 | 96.9 | ▁▃▅▇▆ |

``` r
characters %>% 
  group_by(uni_name) %>% 
  summarize(m = mean(notability)) %>% 
  arrange(-m) %>%
  mutate(uni_name = fct_reorder(uni_name, m)) %>% 
  slice(c(1:10, 91:100)) %>% 
  ggplot(aes(m, uni_name, fill = 60 > m)) + geom_col(color = "black") +
  labs(y = "", x = "Average Notability", title = "Average Notability of Characters in Shows") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") + 
  scale_fill_manual(values = c("lightblue", "darkred"))
```

![](Characters_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
