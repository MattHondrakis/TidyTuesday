Bigfoot
================
Matthew
2022-09-13

``` r
skimr::skim(bigfoot)
```

|                                                  |         |
|:-------------------------------------------------|:--------|
| Name                                             | bigfoot |
| Number of rows                                   | 5021    |
| Number of columns                                | 28      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |         |
| Column type frequency:                           |         |
| character                                        | 10      |
| Date                                             | 1       |
| numeric                                          | 17      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |         |
| Group variables                                  | None    |

Data summary

**Variable type: character**

| skim_variable    | n_missing | complete_rate | min |   max | empty | n_unique | whitespace |
|:-----------------|----------:|--------------:|----:|------:|------:|---------:|-----------:|
| observed         |        38 |          0.99 |   1 | 30374 |     0 |     4982 |          0 |
| location_details |       758 |          0.85 |   1 |  3876 |     0 |     4196 |          0 |
| county           |         0 |          1.00 |  10 |    30 |     0 |     1037 |          0 |
| state            |         0 |          1.00 |   4 |    14 |     0 |       49 |          0 |
| season           |         0 |          1.00 |   4 |     7 |     0 |        5 |          0 |
| title            |       976 |          0.81 |  23 |   235 |     0 |     4045 |          0 |
| classification   |         0 |          1.00 |   7 |     7 |     0 |        3 |          0 |
| geohash          |       976 |          0.81 |  10 |    10 |     0 |     4001 |          0 |
| precip_type      |      3298 |          0.34 |   4 |     4 |     0 |        2 |          0 |
| summary          |      1655 |          0.67 |  15 |   103 |     0 |      321 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |       976 |          0.81 | 1869-11-10 | 2021-11-27 | 2003-11-16 |     3111 |

**Variable type: numeric**

| skim_variable      | n_missing | complete_rate |     mean |       sd |      p0 |     p25 |      p50 |      p75 |     p100 | hist  |
|:-------------------|----------:|--------------:|---------:|---------:|--------:|--------:|---------:|---------:|---------:|:------|
| latitude           |       976 |          0.81 |    39.36 |     5.68 |   25.14 |   35.35 |    39.30 |    43.93 |    64.89 | ▂▇▆▁▁ |
| longitude          |       976 |          0.81 |   -97.42 |    16.73 | -167.13 | -117.06 |   -91.77 |   -83.07 |   -68.23 | ▁▁▆▆▇ |
| number             |         0 |          1.00 | 21520.23 | 19259.15 |   60.00 | 4595.00 | 15473.00 | 33979.00 | 71997.00 | ▇▃▂▂▁ |
| temperature_high   |      1683 |          0.66 |    67.12 |    17.78 |   -0.62 |   55.14 |    69.97 |    81.10 |   106.51 | ▁▂▅▇▃ |
| temperature_mid    |      1835 |          0.63 |    57.84 |    16.40 |   -8.46 |   46.77 |    59.36 |    70.38 |    94.03 | ▁▁▆▇▃ |
| temperature_low    |      1832 |          0.64 |    48.64 |    15.94 |  -22.78 |   37.50 |    49.40 |    60.66 |    84.34 | ▁▁▅▇▃ |
| dew_point          |      1648 |          0.67 |    46.23 |    16.44 |  -11.21 |   34.77 |    46.69 |    59.00 |    77.40 | ▁▂▆▇▅ |
| humidity           |      1648 |          0.67 |     0.71 |     0.16 |    0.08 |    0.62 |     0.73 |     0.82 |     1.00 | ▁▁▃▇▅ |
| cloud_cover        |      1937 |          0.61 |     0.44 |     0.33 |    0.00 |    0.12 |     0.40 |     0.73 |     1.00 | ▇▅▃▃▅ |
| moon_phase         |      1625 |          0.68 |     0.50 |     0.29 |    0.00 |    0.25 |     0.49 |     0.75 |     1.00 | ▇▇▇▇▇ |
| precip_intensity   |      2309 |          0.54 |     0.01 |     0.05 |    0.00 |    0.00 |     0.00 |     0.00 |     2.07 | ▇▁▁▁▁ |
| precip_probability |      2311 |          0.54 |     0.30 |     0.42 |    0.00 |    0.00 |     0.00 |     0.73 |     1.00 | ▇▁▁▁▃ |
| pressure           |      2402 |          0.52 |  1017.08 |     6.14 |  980.34 | 1013.42 |  1016.96 |  1020.64 |  1042.41 | ▁▁▇▆▁ |
| uv_index           |      1629 |          0.68 |     5.16 |     3.14 |    0.00 |    3.00 |     5.00 |     8.00 |    13.00 | ▆▇▅▆▁ |
| visibility         |      1972 |          0.61 |     8.49 |     2.06 |    0.74 |    7.66 |     9.45 |    10.00 |    10.00 | ▁▁▁▂▇ |
| wind_bearing       |      1634 |          0.67 |   196.57 |    96.38 |    0.00 |  128.00 |   203.00 |   273.00 |   359.00 | ▅▅▇▇▆ |
| wind_speed         |      1632 |          0.67 |     3.87 |     3.28 |    0.00 |    1.34 |     2.93 |     5.56 |    23.94 | ▇▃▁▁▁ |

``` r
bigfoot %>% 
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) + geom_histogram() +
  facet_wrap(~key, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Bigfoot_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
bigfoot %>% 
  unnest_tokens(word, summary) %>% 
  anti_join(stop_words) %>%
  group_by(classification) %>% 
  count(word, sort = TRUE) %>% 
  filter(!is.na(word)) %>% 
  head(15) %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(n, word)) + geom_col(color = "black", position = position_dodge2(preserve = "single"), aes(fill = classification)) + 
  ggtitle(str_to_title("Most common words describing the day"))
```

    ## Joining, by = "word"

![](Bigfoot_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
bigfoot %>% 
  unnest_tokens(word, observed) %>% 
  anti_join(stop_words) %>%
  group_by(classification) %>% 
  count(word, sort = TRUE) %>% 
  filter(!is.na(word)) %>% 
  head(20) %>% 
  ungroup() %>% 
  mutate(word = fct_reorder(word, n, sum)) %>% 
  ggplot(aes(n, word)) + geom_col(color = "black", position = position_dodge2(preserve = "single"), aes(fill = classification)) + 
  ggtitle(str_to_title("Most common words describing the story"))
```

    ## Joining, by = "word"

![](Bigfoot_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
