Weather Forcasts
================
Matthew
2022-12-20

-   <a href="#data-cleaning" id="toc-data-cleaning">Data Cleaning</a>
    -   <a href="#data-view" id="toc-data-view">Data View</a>
    -   <a href="#missing-values" id="toc-missing-values">Missing Values</a>
    -   <a href="#outliers" id="toc-outliers">Outliers</a>
-   <a href="#exploratory-data-analysis"
    id="toc-exploratory-data-analysis">Exploratory Data Analysis</a>
    -   <a href="#temperature-difference"
        id="toc-temperature-difference">Temperature Difference</a>
        -   <a href="#states" id="toc-states">States</a>

# Data Cleaning

``` r
weather_forecasts <- weather_forecasts %>% 
  left_join(outlook_meanings)  
```

    ## Joining, by = "forecast_outlook"

The *weather forecasts* dataset contains abbreviated weather outlooks,
while the *outlook meanings* dataset contains non-abbreviated weather
outlooks. The *weather forecasts* dataset and the *outlook meanings*
dataset were joined together using the “forecast_outlook” column as a
common key. This resulted in a combined dataset that includes both the
abbreviated and non-abbreviated weather outlooks from the two datasets.

``` r
weather_forecasts %>% 
  count(meaning, sort = TRUE)
```

    ## # A tibble: 24 x 2
    ##    meaning            n
    ##    <chr>          <int>
    ##  1 Sunny         173769
    ##  2 Partly Cloudy 104907
    ##  3 Rain Showers   95671
    ##  4 Mostly Cloudy  80178
    ##  5 Thunderstorms  72927
    ##  6 <NA>           37875
    ##  7 Rain           24198
    ##  8 Snow           14240
    ##  9 Windy          13368
    ## 10 Snow Showers    7924
    ## # ... with 14 more rows

## Data View

``` r
weather_forecasts %>% 
  head(n = 10) %>% 
  knitr::kable()
```

| date       | city         | state | high_or_low | forecast_hours_before | observed_temp | forecast_temp | observed_precip | forecast_outlook | possible_error | meaning |
|:-----------|:-------------|:------|:------------|----------------------:|--------------:|--------------:|----------------:|:-----------------|:---------------|:--------|
| 2021-01-30 | ABILENE      | TX    | high        |                    48 |            70 |            NA |            0.00 | NA               | none           | NA      |
| 2021-01-30 | ABILENE      | TX    | high        |                    36 |            70 |            NA |            0.00 | NA               | none           | NA      |
| 2021-01-30 | ABILENE      | TX    | high        |                    24 |            70 |            NA |            0.00 | NA               | none           | NA      |
| 2021-01-30 | ABILENE      | TX    | high        |                    12 |            70 |            70 |            0.00 | DUST             | none           | Dust    |
| 2021-01-30 | ABILENE      | TX    | low         |                    48 |            42 |            NA |            0.00 | NA               | none           | NA      |
| 2021-01-30 | ABILENE      | TX    | low         |                    36 |            42 |            NA |            0.00 | NA               | none           | NA      |
| 2021-01-30 | ABILENE      | TX    | low         |                    24 |            42 |            39 |            0.00 | DUST             | none           | Dust    |
| 2021-01-30 | ABILENE      | TX    | low         |                    12 |            42 |            38 |            0.00 | SUNNY            | none           | Sunny   |
| 2021-01-30 | AKRON_CANTON | OH    | high        |                    48 |            29 |            NA |            0.09 | NA               | none           | NA      |
| 2021-01-30 | AKRON_CANTON | OH    | high        |                    36 |            29 |            NA |            0.09 | NA               | none           | NA      |

## Missing Values

``` r
skimr::skim(weather_forecasts) %>% select(skim_type, skim_variable, n_missing)
```

|                                                  |                   |
|:-------------------------------------------------|:------------------|
| Name                                             | weather_forecasts |
| Number of rows                                   | 651968            |
| Number of columns                                | 11                |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                   |
| Column type frequency:                           |                   |
| character                                        | 6                 |
| Date                                             | 1                 |
| numeric                                          | 4                 |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                   |
| Group variables                                  | None              |

Data summary

**Variable type: character**

| skim_variable    | n_missing |
|:-----------------|----------:|
| city             |         0 |
| state            |         0 |
| high_or_low      |         0 |
| forecast_outlook |     37875 |
| possible_error   |         0 |
| meaning          |     37875 |

**Variable type: Date**

| skim_variable | n_missing |
|:--------------|----------:|
| date          |         0 |

**Variable type: numeric**

| skim_variable         | n_missing |
|:----------------------|----------:|
| forecast_hours_before |         0 |
| observed_temp         |     47744 |
| forecast_temp         |     37313 |
| observed_precip       |     50416 |

``` r
weather_forecasts %>% 
  group_by(NA_meaning = is.na(meaning)) %>% 
  summarize(sum(is.na(forecast_temp)))
```

    ## # A tibble: 2 x 2
    ##   NA_meaning `sum(is.na(forecast_temp))`
    ##   <lgl>                            <int>
    ## 1 FALSE                                4
    ## 2 TRUE                             37309

``` r
weather_forecasts %>% 
  group_by(forecast_hours_before) %>% 
  summarize(sum(is.na(forecast_temp)))
```

    ## # A tibble: 4 x 2
    ##   forecast_hours_before `sum(is.na(forecast_temp))`
    ##                   <dbl>                       <int>
    ## 1                    12                        9189
    ## 2                    24                        9319
    ## 3                    36                        9319
    ## 4                    48                        9486

``` r
weather_forecasts %>% 
  group_by(forecast_hours_before) %>% 
  summarize(sum(is.na(forecast_temp)))
```

    ## # A tibble: 4 x 2
    ##   forecast_hours_before `sum(is.na(forecast_temp))`
    ##                   <dbl>                       <int>
    ## 1                    12                        9189
    ## 2                    24                        9319
    ## 3                    36                        9319
    ## 4                    48                        9486

Missing values for *forecast_temp* tend to appear where forecast
*meaning* is also NA. They also slightly increase as the number of hours
before. It is assumed that these missing values are due to there being
no forecast at that time. Thus, these rows will not be dropped.

## Outliers

``` r
weather_forecasts %>% 
  count(possible_error, sort = TRUE)
```

    ## # A tibble: 4 x 2
    ##   possible_error        n
    ##   <chr>             <int>
    ## 1 none             651875
    ## 2 forecast_outlook     77
    ## 3 forecast_temp         8
    ## 4 observed_temp         8

``` r
weather_forecasts %>% 
  filter(possible_error == "observed_temp") %>% 
  select(contains("temp"))
```

    ## # A tibble: 8 x 2
    ##   observed_temp forecast_temp
    ##           <dbl>         <dbl>
    ## 1             0            64
    ## 2             0            64
    ## 3             0            63
    ## 4             0            63
    ## 5           108            46
    ## 6           108            45
    ## 7           108            45
    ## 8           108            45

``` r
weather_forecasts %>% 
  filter(possible_error == "forecast_temp") %>% 
  select(contains("temp"))
```

    ## # A tibble: 8 x 2
    ##   observed_temp forecast_temp
    ##           <dbl>         <dbl>
    ## 1            41           -10
    ## 2            43           -10
    ## 3            73           -10
    ## 4            91           -10
    ## 5            75           -10
    ## 6            97           -10
    ## 7            59           -10
    ## 8            58           -10

Column *possible_error* appears to show what **potentially** is flawed
in that row, instead of inconsistancies between the forecast and
observed. For instance, when *possible_error* equals “forecast_temp”,
all values for *forecast_temp* are **-10** and when it equals
“observed_temp”, values are either **0** or **108**.

# Exploratory Data Analysis

## Temperature Difference

``` r
weather_forecasts <- weather_forecasts %>% 
  mutate(diff_temp = observed_temp - forecast_temp) 
```

Column of temperature differences created above.

### States

``` r
weather_forecasts %>% 
  filter(possible_error == "none") %>% 
  group_by(state) %>% 
  summarize(avg_diff = mean(abs(diff_temp), na.rm = TRUE)) %>% 
  slice_max(avg_diff, n = 10) %>% 
  ggplot(aes(avg_diff, fct_reorder(state, avg_diff), fill = avg_diff)) +
  geom_col(color = "black") + 
  scale_fill_viridis_c() +
  labs(x = "Average Forecast Temperature Error", y = "", 
       title = "Top 10 States with Highest Average Prediction Error") +
  theme(legend.position = "none")
```

![](Weather-Forecasts_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
top10states <- weather_forecasts %>% 
  filter(possible_error == "none") %>% 
  group_by(state) %>% 
  summarize(avg_diff = mean(abs(diff_temp), na.rm = TRUE)) %>% 
  slice_max(avg_diff, n = 6) %>% 
  pull(state)
```

``` r
weather_forecasts %>% 
  filter(state %in% top10states & possible_error == "none") %>% 
  ggplot(aes(date, diff_temp, color = state)) +
  geom_line() +
  facet_wrap(~state, scales = "free")
```

![](Weather-Forecasts_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->