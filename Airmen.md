Airmen
================
Matthew
2/8/2022

``` r
airmen <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   name = col_character(),
    ##   last_name = col_character(),
    ##   first_name = col_character(),
    ##   graduation_date = col_datetime(format = ""),
    ##   rank_at_graduation = col_character(),
    ##   class = col_character(),
    ##   graduated_from = col_character(),
    ##   pilot_type = col_character(),
    ##   military_hometown_of_record = col_character(),
    ##   state = col_character(),
    ##   aerial_victory_credits = col_character(),
    ##   number_of_aerial_victory_credits = col_double(),
    ##   reported_lost = col_character(),
    ##   reported_lost_date = col_datetime(format = ""),
    ##   reported_lost_location = col_character(),
    ##   web_profile = col_character()
    ## )

``` r
airmen <- airmen %>% select(-web_profile)
```

# EDA

## Hometowns

``` r
gplot <- function(x){
  airmen %>%
    group_by({{x}}) %>%
    summarize(n = n(), credits = sum(number_of_aerial_victory_credits)) %>%
    arrange(-credits) %>%
    head(12) %>%
    ggplot(aes(credits, fct_reorder({{x}}, credits), fill = n)) + geom_col(color = "black")
}
gplot(military_hometown_of_record) +
  scale_x_continuous(breaks = seq(0,10,2)) +
  labs(x = "Total Credits", y = "", title = "Top 12 Hometowns", fill = "# of Airmen") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](Airmen_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Class

``` r
gplot(class) + labs(x = "Total Credits", y = "", title = "Top 12 Classes", fill = "# of Airmen") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](Airmen_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
