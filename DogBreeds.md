Dog Breeds
================
Matthew
2/1/2022

# EDA

``` r
breed_traits %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + geom_histogram() + facet_wrap(~key)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](DogBreeds_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
library(ggcorrplot)
breed_traits %>%
  keep(is.numeric) %>%
  cor() %>%
  round(1) %>%
  ggcorrplot(type = "lower", lab = TRUE, p.mat = cor_pmat(keep(breed_traits, is.numeric)), insig = "blank")
```

![](DogBreeds_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
breed_long %>%
  group_by(breed) %>%
  mutate(value = ifelse(type %in% c("drooling level", "coat grooming frequency", "shedding level", "barking level"),
                        -value, value)) %>%
  summarize(total_score = sum(value)) %>%
  arrange(-total_score) %>% 
  head(13) %>%
  ggplot(aes(total_score, fct_reorder(breed, total_score), fill = breed)) + geom_col() +
  theme(legend.position = "") +
  labs(title = "Top 13 Dogs", subtitle = "The sum of positive and negative trait scores",
       y = "", x = "Total Score", 
       caption = "Drooling, Grooming frequency, Shedding and Barking = Negative Scores\n
                  The choice of displaying 13 was because the following 10 (#14-23) were a tie")
```

![](DogBreeds_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
