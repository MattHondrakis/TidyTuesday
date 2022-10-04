Product Hunt
================
Matthew
2022-10-04

-   <a href="#eda" id="toc-eda">EDA</a>
    -   <a href="#most-common-words-in-product-description"
        id="toc-most-common-words-in-product-description">Most Common Words in
        Product Description</a>
    -   <a href="#releases-of-time" id="toc-releases-of-time">Releases of
        Time</a>

# EDA

``` r
product <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-04/product_hunt.csv')
```

    ## Rows: 76822 Columns: 12
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (7): id, name, product_description, main_image, category_tags, hunter, ...
    ## dbl  (2): product_ranking, upvotes
    ## dttm (2): release_date, last_updated
    ## date (1): product_of_the_day_date
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
knitr::kable(head(product[,1:6]))
```

| id                                    | name                                    | product_description                                                                                                                                                                                                                                                                                                         | release_date | product_of_the_day_date | product_ranking |
|:--------------------------------------|:----------------------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-------------|:------------------------|----------------:|
| ar-fidget-spinner                     | AR Fidget Spinner                       | AR Fidget Spinner brings back the fidget spinner, in augmented reality. Play with multiple colors of spinners, anytime, anywhere. Never lose yours again.                                                                                                                                                                   | 2017-09-19   | NA                      |              NA |
| e-botfinder                           | e-botfinder                             | NA                                                                                                                                                                                                                                                                                                                          | 2016-05-29   | NA                      |              NA |
| bioglo                                | BioGlo                                  | BioGlo brings the beauty of bioluminescence into your home or classroom. Inside its sleek hourglass shape, BioGlo contains hundreds of tiny microbes called dinoflagellates (aka dinos). These dinos have the natural ability to absorb the sun’s rays during the day and emit a mysterious blue glow when shaken at night! | 2018-01-06   | NA                      |              NA |
| onsites                               | Onsites                                 | Onsites helps companies craft the best candidate interview experience by offering tools for hiring teams. We help companies with two products: detailed onsite schedules and fully transparent offer letters.                                                                                                               | 2018-02-09   | 2018-02-09              |               4 |
| easylistr-ios-twitter-lists-made-easy | Easylistr iOS - Twitter lists made easy | NA                                                                                                                                                                                                                                                                                                                          | 2016-06-01   | NA                      |              NA |
| cuttlefish                            | Cuttlefish                              | NA                                                                                                                                                                                                                                                                                                                          | 2014-10-30   | NA                      |              NA |

``` r
knitr::kable(head(product[,7:12]))
```

| main_image                                                             | upvotes | category_tags                                                                   | hunter          | makers                        | last_updated        |
|:-----------------------------------------------------------------------|--------:|:--------------------------------------------------------------------------------|:----------------|:------------------------------|:--------------------|
| <https://ph-files.imgix.net/9f8f55b2-2a94-4915-84f1-dbb5ef3336c4.jpeg> |     134 | \[‘IPAD’, ‘IPHONE’, ‘ARKIT’, ‘AUGMENTED REALITY’\]                              | shanev          | \[‘shanev’, ‘tettoffensive’\] | 2022-01-11 01:34:10 |
| <https://ph-files.imgix.net/bca825ce-a4e9-4d95-9a5f-1310cb7059f5.png>  |     183 | \[‘BOTS’, ‘ARTIFICIAL INTELLIGENCE’, ‘TECH’, ‘SLACK’, ‘MESSAGING’\]             | fabian_beringer | \[‘fabian_beringer’\]         | 2022-01-12 19:28:49 |
| <https://ph-files.imgix.net/99d69c10-0e58-4b0b-8c93-e80c379968b5.png>  |     196 | \[‘TECH’, ‘EDUCATION’, ‘CROWDFUNDING’, ‘HOME’, ‘BIOHACKING’\]                   | keegan_cooke    | \[‘keegan_cooke’\]            | 2022-01-11 09:00:59 |
| <https://ph-files.imgix.net/b6483b87-d952-40ff-95bc-5803b6ea77b1.png>  |     315 | \[‘PRODUCTIVITY’, ‘USER EXPERIENCE’, ‘TECH’, ‘EMAIL’, ‘HIRING AND RECRUITING’\] | \_shahedk       | \[‘jmtame’\]                  | 2022-01-12 11:04:49 |
| <https://ph-files.imgix.net/bd5cb6ff-78da-4762-95cc-fb35b21580e4.jpeg> |     104 | \[‘TWITTER’, ‘PRODUCTIVITY’, ‘IPHONE’, ‘TECH’\]                                 | igorbogdanovski | \[\]                          | 2022-01-12 14:52:25 |
| NA                                                                     |      69 | \[‘TECH’\]                                                                      | erictwillis     | \[‘matthewlandauer’\]         | 2022-01-11 17:03:06 |

``` r
skimr::skim(product)
```

|                                                  |         |
|:-------------------------------------------------|:--------|
| Name                                             | product |
| Number of rows                                   | 76822   |
| Number of columns                                | 12      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |         |
| Column type frequency:                           |         |
| character                                        | 7       |
| Date                                             | 1       |
| numeric                                          | 2       |
| POSIXct                                          | 2       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |         |
| Group variables                                  | None    |

Data summary

**Variable type: character**

| skim_variable       | n_missing | complete_rate | min |  max | empty | n_unique | whitespace |
|:--------------------|----------:|--------------:|----:|-----:|------:|---------:|-----------:|
| id                  |         0 |          1.00 |   1 |  109 |     0 |    76822 |          0 |
| name                |         3 |          1.00 |   1 |  110 |     0 |    73263 |          0 |
| product_description |     32767 |          0.57 |   1 | 2019 |     0 |    43789 |          0 |
| main_image          |     10078 |          0.87 |  63 |   83 |     0 |    66513 |          0 |
| category_tags       |         0 |          1.00 |   2 |  307 |     0 |    29190 |          0 |
| hunter              |         0 |          1.00 |   1 |   30 |     0 |    27954 |          0 |
| makers              |         0 |          1.00 |   2 |  804 |     0 |    47420 |          0 |

**Variable type: Date**

| skim_variable           | n_missing | complete_rate | min        | max        | median     | n_unique |
|:------------------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| product_of_the_day_date |     62200 |          0.19 | 2014-01-01 | 2021-12-31 | 2018-01-11 |     2920 |

**Variable type: numeric**

| skim_variable   | n_missing | complete_rate |   mean |     sd |  p0 | p25 | p50 | p75 |  p100 | hist  |
|:----------------|----------:|--------------:|-------:|-------:|----:|----:|----:|----:|------:|:------|
| product_ranking |     62140 |          0.19 |   3.05 |   1.40 |   1 |   2 |   3 |   4 |     5 | ▇▇▇▇▇ |
| upvotes         |         0 |          1.00 | 214.53 | 351.82 |   0 |  48 | 104 | 246 | 21798 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| release_date  |         0 |             1 | 2014-01-01 00:00:00 | 2021-12-31 00:00:00 | 2017-12-28 00:00:00 |     2922 |
| last_updated  |         0 |             1 | 2020-10-10 21:33:29 | 2022-01-14 15:02:31 | 2022-01-12 01:02:32 |    63077 |

``` r
product %>% 
  count(makers, sort = TRUE)
```

    ## # A tibble: 47,420 x 2
    ##    makers                n
    ##    <chr>             <int>
    ##  1 []                17152
    ##  2 ['mijustin']         55
    ##  3 ['ericosiu']         37
    ##  4 ['levelsio']         34
    ##  5 ['mubashariqbal']    33
    ##  6 ['evanspiegel']      32
    ##  7 ['tim1']             30
    ##  8 ['awt']              29
    ##  9 ['clarkvalberg']     29
    ## 10 ['jason']            29
    ## # ... with 47,410 more rows

## Most Common Words in Product Description

``` r
product %>% 
  unnest_tokens(word,product_description) %>% 
  anti_join(stop_words) %>%
  mutate(word = fct_lump(word, 10)) %>% 
  filter(word != "Other", !is.na(word)) %>% 
  group_by(word) %>% 
  summarize(m = mean(upvotes, na.rm = TRUE)) %>% 
  arrange(-m) %>% 
  ggplot(aes(m, fct_reorder(word, m))) + geom_col(color = "black", fill = "lightblue") +
  labs(y = "Words", x = "Average Upvotes", title = "Average Upvotes of the 10 Most Common Words")
```

    ## Joining, by = "word"

![](Product_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Releases of Time

``` r
product %>% 
  count(release_date, sort = TRUE) %>% 
  ggplot(aes(release_date, n)) + geom_point(alpha = 0.2) +
  geom_smooth() + labs(title = "Number of Releases over Time", x = "Date", y = "Releases")
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](Product_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
