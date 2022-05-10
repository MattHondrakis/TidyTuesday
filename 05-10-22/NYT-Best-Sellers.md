NY Times Best Sellers
================
Matthew
5/10/2022

``` r
nyt_titles <- read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
```

    ## Rows: 7431 Columns: 8
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: "\t"
    ## chr  (2): title, author
    ## dbl  (5): id, year, total_weeks, debut_rank, best_rank
    ## date (1): first_week
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
nyt_full <- read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')
```

    ## Rows: 60386 Columns: 6
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: "\t"
    ## chr  (2): title, author
    ## dbl  (3): year, rank, title_id
    ## date (1): week
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
nyt <- nyt_titles %>% 
  full_join(nyt_full)
```

    ## Joining, by = c("title", "author", "year")

``` r
nyt %>% 
  count(author, sort = TRUE)
```

    ## # A tibble: 2,206 x 2
    ##    author                 n
    ##    <chr>              <int>
    ##  1 Danielle Steel       957
    ##  2 Stephen King         892
    ##  3 John Grisham         789
    ##  4 Taylor Caldwell      524
    ##  5 James A. Michener    494
    ##  6 Robert Ludlum        406
    ##  7 Leon Uris            405
    ##  8 Mary Higgins Clark   403
    ##  9 David Baldacci       396
    ## 10 Nicholas Sparks      390
    ## # ... with 2,196 more rows

``` r
nyt %>% 
  filter(author %in% (nyt %>% count(author, sort = TRUE) %>% head(10) %>% pull(author))) %>% 
  group_by(author) %>% 
  ggplot(aes(rank, fct_reorder(author, rank, median))) + geom_boxplot() + 
  labs(y = "", title = "Top 10 most popular authors")
```

![](NYT-Best-Sellers_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
