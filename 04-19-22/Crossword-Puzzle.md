Crossword Puzzle
================
Matthew
4/18/2022

``` r
big_dave <- 
  read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/big_dave.csv')
```

    ## Rows: 214794 Columns: 9
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (7): clue, answer, definition, clue_number, puzzle_name, source_url, so...
    ## dbl  (1): rowid
    ## date (1): puzzle_date
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
times <- 
  read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv')
```

    ## Rows: 100661 Columns: 9
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (7): clue, answer, definition, clue_number, puzzle_name, source_url, so...
    ## dbl  (1): rowid
    ## date (1): puzzle_date
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

# EDA

``` r
big_dave %>% 
  count(definition, sort = TRUE)
```

    ## # A tibble: 86,912 x 2
    ##    definition     n
    ##    <chr>      <int>
    ##  1 <NA>       69595
    ##  2 Bird         249
    ##  3 drink        202
    ##  4 game         194
    ##  5 Country      179
    ##  6 plant        176
    ##  7 Plant        163
    ##  8 country      160
    ##  9 fish         154
    ## 10 Drink        149
    ## # ... with 86,902 more rows

``` r
times %>% 
  count(definition, sort = TRUE)
```

    ## # A tibble: 64,377 x 2
    ##    definition     n
    ##    <chr>      <int>
    ##  1 <NA>        3215
    ##  2 Bird         150
    ##  3 plant        147
    ##  4 Girl         131
    ##  5 drink        114
    ##  6 fish         108
    ##  7 Fish         108
    ##  8 game         108
    ##  9 Plant        108
    ## 10 this         105
    ## # ... with 64,367 more rows

``` r
big_dave %>% 
  count(puzzle_name, sort = TRUE)
```

    ## # A tibble: 7,328 x 2
    ##    puzzle_name               n
    ##    <chr>                 <int>
    ##  1 MPP – 061                72
    ##  2 Double Toughie 100003    64
    ##  3 Toughie 2766             64
    ##  4 Double Toughie 100011    63
    ##  5 NTSPP – 210              60
    ##  6 Toughie 758              58
    ##  7 Double Toughie 100007    57
    ##  8 NTSPP – 200              57
    ##  9 Sunday Telegraph 2789    56
    ## 10 Toughie 2635             55
    ## # ... with 7,318 more rows

``` r
times %>% 
  count(puzzle_name, sort = TRUE)
```

    ## # A tibble: 3,453 x 2
    ##    puzzle_name                 n
    ##    <chr>                   <int>
    ##  1 Jumbo 1423                 90
    ##  2 Saturday,                  86
    ##  3 Times 27195                76
    ##  4 Bank Holiday Jumbo 1326    62
    ##  5 Jumbo 1197                 62
    ##  6 Jumbo 1210                 62
    ##  7 Jumbo 1212                 62
    ##  8 Jumbo 1215                 62
    ##  9 Jumbo 1222                 62
    ## 10 Jumbo 1230                 62
    ## # ... with 3,443 more rows
