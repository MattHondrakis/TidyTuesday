European Drug Development
================
Matthew
2023-03-14

- <a href="#eda" id="toc-eda">EDA</a>
  - <a href="#categoricals" id="toc-categoricals">Categoricals</a>
  - <a href="#therapeutic-areas" id="toc-therapeutic-areas">Therapeutic
    Areas</a>

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

``` r
drugs %>% 
  count(active_substance, sort = TRUE) %>% 
  slice_max(n, n = 5) %>% 
  knitr::kable()
```

| active_substance |   n |
|:-----------------|----:|
| meloxicam        |  18 |
| adalimumab       |  16 |
| bevacizumab      |  12 |
| insulin human    |  12 |
| olanzapine       |  12 |
| pegfilgrastim    |  12 |

## Therapeutic Areas

``` r
drugs %>% 
  mutate(len = nchar(therapeutic_area)) %>% 
  arrange(-len) %>% 
  head(3) %>% 
  pull(therapeutic_area)
```

    ## [1] "Lambert-Eaton Myasthenic Syndrome;  Paraneoplastic Syndromes, Nervous System;  Nervous System Neoplasms;  Paraneoplastic Syndromes;  Nervous System Diseases;  Autoimmune Diseases of the Nervous System;  Neurodegenerative Diseases;  Neuromuscular Diseases;  Neuromuscular Junction Diseases;  Immune System Diseases;  Autoimmune Diseases;  Autoimmune Diseases of the Nervous System;  Cancer;  Neoplasms"
    ## [2] "Multiple Myeloma;  Neoplasms;  Cancer;  Neoplasms, Plasma Cell;  Hemostatic Disorders;  Vascular Diseases;  Cardiovascular Diseases;  Paraproteinemias;  Blood Protein Disorders;  Hematologic Diseases;  Hemic and Lymphatic Diseases;  Hemorrhagic Disorders;  Infectious Mononucleosis;  Lymphoproliferative Disorders;  Immunoproliferative Disorders;  Immune System Diseases"                              
    ## [3] "Precursor Cell Lymphoblastic Leukemia-Lymphoma;  Gastrointestinal Stromal Tumors;  Dermatofibrosarcoma;  Myelodysplastic-Myeloproliferative Diseases;  Leukemia, Myelogenous, Chronic, BCR-ABL Positive;  Hypereosinophilic Syndrome"

As it can be seen, the column *therapeutic_area* contains all possible
uses for each drug, separated by **“;”**. Thus in order to count how
many distinct therapeutic areas a drug is used for, we will need to
separate the column into multiple rows based on the delimiter, **“;”**.

``` r
drugs %>% 
  filter(active_substance == "adalimumab") %>% 
  select(active_substance, therapeutic_area) %>% 
  separate_rows(therapeutic_area, sep = ";  ") %>% 
  distinct(therapeutic_area)
```

    ## # A tibble: 11 x 1
    ##    therapeutic_area              
    ##    <chr>                         
    ##  1 Arthritis, Psoriatic          
    ##  2 Arthritis, Juvenile Rheumatoid
    ##  3 Arthritis, Rheumatoid         
    ##  4 Colitis, Ulcerative           
    ##  5 Crohn Disease                 
    ##  6 Hidradenitis Suppurativa      
    ##  7 Psoriasis                     
    ##  8 Spondylitis, Ankylosing       
    ##  9 Uveitis                       
    ## 10 Skin Diseases, Papulosquamous 
    ## 11 Arthritis

``` r
drugs %>% 
  group_by(active_substance) %>% 
  select(active_substance, therapeutic_area) %>% 
  separate_rows(therapeutic_area, sep = ";  ") %>% 
  distinct(therapeutic_area) %>% 
  summarize(n = n()) %>% 
  arrange(-n) %>% 
  slice_max(n, n = 5) %>% 
  ggplot(aes(n, fct_reorder(active_substance, n))) +
  geom_col(fill = "steelblue4") +
  geom_text(aes(label = n), hjust = 2, color = "white") +
  labs(y = "Active Substance",
       x = "",
       title = "Top 5 Substances by the Number of Therapeutic Applications") +
  theme(plot.title = element_text(hjust = 1))
```

![](Drugs_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
