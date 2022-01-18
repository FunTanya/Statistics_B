Chapter 1: Tables
================

# Employees of a Small Business Company: Using Simple Commnads

-   reading data:

``` r
EX_DATA_DF <- readRDS("../data/EX_DATA_DF.rds")
```

-   frequency distribution for Degree

``` r
fi = table(EX_DATA_DF$Degree)
print(fi)
```

    ## 
    ##  Associate Bachelor's   Master's   Doctoral 
    ##          5         17          6          4

-   percent

``` r
(percent = prop.table(table(EX_DATA_DF$Degree))*100)
```

    ## 
    ##  Associate Bachelor's   Master's   Doctoral 
    ##     15.625     53.125     18.750     12.500

-   cumulative frequency

``` r
(cum.fi = cumsum(table(EX_DATA_DF$Degree)))
```

    ##  Associate Bachelor's   Master's   Doctoral 
    ##          5         22         28         32

-   cumulative percent

``` r
(cum.percent = cumsum(prop.table(table(EX_DATA_DF$Degree)))*100)
```

    ##  Associate Bachelor's   Master's   Doctoral 
    ##     15.625     68.750     87.500    100.000

-   all together

``` r
cbind(fi,percent,cum.fi,cum.percent)
```

    ##            fi percent cum.fi cum.percent
    ## Associate   5  15.625      5      15.625
    ## Bachelor's 17  53.125     22      68.750
    ## Master's    6  18.750     28      87.500
    ## Doctoral    4  12.500     32     100.000

-   contingency table with row and column totals

``` r
addmargins(table(EX_DATA_DF$Gender,EX_DATA_DF$Degree))
```

    ##         
    ##          Associate Bachelor's Master's Doctoral Sum
    ##   Female         2          8        2        1  13
    ##   Male           3          9        4        3  19
    ##   Sum            5         17        6        4  32

# Employees of a Small Business Company: Using Tidyverse and pander packages

-   frequency distribution for Degree with

``` r
library(tidyverse)
EX_DATA_DF %>% select(Degree) %>%
    table()
```

    ## .
    ##  Associate Bachelor's   Master's   Doctoral 
    ##          5         17          6          4

-   frequency distribution for Degree: (simple) tables with pander

``` r
library(pander)
EX_DATA_DF %>% select(Degree) %>%
    table() %>% 
    pander()
```

| Associate | Bachelor’s | Master’s | Doctoral |
|:---------:|:----------:|:--------:|:--------:|
|     5     |     17     |    6     |    4     |

-   percent

``` r
EX_DATA_DF %>% select(Degree) %>%
  table() %>%
  prop.table()  %>% 
  "*"(100) %>% 
   pander()
```

| Associate | Bachelor’s | Master’s | Doctoral |
|:---------:|:----------:|:--------:|:--------:|
|   15.62   |   53.12    |  18.75   |   12.5   |

-   cumulative frequency

``` r
EX_DATA_DF %>% select(Degree) %>%
  table() %>%
  cumsum( ) %>% 
  pander()
```

| Associate | Bachelor’s | Master’s | Doctoral |
|:---------:|:----------:|:--------:|:--------:|
|     5     |     22     |    28    |    32    |

-   cumulative percent

``` r
EX_DATA_DF %>% select(Degree) %>%
  table() %>%
  prop.table() %>% 
  cumsum() %>% 
  "*"(100) %>% 
  pander()
```

| Associate | Bachelor’s | Master’s | Doctoral |
|:---------:|:----------:|:--------:|:--------:|
|   15.62   |   68.75    |   87.5   |   100    |

-   all together

``` r
EX_DATA_DF %>%
  group_by(Degree) %>% 
  summarize(fi=n()) %>% 
  mutate(percent = round(prop.table(fi)*100,2))%>% 
  mutate(cum.fi = cumsum(fi)) %>% 
  mutate(cum.percent = cumsum(percent)) %>% 
  pander()
```

|   Degree   | fi  | percent | cum.fi | cum.percent |
|:----------:|:---:|:-------:|:------:|:-----------:|
| Associate  |  5  |  15.62  |   5    |    15.62    |
| Bachelor’s | 17  |  53.12  |   22   |    68.74    |
|  Master’s  |  6  |  18.75  |   28   |    87.49    |
|  Doctoral  |  4  |  12.5   |   32   |    99.99    |

-   contingency table with row and column totals (a)

``` r
EX_DATA_DF %>% select(Gender,Degree) %>%
  table() %>%
  addmargins() %>% 
  pander()
```

|            | Associate | Bachelor’s | Master’s | Doctoral | Sum |
|:----------:|:---------:|:----------:|:--------:|:--------:|:---:|
| **Female** |     2     |     8      |    2     |    1     | 13  |
|  **Male**  |     3     |     9      |    4     |    3     | 19  |
|  **Sum**   |     5     |     17     |    6     |    4     | 32  |

-   contingency table with row and column totals (b)

``` r
library(pander)
library(janitor)
EX_DATA_DF %>% 
  group_by(Gender,Degree) %>% 
  summarise(fij=n(), .groups = 'drop') %>% 
  spread(Degree,fij) %>% 
  adorn_totals(name = 'col.sum') %>% 
  adorn_totals(name = 'row.sum', where = 'col') %>% 
  pander()
```

| Gender  | Associate | Bachelor’s | Master’s | Doctoral | row.sum |
|:-------:|:---------:|:----------:|:--------:|:--------:|:-------:|
| Female  |     2     |     8      |    2     |    1     |   13    |
|  Male   |     3     |     9      |    4     |    3     |   19    |
| col.sum |     5     |     17     |    6     |    4     |   32    |
