Chapter 1: Tables
================

# Employees of a Small Business Company: Using Simple Commnads

-   reading data:

``` r
EX_DATA_DF <- readRDS("data/EX_DATA_DF.rds")
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
