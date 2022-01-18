Chapter 1: Tables
================

## R Markdown

# Employees of a Small Business Company

``` r
EX_DATA_DF <- readRDS("data/EX_DATA_DF.rds")
```

frequency distribution for Degree

``` r
fi = table(EX_DATA_DF$Degree)
print(fi)
```

    ## 
    ##  Associate Bachelor's   Master's   Doctoral 
    ##          5         17          6          4
