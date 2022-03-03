Chapter 1: Measures
================

-   Employees of a Small Business Company: reading data

``` r
EX_DATA_DF <- readRDS("../data/EX_DATA_DF.rds")
salary <- EX_DATA_DF$Salary
```

## measures of central tendency

-   mean

``` r
mean(salary) 
```

    ## [1] 42246.53

-   median

``` r
median(salary)  
```

    ## [1] 40863.5

-   trimmed mean

``` r
mean(salary,trim=0.1)   # 10% trimmed mean
```

    ## [1] 41307.35

-   mode

``` r
fi = table(EX_DATA_DF$Children)
fi[fi==max(fi)] 
```

    ##  1 
    ## 12

## measures of variability

-   sample variance

``` r
var(salary) 
```

    ## [1] 59235971

-   population variance

``` r
var(salary) * (length(salary)-1) / length(salary)  
```

    ## [1] 57384846

-   sample standard deviation

``` r
sd(salary) 
```

    ## [1] 7696.491

-   sample coefficient of variation

``` r
sd(salary)/mean(salary) 
```

    ## [1] 0.1821804

## measures of shape

-   skewness

``` r
DescTools::Skew(salary,method = 1) # "population" skewness g1
```

    ## [1] 1.277268

``` r
DescTools::Skew(salary,method = 2) # "sample" skewness G1
```

    ## [1] 1.340963

``` r
DescTools::Skew(salary,method = 3) # "sample" skewness b1
```

    ## [1] 1.217867

-   kurtosis

``` r
DescTools::Kurt(salary,method = 1) # "population" kurtosis g2
```

    ## [1] 1.480536

``` r
DescTools::Kurt(salary,method = 2) # "sample" kurtosis G2
```

    ## [1] 1.954699

``` r
DescTools::Kurt(salary,method = 3) # "sample" kurtosis b2
```

    ## [1] 1.204878

## measures of position

-   five-number-summary

``` r
#?quantile # type 1..9 available
quantile(salary)  # type = 7 by default,
```

    ##      0%     25%     50%     75%    100% 
    ## 31049.0 37166.5 40863.5 44428.0 63656.0

-   first decile

``` r
quantile(salary, probs = 0.1)
```

    ##     10% 
    ## 35341.3

-   interquartile range

``` r
IQR(salary) #type = 7 again
```

    ## [1] 7261.5

-   Box and Whisker Plot: simple

``` r
boxplot(salary)
```

![](Chapter_1_Measures_files/figure-gfm/BW%20plot%20simple-1.png)<!-- -->
- Box and Whisker Plot: advanced

``` r
library(tidyverse) #  ggplot2
ggplot(EX_DATA_DF,aes(x=Salary,y="")) +
    geom_boxplot()+
    labs(x ="Salary ($)", y = "")+
    stat_summary(fun=mean, geom="point", shape=4)+
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y =  element_blank(),
          panel.grid.minor = element_blank()
    )+  scale_x_continuous(labels = scales::comma) 
```

![](Chapter_1_Measures_files/figure-gfm/BW%20plot%20advanced-1.png)<!-- -->

## measures of association between two variables

-   sample covariance

``` r
cov(EX_DATA_DF$Salary,EX_DATA_DF$Months)
```

    ## [1] 138600.5

-   correlation coefficient

``` r
cor(EX_DATA_DF$Salary,EX_DATA_DF$Months) 
```

    ## [1] 0.6400281
