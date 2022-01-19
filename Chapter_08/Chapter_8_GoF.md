Chapter 8: Goodness-of-fit Tests
================

### Example 8.2

-   test and its attributes:

``` r
(MilkShakeTest <-
  chisq.test(x = c(29, 20, 18, 13),
             p = c(0.5, 0.25, 0.15, 0.1)))
```

    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  c(29, 20, 18, 13)
    ## X-squared = 9.15, df = 3, p-value = 0.02736

``` r
names(MilkShakeTest)
```

    ## [1] "statistic" "parameter" "p.value"   "method"    "data.name" "observed" 
    ## [7] "expected"  "residuals" "stdres"

-   summary and post hoc analysis:

``` r
flavor <- c("Strawberry", "Chocolate","Vanilla","Banana")
with(MilkShakeTest,
     data.frame(
        flavor, observed, expected, residuals, stdres, 
        phc = abs(stdres) > qnorm(1-0.05/2)
     )) %>%
     pander()
```

|   flavor   | observed | expected | residuals | stdres |  phc  |
|:----------:|:--------:|:--------:|:---------:|:------:|:-----:|
| Strawberry |    29    |    40    |  -1.739   | -2.46  | TRUE  |
| Chocolate  |    20    |    20    |     0     |   0    | FALSE |
|  Vanilla   |    18    |    12    |   1.732   | 1.879  | FALSE |
|   Banana   |    13    |    8     |   1.768   | 1.863  | FALSE |

### Example 8.3

-   probabilities

``` r
(pi = dbinom(x = 0:3, size = 3, prob = 1/6))
```

    ## [1] 0.57870370 0.34722222 0.06944444 0.00462963

-   test

``` r
(My.test <- chisq.test( x = c(71, 58, 20 + 1),
            p = c(pi[1:2], sum(pi[3:4]))))
```

    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  c(71, 58, 20 + 1)
    ## X-squared = 12.351, df = 2, p-value = 0.00208

-   summary and phc

``` r
with(My.test,
     data.frame(
        x = 0:2, observed, expected, residuals, stdres, 
        phc = abs(stdres) > qnorm(1-0.01/2)
     )) %>%
     pander()
```

|  x  | observed | expected | residuals | stdres |  phc  |
|:---:|:--------:|:--------:|:---------:|:------:|:-----:|
|  0  |    71    |  86.81   |  -1.696   | -2.614 | TRUE  |
|  1  |    58    |  52.08   |  0.8198   | 1.015  | FALSE |
|  2  |    21    |  11.11   |   2.967   | 3.083  | TRUE  |

### Example 8.4

``` r
(DefPartTest =
   chisq.test(x = c(11, 229),p = c(0.025, 0.975),
              correct = FALSE) # without continuity correction
) 
```

    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  c(11, 229)
    ## X-squared = 4.2735, df = 1, p-value = 0.03871

``` r
DefPartTest$stdres
```

    ## [1]  2.067246 -2.067246

### Example 8.5

-   probabilities

``` r
round(dpois(x = 0:5, lambda = 161/144), 4)
```

    ## [1] 0.3269 0.3655 0.2043 0.0762 0.0213 0.0048

-   test

``` r
(PoisonTest <- chisq.test(x = c(48,52,28,13,3),
                          p =c(dpois(x = 0:3,lambda = 161/144),
                               ppois(q = 3,lambda = 161/144, lower.tail = FALSE)))
)
```

    ## Warning in chisq.test(x = c(48, 52, 28, 13, 3), p = c(dpois(x = 0:3, lambda =
    ## 161/144), : Chi-squared approximation may be incorrect

    ## 
    ##  Chi-squared test for given probabilities
    ## 
    ## data:  c(48, 52, 28, 13, 3)
    ## X-squared = 0.68046, df = 4, p-value = 0.9537

-   summary (we fail to reject *H*<sub>0</sub>, no post hoc analysis
    needed)

``` r
with(PoisonTest,
     data.frame(
        x = 0:4, observed, expected, residuals, stdres
     )) %>%
     pander()
```

|  x  | observed | expected | residuals | stdres  |
|:---:|:--------:|:--------:|:---------:|:-------:|
|  0  |    48    |  47.08   |  0.1347   | 0.1642  |
|  1  |    52    |  52.63   | -0.08729  | -0.1096 |
|  2  |    28    |  29.42   |  -0.2624  | -0.2942 |
|  3  |    13    |  10.97   |  0.6143   | 0.6391  |
|  4  |    3     |  3.902   |  -0.4565  | -0.4629 |
