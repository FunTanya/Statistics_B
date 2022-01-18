Chapter 7: Welchs and Kruskal-Wallis
================

One-Way ANOVA with `onewaytests` package

### Example 7.5: A soft drink problem

``` r
SoftDrink <-  read.csv("../data/SoftDrink.csv", stringsAsFactors = TRUE)
library(pander)
library(tidyverse)
SoftDrink %>% 
  group_by(Color) %>% 
  summarize(n=n(),Mean = mean(Sales),Std = sd(Sales)) %>% 
  pander()
```

| Color  |  n  | Mean  |  Std  |
|:------:|:---:|:-----:|:-----:|
|  blue  |  6  | 99.83 | 11.34 |
| green  |  6  | 127.5 | 9.711 |
|  red   |  7  | 196.4 | 11.25 |
| yellow |  6  | 151.3 | 28.86 |

-   normality assumption

``` r
library(onewaytests)
nor.test(formula = Sales ~ Color, data = SoftDrink, alpha = 0.05)
```

    ## 
    ##   Shapiro-Wilk Normality Test (alpha = 0.05) 
    ## -------------------------------------------------- 
    ##   data : Sales and Color 
    ## 
    ##    Level Statistic   p.value   Normality
    ## 1   blue 0.8865717 0.3006129  Not reject
    ## 2  green 0.8627514 0.1988074  Not reject
    ## 3    red 0.9199404 0.4689291  Not reject
    ## 4 yellow 0.9474516 0.7196053  Not reject
    ## --------------------------------------------------

![](Chapter_7_Welch_KW_files/figure-gfm/Normality%20for%20Soft%20Drink-1.png)<!-- -->

-   homogeneity assumption

``` r
homog.test(formula = Sales ~ Color, data = SoftDrink,
           method = "Bartlett", alpha = 0.05)
```

    ## 
    ##   Bartlett's Homogeneity Test (alpha = 0.05) 
    ## ----------------------------------------------- 
    ##   data : Sales and Color 
    ## 
    ##   statistic  : 8.519684 
    ##   parameter  : 3 
    ##   p.value    : 0.03640795 
    ## 
    ##   Result     : Variances are not homogeneous. 
    ## -----------------------------------------------

-   Welch’s ANOVA

``` r
MyWelch  =  welch.test(formula = Sales ~ Color, data = SoftDrink, alpha = 0.05)
```

    ## 
    ##   Welch's Heteroscedastic F Test (alpha = 0.05) 
    ## ------------------------------------------------------------- 
    ##   data : Sales and Color 
    ## 
    ##   statistic  : 77.60087 
    ##   num df     : 3 
    ##   denom df   : 11.18273 
    ##   p.value    : 9.105292e-08 
    ## 
    ##   Result     : Difference is statistically significant. 
    ## -------------------------------------------------------------

### Example 7.6: Post Hoc Analysis

-   multiple comparison with Holm correction

``` r
paircomp(x = MyWelch, adjust.method = "holm")
```

    ## 
    ##   Holm Correction (alpha = 0.05) 
    ## ----------------------------------------------------- 
    ##   Level (a) Level (b)      p.value   No difference
    ## 1      blue     green 4.565633e-03          Reject
    ## 2      blue       red 7.701674e-08          Reject
    ## 3      blue    yellow 1.664218e-02          Reject
    ## 4     green       red 6.616904e-07          Reject
    ## 5     green    yellow 1.026923e-01      Not reject
    ## 6       red    yellow 2.090354e-02          Reject
    ## -----------------------------------------------------

-   Recommended Games-Howell Test:

``` r
rstatix::games_howell_test(formula = Sales ~ Color, data = SoftDrink) %>% pander()
```

|  .y.  | group1 | group2 | estimate | conf.low | conf.high |  p.adj   | p.adj.signif |
|:-----:|:------:|:------:|:--------:|:--------:|:---------:|:--------:|:------------:|
| Sales |  blue  | green  |  27.67   |  8.941   |   46.39   |  0.005   |     \*\*     |
| Sales |  blue  |  red   |   96.6   |  77.58   |   115.6   | 6.72e-08 |   \*\*\*\*   |
| Sales |  blue  | yellow |   51.5   |  8.747   |   94.25   |  0.022   |      \*      |
| Sales | green  |  red   |  68.93   |  51.43   |   86.43   | 6.82e-07 |   \*\*\*\*   |
| Sales | green  | yellow |  23.83   |  -18.93  |   66.6    |  0.312   |      ns      |
| Sales |  red   | yellow |  -45.1   |  -87.81  |  -2.382   |   0.04   |      \*      |

### Example 7.5: A Consumer Rating Problem

``` r
ConsumersRating <- read.csv("../data/ConsumersRating.csv",stringsAsFactors = TRUE)
ConsumersRating %>% 
  group_by(product) %>% 
  summarize(n=n(),Mean = mean(rating),Std = sd(rating)) %>% 
  pander()
```

| product |  n  | Mean |  Std  |
|:-------:|:---:|:----:|:-----:|
|    A    |  5  | 7.6  | 2.074 |
|    B    |  5  | 5.6  | 2.51  |
|    C    |  5  |  4   | 2.121 |

-   Kruskal-Wallis Test

``` r
KW_test_products <-  kw.test(formula = rating ~ product, 
                     data = ConsumersRating, alpha = 0.1)
```

    ## 
    ##   Kruskal-Wallis Test (alpha = 0.1) 
    ## ------------------------------------------------------------- 
    ##   data : rating and product 
    ## 
    ##   statistic  : 4.916576 
    ##   parameter  : 2 
    ##   p.value    : 0.08558136 
    ## 
    ##   Result     : Difference is statistically significant. 
    ## -------------------------------------------------------------

-   Example 7.8: Post Hoc Analysis
-   multiple comparison with Holm correction

``` r
paircomp(KW_test_products, adjust.method = "holm" )
```

    ## 
    ##   Holm Correction (alpha = 0.1) 
    ## ----------------------------------------------------- 
    ##   Level (a) Level (b)   p.value   No difference
    ## 1         A         B 0.4145997      Not reject
    ## 2         A         C 0.1067365      Not reject
    ## 3         B         C 0.4145997      Not reject
    ## -----------------------------------------------------

-   Recommended Dunn’s Test with Holm correction:

``` r
DescTools::DunnTest(rating ~ as.factor(product), data = ConsumersRating, 
                    method = "holm")
```

    ## 
    ##  Dunn's test of multiple comparisons using rank sums : holm  
    ## 
    ##     mean.rank.diff   pval    
    ## B-A           -3.4 0.4494    
    ## C-A           -6.2 0.0805 .  
    ## C-B           -2.8 0.4494    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
