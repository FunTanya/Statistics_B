Chapter 8: Goodness-of-fit Tests
================

Graphical Techniques

### Normal Plots

``` r
library(pander) #nice tables
library(tidyverse) # %>%  and ggplot2
WaitingTimes <- read.csv("../data/WaitingTimes.csv")$time
sorted.data <- sort(WaitingTimes)
n <- length(sorted.data)
```

-   theoretical CDF *p*<sub>*i*</sub>

``` r
pi <-  (1:n - 3/8)/(n+1-2*3/8)
```

-   empirical CDF *F*(*x*<sub>(*i*)</sub>)

``` r
Fxi <- pnorm(sorted.data, mean(sorted.data), sd(sorted.data))
```

-   theoretical quantiles *z*<sub>1 − *p*<sub>*i*</sub></sub> (z-scores)

``` r
z.scores <- qnorm(pi)
```

-   sample quantiles *x*<sub>(*i*)</sub> (sorted data)

``` r
sorted.data
```

    ##  [1] 2.3 2.8 3.1 3.8 5.0 6.3 7.0 7.4 7.9 8.9

-   summary

``` r
data.frame(pi, Fxi, z.scores, data = sorted.data ) %>% pander()
```

|   pi    |   Fxi   | z.scores | data |
|:-------:|:-------:|:--------:|:----:|
| 0.06098 | 0.09119 |  -1.547  | 2.3  |
| 0.1585  |  0.131  |    -1    | 2.8  |
| 0.2561  | 0.1599  | -0.6554  | 3.1  |
| 0.3537  | 0.2424  | -0.3755  | 3.8  |
| 0.4512  | 0.4245  | -0.1226  |  5   |
| 0.5488  | 0.6405  |  0.1226  | 6.3  |
| 0.6463  | 0.7441  |  0.3755  |  7   |
| 0.7439  | 0.7954  |  0.6554  | 7.4  |
| 0.8415  | 0.8502  |    1     | 7.9  |
|  0.939  | 0.9279  |  1.547   | 8.9  |

-   Normal PP Plot

``` r
library(qqplotr)
df=data.frame(x=sorted.data)
mean <- mean(df$x)
sd <- sd(df$x)

df %>% ggplot(aes(sample=x)) +
    stat_pp_line(linetype = "dashed")+
    stat_pp_point(distribution = "norm", dparams = list(mean,sd)) +
    labs(y ="Empirical CDF", x = "Theoretical CDF")
```

![](Chapter_8_graphical_files/figure-gfm/pp%20plot-1.png)<!-- -->

-   Normal QQ Plot

``` r
df %>% ggplot(aes(sample=x)) +
    geom_qq_line(linetype = "dashed")+
    geom_qq() +
    labs(y = "Sample Quantiles", x = "Theoretical Quantiles")
```

![](Chapter_8_graphical_files/figure-gfm/qq%20plot-1.png)<!-- -->
