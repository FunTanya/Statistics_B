Chapter 1: Plots (simple)
================

# Employees of a Small Business Company: Categorical Data

-   reading data:

``` r
EX_DATA_DF <- readRDS("../data/EX_DATA_DF.rds")
EX_DATA_DF$Years <- floor(EX_DATA_DF$Months/12)
```

-   bar chart using plot or barplot

``` r
plot(EX_DATA_DF$Degree, xlab = "Academic Degree", ylab = "Frequency")
barplot(table(EX_DATA_DF$Degree), xlab = "Academic Degree", ylab = "Frequency")
```

![](Chapter_1_Plots_Simple_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
- Pareto Chart

``` r
qcc::pareto.chart(table(EX_DATA_DF$Degree), main =" Pareto Chart for Degree")
```

![](Chapter_1_Plots_Simple_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

    ##             
    ## Pareto chart analysis for table(EX_DATA_DF$Degree)
    ##              Frequency Cum.Freq. Percentage Cum.Percent.
    ##   Bachelor's    17.000    17.000     53.125       53.125
    ##   Master's       6.000    23.000     18.750       71.875
    ##   Associate      5.000    28.000     15.625       87.500
    ##   Doctoral       4.000    32.000     12.500      100.000

-   Grouped Bar Charts

``` r
barplot(table(EX_DATA_DF$Degree,EX_DATA_DF$Gender), beside = TRUE)
```

![](Chapter_1_Plots_Simple_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
- Component Bar Charts

``` r
barplot(table(EX_DATA_DF$Degree,EX_DATA_DF$Gender), beside = FALSE)
```

![](Chapter_1_Plots_Simple_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
- Spine Plot

``` r
spineplot(table(EX_DATA_DF$Gender,EX_DATA_DF$Degree), main = "Spine Plot for Company Employee Data")
```

![](Chapter_1_Plots_Simple_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
- or Mosaic Plot

``` r
mosaicplot(table(EX_DATA_DF$Gender,EX_DATA_DF$Degree), main = "Mosaic Plot for Company Employee Data")
```

![](Chapter_1_Plots_Simple_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
- Pie Chart

``` r
pie(table(EX_DATA_DF$Degree))
```

![](Chapter_1_Plots_Simple_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

# Employees of a Small Business Company: Numerical Data

-   Bar chart

``` r
plot(table(EX_DATA_DF$Children))
```

![](Chapter_1_Plots_Simple_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
barplot(table(factor(EX_DATA_DF$Children, levels = 0:5)))
```

![](Chapter_1_Plots_Simple_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->
- Dot plot

``` r
stripchart(EX_DATA_DF$Years, method = "stack", offset = 0.5, pch=19)
```

![](Chapter_1_Plots_Simple_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
- Histogram

``` r
hist(EX_DATA_DF$Salary,breaks = 8, main = "Histogram of Employee Salaries", xlab = "Salary")
```

![](Chapter_1_Plots_Simple_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
- Ogive

``` r
plot(ecdf(EX_DATA_DF$Salary),verticals = FALSE)
```

![](Chapter_1_Plots_Simple_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
plot(actuar::ogive(EX_DATA_DF$Salary), main = "Ogive for Grouped Data")
```

![](Chapter_1_Plots_Simple_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->
- Scatter Plot

``` r
plot(EX_DATA_DF$Years,EX_DATA_DF$Salary, xlab = "Duration", ylab = "Salary")
```

![](Chapter_1_Plots_Simple_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
