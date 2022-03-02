Chapter 11: Multiple Linear Regression
================

## Section 11.3: Categorical Independent Variables

### data (Four-Month Weight Loss Program)

``` r
library(pander) #nice tables
library(tidyverse) # %>%  and ggplot2
DFMLM <- readRDS("../data/WeightLossData.rds")
DFMLM[c(1:3,23:24),] %>% pander
```

|        | Age | Weight | BMI  | Fat  | ExerciseTime | WeightLoss |
|:------:|:---:|:------:|:----:|:----:|:------------:|:----------:|
| **1**  | 33  |   80   | 28.1 | 36.5 |   Morning    |    6.6     |
| **2**  | 27  |  96.5  | 31.5 | 44.3 |  Afternoon   |    7.8     |
| **3**  | 22  |   87   | 30.8 | 42.1 |   Morning    |     8      |
| **23** | 33  |  99.8  | 34.6 | 48.9 |  Afternoon   |    8.7     |
| **24** | 36  |  77.7  | 29.6 | 39.7 |  Afternoon   |    5.7     |

### MLM: total weight loss as a linear function of BMI and Exercise Time

-   Exercise time as a dummy variable (Afternoon = 0, Morning = 1)

``` r
MLM_BMI_Time = lm(formula = WeightLoss ~ BMI + ExerciseTime, data = DFMLM)
summary(MLM_BMI_Time)
```

    ## 
    ## Call:
    ## lm(formula = WeightLoss ~ BMI + ExerciseTime, data = DFMLM)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.6200 -0.7016 -0.1395  0.4000  1.8854 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -14.29495    2.75778  -5.184 3.89e-05 ***
    ## BMI                   0.69089    0.09162   7.541 2.10e-07 ***
    ## ExerciseTimeMorning   1.71543    0.42252   4.060 0.000563 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9748 on 21 degrees of freedom
    ## Multiple R-squared:  0.7424, Adjusted R-squared:  0.7179 
    ## F-statistic: 30.26 on 2 and 21 DF,  p-value: 6.526e-07

``` r
predictions = predict(MLM_BMI_Time)
ggplot(data=DFMLM, 
       mapping=aes(x=BMI, y=WeightLoss,color=ExerciseTime)) + 
  geom_point(col="black",data = DFMLM[DFMLM$ExerciseTime=="Morning",])+
  geom_point(col="red",data = DFMLM[DFMLM$ExerciseTime=="Afternoon",])+
  geom_line(mapping=aes(y=predictions))+
  scale_color_manual(values= c("red", "black"))+
  labs(y="Weight Loss (kg)", x= expression(paste("Baseline BMI (",X[1],")")))
```

![](categorical_files/figure-gfm/MLM%20categorical-1.png)<!-- -->

### MLM: total weight loss as a linear function of Age, BMI and Exercise Time

``` r
summary(lm(WeightLoss ~ Age + BMI + ExerciseTime, data = DFMLM))
```

    ## 
    ## Call:
    ## lm(formula = WeightLoss ~ Age + BMI + ExerciseTime, data = DFMLM)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.3145 -0.6909  0.0015  0.5186  1.4407 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -10.25248    3.02947  -3.384  0.00295 ** 
    ## Age                  -0.07841    0.03322  -2.361  0.02851 *  
    ## BMI                   0.63075    0.08685   7.263 5.02e-07 ***
    ## ExerciseTimeMorning   1.32197    0.41759   3.166  0.00486 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8834 on 20 degrees of freedom
    ## Multiple R-squared:  0.7985, Adjusted R-squared:  0.7683 
    ## F-statistic: 26.43 on 3 and 20 DF,  p-value: 3.681e-07

## Section 11.4: General Linear Model

-   Anscombe Set II

``` r
head(datasets::anscombe, 3) # x2 and y2 form Anscombe Set II
```

    ##   x1 x2 x3 x4   y1   y2    y3   y4
    ## 1 10 10 10  8 8.04 9.14  7.46 6.58
    ## 2  8  8  8  8 6.95 8.14  6.77 5.76
    ## 3 13 13 13  8 7.58 8.74 12.74 7.71

``` r
summary(Quadratic <-  lm(formula = y2 ~ x2 + I(x2^2), data = anscombe) )
```

    ## 
    ## Call:
    ## lm(formula = y2 ~ x2 + I(x2^2), data = anscombe)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0013287 -0.0011888 -0.0006294  0.0008741  0.0023776 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -5.9957343  0.0043299   -1385   <2e-16 ***
    ## x2           2.7808392  0.0010401    2674   <2e-16 ***
    ## I(x2^2)     -0.1267133  0.0000571   -2219   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.001672 on 8 degrees of freedom
    ## Multiple R-squared:      1,  Adjusted R-squared:      1 
    ## F-statistic: 7.378e+06 on 2 and 8 DF,  p-value: < 2.2e-16

-   predicction

``` r
predict(object = Quadratic, newdata = data.frame(x2 = 7), interval = "confidence")
```

    ##        fit      lwr      upr
    ## 1 7.261189 7.259602 7.262775

## Section 11.5: Categorical Response Variable

-   data

``` r
dfmachine <- readRDS("../data/dfmachine.rds")
head(dfmachine)
```

    ##   dimension oldmachine
    ## 1     14.10          0
    ## 2     15.18          1
    ## 3     15.63          1
    ## 4     13.21          0
    ## 5     15.02          1
    ## 6     14.92          0

-   Let’s fit a logistic model (family = binomial)

``` r
summary(
  LogisticModel <-  glm(formula = oldmachine ~ dimension, 
                        data = dfmachine,
                        family = binomial))
```

    ## 
    ## Call:
    ## glm(formula = oldmachine ~ dimension, family = binomial, data = dfmachine)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6570  -0.4581  -0.0275   0.4236   1.9042  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)  -48.935     22.605  -2.165   0.0304 *
    ## dimension      3.352      1.548   2.166   0.0303 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 22.181  on 15  degrees of freedom
    ## Residual deviance: 10.413  on 14  degrees of freedom
    ## AIC: 14.413
    ## 
    ## Number of Fisher Scoring iterations: 6

-   estimated probability

``` r
(pred = predict(LogisticModel, newdata = data.frame(dimension = 14.5),
                type="response"))
```

    ##         1 
    ## 0.4188703

-   plot

``` r
ggplot(dfmachine, aes(x=dimension, y=oldmachine)) + 
  geom_point() +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  labs(x="Dimensions", y="Probability of Old Machine")+
  geom_hline(yintercept = pred)+
  geom_segment(aes(x=14.5,xend=14.5,y=0,yend=0.412))
```

![](categorical_files/figure-gfm/MLM%20logistic-1.png)<!-- -->
