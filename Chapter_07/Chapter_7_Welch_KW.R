## ----setup, include=FALSE---------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----Soft Drink Data, warning=FALSE, message=FALSE--------------------------------------------------------------
SoftDrink <-  read.csv("../data/SoftDrink.csv", stringsAsFactors = TRUE)
library(pander)
library(tidyverse)
SoftDrink %>% 
  group_by(Color) %>% 
  summarize(n=n(),Mean = mean(Sales),Std = sd(Sales)) %>% 
  pander()


## ----Normality for Soft Drink-----------------------------------------------------------------------------------
library(onewaytests)
nor.test(formula = Sales ~ Color, data = SoftDrink, alpha = 0.05)


## ----Homogeneity for Soft Drink---------------------------------------------------------------------------------
homog.test(formula = Sales ~ Color, data = SoftDrink,
           method = "Bartlett", alpha = 0.05)


## ----Welch------------------------------------------------------------------------------------------------------
MyWelch  =  welch.test(formula = Sales ~ Color, data = SoftDrink, alpha = 0.05)


## ----Multiple comparison with Holm correction-------------------------------------------------------------------
paircomp(x = MyWelch, adjust.method = "holm")


## ----Games-Howell Test------------------------------------------------------------------------------------------
rstatix::games_howell_test(formula = Sales ~ Color, data = SoftDrink) %>% pander()


## ----Consumer Rating Data---------------------------------------------------------------------------------------
ConsumersRating <- read.csv("../data/ConsumersRating.csv",stringsAsFactors = TRUE)
ConsumersRating %>% 
  group_by(product) %>% 
  summarize(n=n(),Mean = mean(rating),Std = sd(rating)) %>% 
  pander()


## ----Kruskal-Wallis---------------------------------------------------------------------------------------------
KW_test_products <-  kw.test(formula = rating ~ product, 
                     data = ConsumersRating, alpha = 0.1)


## ----multiple comparison with Holm correction KW----------------------------------------------------------------
paircomp(KW_test_products, adjust.method = "holm" )


## ----Dunns Test-------------------------------------------------------------------------------------------------
DescTools::DunnTest(rating ~ as.factor(product), data = ConsumersRating, 
                    method = "holm")

