## ----setup, include=FALSE---------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----reading Male Runners---------------------------------------------------------------------------------------
MaleRunners <- readRDS("../data/MaleRunners.rds")
#MaleRunners <- read.csv("../data/MaleRunners.csv")
#MaleRunners$program <- as.factor(MaleRunners$program)


## ----Malerunners summary, warning=FALSE,message=FALSE-----------------------------------------------------------
str(MaleRunners)
library(tidyverse)
library(pander)
MaleRunners %>% 
  group_by(program) %>% 
  summarize(n=n(),Mean = mean(time),Std = sd(time)) %>% 
  pander()


## ----Normality M------------------------------------------------------------------------------------------------
library(onewaytests)
nor.test(formula = time ~ program, data = MaleRunners, alpha = 0.05)


## ----Homogeneity------------------------------------------------------------------------------------------------
homog.test(formula = time ~ program, data = MaleRunners, 
           method = "Bartlett", alpha = 0.05)


## ----ANOVA F-test M---------------------------------------------------------------------------------------------
aov.test(formula = time ~ program, data = MaleRunners, alpha = 0.05) 


## ----reading Female Runners-------------------------------------------------------------------------------------
FemaleRunners <- readRDS("../data/FemaleRunners.rds")
#FemaleRunners <- read.csv("../data/FemaleRunners.csv")
#FemaleRunners$program <- as.factor(FemaleRunners$program)


## ----Female Runners Summary, warning=FALSE,message=FALSE--------------------------------------------------------
str(FemaleRunners)
FemaleRunners %>% 
  group_by(program) %>% 
  summarize(n=n(),Mean = mean(time),Std = sd(time)) %>% 
  pander()


## ----Normality F------------------------------------------------------------------------------------------------
nor.test(formula = time ~ program, data = FemaleRunners, alpha = 0.05)


## ----Homogeneity Bartlett---------------------------------------------------------------------------------------
homog.test(formula = time ~ program, data = FemaleRunners, 
           method = "Bartlett", alpha = 0.05)


## ----Homogeneity Levene-----------------------------------------------------------------------------------------
homog.test(formula = time ~ program, data = FemaleRunners,
                alpha = 0.05, method = "Levene") # location parameter = mean


## ----ANOVA F-test F---------------------------------------------------------------------------------------------
aov.test(formula = time ~ program, data = FemaleRunners, alpha = 0.05)


## ----Multiple Comparision with Holm Correction------------------------------------------------------------------
Anova_Female = aov.test(formula = time ~ program, data = FemaleRunners, alpha = 0.05, verbose = FALSE)
paircomp(Anova_Female, adjust.method = "holm")


## ----Tukey-Kramer-----------------------------------------------------------------------------------------------
TukeyHSD( aov(formula = time ~ program, data = FemaleRunners) )

