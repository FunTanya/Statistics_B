# data for Chapter 10 ####
AppleTrees <- readRDS("data/AppleTrees.rds")
AppleTrees <- read.csv("data/AppleTrees.csv")
head(x = AppleTrees,3)

# Simple Linear Model ####
# the order is important formula = Y ~ X (response ~ explanatory)
(SLModel <- lm(formula = Yield ~ Fertilizer, data = AppleTrees))
# all fitted values
fitted(object = SLModel) 
predict(object = SLModel, # our explanatory variable X (Fertilizer) takes value of 12
        newdata = data.frame(Fertilizer = 12))

# population variance estimate = residual standard error (s) ####
sigma(SLModel) 
#or ... see possible outcomes from summary(object = SLModel)
names(summary(object = SLModel))
# and thus the population variance estimate is
summary(SLModel)$sigma 

# statistical inference ####
# point estimates of betas and test for zero value
summary(SLModel)$coefficients
# interval estimates of betas
confint(object = SLModel,level = 0.95)
# coefficient of determination
summary(SLModel)$r.squared
# test for zero correlation coefficient
cor.test(formula = ~ Yield + Fertilizer, data = AppleTrees)

# Using SLM for estimation and prediction
predict(object = SLModel, data.frame(Fertilizer = 12), level = 0.95,
        interval = "confidence") # interval for mean value
predict(object = SLModel, data.frame(Fertilizer = 12), level = 0.95,
        interval = "prediction") # interval for individual value

# Residual Analysis
#Third Data set from Ansombe's Quartet (+ x increasing)
A3 <- data.frame(x=anscombe$x3, y=anscombe$y3)
A3 <- A3[order(A3$x),];rownames(A3) <- 1:11
A3
Anscombe_LM_Third <- lm(y~x, data=A3)
# outlier:
residuals(object = Anscombe_LM_Third)[10] # raw residual for 10th observation
hatvalues(model = Anscombe_LM_Third)[10] # leverage hii for i=10
rstandard(model = Anscombe_LM_Third)[10] # internally studentized
rstudent(model = Anscombe_LM_Third)[10]  # externally studentized

#checking normality
shapiro.test(rstudent(SLModel))

#AppleTreeData
residuals(object = SLModel) # raw residuals 
hatvalues(model = SLModel) # leverages hii 
rstandard(model = SLModel) # internally studentized
rstudent(model = SLModel)  # externally studentized

# the highes externally studentized residual
max(ABSstudentized <- abs(rstudent(SLModel)))
AppleTrees[which.max(ABSstudentized),]

# the highest leverage
max(MAXleverage <- abs(hatvalues(SLModel)))
AppleTrees[which.max(MAXleverage),]

# Cook's distance
(Cook_mean_3 <- mean(cooks.distance(model = SLModel))*3) # three times average D
# let us see influential observations:
AppleTrees[cooks.distance(model = SLModel) > Cook_mean_3,]
cooks.distance(model = SLModel)[10] # specific Cook's Distance

# the effect of deleting the 10th observation
AppleTrees_omit_10 <- AppleTrees[-10,] # new data, 10th observation deleted
SLModel_omit_10 <- lm(formula = Yield ~ Fertilizer, data = AppleTrees_omit_10)
summary(object = SLModel_omit_10)