library(onewaytests)
# Example 7.1 (including assumptions) ###########
# Male Runners
#read data from *.rds
MaleRunners <- readRDS("data/MaleRunners.rds")
# OR read data from *.csv
MaleRunners <- read.csv("data/MaleRunners.csv")
MaleRunners$program <- as.factor(MaleRunners$program)
# a data frame with factor program:
str(MaleRunners)


# normality assumption (Shapiro-Wilk Tests by default)
nor.test(formula = time ~ program, data = MaleRunners, alpha = 0.05)
# homogeneity assumption
homog.test(formula = time ~ program, data = MaleRunners, 
           method = "Bartlett", alpha = 0.05)
# ANOVA F-test (fail to reject H0)
aov.test(formula = time ~ program, data = MaleRunners, alpha = 0.05) 

# Female Runners
#read data from *.rds
FemaleRunners <- readRDS("data/FemaleRunners.rds")
# OR read data from *.csv
FemaleRunners <- read.csv("data/FemaleRunners.csv")
FemaleRunners$program <- as.factor(FemaleRunners$program)
# program is Factor
str(FemaleRunners)


# normality assumption
nor.test(formula = time ~ program, data = FemaleRunners, alpha = 0.05)
# homogeneity assumption (Example 7.3)
homog.test(formula = time ~ program, data = FemaleRunners, 
           method = "Bartlett", alpha = 0.05)
# ANOVA F-test (reject H0)
aov.test(formula = time ~ program, data = FemaleRunners, alpha = 0.05)


# Example 7.2 ########################################
# multiple comparison with Holm correction
Anova_Female = aov.test(formula = time ~ program, data = FemaleRunners, alpha = 0.05)
paircomp(Anova_Female, adjust.method = "holm")
# Recommended Tukey-Kramer Test:
TukeyHSD( aov(formula = time ~ program, data = FemaleRunners) )

# Example 7.4 ########################################
homog.test(formula = time ~ program, data = FemaleRunners,
                alpha = 0.05, method = "Levene") # location parameter = mean
                        

######################################################
# Example 7.5 ########################################
SoftDrink <-  read.csv("data/SoftDrink.csv", stringsAsFactors = TRUE)
# normality assumption
nor.test(formula = Sales ~ Color, data = SoftDrink, alpha = 0.05)
# homogeneity assumption 
homog.test(formula = Sales ~ Color, data = SoftDrink,
           method = "Bartlett", alpha = 0.05)
# Welch’s ANOVA
MyWelch  =  welch.test(formula = Sales ~ Color, data = SoftDrink, alpha = 0.05)
# Example 7.6 ########################################
# multiple comparison with Holm correction
paircomp(x = MyWelch, adjust.method = "holm")
# Recommended Games-Howell Test:
rstatix::games_howell_test(formula = Sales ~ Color, data = SoftDrink)


######################################################
# Example 7.7 ####
ConsumersRating <- read.csv("data/ConsumersRating.csv",stringsAsFactors = TRUE)
# Kruskal-Wallis Test 
KW_test_products <-  kw.test(formula = rating ~ product, 
                     data = ConsumersRating, alpha = 0.1)
# Example 7.8 ####
# multiple comparison with Holm correction
paircomp(KW_test_products, adjust.method = "holm" )
# Recommended  Dunn's Test with Holm correction:
DescTools::DunnTest(rating ~ as.factor(product), data = ConsumersRating, 
                    method = "holm")
