#Example 9.2 ####
HairEyeColor <- readRDS("data/HairEyeColor.rds")
head(HairEyeColor, 3)
# contingency table
table(HairEyeColor)
# chi-square test
(HairEyeTest = chisq.test(x = table(HairEyeColor)))
# expected frequencies (assumptions met)
HairEyeTest$expected
# post hoc analysis: adjusted residuals
HairEyeTest$stdres
# threshold
qnorm(p = 1 - 0.05/2)
# significant differences
abs(HairEyeTest$stdres) > qnorm(p = 1 - 0.05/2)


#Example 9.3 ####
GenderSeason <- read.csv("data/season.csv")
(ContTable <- table(GenderSeason))
GSTest <- chisq.test(x = ContTable)
GSTest$expected
abs(GSTest$stdres) > qnorm(p = 1 - 0.05/2)

#Example 9.4 ####
Employees <- readRDS("data/Employees.rds")
head(Employees,3)
table(Employees)
(EmployeeTest = chisq.test(table(Employees),
  correct = FALSE) # correction available only in 2x2 tables
  ) # correct = TRUE by default
EmployeeTest$expected
print(EmployeeTest$stdres) # print() is optional

#Example 9.6 ####
SpamData <- readRDS("data/SpamData.rds")
head(SpamData,3)
table(SpamData)
# use chi-square test to check assumptions:
chisq.test(table(SpamData))$expected
# but conduct Fisher's exact test:
fisher.test(x = table(SpamData))
fisher.test(x = table(SpamData),
            alternative = "g") # handy abbreviation for "greater"

#Example 9.7 ####
BleedingData <- readRDS("data/BleedingData.rds")
head(BleedingData,3)
#  McNemar’s Test with mcnemar.test() is alwasy two-tailed
mcnemar.test(x = table(BleedingData), 
             correct = FALSE) # without continuity correction
# one-tailed test: adjust your p-value, or run prop.test():
prop.test(x = 28, n = 12+28, p = 0.5, correct = FALSE,
          alternative = "g") # for any alternative
# exact test for small samples:
binom.test(x = 28, n = 12 + 28, p = 0.5, alternative = "g")

