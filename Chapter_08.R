# Probability Plots
WaitingTimes <- c(readRDS("data/Times.rds"),7)
data <- sort(WaitingTimes)
n <- length(data)
#theoretical probabilities
pi <-  (1:n - 3/8)/(n+1-2*3/8)
#ECDF
pnorm(data, mean(data), sd(data))
#theoretical z-scores
qnorm(pi)
#sample
data


# Example 8.2 ####
(MilkShakeTest <-
  chisq.test(x = c(29, 20, 18, 13),
             p = c(0.5, 0.25, 0.15, 0.1)))
names(MilkShakeTest)
MilkShakeTest$expected
MilkShakeTest$stdres
abs(MilkShakeTest$stdres)>qnorm(1-0.05/2)


# Example 8.3 ####
round(dbinom(x = 0:3, size = 3, prob = 1/6), 4)
pi = dbinom(x = 0:3, size = 3, prob = 1/6)
(My.test <- chisq.test( x = c(71, 58, 20 + 1),
            p = c(pi[1:2], sum(pi[3:4]))))
My.test$stdres
abs(My.test$stdres)>qnorm(1-0.01/2)

# Example 8.4 ####
(DefPartTest =
   chisq.test(x = c(11, 229),p = c(0.025, 0.975),
              correct = FALSE) # without continuity correction
) 
DefPartTest$stdres

# Example 8.5 ####
round(dpois(x = 0:5, lambda = 161/144), 4)
round(1 - ppois(q=5, lambda = 161/144), 4)
(PoisonTest <- chisq.test(x = c(48,52,28,13,3),
                          p =c(dpois(x = 0:3,lambda = 161/144),
                               ppois(q = 3,lambda = 161/144, lower.tail = FALSE)))
)
PoisonTest$expected


# Example 8.6 ####
Volumes <- readRDS("data/Volumes.rds")
#equiprobalbe qunatiles and frequencies:
n <- length(Volumes)
k <- ceiling(2*n^(2/5))
Myquantiles <- qnorm(p = 0:k /k,mean = mean(Volumes),sd = sd(Volumes))
Oi <- hist(Volumes, breaks=c(min(Volumes),Myquantiles[2:8],max(Volumes)),plot = FALSE)$counts
pi <- rep(1/k,k)
Ei <- pi*n
#using nortest package
#library(nortest)
nortest::pearson.test(x = Volumes,
                      adjust = TRUE) # composite H0
nortest::pearson.test(x = Volumes,
                      adjust = FALSE) # simple H0

# Example 8.7 ####
1-.Call(stats:::C_pKolmogorov2x, STAT = 0.2421539, n = 10)
WaitingTimes <- c(readRDS("data/Times.rds"),7)
sorted.data <- sort(WaitingTimes)
ecdf = 1:10/10
ecdfminus = 0:9/10
pi = round(pnorm(sorted.data,mean = 5,sd = 2),3)
D <- max(max(ecdf-pi),max(pi-ecdfminus))




ks.test(x = WaitingTimes, exact = TRUE, #exact test
        y = "pnorm", 5, 2) # normal distribution N( 5 , 4 )
ks.test(x = WaitingTimes, y = "pnorm", 5, 2, exact = FALSE)

# Example 8.8 ####
#library(nortest)
nortest::lillie.test(x = Volumes)
nortest::ad.test(x = Volumes)
#library(DescTools)
DescTools::Skew(Volumes,method = 1)
DescTools::Kurt(Volumes,method = 1)
DescTools::JarqueBeraTest(x = Volumes, robust = FALSE, method = "chisq")
shapiro.test(x = Volumes)
#library(onewaytests)
onewaytests::nor.test(formula = Sales ~ Color, data = SoftDrink, plot= NULL,
                      method = "SW") # Shapiro-Wilk by default
