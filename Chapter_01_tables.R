######################################################
# Example 1.3                         ################
# Employees of a Small Company #######################
EX_DATA_DF <- readRDS("data/EX_DATA_DF.rds")
# frequency distribution for Degree
table(EX_DATA_DF$Degree)
# percent
prop.table(table(EX_DATA_DF$Degree))*100
# cumulative frequency
cumsum(table(EX_DATA_DF$Degree))
# cumulative percent
cumsum(prop.table(table(EX_DATA_DF$Degree)))*100
# contingency table with row and column totals
addmargins(table(EX_DATA_DF$Gender,EX_DATA_DF$Degree))

# using Tidverse Packages
library(tidyverse)
EX_DATA_DF %>% select(Degree) %>%
    table()
  
EX_DATA_DF %>% select(Degree) %>%
  table() %>%
  prop.table()  %>% 
  "*"(100)

EX_DATA_DF %>% select(Degree) %>%
  table() %>%
  cumsum()

EX_DATA_DF %>% select(Degree) %>%
  table() %>%
  prop.table() %>% 
  cumsum() %>% 
  "*"(100)
 
EX_DATA_DF %>% select(Gender,Degree) %>%
  table() %>%
  addmargins()
