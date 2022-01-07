######################################################
# Example 1.3                               ##########
# Employees of a Small Business Company ##############
EX_DATA_DF <- readRDS("data/EX_DATA_DF.rds")
# frequency distribution for Degree
(fi = table(EX_DATA_DF$Degree))
# percent
(percent = prop.table(table(EX_DATA_DF$Degree))*100)
# cumulative frequency
(cum.fi = cumsum(table(EX_DATA_DF$Degree)))
# cumulative percent
(cum.percent = cumsum(prop.table(table(EX_DATA_DF$Degree)))*100)
# all together
cbind(fi,percent,cum.fi,cum.percent)
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

# all together
EX_DATA_DF %>%
  group_by(Degree) %>% 
  summarize(fi=n()) %>% 
  mutate(percent = round(prop.table(fi)*100,2))%>% 
  mutate(cum.fi = cumsum(fi)) %>% 
  mutate(cum.percent = cumsum(percent))

# contingency table with row and column totals
EX_DATA_DF %>% select(Gender,Degree) %>%
  table() %>%
  addmargins()

library(janitor)
EX_DATA_DF %>% 
  group_by(Gender,Degree) %>% 
  summarise(fij=n(), .groups = 'drop') %>% 
  spread(Degree,fij) %>% 
  adorn_totals(name = 'col.sum') %>% 
  adorn_totals(name = 'row.sum', where = 'col')