## ----setup, include=FALSE---------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---------------------------------------------------------------------------------------------------------------
EX_DATA_DF <- readRDS("../data/EX_DATA_DF.rds")
EX_DATA_DF$Years <- floor(EX_DATA_DF$Months/12)
library(tidyverse)


## ---------------------------------------------------------------------------------------------------------------
EX_DATA_DF %>% 
  ggplot(aes(Degree)) +    # plotted variable
  geom_bar(fill="grey") +  # type of plot
  labs(x="Academic Degree", y= "Frequency")  #x,y-labs 


## ---------------------------------------------------------------------------------------------------------------
library(ggQC)
EX_DATA_DF %>% 
  group_by(Degree) %>%            # variable of interest
  summarise(Frequency=n()) %>%    # we need its frequency distribution
  ggplot(aes(x = Degree, y = Frequency)) + # plotted variables
  labs(x = "Academic Degree", y = "Frequency")+
  stat_pareto(bars.fill = "grey")


## ---------------------------------------------------------------------------------------------------------------
EX_DATA_DF %>% 
  ggplot(aes(x = Gender, fill = Degree ))  + # plotted variables
  geom_bar(stat="count", position = "dodge")  


## ---------------------------------------------------------------------------------------------------------------
EX_DATA_DF %>% 
  ggplot(aes(x = Gender, fill = Degree ))  + # plotted variables
  geom_bar(stat="count", position = "stack")  


## ---------------------------------------------------------------------------------------------------------------
EX_DATA_DF %>% 
  ggplot(aes(x = Gender, fill = Degree ))  + # plotted variables
  geom_bar(stat="count", position = "fill")    


## ---------------------------------------------------------------------------------------------------------------
library(ggmosaic)
EX_DATA_DF %>% 
  ggplot()+
  geom_mosaic(aes(x = product(Degree, Gender), fill = Degree))


## ---------------------------------------------------------------------------------------------------------------
EX_DATA_DF %>% 
  ggplot(aes(x="", fill=Degree)) +
  geom_bar(stat="count")+
  coord_polar("y")


## ---------------------------------------------------------------------------------------------------------------
EX_DATA_DF %>% 
  mutate(Children=factor(Children,levels = 0:5)) %>% 
  ggplot(aes(Children)) +    
  scale_x_discrete(limits=factor(0:5)) +
  geom_bar(fill="grey") +  
  labs(x="Number of Children", y= "Frequency")  


## ---------------------------------------------------------------------------------------------------------------
EX_DATA_DF %>% 
  ggplot(aes(x = Years))+ 
  geom_dotplot(binwidth = .25,  stackdir = "center", stackratio = 1.5) +
  scale_y_continuous(NULL, breaks = NULL)


## ---------------------------------------------------------------------------------------------------------------
EX_DATA_DF %>% 
  ggplot(aes(Salary)) +
  geom_histogram(breaks=seq(30000,65000,5000), fill = "grey") 


## ---------------------------------------------------------------------------------------------------------------
EX_DATA_DF %>% 
  ggplot(aes(Salary)) + 
  stat_ecdf(geom = "step")+
  labs(x ="Salary ($)",y="Cumulative Percentage")


## ---------------------------------------------------------------------------------------------------------------
EX_DATA_DF %>% 
  ggplot(aes(x=Years, y=Salary)) +
  geom_point()+
  labs(y ="Salary ($)",x="Duration (years)")+
  expand_limits(x = 0)

