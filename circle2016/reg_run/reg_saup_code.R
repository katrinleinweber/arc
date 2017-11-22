####making graph to compare scores with SAUP and reg data####
library(ggplot2)
library(gcookbook)   # source of example data
library(knitr)       # functions for knitting Rmd documents to html
library(RColorBrewer)
library(scales)
library(tidyr)
library(dplyr)
data<- read.csv('reg_run/reg_saup_compare.csv')
datafp<- data %>% gather("Data", "Score", 3:4)%>%
  filter(Goal=="Food Provision")
datafp$Region <- factor(datafp$Region,levels = c("Index", "Arctic Alaska",
                                                 "Nunavut", "Canadian Beaufort", "Arctic Russia", "Svalbard",
                                                 "Arctic Norway", "Jan Mayen", "West Greenland", "East Greenland"))
dataindex<- data %>% gather("Data", "Score", 3:4)%>%
  filter(Goal=="Index")
dataindex$Region <- factor(dataindex$Region,levels = c("Index", "Arctic Alaska",
                                               "Nunavut", "Canadian Beaufort", "Arctic Russia", "Svalbard", "Arctic Norway",
                                               "Jan Mayen", "West Greenland", "East Greenland"))

ggplot(datafp, aes(x = Region, y=Score, fill=Data))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual("Data", values = c("SAUP" = "Light Blue", "Watson.2017" = "coral2"), labels=c("SAUP", "Watson 2017"))+
  labs(title="Food Provision", hjust=0.5)+
  scale_x_discrete(labels = wrap_format(10))+
  ylim(0,100)

ggplot(dataindex, aes(x = Region, y=Score, fill=Data))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual("Data", values = c("SAUP" = "Light Blue", "Watson.2017" = "coral2"), labels=c("SAUP", "Watson 2017"))+
  labs(title="Arctic Ocean Health Index", hjust=0.5)+
  scale_x_discrete(labels = wrap_format(10))+
  ylim(0,100)
