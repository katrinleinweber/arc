####making graph to compare scores with SAUP and reg data####
library(ggplot2)
library(gcookbook)   # source of example data
library(knitr)       # functions for knitting Rmd documents to html
library(RColorBrewer)
library(scales)
library(tidyr)
library(dplyr)
data<- read.csv('reg_run/reg_saup_compare.csv')
<<<<<<< HEAD
datafp<- data %>% gather("Data", "Score", 3:4)%>%
  filter(Goal=="Fisheries")
=======
datafp<- data %>% dplyr::gather("Data", "Score", 3:4)%>%
  filter(Goal=="Food Provision")
<<<<<<< HEAD
datafp$Region <- factor(datafp$Region,levels = c("Index", "Arctic Alaska", "Nunavut", "Canadian Beaufort Sea", "Arctic Russia", "Svalbard", "Arctic Norway", "Jan Mayen", "West Greenland", "East Greenland"))
=======
>>>>>>> a113f72d1635b50b8063a05970e392d08bdededf
datafp$Region <- factor(datafp$Region,levels = c("Index", "Arctic Alaska",
                                                 "Nunavut", "Canadian Beaufort", "Arctic Russia", "Svalbard",
                                                 "Arctic Norway", "Jan Mayen", "West Greenland", "East Greenland"))
>>>>>>> f9b4a8559238d58fc70f3e58d2f6e06e8328b373
dataindex<- data %>% gather("Data", "Score", 3:4)%>%
  filter(Goal=="Index")
dataindex$Region <- factor(dataindex$Region,levels = c("Index", "Arctic Alaska",
                                               "Nunavut", "Canadian Beaufort", "Arctic Russia", "Svalbard", "Arctic Norway",
                                               "Jan Mayen", "West Greenland", "East Greenland"))

ggplot(datafp, aes(x = Region, y=Score, fill=Data))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual("Data", values = c("SAUP" = "Light Blue", "Watson.2017" = "coral2"), labels=c("SAUP", "Watson 2017"))+
<<<<<<< HEAD
  labs(title="Fisheries", hjust=0.5)+
=======
  labs(title="Food Provision", hjust=0.5)+
<<<<<<< HEAD
  scale_x_discrete(limits=datafp$region, labels = wrap_format(10))



=======
>>>>>>> a113f72d1635b50b8063a05970e392d08bdededf
  scale_x_discrete(labels = wrap_format(10))+
  ylim(0,100)
>>>>>>> f9b4a8559238d58fc70f3e58d2f6e06e8328b373

ggplot(dataindex, aes(x = Region, y=Score, fill=Data))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual("Data", values = c("SAUP" = "Light Blue", "Watson.2017" = "coral2"), labels=c("SAUP", "Watson 2017"))+
  labs(title="Arctic Ocean Health Index", hjust=0.5)+
<<<<<<< HEAD
  scale_x_discrete(labels = wrap_format(10))



=======
  scale_x_discrete(labels = wrap_format(10))+
  ylim(0,100)
<<<<<<< HEAD

multiplot(p1,p2, cols=2)
=======
>>>>>>> f9b4a8559238d58fc70f3e58d2f6e06e8328b373
>>>>>>> a113f72d1635b50b8063a05970e392d08bdededf
