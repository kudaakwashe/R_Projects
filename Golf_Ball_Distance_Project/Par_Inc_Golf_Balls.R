#Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DataExplorer)
library(gridExtra)
library(pastecs)
library(formattable)
library(moments)

#Set working directory
setwd("C:/Users/kudaakwashe/Documents/Study/PGPDSBA/Fundamentals_of_Business_Statistics/Project")

golf_data = read_xls("Golf.xls")

attach(golf_data)

#Understanding our data set
head(golf_data)
str(golf_data)
summary(golf_data)
plot_str(golf_data)
plot_intro(golf_data)
plot_missing(golf_data)

#remove scinetific notation
options(scipen=1, digits = 6)

#Variable Analysis
stat_table = stat.desc(golf_data)
formattable(stat_table)

#Plots Histogram, Box Plots and Density Plots
c.Hist = ggplot(golf_data, aes(Current)) + 
         geom_histogram(aes(y = ..density..), col = "black", fill = "#EA586F",
                        position = "identity", bins = 7) +
         geom_density()

c.Box =  ggplot(golf_data) + 
         geom_boxplot(aes(y = Current), color = "black", fill = "#EA586F") + 
         coord_flip()

n.Hist = ggplot(golf_data, aes(New)) + 
         geom_histogram(aes(y = ..density..), col = "black", fill = "#56A3A6",
                        position = "identity", bins = 7) +
         geom_density()

n.Box =  ggplot(golf_data) + 
         geom_boxplot(aes(y = New), color = "black", fill = "#56A3A6") + 
         coord_flip()

#graph layout
grid.arrange(c.Hist, c.Box, n.Hist, n.Box, nrow = 2)

#Check normality
plot_qq(golf_data)

skewness(Current)
skewness(New)

#t-tests
t.test(Current)
t.test(New)

t.test(Current, New,
       paired = FALSE,
       conf.level = 0.95,
       alternative = "two.sided")

#Question 5
sd(Current - New)
mean(Current - New)

qt(0.025, 80)

pooledstad = pooled.sd(golf_data)
#Beta
pt(0.71,80, lower.tail = TRUE)-pt(-3.26,80, lower.tail = TRUE)

#finding sample size
power.t.test(power = 0.95,
             delta = 2.77,
             sd = 9,
             sig.level = 0.05,
             type = "two.sample",
             alternative = "two.sided")

#Reshaping data set for easier visualisation
golf_data_vis = golf_data %>% gather(Golf.Ball.Model, Driving.Distance, Current:New)
head(golf_data_vis)

#Plotting density plots
ggplot(golf_data_vis, aes(x = Driving.Distance)) + geom_density(aes(fill = Golf.Ball.Model), alpha = 0.4)
detach(golf_data)


