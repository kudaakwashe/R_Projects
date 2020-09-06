######------Exploratory Data Analysis------######

######------Cardio Good Fitness Project-----#####



##Environment Set up and data set import

#Required libraries
library(readr)
library(dplyr)
library(corrplot)
library(data.table)
library(ggplot2)
library(cowplot)
library(pastecs)
library(lattice)
library(rpivotTable)
library(formattable)
library(moments)
library(latticeExtra) #Combines laa=ttice graphs into one

#Set working directory
setwd("C:/Users/kudaakwashe/Documents/Study/PGPDSBA/Introduction_To_Analytics/Project_1")
getwd() #check if directory is correctly set up

#Import data set
CGF_data = read.csv("CardioGoodFitness.csv")
attach(CGF_data)

#get a picture of the data set
dim(CGF_data)
head(CGF_data)
tail(CGF_data)
str(CGF_data)
summary(CGF_data)

#checking for missing values
anyNA(CGF_data)
sum(is.na(CGF_data))


##Univariate analysis
#factor variable analysis
table(Product)
table(Gender)
table(MaritalStatus)

prop.table(table(Product))
prop.table(table(Gender))
prop.table(table(Fitness))
table(Usage)

#Summary of statistical measures
options(digits = 2)
options(scipen = 999)

ProductB = (ggplot(CGF_data,aes(Product)) 
              + geom_bar(aes(fill=Product)) 
              + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)))

GenderB = (ggplot(CGF_data,aes(Gender))
             + geom_bar(aes(fill=Gender)) 
             + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)))

MaritalStatusB = (ggplot(CGF_data,aes(MaritalStatus)) 
                    + geom_bar(aes(fill=MaritalStatus)) 
                    + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)))

#Plot categorical graphs onto same area
plot_grid(ProductB,GenderB,MaritalStatusB,ncol = 1)


#Integer variable graphs
EducationB = (ggplot(CGF_data,aes(x=Education))
                + geom_bar(aes(y=..count..),color="black", fill="#A1C1FF"))
                

UsageB = (ggplot(CGF_data,aes(x=Usage))
            + geom_bar(aes(y=..count..),color="black", fill="#A1C1FF"))

FitnessB = (ggplot(CGF_data,aes(x=Fitness))
              + geom_bar(aes(y=..count..),color="black", fill="#A1C1FF"))


AgeH = (ggplot(CGF_data,aes(x=Age))
        + geom_histogram(aes(y=..count..), breaks=seq(15,50, by=5),color="black", fill="#A1C1FF"))


IncomeH = (ggplot(CGF_data,aes(x=Income))
              + geom_histogram(aes(y=..count..),breaks=seq(25000,105000, by=5000),color="black", fill="#A1C1FF"))

MilesH = (ggplot(CGF_data,aes(x=Miles))
                + geom_histogram(aes(y=..count..),breaks=seq(20,360, by=10),color="black", fill="#A1C1FF"))
#Plot graphs onto same area
plot_grid(EducationB,UsageB,FitnessB,AgeH,IncomeH,MilesH,ncol = 3)


#Box and Density Plots
AgeBx = (boxplot(Age, main = "Age", xlab = "Years", ylab = "Age", 
                 col = "#F15E75", horizontal = TRUE))
AgeD = plot(density(Age), main = "Age")


UsageBx = (boxplot(Usage, main = "Usage", xlab = "Times", ylab = "Usage", 
                       col = "#F15E75", horizontal = TRUE))
UsageD = plot(density(Usage, adjust = 2), main = "Usage")


FitnessBx = (boxplot(Fitness, main = "Fitness", xlab = "Score", ylab = "Fitness", 
                   col = "#F15E75", horizontal = TRUE))
FitnessD = plot(density(Fitness, adjust = 2), main = "Fitness")


MilesBx = (boxplot(Miles, main = "Miles", xlab = "Miles", ylab = "Miles", 
                   col = "#F15E75", horizontal = TRUE))
MilesD = plot(density(Miles), main = "Miles")


IncomeBx = (boxplot(Income, main = "Income", xlab = "Dollars", ylab = "Income", 
                     col = "#F15E75", horizontal = TRUE))
IncomeD = plot(density(Income), main = "Income")


EducationBx = (boxplot(Education, main = "Education", xlab = "Years", ylab = "Education", 
                       col = "#F15E75", horizontal = TRUE))
EducationD = plot(density(Education, adjust = 2), main = "Education")


#Pivoting data and generating graphs
rpivotTable(CGF_data)

#Grouping Numeric variables
CGF_Numeric = CGF_data %>% select_if(is.numeric)

#Generating Statistical summaries
stat.desc(CGF_Numeric, basic =TRUE)
formattable(stat.desc(CGF_Numeric, basic =TRUE))

#Group by Product for Numeric Variables
P_TM195 = (CGF_data[which(Product=="TM195"),])%>%select_if(is.numeric)
P_TM498 = (CGF_data[which(Product=="TM498"),])%>%select_if(is.numeric)
P_TM798 = (CGF_data[which(Product=="TM798"),])%>%select_if(is.numeric)

Stats_TM195 = (setnames(P_TM195, old = c("Age", "Education", "Usage", "Fitness", "Income", "Miles"),
                        new = c("Age_TM195", "Education_TM195", "Usage_TM195", "Fitness_TM195", "Income_TM195", "Miles_TM195")))
formattable(stat.desc(Stats_TM195))

Stats_TM498 = (setnames(P_TM498, old = c("Age", "Education", "Usage", "Fitness", "Income", "Miles"),
                        new = c("Age_TM498", "Education_TM498", "Usage_TM498", "Fitness_TM498", "Income_TM498", "Miles_TM498")))
formattable(stat.desc(Stats_TM498))

Stats_TM798 = (setnames(P_TM798, old = c("Age", "Education", "Usage", "Fitness", "Income", "Miles"),
                        new = c("Age_TM798", "Education_TM798", "Usage_TM798", "Fitness_TM798", "Income_TM798", "Miles_TM798")))
formattable(stat.desc(Stats_TM798))

#Determining skewness
skewness(P_TM195$Usage_TM195)
skewness(P_TM498$Miles_TM498)
skewness(P_TM798$Usage_TM798)

###BIVARIATE ANALYSIS
#Bivariate analysis of categorical variables
table(Product,Gender)
prop.table(table(Product,Gender))

table(Product,MaritalStatus)
prop.table(table(Product,MaritalStatus),2)

by(CGF_data, INDICES = Product, FUN = summary)

#Generating graphs for Product vs other variable analysis
Pro_Ed_H = histogram(~Education | factor(Product), data = CGF_data)
Pro_Ed_B = bwplot(~Education | factor(Product), data = CGF_data)
Pro_Ed_D = densityplot(~Education | factor(Product), data = CGF_data)

Pro_Us_H = histogram(~Usage | factor(Product), data = CGF_data)
Pro_Us_B = bwplot(~Usage | factor(Product), data = CGF_data)
Pro_Us_D = densityplot(~Usage | factor(Product), data = CGF_data)

Pro_Fi_H = histogram(~Fitness | factor(Product), data = CGF_data)
Pro_Fi_B = bwplot(~Fitness | factor(Product), data = CGF_data)
Pro_Fi_D = densityplot(~Fitness | factor(Product), data = CGF_data)

Pro_Ag_H = histogram(~Age | factor(Product), data = CGF_data)
Pro_Ag_B = bwplot(~Age | factor(Product), data = CGF_data)
Pro_Ag_D = densityplot(~Age | factor(Product), data = CGF_data)

Pro_In_H = histogram(~Income | factor(Product), data = CGF_data)
Pro_In_B = bwplot(~Income | factor(Product), data = CGF_data)
Pro_In_D = densityplot(~Income | factor(Product), data = CGF_data)

Pro_Mi_H = histogram(~Miles | factor(Product), data = CGF_data)
Pro_Mi_B = bwplot(~Miles | factor(Product), data = CGF_data)
Pro_Mi_D = densityplot(~Miles | factor(Product), data = CGF_data)

#Grouping the different graphs together
c(Pro_Ed_B,Pro_Ed_D,Pro_Ed_H,layout=c(3,3))
c(Pro_Us_B,Pro_Us_D,Pro_Us_H,layout=c(3,3))
c(Pro_Fi_B,Pro_Fi_D,Pro_Fi_H,layout=c(3,3))
c(Pro_Ag_B,Pro_Ag_D,Pro_Ag_H,layout=c(3,3))
c(Pro_In_B,Pro_In_D,Pro_In_H,layout=c(3,3))
c(Pro_Mi_B,Pro_Mi_D,Pro_Mi_H,layout=c(3,3))

#Bivariate Anallysis of numeric variables
corrplot(cor(CGF_Numeric))

#Scatterplot Matrix
splom(CGF_Numeric)

CGF_cor = round(cor(CGF_Numeric),2)
CGF_cor

#Income log transformation
CGF_data$Income_log = log(Income)

#comparing skewness change as a result of transformation
par(mfrow=c(1,2))
hist(Income, col="#F15E75")
hist(CGF_data$Income_log, col="#F15E75")
par(mfrow=c(1,1))

#Adding a new feature
CGF_data$Miles_Per_Usage = (Miles/Usage)
CGF_data

detach(CGF_data)

histogram(~Miles_Per_Usage | factor(Product), data = CGF_data)

pairs(CGF_Numeric)
AGI =Age[which(Gender=="Male" & Age > 30 & Income < 75000)]

histogram(~AGI | factor(Product), data = CGF_data)

plot(Miles, type = "b", lty=3, lwd=3, pch=12, cex=.5, bg="#F15E75")

myCols = brewer.pal(7, "Set1")
barplot(rep(1,7), col=myCols)
display.brewer.all()
n <- 10
mycolors <- rainbow(n)
pie(rep(1, n), labels=mycolors, col=mycolors)
mygrays <- gray(0:n/n)
pie(rep(1, n), labels=mygrays, col=mygrays)


dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)

opar <- par(no.readonly=TRUE)
par(pin=c(2, 3))
par(lwd=2, cex=1.5)
par(cex.axis=.75, font.axis=3)
plot(dose, drugA, type="b", pch=19, lty=2, col="red")
plot(dose, drugB, type="b", pch=23, lty=6, col="blue", bg="green")
par(opar)

