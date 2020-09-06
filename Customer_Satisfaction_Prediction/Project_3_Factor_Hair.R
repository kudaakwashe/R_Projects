#Load Libraries
library(DataExplorer)
library(dplyr)
library(formattable)
library(packHV)
library(corrplot)
library(car)
library(pastecs)
library(nFactors)
library(psych)
library(bestNormalize)

#Set working directory
setwd("C:/Users/kudaakwashe/Documents/Study/PGPDSBA/Advanced Statistics/Project_3")

#load data
hairData = read.csv("Factor-Hair-Revised.csv", header = TRUE)

#Understanding the data
head(hairData)
tail(hairData)
str(hairData)
summary(hairData)

plot_str(hairData, fontSize = 40)
plot_intro(hairData, title = "Hair Factors Dataset Structure")
plot_missing(hairData, title = "Hair Factors Dataset Missing Values")

#Variables and their full names
expandVar = c("Product Quality", "E-Commerce","Technical Support","Complaint Resolution",
              "Advertising", "Product Line","Salesforce Image","Competitive Pricing",
              "Warranty & Claims", "Order & Billing", "Delivery Speed","Customer Satisfaction")

varNames = cbind(names(hairData), expandVar)
colnames(varNames) = c("Variable", "Expanded Name")
formattable(as.data.frame(varNames), align = 'l')

#Remove the ID column
hairData = subset(hairData, select = -c(ID))

#Variable statistics
formattable(stat.desc(hairData), digits=2)

attach(hairData)

#Plot histograms, density and box plots
par(mfrow = c(3,4))
for (i in 1:12){
  r = hairData[,i]
  hist_boxplot(r, freq = FALSE, density = TRUE, main = colnames(hairData[i]), xlab = "Score")
}
dev.off()

#Checking for normality - Q-Q Plots
plot_qq(hairData, ncol = 4, nrow = 3, title = "Q-Q Plots")

#Correlation matrix
corrplot::corrplot(cor(hairData), method = "number", type = "upper")

#Value Inflation Fatcor
modelHairData = lm(Satisfaction ~ ProdQual+Ecom+TechSup+CompRes+Advertising+ProdLine+SalesFImage+ComPricing+WartyClaim+OrdBilling+DelSpeed,
                   data = hairData)
VIF_Value = vif(modelHairData)
formattable(as.data.frame(VIF_Value))

#Regression model
regHairData = summary(modelHairData)
regHairData

#Shapiro-Wilks Test
hairLapply = lapply(hairData, shapiro.test)
str(hairLapply)

hairSapply = sapply(hairLapply, '[', c("statistic","p.value"))
hairShapiro = t(hairSapply)
hairShapiro = as.data.frame(hairShapiro)
formattable(hairShapiro, digits = 4)

#normalisation using bestNormalize
ProdQualNorm = bestNormalize::orderNorm(ProdQual, warn = FALSE)
ProdQualNorm

EcomNorm = bestNormalize::arcsinh_x(Ecom)
EcomNorm

SalesFImageNorm = bestNormalize::sqrt_x(SalesFImage)
SalesFImageNorm

ComPricingNorm = bestNormalize::orderNorm(ComPricing)
ComPricingNorm

OrdBillingNorm = bestNormalize::boxcox(OrdBilling)
OrdBillingNorm

par(mfrow = c(2,3))
hist_boxplot(ProdQualNorm[["x.t"]],freq = FALSE, density = TRUE, main = "ProdQual - Normalised", xlab = "Scale")
hist_boxplot(EcomNorm[["x.t"]],freq = FALSE, density = TRUE, main = "Ecom - Normalised", xlab = "Scale")
hist_boxplot(SalesFImageNorm[["x.t"]],freq = FALSE, density = TRUE, main = "SalesFImage - Normalised", xlab = "Scale")
hist_boxplot(ComPricingNorm[["x.t"]],freq = FALSE, density = TRUE, main = "ComPricing - Normalised", xlab = "Scale")
hist_boxplot(OrdBillingNorm[["x.t"]],freq = FALSE, density = TRUE, main = "OrdBilling - Normalised", xlab = "Scale")
dev.off()

transfData = cbind(ProdQualNorm[["x.t"]],EcomNorm[["x.t"]],SalesFImageNorm[["x.t"]], ComPricingNorm[["x.t"]], OrdBillingNorm[["x.t"]])
colnames(transfData) = c("ProdQualT","EcomT","SalesFImageT","ComPricingT","OrdBillingT")
transfData = as.data.frame(transfData)
head(transfData)
str(transfData)
summary(transfData)

#Checking Shapiro-Wilks Test
transLapply = lapply((transfData), shapiro.test)
transSapply = sapply(transLapply, '[', c("statistic","p.value"))
transShapiro = t(transSapply)
transShapiro = as.data.frame(transShapiro)
formattable(transShapiro, digits = 6)

#Scaling the remaining variables
set.seed(12)
hairDataS = hairData%>%dplyr::select(-c(ProdQual,Ecom,SalesFImage,ComPricing,OrdBilling))
head(hairDataS) 
hairDataScaled = sapply(hairDataS, scale)  
head(hairDataScaled)
hairDataScaled = as.data.frame(hairDataScaled)%>%dplyr::rename(TechSupT = TechSup, 
                                         CompResT = CompRes,
                                         AdvertisingT =Advertising,
                                         ProdLineT = ProdLine,
                                         WartyClaimT = WartyClaim,
                                         DelSpeedT = DelSpeed,
                                         SatisfactionT = Satisfaction)

#Combining the scaled and transformed datasets
combinedHairData = cbind(hairDataScaled,transfData)

#Correlation matrix
corrplot::corrplot(cor(combinedHairData), method = "number", type = "upper")

#Exclude the dependent variable
combinedHairDataX = subset(combinedHairData, select = -c(SatisfactionT))

#Bartlett's Test for Sphericity
set.seed(0)

bartlettST = cortest.bartlett(cor(combinedHairDataX), n = nrow(combinedHairDataX))
formattable(as.data.frame(bartlettST))

#Scree Plot
ev = eigen(cor(combinedHairDataX))
ev
eigenValues = ev$values
eigenValues
factors = c(1:11)
screeData = data.frame(factors, eigenValues)
plot(screeData, main = "Scree Plot")
lines(screeData, col = "red")
abline(h = 1)

#PCA Implementation
unrotated = principal(combinedHairDataX, nfactors = 4, rotate = "none")
print(unrotated, digits = 4)

rotated = principal(combinedHairDataX, nfactors = 4, rotate = "varimax")
print(rotated, digits = 4)

#Plot of principal components
plot(rotated, row.names(rotated$loadings))

#combining scores and rotated components
compoHairData = as.data.frame(cbind(hairData$Satisfaction, rotated$scores))
compoHairData = compoHairData%>%rename(Satisfaction_RC = V1)

#checking for normality
plot_qq(compoHairData, ncol = 2, nrow = 3, title = "Q-Q Plots ")

compoLapply = lapply(compoHairData, shapiro.test)
compoSapply = sapply(compoLapply, '[', c("statistic","p.value"))
compoShapiro = t(compoSapply)
compoShapiro = as.data.frame(compoShapiro)
formattable(compoShapiro, digits = 4)

#Plot histograms, density and box plots
par(mfrow = c(3,2))
for (i in 1:5){
  c = compoHairData[,i]
  hist_boxplot(c, freq = FALSE, density = TRUE, main = colnames(compoHairData[i]), xlab = "Score")
}
dev.off()

#Linear models - all variables
model1 = lm(Satisfaction_RC ~ RC1+RC2+RC3+RC4, data = compoHairData)
summary(model1)
par(mfrow = c(2,2))
plot(model1)
dev.off()

#Linear model excluding RC3
model2 = lm(Satisfaction_RC ~ RC1+RC2+RC4, data = compoHairData)
summary(model2)
par(mfrow = c(2,2))
plot(model2)
dev.off()

