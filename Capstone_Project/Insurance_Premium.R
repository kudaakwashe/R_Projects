#libraries used
library(readxl)
library(DataExplorer) # for data exploration
library(dplyr) #for data manipulation
library(ggplot2) #for plots
library(formattable) #formating output
library(packHV) #Histogram, density and boxplot combo charts
library(scales)#for scales on plots
library(pastecs) #SUmmary statistics
library(rpivotTable) #for categorical plot comparisons
library(corrplot) #Correlation plot
library(caret) #for plots
library(bestNormalize) #variable normalisation
library(psych) # Bartlett Test
library(DMwR) #for SMOTE
library(pROC) # for ROC
library(MLeval) #for ROC

#Setting working directory and reading data
setwd("C:/Users/kudaakwashe/Documents/Study/PGPDSBA/Capstone")

premium_data = read_excel("Insurance Premium Default-Dataset.xlsx", sheet = "premium")

#Viewing the dataset
head(premium_data)
tail(premium_data)
str(premium_data)

summary(premium_data)
plot_str(premium_data, fontSize = 58)

#Visual inspection
plot_intro(premium_data)
plot_missing(premium_data,title = "Check for Missing Values")

#Changing variable types
premium_data = premium_data%>%mutate(id = as.factor(id),
                                     sourcing_channel = as.factor(sourcing_channel),
                                     residence_area_type = as.factor(residence_area_type),
                                     `Marital Status` = as.factor(`Marital Status`),
                                     Accomodation = as.factor(Accomodation),
                                     `Count_3-6_months_late` = as.factor(`Count_3-6_months_late`),
                                     `Count_6-12_months_late` = as.factor(`Count_6-12_months_late`),
                                     `Count_more_than_12_months_late` = as.factor(`Count_more_than_12_months_late`),
                                     default = as.factor(default))%>%
                                       rename(income = Income,
                                              cash_premium_percent = perc_premium_paid_by_cash_credit,
                                              late_3_6_months =`Count_3-6_months_late`,
                                              late_6_12_months = `Count_6-12_months_late`,
                                              late_over_12_months = `Count_more_than_12_months_late`,
                                              marital_status = `Marital Status`,
                                              vehicles_owned = Veh_Owned,
                                              dependents = No_of_dep,
                                              accommodation = Accomodation)
#Id variable count distinc values
premium_data%>%n_distinct(id)

#Changing age variable and premium per payment
premium_data = premium_data%>%mutate(age_in_years = age_in_days%/%365,ave_premium_per_payment = premium%/%no_of_premiums_paid)%>%select(-age_in_days)

#replace all values above zero with 1 for late payment
premium_data = premium_data%>%mutate(late_3_6_months = replace(late_3_6_months, late_3_6_months != "0", "1"),
                                     late_6_12_months = replace(late_6_12_months, late_6_12_months != "0", "1"),
                                     late_over_12_months = replace(late_over_12_months, late_over_12_months != "0", "1"))%>%
                                    filter(late_3_6_months %in% c("0","1"),
                                           late_6_12_months %in% c("0","1"),
                                           late_3_6_months %in% c("0","1"),
                                           late_over_12_months %in% c("0","1"))%>%droplevels()

summary(premium_data)
str(premium_data)

#Separate numeric and categorical variables
premium_numeric_data  = as.data.frame(premium_data%>%select_if(is.numeric))
premium_factor_data  = as.data.frame(premium_data%>%select_if(is.factor))%>%select(-id)

head(premium_numeric_data)
head(premium_factor_data)
#Numeric variable univariate
#Summary stats
options(scipen=999, digits = 3)
formattable(as.data.frame(stat.desc(premium_numeric_data[,1:4])))
options(scipen = 0)

dim(premium_numeric_data)

#Plots
#hist and density plots
par(mfrow = c(5,2))
for(t in 9){
  s = premium_numeric_data[,t]
  hist_boxplot(s, freq = FALSE, density = TRUE, main = colnames(premium_numeric_data[t]), xlab = "Value")
}
dev.off()

# barplots for numeric descrete variables
vehicle_bar = ggplot(premium_numeric_data, aes(vehicles_owned)) +
              geom_bar(aes(y = (..count..)/sum(..count..), fill = as.factor(vehicles_owned)), stat= "count") +
              geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                            y= ((..count..)/sum(..count..))), stat="count", vjust = -.25) +
              scale_y_continuous(labels = percent) +
              labs(title = "Vehicles Owned", y = "Percent")

vehicle_bar

dependents_bar = ggplot(premium_numeric_data, aes(dependents)) +
              geom_bar(aes(y = (..count..)/sum(..count..), fill = as.factor(dependents)), stat= "count") +
              geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                            y= ((..count..)/sum(..count..))), stat="count", vjust = -.25) +
              scale_y_continuous(labels = percent) +
              labs(title = "Dependents", y = "Percent")

dependents_bar


#QQ-Plots
q=6
colnames(premium_numeric_data[q])
plot_qq(premium_numeric_data[,q], title = colnames(premium_numeric_data[q]), ncol = 1)


#Customers paying all premiums in cash
hundred_percent = premium_numeric_data[premium_numeric_data$cash_premium_percent == 1,]
dim(hundred_percent)
nrow(hundred_percent)/nrow(premium_data)

#Ouliers in income
outliers_income = boxplot(premium_numeric_data$income, plot = FALSE)
length(outliers_income$out)
length(outliers_income$out)/nrow(premium_data)

#Ouliers in risk score
outliers_risk_score = boxplot(premium_numeric_data$risk_score, plot = FALSE)
length(outliers_risk_score$out)
length(outliers_risk_score$out)/nrow(premium_data)

#Ouliers in number of premium
outliers_no_premiums = boxplot(premium_numeric_data$no_of_premiums_paid, plot = FALSE)
length(outliers_no_premiums$out)
length(outliers_no_premiums$out)/nrow(premium_data)

#Ouliers in total premiums
outliers_total_premiums = boxplot(premium_numeric_data$premium, plot = FALSE)
length(outliers_total_premiums$out)
length(outliers_total_premiums$out)/nrow(premium_data)

#Ouliers age
outliers_age = boxplot(premium_numeric_data$age, plot = FALSE)
length(outliers_age$out)
(length(outliers_age$out)/nrow(premium_data))*100

#Ouliers average premiums paid per payment
outliers_premium_per_payment = boxplot(premium_numeric_data$ave_premium_per_payment, plot = FALSE)
length(outliers_premium_per_payment$out)
(length(outliers_premium_per_payment$out)/nrow(premium_data))*100

#Categorical variables
marital_bar = ggplot(premium_factor_data, aes(marital_status)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = marital_status), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= ((..count..)/sum(..count..))), stat="count", vjust = -.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Marital Status", y = "Percent")

marital_bar

accommodation_bar = ggplot(premium_factor_data, aes(accommodation)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = accommodation), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= ((..count..)/sum(..count..))), stat="count", vjust = -.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Accommodation", y = "Percent")

accommodation_bar

sourcing_bar = ggplot(premium_factor_data, aes(sourcing_channel)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = sourcing_channel), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= ((..count..)/sum(..count..))), stat="count", vjust = -.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Sourcing Channel", y = "Percent")

sourcing_bar

residence_bar = ggplot(premium_factor_data, aes(residence_area_type)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = residence_area_type), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= ((..count..)/sum(..count..))), stat="count", vjust = -.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Residence Area Type", y = "Percent")

residence_bar

default_bar = ggplot(premium_factor_data, aes(default)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = default), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= ((..count..)/sum(..count..))), stat="count", vjust = -.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Default", y = "Percent")

default_bar


late_3_6_bar = ggplot(premium_factor_data, aes(late_3_6_months)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = as.factor(late_3_6_months)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= ((..count..)/sum(..count..))), stat="count", vjust = -.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Times late from 3 - 6 months", y = "Percent")

late_3_6_bar

late_6_12_bar = ggplot(premium_factor_data, aes(late_6_12_months)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = as.factor(late_6_12_months)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= ((..count..)/sum(..count..))), stat="count", vjust = -.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Times late from 6 - 12 months", y = "Percent")

late_6_12_bar

late_over_12_bar = ggplot(premium_factor_data, aes(late_over_12_months)) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = as.factor(late_over_12_months)), stat= "count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= ((..count..)/sum(..count..))), stat="count", vjust = -.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Times late over 12 months", y = "Percent")

late_over_12_bar

dim(premium_numeric_data)

#Bi-Variate Analysis
rpivotTable(premium_factor_data)

#Bi-variate plots
featurePlot(x = premium_numeric_data[,1:5],
            y = premium_data$default,
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"),
                          y = list(relation="free")))

featurePlot(x = premium_numeric_data[,1:5],
            y = premium_data$default,
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"),
                          y = list(relation="free")),
            auto.key = list(columns = 3))

head(premium_numeric_data)

#Correlation Matrix
corr_matrix = cor(premium_numeric_data)
corrplot(corr_matrix, method = "number", type = "upper", diag = F, title = "Correlation Matrix", mar = c(2, 2, 5, 2) + 0.1)

#Plot Default vs Categorical vs Continuous
ggplot(premium_data, aes(x = sourcing_channel, y = income)) + 
  geom_boxplot(aes(fill = default)) +
  scale_y_log10() +
  labs(title = "Default vs Sourcing Channel vs Income")

#Plot Default vs 2 continuous
ggplot(premium_data, aes(x = sourcing_channel, y = income)) + 
  geom_jitter(aes(col = default)) +
  scale_y_log10() +
  labs(title = "Default vs Income vs Age")

ggplot(premium_data, aes(x = premium, y = no_of_premiums_paid)) + 
  geom_jitter(aes(col = default)) +
  labs(title = "Default vs Premium vs Age")

# 4 Variables
ggplot(premium_data, aes(x = age_in_years, y = risk_score)) + 
  geom_jitter(aes(col = default, size = factor(dependents))) +
  labs(title = "Default vs Income vs Age")


#pairs(premium_numeric_data)
rpivotTable(premium_factor_data)

#income level filter
below_1_6_mil = nrow(premium_data[premium_data$income <= 1600000,])/nrow(premium_data)
below_1_6_mil

below_1_6_mil_def = nrow(premium_data[premium_data$income <= 1600000 & premium_data$default == '0',])/nrow(premium_data[premium_data$income <= 1600000,])
below_1_6_mil_def

above_1_6_mil = nrow(premium_data[premium_data$income >= 1600000,])/nrow(premium_data)
above_1_6_mil

above_1_6_mil_def = nrow(premium_data[premium_data$income > 1600000 & premium_data$default == '0',])/nrow(premium_data[premium_data$income > 1600000,])
above_1_6_mil_def

below_0_5_cash_def = nrow(premium_data[premium_data$cash_premium_percent <= 0.5 & premium_data$default == '0',])/nrow(premium_data[premium_data$cash_premium_percent <= 0.5,])
below_0_5_cash_def

above_0_5_cash_def = nrow(premium_data[premium_data$cash_premium_percent > 0.5 & premium_data$default == '0',])/nrow(premium_data[premium_data$cash_premium_percent > 0.5,])
above_0_5_cash_def

median_channel_data_A_ND = premium_data[premium_data$sourcing_channel == 'A' & premium_data$default == '1',]
median_channel_A = median(median_channel_data_A_ND$age_in_years)
median_channel_A

median_channel_data_D_D = premium_data[premium_data$sourcing_channel == 'D' & premium_data$default == '0',]
median_channel_D = median(median_channel_data_D_D$age_in_years)
median_channel_D

late_3_6_premiums_data = premium_data[premium_data$late_3_6_months == "1" & premium_data$no_of_premiums_paid <= 20,]

late_3_6_premiums_data_D = nrow(late_3_6_premiums_data[late_3_6_premiums_data$default == '0',])/nrow(late_3_6_premiums_data)
late_3_6_premiums_data_D

str(premium_numeric_data)

#Oulier Treatment
#Income
income_out = premium_numeric_data$income
qnt_income_out = quantile(income_out, probs = c(.25, .75), na.rm = TRUE)
caps_income_out = quantile(income_out, probs = c(.05, .95), na.rm = TRUE)
H_income_out <- 1.5 * IQR(income_out, na.rm = T)
income_out[income_out < (qnt_income_out[1] - H_income_out)] <- caps_income_out[1]
income_out[income_out > (qnt_income_out[2] + H_income_out)] <- caps_income_out[2]


#risk_score
risk_score_out = premium_numeric_data$risk_score
qnt_risk_score = quantile(risk_score_out, probs = c(.25, .75), na.rm = TRUE)
caps_risk_score = quantile(risk_score_out, probs = c(.05, .95), na.rm = TRUE)
H_risk_score <- 1.5 * IQR(risk_score_out, na.rm = T)
risk_score_out[risk_score_out < (qnt_risk_score[1] - H_risk_score)] <- caps_risk_score[1]
risk_score_out[risk_score_out > (qnt_risk_score[2] + H_risk_score)] <- caps_risk_score[2]


hist(premium_data$risk_score)

#no_of_premiums_paid
no_of_premiums_paid_out = premium_numeric_data$no_of_premiums_paid
qnt_no_of_premiums_paid = quantile(no_of_premiums_paid_out, probs = c(.25, .75), na.rm = TRUE)
caps_no_of_premiums_paid = quantile(no_of_premiums_paid_out, probs = c(.05, .95), na.rm = TRUE)
H_no_of_premiums_paid <- 1.5 * IQR(no_of_premiums_paid_out, na.rm = T)
no_of_premiums_paid_out[no_of_premiums_paid_out < (qnt_no_of_premiums_paid[1] - H_no_of_premiums_paid)] <- caps_no_of_premiums_paid[1]
no_of_premiums_paid_out[no_of_premiums_paid_out > (qnt_no_of_premiums_paid[2] + H_no_of_premiums_paid)] <- caps_no_of_premiums_paid[2]


hist(premium_data$no_of_premiums_paid)

#premium
premium_out = premium_numeric_data$premium
qnt_premium = quantile(premium_out, probs = c(.25, .75), na.rm = TRUE)
caps_premium = quantile(premium_out, probs = c(.05, .94), na.rm = TRUE)
H_premium <- 1.5 * IQR(premium_out, na.rm = T)
premium_out[premium_out < (qnt_premium[1] - H_premium)] <- caps_premium[1]
premium_out[premium_out > (qnt_premium[2] + H_premium)] <- caps_premium[2]


hist(premium_data$premium)

#age_in_years
age_in_years_out = premium_numeric_data$age_in_years
qnt_age_in_years = quantile(age_in_years_out, probs = c(.25, .75), na.rm = TRUE)
caps_age_in_years = quantile(age_in_years_out, probs = c(.05, .95), na.rm = TRUE)
H_age_in_years <- 1.5 * IQR(age_in_years_out, na.rm = T)
age_in_years_out[age_in_years_out < (qnt_age_in_years[1] - H_age_in_years)] <- caps_age_in_years[1]
age_in_years_out[age_in_years_out > (qnt_age_in_years[2] + H_age_in_years)] <- caps_age_in_years[2]

hist(age_in_years_out)

#ave_premium_per_payment
ave_premium_per_payment_out = premium_numeric_data$ave_premium_per_payment
qnt_ave_premium_per_payment = quantile(ave_premium_per_payment_out, probs = c(.25, .75), na.rm = TRUE)
caps_ave_premium_per_payment = quantile(ave_premium_per_payment_out, probs = c(.05, .946), na.rm = TRUE)
H_ave_premium_per_payment <- 1.5 * IQR(ave_premium_per_payment_out, na.rm = T)
ave_premium_per_payment_out[ave_premium_per_payment_out < (qnt_ave_premium_per_payment[1] - H_ave_premium_per_payment)] <- caps_ave_premium_per_payment[1]
ave_premium_per_payment_out[ave_premium_per_payment_out > (qnt_ave_premium_per_payment[2] + H_ave_premium_per_payment)] <- caps_ave_premium_per_payment[2]

hist(ave_premium_per_payment_out)

premium_outliers_treated = as.data.frame(cbind(income_out, risk_score_out, no_of_premiums_paid_out,
                                 premium_out, age_in_years_out, ave_premium_per_payment_out))

head(premium_outliers_treated)

dim(premium_outliers_treated)



#plots after outlier treatment
par(mfrow = c(2,3))
cols_outliers = as.list(colnames(premium_outliers_treated))


for(o in 1:6){
  m = premium_outliers_treated[,o]
  boxplot(m, freq = FALSE, density = TRUE, main = colnames(as.data.frame(premium_outliers_treated)[,o]), xlab = cols_outliers[o])
}
dev.off()

colnames(premium_numeric_data)
colnames(premium_outliers_treated)

cash_premium_percent = premium_numeric_data$cash_premium_percent
vehicles_owned = premium_numeric_data$vehicles_owned
dependents = premium_numeric_data$dependents

premium_numeric_full = as.data.frame(cbind(premium_outliers_treated, cash_premium_percent,
                                           vehicles_owned,
                                           dependents))
str(premium_numeric_full)

#Scaling
premium_numeric_full_s_model = preProcess(premium_numeric_full, method = "range")
premium_numeric_scaled = predict(premium_numeric_full_s_model, newdata = premium_numeric_full)

head(premium_numeric_scaled)

premium_numeric_scaled = premium_numeric_scaled%>%rename(income = income_out,
                                                         risk_score = risk_score_out,
                                                         no_of_premiums_paid = no_of_premiums_paid_out,
                                                         premium = premium_out,
                                                         age_in_years = age_in_years_out,
                                                         ave_premium_per_payment = ave_premium_per_payment_out)

cols_scaled = as.list(colnames(premium_numeric_scaled))

par(mfrow = c(3,3))
for(a in 1:12){
  b = premium_numeric_scaled[,a]
  boxplot(b, freq = FALSE, density = TRUE, main = colnames(as.data.frame(premium_numeric_scaled)[,a]), xlab = cols_scaled[a])
}
dev.off()


#Normalisation of data
bestNormalize(premium_numeric_scaled$income)
orderNorm_Income = orderNorm(premium_numeric_scaled$income)

bestNormalize(premium_numeric_scaled$risk_score)
orderNorm_Risk_Score = orderNorm(premium_numeric_scaled$risk_score)

bestNormalize(premium_numeric_scaled$no_of_premiums_paid)
sqrt_Premiums_Paid = sqrt_x(premium_numeric_scaled$no_of_premiums_paid)

bestNormalize(premium_numeric_scaled$premium)
orderNorm_Premium = orderNorm(premium_numeric_scaled$premium)

bestNormalize(premium_numeric_scaled$cash_premium_percent)
orderNorm_Premium_Cash = orderNorm(premium_numeric_scaled$cash_premium_percent)

bestNormalize(premium_numeric_scaled$ave_premium_per_payment)
orderNorm_Premium_per_Payment = orderNorm(premium_numeric_scaled$ave_premium_per_payment)


par(mfrow = c(2,3))
MASS::truehist(orderNorm_Income$x.t, main = "Income Transformed", nbins = 7)
MASS::truehist(orderNorm_Risk_Score$x.t, main = "Risk Score Transformed", nbins = 7)
MASS::truehist(sqrt_Premiums_Paid$x.t, main = "Number of Premiums Paid Transformed", nbins = 7)
MASS::truehist(orderNorm_Premium$x.t, main = "Premium Transformed", nbins = 7)
MASS::truehist(orderNorm_Premium_Cash$x.t, main = "Cash Premiums Transformed", nbins = 7)
MASS::truehist(orderNorm_Premium_per_Payment$x.t, main = "Premium Per Payment", nbins = 7)
dev.off()

premium_numeric_norm = premium_numeric_scaled%>%mutate(income_n = orderNorm_Income$x.t,
                                                       risk_score_n = orderNorm_Risk_Score$x.t,
                                                       no_of_premiums_paid_n = sqrt_Premiums_Paid$x.t,
                                                       premium_n = orderNorm_Premium$x.t,
                                                       cash_premium_percent_n = orderNorm_Premium_Cash$x.t,
                                                       ave_premium_per_payment_n = orderNorm_Premium_per_Payment$x.t)%>%
                                                select(-c(income, no_of_premiums_paid, premium, cash_premium_percent, risk_score, ave_premium_per_payment))%>%
                                                rename(income = income_n,
                                                       risk_score = risk_score_n,
                                                       no_of_premiums_paid = no_of_premiums_paid_n,
                                                       premium = premium_n,
                                                       cash_premium_percent = cash_premium_percent_n,
                                                       ave_premium_per_payment = ave_premium_per_payment_n)
colnames(premium_numeric_norm)
head(premium_numeric_norm)
dim(premium_numeric_norm)

#Bartlett's Test for Sphericity
set.seed(0)

bartlettST = cortest.bartlett(cor(premium_numeric_norm), n = nrow(premium_numeric_norm))
formattable(as.data.frame(bartlettST))



corr_matrix_S = cor(premium_numeric_norm)

ev = eigen(corr_matrix_S)
ev

eigen_values = ev$values
eigen_values

plot(eigen_values, xlab = "Factors", ylab="Eigen Value", pch=20, col="blue", main = "Scree Plot")
lines(eigen_values, col="red")
abline(h = 1)


#Obimin is used as assumption is there is correlation
eFactors = fa(premium_numeric_norm, nfactors=4, rotate="oblimin", fm = "minres")
eFactors
fa.diagram(eFactors)


#Drop added variable: Ave_premium_per_payment
premium_numeric_norm = premium_numeric_norm%>%select(-ave_premium_per_payment)
head(premium_numeric_norm)

head(premium_factor_data)


#Combining datasets
insurance_data = cbind(premium_factor_data, premium_numeric_norm)
head(insurance_data)
dim(insurance_data)
str(insurance_data)

#Split dataset
set.seed(123)
train_rows = createDataPartition(insurance_data$default, p = 0.75, list = FALSE)
train_data = insurance_data[train_rows,]
test_data = insurance_data[-train_rows,]

nrow(train_data)
nrow(test_data)
nrow(insurance_data)

xx = train_data%>%select(-default)
yy = train_data%>%select(default)

str(train_data)

#One hot encoding - training
dummy_model = dummyVars(default ~., data = train_data)
train_data_mat = predict(dummy_model, newdata = train_data)
train_data_d = data.frame(train_data_mat)
train_data_dummy = cbind(train_data_d, yy)
str(train_data_dummy)

#One hot encoding testing
test_data_mat = predict(dummy_model, newdata = test_data)
test_data_d = data.frame(test_data_mat)
test_data_dummy = cbind(test_data_d, (test_data%>%select(default)))
str(test_data_dummy)

#SMOTE
set.seed(987)
train_smote = SMOTE(default ~., train_data_dummy, perc.over = 450, perc.under = 200)

head(train_smote)

table(train_data$default)

table(train_smote$default)

proportion_smote = 18745/dim(train_smote)[1]
proportion_smote

#KNN model
trctrl = trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(234)
knn_fit = train(default ~., data = train_data_dummy, method = "knn",
                 trControl=trctrl,
                 tuneLength = 10)
knn_fit

plot(knn_fit)

#Saving model
saveRDS(knn_fit, "./knn_fit.rds")
knn_fit = readRDS("./knn_fit.rds")

#Predicting knn
test_pred_knn = predict(knn_fit, newdata = test_data_dummy)

#Confusion matrix knn
confusionMatrix(test_pred_knn, test_data$default)

#variable importance
var_knn = varImp(knn_fit)
plot(var_knn, main = "Variable importance with KNN")

#ROCR
test_pred_knn_prob = predict(knn_fit, newdata = test_data_dummy, type = "prob")


results_roc_knn = roc(test_data_dummy$default, test_pred_knn_prob$`0`)
plot(results_roc_knn, print.thres="best", print.thres.best.method="closest.topleft", main = "ROC and AUC (0.76) - KNN")

#AUC
auc(results_roc_knn)


#Naive bayes
set.seed(567)
nb_fit = train(default ~., data = train_data_dummy, method = "nb",
               trControl=trctrl,
               tuneLength = 10)
nb_fit

plot(nb_fit, main = "Naive Bayes - Accuracy vs Distribution Type")

#Saving model
saveRDS(nb_fit, "./nb_fit.rds")
nb_fit = readRDS("./nb_fit.rds")

#Predicting knn
test_pred_nb = predict(nb_fit, newdata = test_data_dummy)

#Confusion matrix knn
confusionMatrix(test_pred_nb, test_data$default)

#variable importance
var_nb = varImp(nb_fit)
plot(var_nb, main = "Variable importance with Naive Bayes")

#ROCR
test_pred_nb_prob = predict(nb_fit, newdata = test_data_dummy, type = "prob")


results_roc_nb = roc(test_data_dummy$default, test_pred_nb_prob$`0`)

#AUC
auc(results_roc_nb)

plot(results_roc_nb, print.thres="best", print.thres.best.method="closest.topleft", main = "ROC and AUC (0.832) - Naive Bayes")


#SMOTE
set.seed(987)
train_smote = SMOTE(default ~., train_data_dummy, perc.over = 450, perc.under = 200)

head(train_smote)

table(train_data$default)

table(train_smote$default)

proportion_smote = 18745/dim(train_smote)[1]
proportion_smote

#logistic regression
logistic_fit = train(default ~., data = train_smote, method = "glm",
                     family = "binomial",
                     trControl=trctrl,
                     tuneLength = 10)

summary(logistic_fit)

plot(logistic_fit)

#Saving model
saveRDS(logistic_fit, "./logistic_fit.rds")
logistic_fit = readRDS("./logistic_fit.rds")

#Predicting logistic
test_pred_logistic = predict(logistic_fit, newdata = test_data_dummy, type = "prob")


test_pred_logistic = test_pred_logistic%>%select(`0`)

head(test_pred_logistic)

log_default = test_pred_logistic%>%mutate(defaulted = replace(`0`, `0` > 0.5, 0))%>%select(-`0`)
log_default$defaulted[log_default$defaulted > 0] = 1


head(log_default)
summary(log_default)
dim(log_default)
dim(test_data$default)
head(test_data$default)

log_default$defaulted = as.factor(log_default$defaulted)

#Confusion matrix
confusionMatrix(log_default$defaulted, test_data$default)

#variable importance
var_logistic = varImp(logistic_fit)
plot(var_logistic, main = "Variable importance with Logistic Regression")

#ROCR
test_pred_log_prob = predict(logistic_fit, newdata = test_data_dummy, type = "prob")


results_roc_log = roc(test_data_dummy$default, test_pred_log_prob$`0`)

#AUC
auc(results_roc_log)

plot(results_roc_log, print.thres="best", print.thres.best.method="closest.topleft", main = "ROC and AUC (0.823) - Logistic Regression")



#Bagging
set.seed(321)

bagging_fit = train(default ~., data = train_smote, method = "treebag",
               trControl=trctrl,
               tuneLength = 10)
bagging_fit

plot(bagging_fit)

#Saving model
saveRDS(bagging_fit, "./bagging_fit.rds")
bagging_fit = readRDS("./bagging_fit.rds")

#Predicting bagging
test_pred_bagging = predict(bagging_fit, newdata = test_data_dummy)

#Confusion matrix bagging
confusionMatrix(test_pred_bagging, test_data$default)

#variable importance
var_bagging = varImp(bagging_fit)
plot(var_bagging, main = "Variable importance with Bagging")

#ROCR
test_pred_bagging_prob = predict(bagging_fit, newdata = test_data_dummy, type = "prob")


results_roc_bagging = roc(test_data_dummy$default, test_pred_bagging_prob$`0`)

#AUC
auc(results_roc_bagging)

plot(results_roc_bagging, print.thres="best", print.thres.best.method="closest.topleft", main = "ROC and AUC (0.801) - Bagging")



#XGBoost
train_x = train_smote%>%select(-default)
train_y = train_smote%>%select(default)
test_data_matrix = as.matrix(test_data_dummy)

str(train_x)
str(train_y)

input_x = as.matrix(train_x)
input_y = as.matrix(train_y)

#hyper parameters

tune_grid = expand.grid(nrounds = seq(from = 200, to = 1000, by = 50),
                        eta = c(0.025, 0.05, 0.1, 0.3),
                        max_depth = c(2, 3, 4, 5, 6),
                        gamma = 0,
                        colsample_bytree = 1,
                        min_child_weight = 1,
                        subsample = 1)

set.seed(741)
xgb_fit =train(x = input_x,
               y = input_y,
               trControl=trctrl,
               tuneGrid = tune_grid,
               method = "xgbTree",
               verbose = 0)


xgb_fit

xgb_fit$bestTune

plot(xgb_fit)

#Saving model
saveRDS(xgb_fit, "./xgb_fit.rds")
xgb_fit = readRDS("./xgb_fit.rds")

#Predicting bagging
test_pred_xgb = predict(xgb_fit, newdata = test_data_dummy)

#Confusion matrix bagging
confusionMatrix(test_pred_xgb, test_data$default)

#variable importance
var_xgb = varImp(xgb_fit)
plot(var_xgb, main = "Variable importance with XGBoost")

#ROCR
test_pred_xgb_prob = predict(xgb_fit, newdata = test_data_dummy, type = "prob")


results_roc_xgb = roc(test_data_dummy$default, test_pred_xgb_prob$`0`)

#AUC
auc(results_roc_xgb)

plot(results_roc_xgb, print.thres="best", print.thres.best.method="closest.topleft", main = "ROC and AUC (0.814) - XGBoost")



