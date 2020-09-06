library(DataExplorer) # for data exploration
library(caret) #for machine learning
library(dplyr) #for data manipulation
library(formattable) #formating output
library(mice) #Missing value imputation
library(ggplot2) #for plots
library(scales)#for scales on plots
library(pastecs) #SUmmary statistics
library(packHV) #Histogram, density and boxplot combo charts
library(corrplot) #Correlation plot
library(psych) #Bartlett test
library(GPArotation) #required package for factor analysis rotation
library(rpivotTable) #for categorical plot comparisons
library(ROCR) #model evaluation
library(e1071) #for naive bayes
library(ipred) #for bagging
library(rpart) #for bagging
library(xgboost) #for boosting
library(DMwR) #for SMOTE

setwd("C:/Users/kudaakwashe/Documents/Study/PGPDSBA/Predictive Modelling/Project")

cars_data = read.csv("Cars-dataset.csv", header = TRUE)

head(cars_data)
tail(cars_data)
str(cars_data)

summary(cars_data)
plot_str(cars_data, fontSize = 38)

plot_intro(cars_data)
plot_missing(cars_data,title = "Before Missing Value Treatment")

cars_data = cars_data%>%mutate(Engineer = as.factor(Engineer),
                               MBA = as.factor(MBA),
                               License = as.factor(license))%>%select(-license)

#Impute missing values
set.seed(248)
mice_mod = mice(cars_data[, !names(cars_data) %in% "Transport"], method = "rf")
cars_mice = complete(mice_mod)

cars_complete = cbind(cars_mice, cars_data$Transport)

cars_complete = cars_complete%>%rename(Transport = "cars_data$Transport")

cars_complete[243,]

summary(cars_complete)

#Bar Plots
transport_bar = ggplot(cars_complete, aes(Transport)) +
                geom_bar(aes(y = (..count..)/sum(..count..), fill = Transport), stat= "count") +
                geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                              y= ((..count..)/sum(..count..))), stat="count", vjust = -.25) +
                scale_y_continuous(labels = percent) +
                labs(title = "Transport", y = "Percent")

transport_bar

gender_bar = ggplot(cars_complete, aes(Gender)) +
             geom_bar(aes(y = (..count..)/sum(..count..), fill = Gender), stat= "count") +
             geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                            y= ((..count..)/sum(..count..))), stat="count", vjust = -.25) +
             scale_y_continuous(labels = percent) +
             labs(title = "Gender", y = "Percent")

gender_bar


engineer_bar = ggplot(cars_complete, aes(Engineer)) +
               geom_bar(aes(y = (..count..)/sum(..count..), fill = Engineer), stat= "count") +
               geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                              y= ((..count..)/sum(..count..))), stat="count", vjust = -.25) +
               scale_y_continuous(labels = percent) +
               scale_x_discrete(labels = c("Not Engineer", "Engineer")) +
               labs(title = "Engineer", y = "Percent")

engineer_bar


mba_bar = ggplot(cars_complete, aes(MBA)) +
          geom_bar(aes(y = (..count..)/sum(..count..), fill = MBA), stat= "count") +
          geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                        y= ((..count..)/sum(..count..))), stat="count", vjust = -.25) +
          scale_y_continuous(labels = percent) +
          scale_x_discrete(labels = c("Not MBA", "MBA")) +
          labs(title = "MBA", y = "Percent")

mba_bar



license_bar = ggplot(cars_complete, aes(License)) +
              geom_bar(aes(y = (..count..)/sum(..count..), fill = License), stat= "count") +
              geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                            y= ((..count..)/sum(..count..))), stat="count", vjust = -.25) +
              scale_y_continuous(labels = percent) +
              scale_x_discrete(labels = c("No License", "License")) +
              labs(title = "License", y = "Percent")

license_bar

#Separating categorical and numeric factors
cars_numeric = cars_complete%>%select_if(is.numeric)

cars_factor = cars_complete%>%select_if(is.factor)

#Numeric variable univariate
#Summary stats
options(scipen=999, digits = 3)
formattable(as.data.frame(stat.desc(cars_numeric)))
options(scipen = 0)

#hist and density plots
par(mfrow = c(2,2))
for(t in 1:4){
  s = cars_numeric[,t]
  hist_boxplot(s, freq = FALSE, density = TRUE, main = colnames(cars_numeric[t]), xlab = "Value")
}
dev.off()

#QQ-Plots
plot_qq(cars_numeric, title = "QQ-Plots", ncol = 2)

# Proporttion of Work.Exp value <= 5 years
nrow(cars_numeric[cars_numeric$Work.Exp <= 5,])/nrow(cars_numeric)


#Bivariate analysis
rpivotTable(cars_factor)

featurePlot(x = cars_numeric,
            y = cars_complete$Transport,
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"),
                          y = list(relation="free")))

featurePlot(x = cars_numeric,
            y = cars_complete$Transport,
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"),
                          y = list(relation="free")),
            auto.key = list(columns = 3))

#Average and max distances by transport mode
cars_complete%>%group_by(Transport)%>%select(Distance, Transport)%>%summarise(Avg = mean(Distance))
cars_complete%>%group_by(Transport)%>%select(Distance, Transport)%>%summarise(Max_dist = max(Distance))
cars_complete%>%select(Distance, Transport)%>%group_by(Transport)%>%arrange(desc(Distance))%>%filter(Transport == "2Wheeler")

cars_complete%>%group_by(Transport)%>%select(Age)%>%summarise(AvgA = mean(Age))

cars_complete%>%group_by(Transport)%>%select(Work.Exp)%>%summarise(AvgA = mean(Work.Exp))

#Checking for multicollinearity
corr_matrix = cor(cars_numeric)
corrplot(corr_matrix, method = "number", type = "upper", diag = F, title = "Correlation Matrix", mar = c(2, 2, 5, 2) + 0.1)

#Test for multicollinearity remember to scale or normalise
set.seed(123)
cars_bartlett = cortest.bartlett(corr_matrix, n = nrow(cars_numeric))
options(scipen=999, digits = 3)
formattable(as.data.frame(cars_bartlett))
options(scipen = 0)

#measure of sampling adequecy and whether model is a suitable candidate for factor analysis
KMO(corr_matrix)

cars_complete1 = cars_complete

#outlier boundaries
#number of outliers in salary
IQR_Salary = IQR(cars_complete1$Salary)
LL_Salary = quantile(cars_complete1$Salary,0.25) - 1.5*IQR_Salary
UL_Salary = quantile(cars_complete1$Salary,0.75) + 1.5*IQR_Salary
out_Salary = subset(cars_complete1, Salary < LL_Salary | Salary > UL_Salary)
nrow(out_Salary)
nrow(out_Salary)/nrow(cars_complete1)

#number of outliers in Distance
IQR_Distance = IQR(cars_complete1$Distance)
LL_Distance = quantile(cars_complete1$Distance,0.25) - 1.5*IQR_Distance
UL_Distance = quantile(cars_complete1$Distance,0.75) + 1.5*IQR_Distance
out_Distance = subset(cars_complete1, Distance < LL_Distance | Distance > UL_Distance)
nrow(out_Distance)
nrow(out_Distance)/nrow(cars_complete1)

#number of outliers in Age
IQR_Age = IQR(cars_complete1$Age)
LL_Age = quantile(cars_complete1$Age,0.25) - 1.5*IQR_Age
UL_Age = quantile(cars_complete1$Age,0.75) + 1.5*IQR_Age
out_Age = subset(cars_complete1, Age < LL_Age | Age > UL_Age)
nrow(out_Age)
nrow(out_Age)/nrow(cars_complete1)

#number of outliers in Work experience
IQR_Work.Exp = IQR(cars_complete1$Work.Exp)
LL_Work.Exp = quantile(cars_complete1$Work.Exp,0.25) - 1.5*IQR_Work.Exp
UL_Work.Exp = quantile(cars_complete1$Work.Exp,0.75) + 1.5*IQR_Work.Exp
out_Work.Exp = subset(cars_complete1, Work.Exp < LL_Work.Exp | Work.Exp > UL_Work.Exp)
nrow(out_Work.Exp)
nrow(out_Work.Exp)/nrow(cars_complete1)


#Outlier treatment algorithm
#Distance imputation
set.seed(123)
cars_complete1$Distance[cars_complete1$Distance > UL_Distance | cars_complete1$Distance < LL_Distance] = NA
mice_dist = mice(cars_complete1, method = "rf")
cars_complete_D = complete(mice_dist)

#Age imputation
set.seed(123)
cars_complete_D$Age[cars_complete_D$Age > UL_Age | cars_complete_D$Age < LL_Age] = NA
mice_age = mice(cars_complete_D, method = "rf")
cars_complete_A = complete(mice_age)

#Work experence imputation
set.seed(123)
cars_complete_A$Work.Exp[cars_complete_A$Work.Exp > UL_Work.Exp | cars_complete_A$Work.Exp < LL_Work.Exp] = NA
mice_work = mice(cars_complete_A, method = "rf")
cars_complete_WE = complete(mice_work)

#Salary imputation
set.seed(123)
cars_complete_WE$Salary[cars_complete_WE$Salary > UL_Salary | cars_complete_WE$Salary < LL_Salary] = NA
mice_sal = mice(cars_complete_WE, method = "rf")
cars_complete_S = complete(mice_sal)

table(is.na(cars_complete_WE))
table(is.na(cars_complete_S))

dim(cars_complete_S)
dim(cars_complete)

cars_numeric_S = cars_complete_S%>%select_if(is.numeric)

#hist and density plots
par(mfrow = c(2,2))
for(v in 1:4){
  w = cars_numeric_S[,v]
  hist_boxplot(w, freq = FALSE, density = TRUE, main = colnames(cars_numeric_S[v]), xlab = "Value")
}
dev.off()

#number of outliers in Work experience after treatment
IQR_Work.Exp_P = IQR(cars_complete_S$Work.Exp)
LL_Work.Exp_P = quantile(cars_complete_S$Work.Exp,0.25) - 1.5*IQR_Work.Exp_P
UL_Work.Exp_P = quantile(cars_complete_S$Work.Exp,0.75) + 1.5*IQR_Work.Exp_P
out_Work.Exp_P = subset(cars_complete_S, Work.Exp < LL_Work.Exp_P | Work.Exp > UL_Work.Exp_P)
nrow(out_Work.Exp_P)
nrow(out_Work.Exp_P)/nrow(cars_complete_S)

#number of outliers in salary after treatment
IQR_Salary_P = IQR(cars_complete_S$Salary)
LL_Salary_P = quantile(cars_complete_S$Salary,0.25) - 1.5*IQR_Salary_P
UL_Salary_P = quantile(cars_complete_S$Salary,0.75) + 1.5*IQR_Salary_P
out_Salary_P = subset(cars_complete_S, Salary < LL_Salary_P | Salary > UL_Salary_P)
nrow(out_Salary_P)
nrow(out_Salary_P)/nrow(cars_complete_S)

#Scaling
cars_numeric_s_model = preProcess(cars_numeric_S, method = "range")
cars_ns = predict(cars_numeric_s_model, newdata = cars_numeric_S)

featurePlot(x = cars_ns,
            y = cars_complete$Transport,
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"),
                          y = list(relation="free")),
            auto.key = list(columns = 3))


#use of factor analysis

corr_matrix_S = cor(cars_ns)

ev = eigen(corr_matrix_S)
ev

eigen_values = ev$values
eigen_values

plot(eigen_values, xlab = "Factors", ylab="Eigen Value", pch=20, col="blue", main = "Scree Plot")
lines(eigen_values, col="red")
abline(h = 1)

#Obimin is used as assumption is there is correlation
eFactors = fa(cars_ns, nfactors=2, rotate="oblimin", fm = "minres")
eFactors
fa.diagram(eFactors)

cars_fa = cars_ns
names(cars_fa)

cars_sub = cars_fa%>%select(-"Work.Exp", -"Age", -"Salary")
cars_treated = cbind(cars_sub, Swa = eFactors$scores[,1])
head(cars_treated)
dim(cars_treated)

#combining datasets
cars_final = cbind(cars_factor, cars_treated)
head(cars_final)
dim(cars_final)

#Split dataset
set.seed(123)
train_rows = createDataPartition(cars_final$Transport, p = 0.75, list = FALSE)
train_data = cars_final[train_rows,]
test_data = cars_final[-train_rows,]

xx = train_data%>%select(-Transport)
yy = train_data%>%select(Transport)

str(train_data)

#One hot encoding - training
dummy_model = dummyVars(Transport ~., data = train_data)
train_data_mat = predict(dummy_model, newdata = train_data)
train_data_d = data.frame(train_data_mat)
train_data_dummy = cbind(train_data_d, yy)
str(train_data_dummy)


#One hot encoding -test
test_data_1 = test_data

test_data_mat = predict(dummy_model, newdata = test_data_1)
test_data_d = data.frame(test_data_mat)
test_data_dummy = cbind(test_data_d, (test_data%>%select(Transport)))
str(test_data_dummy)


#KNN
set.seed(123)
knn_fit = train(Transport ~., data = train_data_dummy, method = "knn", trControl = trainControl(method = "cv", number = 3),
                tuneLength = 10)

knn_fit
predict_knn = predict(knn_fit, newdata = test_data_dummy, type ="raw")
table(test_data$Transport, predict_knn)

#variable importance
var_knn = varImp(knn_fit)
plot(var_knn, main = "Variable importance with KNN")

#Naive Bayes
nb_model = naiveBayes(x = train_data_dummy[,1:10], y = train_data_dummy[,11])
pred_nb = predict(nb_model, newdata = test_data_dummy[,1:10])
table(test_data_dummy[,11], pred_nb)


#Logistic model
test_data_dummy_l = test_data_dummy
set.seed(123)
log_model = glm(Transport ~., data = train_data_dummy, family = "binomial"(link = "logit"))
summary(log_model)

train_data_dummy_l = train_data_dummy%>%select(-"Gender.Male", -"Engineer.0", -"Engineer.1", -"MBA.0", -"MBA.1", -"License.1")
log_model_1 = glm(Transport ~., data = train_data_dummy_l, family = "binomial"(link = "logit"))
summary(log_model_1)

test_data_dummy_l = test_data_dummy%>%select(-"Gender.Male", -"Engineer.0", -"Engineer.1", -"MBA.0", -"MBA.1", -"License.1")

str(test_data_dummy_l)
test_data_dummy_l$log_pred = predict(log_model_1, test_data_dummy_l[,1:4], type = "response")
table(test_data_dummy_l$Transport, test_data_dummy_l$log_pred > 0.5)


#Bagging
set.seed(123)
test_data_dummy_b = test_data_dummy
bagging_model = bagging(Transport ~.,
                        data = train_data_dummy,
                        control = rpart.control(maxdepth = 5, minsplit = 10))

test_data_dummy_b$class_pred = predict(bagging_model, test_data_dummy_b)
table(test_data_dummy_b$Transport, test_data_dummy_b$class_pred)


#Recoding Transport to produce two levels of output
train_data_boost = train_data_dummy
test_data_boost = test_data_dummy
train_data_boost = train_data_boost%>%mutate(Transport = recode(Transport,"Car" = 1,
                                             "2Wheeler" = 0,
                                             "Public Transport" = 0))

test_data_boost = test_data_boost%>%mutate(Transport = recode(Transport,"Car" = 1,
                                                                "2Wheeler" = 0,
                                                                "Public Transport" = 0))

str(train_data_boost)
table(train_data_boost$Transport)

#Boosting
train_boost = as.matrix(train_data_boost[,1:10])
train_label = as.matrix(train_data_boost[,11])
test_boost = as.matrix(test_data_boost[,1:10])

set.seed(123)
xgb_fit = xgboost(data = train_boost,
                  label = train_label,
                  eta = 0.001,
                  max_depth = 3,
                  nrounds = 10000,
                  nfold = 5,
                  objective = "binary:logistic",
                  verbose = 0,
                  early_stopping_rounds = 10)


test_data_boost$xgb_pred_class = predict(xgb_fit, test_boost)

table(test_data_boost$Transport, test_data_boost$xgb_pred_class>=0.5)


#SMOTE
smote_train = train_data_boost
smote_test = test_data_boost
set.seed(123)
smote_train$Transport = as.factor(smote_train$Transport)
balanced_gd = SMOTE(Transport ~., smote_train, perc.over = 100, k = 5)

table(balanced_gd$Transport)

smote_train_f = as.matrix(balanced_gd[,1:10])
smote_label_train = as.matrix(balanced_gd$Transport)


smote_xgb_fit = xgboost(
  data = smote_train_f,
  label = smote_label_train,
  eta = 0.7,
  max_depth = 5,
  nrounds = 50,
  nfold = 5,
  objective = "binary:logistic",
  verbose = 0,
  early_stopping_rounds = 10
)

#SMOTE
smote_features_test = as.matrix(smote_test[,1:10])
smote_test$smote_pred_class = predict(smote_xgb_fit, smote_features_test)

table(smote_test$Transport, smote_test$smote_pred_class>=0.5)


