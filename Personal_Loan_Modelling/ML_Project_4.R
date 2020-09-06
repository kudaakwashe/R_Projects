
library(readxl)
library(DataExplorer)
library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)
library(rpivotTable)
library(packHV)
library(pastecs)
library(formattable)
library(lattice)
library(corrplot)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(data.table)
library(ROCR)
library(ineq)
library("InformationValue")

setwd("C:/Users/kudaakwashe/Documents/Study/PGPDSBA/Machine Learning/ML_Project")

loan_data = read_xlsx("Thera Bank_Personal_Loan_Modelling-dataset-1.xlsx", sheet = "Bank_Personal_Loan_Modelling")

head(loan_data)
tail(loan_data)
str(loan_data)

setnames(loan_data, old = c("Age (in years)", "Experience (in years)", "Income (in K/month)", "ZIP Code",
                            "Family members", "CCAvg", "Personal Loan", "Securities Account", "CD Account", "CreditCard"),
                          new = c("Age", "Experience", "Income", "ZIP_Code", "Family_Members", "CC_Avg",
                                  "Personal_Loan", "Securities_Account", "CD_Account", "Credit_Card"))

factor_cols = c("ID","Education", "ZIP_Code", "Personal_Loan", "Securities_Account", "CD_Account", "Credit_Card", "Online")

loan_data = loan_data%>%mutate_each_(funs(factor(.)), factor_cols)

loan_data = subset(loan_data, select = -c(ID))

loan_data1 = na.omit(loan_data)
plot_missing(loan_data, title = "Before Missing Value Treatment")
plot_missing(loan_data1, title = "After Missing Value Treatment")



EducationB = (ggplot(loan_data1,aes(Education))
            + geom_bar(aes(fill=Education))
            + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))) +
            scale_x_discrete(labels = c("Undergrad", "Graduate", "Advanced/Professional")) +
            ggtitle("Education Levels")

EducationB

ZIP_Bar = loan_data1%>%group_by(ZIP_Code)%>%summarise(n_Obs = n())%>%arrange(desc(n_Obs))%>%head(20)
barplot(ZIP_Bar)

zip_count = table(loan_data1$ZIP_Code)
zip_count = as.data.frame(zip_count)
zip_count_ordered = head(zip_count[order(-zip_count$Freq),],20)
zip_count_ordered
barplot(zip_count_ordered$Freq, main = "Zip Codes and Number of Customers", xlab = "Zip Codes", ylab = "Number of Customers",
        names.arg = zip_count_ordered$Var1, las = 2)

PersonaLoanB = (ggplot(loan_data1,aes(Personal_Loan))
              + geom_bar(aes(fill=Personal_Loan))
              + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)))+
              scale_x_discrete(labels = c("Did not accept", "Accepted"))  +
              ggtitle("Personal Loan Offer")
  
PersonaLoanB

SecuritiesAccountB = (ggplot(loan_data1,aes(Securities_Account))
                + geom_bar(aes(fill=Securities_Account))
                + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)))+
                scale_x_discrete(labels = c("No Securities Account", "Securities Account"))+
                ggtitle("Securities Account")
SecuritiesAccountB

CDAccountB = (ggplot(loan_data1,aes(CD_Account))
                      + geom_bar(aes(fill=CD_Account))
                      + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)))+
                      scale_x_discrete(labels = c("No CD Account", "CD Account"))+
                      ggtitle("CD Account")
CDAccountB

CreditCardB = (ggplot(loan_data1,aes(Credit_Card))
              + geom_bar(aes(fill = Credit_Card))
              + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)))+
              scale_x_discrete(labels = c("No Credit Card", "Credit Card"))+
              ggtitle("Credit Card")
CreditCardB

OnlineB = (ggplot(loan_data1,aes(Online))
               + geom_bar(aes(fill = Online))
               + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)))+
                scale_x_discrete(labels = c("No Online", "Online"))+
                ggtitle("Online Banking")
OnlineB

num_data = loan_data1[,sapply(loan_data1, is.numeric)]
num_data = as.data.frame(num_data)


par(mfrow = c(2,3))
for(i in 1:6){
  r = num_data[,i]
  if(num_data[,i] != num_data$Family_Members){
    hist_boxplot(r, freq = FALSE, density = TRUE, main = colnames(num_data[i]), xlab = "Value")
  }else{
    hist_boxplot(r, freq = FALSE, density = FALSE, main = colnames(num_data[i]))
  }
  
}
dev.off()

options(scipen=999, digits = 3)
stats_table = stat.desc(num_data)
formattable(stats_table)
options(scipen = 0)


rpivotTable(loan_data1)


corrplot(cor(num_data))

dim(loan_data1)

#separating loan datasets and their features
pData = loan_data1[loan_data1$Personal_Loan=="1",]
pData0 = loan_data1[loan_data1$Personal_Loan=="0",]


summary(pData)
summary(pData0)
# 
# plot_histogram(num_data)
head(num_data)

hist(num_data[,1])

num_data_h = num_data

#Scale data
scaled_num_data = scale(num_data_h)

apply(scaled_num_data,2,mean)
apply(scaled_num_data,2,sd)

#distance matrix
dist_matrix_scaled = dist(scaled_num_data, method = "euclidean")

clust = hclust(dist_matrix_scaled, method = "average")
plot(clust)
rect.hclust(clust, k=6, border = "red")

# add cluster to dataset
num_data_h$Cluster = cutree(clust, k=6)
head(num_data_h)

#find customers average profiles
custProfile = aggregate(num_data_h, list(num_data_h$Cluster), FUN = "mean")
custProfile

formattable(custProfile[,-1])

#CART
#Split data
set.seed(1234)
split = sample.split(loan_data1$Personal_Loan, SplitRatio=0.7)
train = as.data.frame(subset(loan_data1, split == TRUE))
test = as.data.frame(subset(loan_data1, split == FALSE))

nrow(train)
nrow(test)

set.seed(1234)
tree = rpart(formula = Personal_Loan ~ ., data = train[,-4], method = "class", minbucket = 10, cp = 0)
tree
rpart.plot(tree)
printcp(tree)
plotcp(tree)

#pruning tree
ptree = prune(tree, cp=0.1, "CP")
printcp(ptree)
rpart.plot(ptree)
ptree

#giving the actual parts
path.rpart(ptree, c(1:7))


head(train)
train1=train[,-4]
#Predicting
train1$Prediction = predict(ptree, data = train, type="class")
train1$Prob = predict(ptree, data = train, type="prob")[,"1"]


#test data CART
set.seed(1234)
tree_test = rpart(formula = Personal_Loan ~ ., data = test[-4], method = "class", minbucket = 10, cp = 0.1)
tree_test
rpart.plot(tree_test)
printcp(tree_test)
plotcp(tree_test)

#pruning tree test
ptree_test = prune(tree_test, cp=0.05, "CP")
printcp(ptree_test)
rpart.plot(ptree_test)
ptree_test

#giving the actual parts
path.rpart(ptree_test, c(1:7))


head(test)
test1=test[,-4]
#Predicting
test1$Prediction = predict(tree_test, data = test[,-4], type="class")
test1$Prob = predict(tree_test, data = test[,-4], type="prob")[,"1"]

head(test1)
nrow(test1)
#Random Forest
sum(train$Personal_Loan=="1")/nrow(train)

nrow(train)
set.seed(1234)
rndForest = randomForest(Personal_Loan ~ ., data = train[,-c(4)], ntree = 501, mtry = 6, nodesize = 5, importance = TRUE)
rndForest

plot(rndForest)

importance(rndForest)

set.seed(1234)
trndForest = tuneRF(x = train[,-c(4,9)], y = train$Personal_Loan, mtryStart = 5, stepFactor = 1.5, ntreeTry = 171, nodesize = 5,
                    trace = TRUE, plot = TRUE, doBest = TRUE, importance = TRUE)

importance(trndForest)


test_rndForest = randomForest(Personal_Loan ~ ., data = test[,-c(4)], ntree = 171, mtry = 7, nodesize = 5, importance = TRUE)
test_rndForest

importance(test_rndForest)
dim(train[,-c(4,9)])

train2 = train[,-4]


train2$Predicted_Class = predict(trndForest, train2, type = "class")
train2$Prob = predict(trndForest, train2, type = "prob")[,"1"]
head(train2)

tbl = table(train2$Personal_Loan, train2$Predicted_Class)
tbl
(tbl[1,2]+tbl[2,1])/nrow(train2)

qs = quantile(train2$Prob, prob = seq(0,1, length=11))
qs

#setting threshold
threshold = qs[10]
mean(train2$Personal_Loan[train2$Prob>threshold]=="1")

#Using test data
test2 = test[,-4]

test2$Predicted_Class = predict(test_rndForest, test2, type = "class")
test2$Prob = predict(test_rndForest, test2, type = "prob")[,"1"]

tbl_t = table(test2$Personal_Loan, test2$Predicted_Class)
tbl_t
(tbl[1,2]+tbl[2,1])/nrow(test2)

qs_t = quantile(test2$Prob, prob = seq(0,1, length=11))
qs_t

#setting threshold
threshold_t = qs[10]
mean(test2$Personal_Loan[test2$Prob>threshold_t]=="1")


for(i in seq(from=0.2, to=1.0, by=0.1)){
  mean_thresh = mean(test2$Personal_Loan[test2$Prob>i]=="1")
  print(mean_thresh)  
}


set.seed(1234)
rndForest_test = tuneRF(x = test[,-c(4,9)], y = test$Personal_Loan, mtryStart = 7, stepFactor = 1.5, ntreeTry = 171, nodesize = 5,
                    trace = TRUE, plot = TRUE, doBest = TRUE, importance = TRUE)

importance(rndForest_test)

rpart.plot(trndForest_test)
plot(rndForest_test)

###Model performance on test and train

tbl_cart_train = table(train1$Personal_Loan, train1$Prediction)
tbl_cart_train

tbl_cart_test = table(test1$Personal_Loan, test1$Prediction)
tbl_cart_test

tbl_rf_train = table(train2$Personal_Loan, train2$Prediction)
tbl_rf_train

tbl_rf_test = table(test2$Personal_Loan, test2$Prediction)
tbl_rf_test

#Error rate - confusion matrix
(tbl_cart_train[1,2]+tbl_cart_train[2,1])/nrow(train1)

head(train1)

#Rank Cart Train
probs = seq(0,1,length=11)
qs_cart_train = quantile(train1$Prob,probs)
train1$Deciles = cut(train1$Prob, unique(qs_cart_train),include.lowest = TRUE)


train1DT = data.table(train1)
rank_cart_table = train1DT[,list(Count=length(Personal_Loan),
                                 Count_PL_1 = sum(Personal_Loan == 1),
                                 Count_PL_0 = sum(Personal_Loan == 0)
                                 
                                 ),by=Deciles][order(-Deciles)]

rank_cart_table$RRate = round(rank_cart_table$Count_PL_1 / rank_cart_table$Count, 4)*100
rank_cart_table$Cum_RRate = cumsum(rank_cart_table$Count_PL_1)
rank_cart_table$Cum_Non_RRate = cumsum(rank_cart_table$Count_PL_0)

rank_cart_table$Cum_Rel_Resp = round(rank_cart_table$Cum_RRate/sum(rank_cart_table$Count_PL_1),4)*100
rank_cart_table$Cum_Non_Resp = round(rank_cart_table$Cum_Non_RRate/sum(rank_cart_table$Count_PL_0),4)*100

rank_cart_table$KS = abs(rank_cart_table$Cum_Rel_Resp - rank_cart_table$Cum_Non_Resp)

rank_cart_table

dim(test1)
#Cart Test
probs_test = seq(0,1,length=11)
qs_cart_test = quantile(test1$Prob,probs_test)
test1$Deciles = cut(test1$Prob, unique(qs_cart_test),include.lowest = TRUE)


test1DT = data.table(test1)
rank_cart_test = test1DT[,list(Count=length(Personal_Loan),
                                 Count_PL_1 = sum(Personal_Loan == 1),
                                 Count_PL_0 = sum(Personal_Loan == 0)
                                 
),by=Deciles][order(-Deciles)]

rank_cart_test$RRate = round(rank_cart_test$Count_PL_1 / rank_cart_test$Count, 4)*100
rank_cart_test$Cum_RRate = cumsum(rank_cart_test$Count_PL_1)
rank_cart_test$Cum_Non_RRate = cumsum(rank_cart_test$Count_PL_0)

rank_cart_test$Cum_Rel_Resp = round(rank_cart_test$Cum_RRate/sum(rank_cart_test$Count_PL_1),4)*100
rank_cart_test$Cum_Non_Resp = round(rank_cart_test$Cum_Non_RRate/sum(rank_cart_test$Count_PL_0),4)*100

rank_cart_test$KS = abs(rank_cart_test$Cum_Rel_Resp - rank_cart_test$Cum_Non_Resp)

rank_cart_test
rank_cart_table



#Rank Random Train
probs = seq(0,1,length=11)
qs_rf_train = quantile(train2$Prob,probs)
train2$Deciles = cut(train2$Prob, unique(qs_rf_train),include.lowest = TRUE)


train2DT = data.table(train2)
rank_rf_table = train2DT[,list(Count=length(Personal_Loan),
                                 Count_PL_1 = sum(Personal_Loan == 1),
                                 Count_PL_0 = sum(Personal_Loan == 0)
                                 
),by=Deciles][order(-Deciles)]

rank_rf_table$RRate = round(rank_rf_table$Count_PL_1 / rank_rf_table$Count, 4)*100
rank_rf_table$Cum_RRate = cumsum(rank_rf_table$Count_PL_1)
rank_rf_table$Cum_Non_RRate = cumsum(rank_rf_table$Count_PL_0)

rank_rf_table$Cum_Rel_Resp = round(rank_rf_table$Cum_RRate/sum(rank_rf_table$Count_PL_1),4)*100
rank_rf_table$Cum_Non_Resp = round(rank_rf_table$Cum_Non_RRate/sum(rank_rf_table$Count_PL_0),4)*100

rank_rf_table$KS = abs(rank_rf_table$Cum_Rel_Resp - rank_rf_table$Cum_Non_Resp)

rank_rf_table

dim(test2)
#Random Forest Test
probs_test = seq(0,1,length=11)
qs_rf_test = quantile(test2$Prob,probs_test)
test2$Deciles = cut(test2$Prob, unique(qs_rf_test),include.lowest = TRUE)


test2DT = data.table(test2)
rank_rf_test = test2DT[,list(Count=length(Personal_Loan),
                               Count_PL_1 = sum(Personal_Loan == 1),
                               Count_PL_0 = sum(Personal_Loan == 0)
                               
),by=Deciles][order(-Deciles)]

rank_rf_test$RRate = round(rank_rf_test$Count_PL_1 / rank_rf_test$Count, 4)*100
rank_rf_test$Cum_RRate = cumsum(rank_rf_test$Count_PL_1)
rank_rf_test$Cum_Non_RRate = cumsum(rank_rf_test$Count_PL_0)

rank_rf_test$Cum_Rel_Resp = round(rank_rf_test$Cum_RRate/sum(rank_rf_test$Count_PL_1),4)*100
rank_rf_test$Cum_Non_Resp = round(rank_rf_test$Cum_Non_RRate/sum(rank_rf_test$Count_PL_0),4)*100

rank_rf_test$KS = abs(rank_rf_test$Cum_Rel_Resp - rank_rf_test$Cum_Non_Resp)

rank_cart_test
rank_cart_table

rank_rf_test
rank_rf_table


###Plot KS Chart Cart
Cum_Base_Percent = probs_test

plot(xlim = c(0,110), rank_rf_table$Cum_Rel_Resp, lty=1)

plot(rank_cart_test$Cum_Rel_Resp, xlim = c(0,10), ylim = c(0,100), type = "o")
lines(rank_cart_test$Cum_Non_Resp, xlim = c(0,10), ylim = c(0,100), type = "o")


#ROCR Curves
pred_cart_train = prediction(train1$Prob, train1$Personal_Loan)
perf_cart_train = performance(pred_cart_train, "tpr", "fpr")
pred_cart_test = prediction(test1$Prob, test1$Personal_Loan)
perf_cart_test = performance(pred_cart_test, "tpr", "fpr")

pred_rf_train = prediction(train2$Prob, train2$Personal_Loan)
perf_rf_train = performance(pred_rf_train, "tpr", "fpr")
pred_rf_test = prediction(test2$Prob, test2$Personal_Loan)
perf_rf_test = performance(pred_rf_test, "tpr", "fpr")

par(mfrow=c(2,2))
plot(perf_cart_train, col = "blue")
plot(perf_cart_test, col = "blue", lty=2)
plot(perf_rf_train, col = "red")
plot(perf_rf_test, col = "red", lty=2)
dev.off()

#AUC
auc_cart_train = performance(pred_cart_train, "auc")
auc_cart_train = as.numeric(auc_cart_train@y.values)
auc_cart_train

auc_cart_test = performance(pred_cart_test, "auc")
auc_cart_test = as.numeric(auc_cart_test@y.values)
auc_cart_test

auc_rf_train = performance(pred_rf_train, "auc")
auc_rf_train = as.numeric(auc_rf_train@y.values)
auc_rf_train

auc_rf_test = performance(pred_rf_test, "auc")
auc_rf_test = as.numeric(auc_rf_test@y.values)
auc_rf_test

#using Gini
gini_cart_train = ineq(train1$Prob,"gini")
gini_cart_train

gini_cart_test = ineq(test1$Prob,"gini")
gini_cart_test

gini_rf_train = ineq(train2$Prob,"gini")
gini_rf_train

gini_rf_test = ineq(test2$Prob,"gini")
gini_rf_test


#Concordance
Concordance(actuals = train1$Personal_Loan, predictedScores = train1$Prob)
Concordance(actuals = test1$Personal_Loan, predictedScores = test1$Prob)

Concordance(actuals = train2$Personal_Loan, predictedScores = train2$Prob)
Concordance(actuals = test2$Personal_Loan, predictedScores = test2$Prob)

# factor_data = loan_data1[,sapply(loan_data1, is.factor)]
# 
# plotBarCharts = function(x, ...){
#   nm = names(x)
#   for(i in seq_along(nm)){
#     par(mfrow = c(3, 3))
#    graphp[i]  = ggplot(x, aes_string(x = nm[i])) + geom_bar()
#    
#    graphp[i]
#   }
# }
# 
# plotBarCharts(factor_data)
# 
# head(factor_data)
# 
# ggplot(factor_data, aes()) + geom_bar()
# 
# fact_d = loan_data1 %>%
#   as_data_frame() %>%
#   select_if(is.factor) %>%
#   gather(key = "variable", value = "value")
# 
# head(fact_d, 3)
# str(loan_data1)
# 
# 
# 
