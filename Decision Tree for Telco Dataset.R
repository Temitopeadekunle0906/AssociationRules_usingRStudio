
Telco <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv", header = TRUE)

names(Telco)
head(Telco)
tail(Telco)
summary(Telco)
str(Telco)

ncol(Telco)
nrow(Telco)
dim(Telco)

Telco$ChurnF <- as.factor(Telco$Churn) 
str(Telco)

set.seed(1234)  
#set.seed function in R is used to reproduce results i.e.  it produces the same sample again and again.   
#sample function can be used to return a random permutation of a vector 
pd <- sample(2, nrow(Telco),replace=TRUE, prob=c(0.8,0.2))
pd 

train <- Telco[pd==1,]
validate <- Telco[pd==2,]

dim(train)
dim(validate)

library(party)
Telco_tree <- ctree(ChurnF ~ MonthlyCharges + tenure + TotalCharges, data = train) 
Telco_tree

print(Telco_tree)
plot(Telco_tree)

plot(Telco_tree, type="simple") 

predict(Telco_tree)

tab <- table(predict(Telco_tree), train$ChurnF)
print(tab)

sum(diag(tab))/sum(tab)

1-sum(diag(tab))/sum(tab) 

test_predict <- table(predict(Telco_tree, newdata= validate), validate$ChurnF) 
print(test_predict)


#Calculate classification accuracy and error on test data set

sum(diag(test_predict))/sum(test_predict)

1-sum(diag(test_predict))/sum(test_predict)

