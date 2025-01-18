library("mlbench")
data("BreastCancer")
head(BreastCancer)
names(BreastCancer)
#ID变量不纳数据分析 它不是特征 所以排除
df <- BreastCancer[-1]
df

sum(is.na(df))
colSums(is.na(df))

df <- na.omit(df)

#划分测试集和训练集
set.seed(1234)
index <- sample(nrow(df), 0.7*nrow(df))
train <- df[index,]
test <- df[-index,]

train$Cl.thickness <- as.numeric(train$Cl.thickness)
train$Cell.size <- as.numeric(train$Cell.size)
train$Cell.shape <- as.numeric(train$Cell.shape)
train$Marg.adhesion <- as.numeric(train$Marg.adhesion)
train$Epith.c.size <- as.numeric(train$Epith.c.size)
train$Bare.nuclei <- as.numeric(train$Bare.nuclei)
train$Bl.cromatin <- as.numeric(train$Bl.cromatin)
train$Normal.nucleoli <- as.numeric(train$Normal.nucleoli)
train$Mitoses <- as.numeric(train$Mitoses)


test$Cl.thickness <- as.numeric(test$Cl.thickness)
test$Cell.size <- as.numeric(test$Cell.size)
test$Cell.shape <- as.numeric(test$Cell.shape)
test$Marg.adhesion <- as.numeric(test$Marg.adhesion)
test$Epith.c.size <- as.numeric(test$Epith.c.size)
test$Bare.nuclei <- as.numeric(test$Bare.nuclei)
test$Bl.cromatin <- as.numeric(test$Bl.cromatin)
test$Normal.nucleoli <- as.numeric(test$Normal.nucleoli)
test$Mitoses <- as.numeric(test$Mitoses)

str(test)
str(train)


install.packages("e1071")
library(e1071)
set.seed(1234)
fit.svm <- svm(Class ~ ., data=train)
fit.svm
#使用test 数据集进行predict

svm.pred <- predict(fit.svm,test)
svm.perf <- table(test$Class,svm.pred,dnn=c("Actual","Predicted"))
svm.perf 




# 提取混淆矩阵confusion matrix中的值
TN <- svm.perf[1, 1]  # True Negatives
FP <- svm.perf[1, 2]  # False Positives
FN <- svm.perf[2, 1]  # False Negatives
TP <- svm.perf[2, 2]  # True Positives

# 计算准确率 (Accuracy)
accuracy <- (TP + TN) / sum(svm.perf)

# 计算精确度 (Precision)
precision <- TP / (TP + FP)

# 计算召回率 (Recall)
recall <- TP / (TP + FN)

# 计算F1-Score
f1_score <- 2 * (precision * recall) / (precision + recall)

list(
  Accuracy = accuracy,
  Precision = precision,
  Recall = recall,
  F1_Score = f1_score
)



#SVM调优
set.seed(1234)
tuned <- tune.svm(Class~.,data=train,gamma=10^(-6:1),cost=10^(-10:10))
tuned 

#多了一个准确判断的。还是提升了的哦！但是几乎提升程度不大的！
fit.svm1 <- svm(Class~., data=train,gamma=0.01,cost=1)
svm.pred1 <- predict(fit.svm1,na.omit(test))
svm.perf1 <- table(na.omit(test)$Class,svm.pred1,dnn=c("Actual","Predicted"))
svm.perf1 






