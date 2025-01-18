install.packages("mlbench")
install.packages("rpart")
install.packages("rpart.plot")

library("mlbench")
library("rpart")
library("rpart.plot")

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

class(train)
str(train)
str(train$Class)
#根据这个输出的顺序 我们就可以知道良性和恶性哪一个是正类哪一个是负类。所以恶性在后面。它是1 也就是所谓的正类
levels(train$Class)

train$Cl.thickness <- as.numeric(train$Cl.thickness)
train$Cell.size <- as.numeric(train$Cell.size)
train$Cell.shape <- as.numeric(train$Cell.shape)
train$Marg.adhesion <- as.numeric(train$Marg.adhesion)
train$Epith.c.size <- as.numeric(train$Epith.c.size)
train$Bare.nuclei <- as.numeric(train$Bare.nuclei)
train$Bl.cromatin <- as.numeric(train$Bl.cromatin)
train$Normal.nucleoli <- as.numeric(train$Normal.nucleoli)
train$Mitoses <- as.numeric(train$Mitoses)

# 将test数据集中的变量转换为数值型
test$Cl.thickness <- as.numeric(test$Cl.thickness)
test$Cell.size <- as.numeric(test$Cell.size)
test$Cell.shape <- as.numeric(test$Cell.shape)
test$Marg.adhesion <- as.numeric(test$Marg.adhesion)
test$Epith.c.size <- as.numeric(test$Epith.c.size)
test$Bare.nuclei <- as.numeric(test$Bare.nuclei)
test$Bl.cromatin <- as.numeric(test$Bl.cromatin)
test$Normal.nucleoli <- as.numeric(test$Normal.nucleoli)
test$Mitoses <- as.numeric(test$Mitoses)

# 检查转换后的数据
str(test)
#逻辑回归
fit <- glm(Class ~ ., data = train, family = binomial())
summary(fit)

#对测试集进行分类
#type = "response" 告诉 R 返回的是每个样本属于目标类别的概率值，而不是类别标签。
#逻辑回归模型是用来预测一个二分类问题的，它的输出是属于“正类”的概率---- 在这里也就是恶性的概率

prob <- predict(fit,test,type="response")
str(prob)
prediction <- factor(prob>0.5, levels=c(FALSE,TRUE), labels = c("benign","malignant"))

#生成混淆矩阵 confusion matrix
perf <- table(test$Class, prediction, dnn=c("Actual","Predicted"))
perf


# 提取混淆矩阵中的值
TN <- perf[1, 1]  # True Negatives
TP <- perf[2, 2]  # True Positives

FP <- perf[1, 2]  # False Positives
FN <- perf[2, 1]  # False Negatives


# 计算准确率 (Accuracy)
accuracy <- (TP + TN) / sum(perf)

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
