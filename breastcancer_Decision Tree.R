library("rpart")
#	parms=list(split="information"): 决策树的划分准则设置为基于信息增益（Information Gain）
dtree <- rpart(Class ~ .,data= train, method="class",parms=list(split="information"))
#另一种常用准则是基尼指数（split="gini"），它是默认值。

print(dtree)
summary(dtree)

plotcp(dtree)
#dtree$cptable 是决策树模型的重要组成部分，它包含了关于模型复杂度和修剪（pruning）过程的详细信息。
#通过 cptable，可以分析决策树在不同复杂度参数（complexity parameter, CP）下的表现，从而选择最佳的修剪点
dtree$cptable

dtree.pruned <- prune(dtree,cp=0.01)

library(rattle)
fancyRpartPlot(dtree.pruned, sub="Classification Tree")


#使用测试集去进行预测
dtree.pred <- predict(dtree.pruned,test,type="class")
perf <- table(test$Class,dtree.pred,dnn=c("Actual","Predicted"))
perf



# 提取混淆矩阵confusion matrix中的值
TN <- perf[1, 1]  # True Negatives
FP <- perf[1, 2]  # False Positives
FN <- perf[2, 1]  # False Negatives
TP <- perf[2, 2]  # True Positives

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

