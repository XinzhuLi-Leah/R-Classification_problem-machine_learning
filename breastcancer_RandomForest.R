library(randomForest)

set.seed(1234)
fit.forest <- randomForest(Class ~ ., data=train,importance= TRUE)
fit.forest

var_importance <- importance(fit.forest, type = 2)
var_importance 
varImpPlot(fit.forest, type = 2, main = "Variable Importance (Gini Index)")


#使用测试集去进行预测
forest.pred <- predict(fit.forest,test)
perf <- table(test$Class,forest.pred,dnn=c("Actual","Predicted"))
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




library("DALEX")

Amy <- data.frame(
  Cl.thickness = 9,
  Cell.size = 3,
  Cell.shape = 1,
  Marg.adhesion =7,
  Epith.c.size = 1,
  Bare.nuclei = 3,
  Bl.cromatin = 3,
  Normal.nucleoli = 6,
  Mitoses = 3
)

predict(fit.forest,Amy,type="prob")

#生成解释器对象
explainer_fr_malignant <- explain(fit.forest,data=train,y=train$Class == "malignant",
                                  predcit_function = function(m,x) predict(m,x,type = "prob")[,2])
rf_pparts1  <- predict_parts(explainer_fr_malignant,new_observation = Amy,type="break_down")
plot(rf_pparts1 )


rf_pparts2  <- predict_parts(explainer_fr_malignant,new_observation = Amy,type="shap")
plot(rf_pparts2 )
