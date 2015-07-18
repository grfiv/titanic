# cforest regression for Titanic
library(party)
cforest.fit = cforest(as.factor(Survived) ~ .-PassengerId,
                      data=raw_data[-test.set,], controls=cforest_unbiased(ntree=500, mtry=4))
pred.cforest = predict(cforest.fit, newdata=raw_data[test.set,], OOB=TRUE, type = "response")
matrix = table(actual=raw_data$Survived[test.set],pred=as.numeric(pred.cforest)-1)

TN = matrix[1]; FP = matrix[3]
FN = matrix[2]; TP = matrix[4]

model_accuracy = (TN + TP) / (TN + FP + FN + TP)

R              = TP / (FN + TP)
P              = TP / (TP + FP)
Fvalue         = 2 * ( (P * R) / (P + R) )

ROCRpred = prediction(as.numeric(pred.cforest)-1, raw_data$Survived[test.set])
ROCRperf = performance(ROCRpred, "tpr", "fpr")
AUC = performance(ROCRpred,"auc")@y.values[[1]]

mse = mean(((as.numeric(pred.cforest)-1) - Y[test.set])^2)

if (exists("comparison")) {
    comparison$cforest = c(model_accuracy,Fvalue,AUC,mse)
} else {
  comparison = data.frame(cforest=c(model_accuracy,Fvalue,AUC,mse))
  rownames(comparison) = c("model_accuracy","Fvalue","AUC","MSE")
}