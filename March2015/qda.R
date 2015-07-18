# qda for titanic
library(MASS)
qda.fit  = qda(formla, data=raw_data[-test.set,])
pred.qda = predict(qda.fit, newdata=raw_data[test.set,])
matrix   = table(actual=raw_data$Survived[test.set],pred=pred.qda$posterior[,2]>threshold)

TN = matrix[1]; FP = matrix[3]
FN = matrix[2]; TP = matrix[4]

model_accuracy = (TN + TP) / (TN + FP + FN + TP)

R              = TP / (FN + TP)
P              = TP / (TP + FP)
Fvalue         = 2 * ( (P * R) / (P + R) )

ROCRpred = prediction(pred.qda$posterior[,2], raw_data$Survived[test.set])
ROCRperf = performance(ROCRpred, "tpr", "fpr")
AUC = performance(ROCRpred,"auc")@y.values[[1]]

mse = mean((as.numeric(pred.qda$posterior[,2]>threshold) - Y[test.set])^2)

if (exists("comparison")) {
    comparison$qda = c(model_accuracy,Fvalue,AUC,mse)
} else {
    print("comparison data.frame does not exist")
}
