# logistic regression for Titanic
logit.fit=glm(formla, data=raw_data[-test.set,], family="binomial")
pred.logit = predict(logit.fit, newdata=raw_data[test.set,], type="response")
matrix = table(actual=raw_data$Survived[test.set],pred=pred.logit>threshold)

TN = matrix[1]; FP = matrix[3]
FN = matrix[2]; TP = matrix[4]

model_accuracy = (TN + TP) / (TN + FP + FN + TP)

R              = TP / (FN + TP)
P              = TP / (TP + FP)
Fvalue         = 2 * ( (P * R) / (P + R) )

ROCRpred = prediction(pred.logit, raw_data$Survived[test.set])
ROCRperf = performance(ROCRpred, "tpr", "fpr")
AUC = performance(ROCRpred,"auc")@y.values[[1]]

mse = mean((pred.logit - Y[test.set])^2)

if (exists("comparison")) {
    comparison$logit = c(model_accuracy,Fvalue,AUC,mse)
} else {
    print("comparison data.frame does not exist")
}