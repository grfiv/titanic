# rpart analysis for Titanic
library(rpart)
#rpart.fit  = rpart(formla, data=raw_data[-test.set,], method="class", model=TRUE)

rpart.fit = rpart(formla, 
              method="anova", 
              data=raw_data[-test.set,], 
              control=rpart.control(xval=10, cp=0.01))

# check if a different cp would be better
cpbest=rpart.fit$cptable[which.min(rpart.fit$cptable[,"xerror"]),"CP"]
#print(cpbest)


pred.rpart = predict(rpart.fit, newdata=raw_data[test.set,])
#matrix     = table(actual=raw_data$Survived[test.set],pred=pred.rpart[,2]>threshold)
matrix     = table(actual=raw_data$Survived[test.set],pred=pred.rpart>threshold)

TN = matrix[1]; FP = matrix[3]
FN = matrix[2]; TP = matrix[4]

model_accuracy = (TN + TP) / (TN + FP + FN + TP)

R              = TP / (FN + TP)
P              = TP / (TP + FP)
Fvalue         = 2 * ( (P * R) / (P + R) )

MCC = ( TP*TN - FN*FP  ) / sqrt( (TN+FN)*(TN+FP)*(TP+FN)*(TP+FP) )

#ROCRpred = prediction(pred.rpart[,2], raw_data$Survived[test.set])
ROCRpred = prediction(pred.rpart, raw_data$Survived[test.set])
ROCRperf = performance(ROCRpred, "tpr", "fpr")
AUC = performance(ROCRpred,"auc")@y.values[[1]]

mse = mean((pred.rpart - Y[test.set])^2)

if (exists("comparison")) {
  comparison$rpart = c(model_accuracy,Fvalue,AUC,mse,MCC)
} else {
  comparison = data.frame(rpart=c(model_accuracy,Fvalue,AUC,mse,MCC))
  rownames(comparison) = c("model_accuracy","Fvalue","AUC","MSE","MCC")
}
