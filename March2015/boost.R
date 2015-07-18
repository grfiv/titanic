# boosting for Titanic
library ( gbm )

boost.fit = gbm(formla, data=raw_data[-test.set,],
                distribution="gaussian" , n.trees =5000 , interaction.depth=1)

boost.pred = predict (boost.fit, newdata=raw_data[test.set,],n.trees =5000)

matrix = table(actual=raw_data$Survived[test.set],pred=boost.pred>0.54)

TN = matrix[1]; FP = matrix[3]
FN = matrix[2]; TP = matrix[4]

model_accuracy = (TN + TP) / (TN + FP + FN + TP)

R              = TP / (FN + TP)
P              = TP / (TP + FP)
Fvalue         = 2 * ( (P * R) / (P + R) )

ROCRpred = prediction(boost.pred, raw_data$Survived[test.set])
ROCRperf = performance(ROCRpred, "tpr", "fpr")
AUC = performance(ROCRpred,"auc")@y.values[[1]]

mse = mean((boost.pred - Y[test.set])^2)

if (exists("comparison")) {
    comparison$boost = c(model_accuracy,Fvalue,AUC,mse)
} else {
  comparison = data.frame(boost=c(model_accuracy,Fvalue,AUC,mse))
  rownames(comparison) = c("model_accuracy","Fvalue","AUC","MSE")
}