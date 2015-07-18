# boosting for Titanic

# using the parameters found in boostParameters.R
best.interaction.depth = 1
best.threshold         = 0.54
best.n.trees           = 5729

# values for the best-mse parameters
# BEST ACCURACY PERFORMED MUCH BETTER THAN BEST MSE
# ==================================
#best.interaction.depth = 3
#best.threshold         = 0.50
#best.n.trees           = 2940

library ( gbm )

boost.fit = gbm(Survived~.-PassengerId, data=raw_data[-test.set,],
                     distribution="bernoulli", 
                     n.trees=best.n.trees, 
                     interaction.depth=best.interaction.depth,
                     shrinkage=0.001)

boost.pred = predict (boost.fit, newdata=raw_data[test.set,],n.trees=best.n.trees,
                      type="response")

matrix = table(actual=raw_data$Survived[test.set], pred=boost.pred>best.threshold)

TN = matrix[1]; FP = matrix[3]
FN = matrix[2]; TP = matrix[4]

model_accuracy = (TN + TP) / (TN + FP + FN + TP)

R              = TP / (FN + TP)
P              = TP / (TP + FP)
Fvalue         = 2 * ( (P * R) / (P + R) )

MCC = ( TP*TN - FN*FP  ) / sqrt( (TN+FN)*(TN+FP)*(TP+FN)*(TP+FP) )

ROCRpred = prediction(boost.pred, raw_data$Survived[test.set])
ROCRperf = performance(ROCRpred, "tpr", "fpr")
AUC = performance(ROCRpred,"auc")@y.values[[1]]

mse = mean((boost.pred - Y[test.set])^2)

if (exists("comparison")) {
  comparison$boostTest = c(model_accuracy,Fvalue,AUC,mse,MCC)
} else {
  comparison = data.frame(boostTest=c(model_accuracy,Fvalue,AUC,mse,MCC))
  rownames(comparison) = c("model_accuracy","Fvalue","AUC","MSE","MCC")
}