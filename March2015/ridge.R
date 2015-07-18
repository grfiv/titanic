# ridge for Titanic
library ( glmnet )
require(doMC)
registerDoMC(cores=4)
glmnetY = factor(Y, levels=c(0,1),labels=c("Perished","Survived"))
glmnetX = model.matrix(Survived~.,data=raw_data)[,-c(1,2)]
grid    = 10^seq(10, -5, length=250)

ridge.cv  = cv.glmnet(x=glmnetX[-test.set,], y=glmnetY[-test.set],
                      alpha=0, standardize=FALSE, family="binomial",
                      lambda=grid,
                      nfolds=10,parallel=TRUE)
#plot(ridge.cv)
best.lambda = ridge.cv$lambda.min

ridge.fit = glmnet(x=glmnetX[-test.set,], y=glmnetY[-test.set],
                   alpha=0, standardize=FALSE, family="binomial",
                   lambda=grid)

ridge.pred = predict (ridge.fit, newx=glmnetX[test.set,], s=best.lambda,
                      type="response")

matrix = table(actual=raw_data$Survived[test.set], pred=ridge.pred>0.561)

TN = matrix[1]; FP = matrix[3]
FN = matrix[2]; TP = matrix[4]

model_accuracy = (TN + TP) / (TN + FP + FN + TP)

R              = TP / (FN + TP)
P              = TP / (TP + FP)
Fvalue         = 2 * ( (P * R) / (P + R) )

MCC = ( TP*TN - FN*FP  ) / sqrt( (TN+FN)*(TN+FP)*(TP+FN)*(TP+FP) )

ROCRpred = prediction(ridge.pred, raw_data$Survived[test.set])
ROCRperf = performance(ROCRpred, "tpr", "fpr")
AUC = performance(ROCRpred,"auc")@y.values[[1]]

#plot_thresholds(ROCRperf)

mse = mean((ridge.pred - Y[test.set])^2)

if (exists("comparison")) {
  comparison$ridge = c(model_accuracy,Fvalue,AUC,mse,MCC)
} else {
  comparison = data.frame(ridge=c(model_accuracy,Fvalue,AUC,mse,MCC))
  rownames(comparison) = c("model_accuracy","Fvalue","AUC","MSE","MCC")
}