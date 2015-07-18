# lasso for Titanic

set.seed(1009)
library(ROCR)
library ( glmnet )
require(doMC)
registerDoMC(cores=4)
glmnetY = factor(Y, levels=c(0,1),labels=c("Perished","Survived"))
glmnetX = model.matrix(Survived~.,data=raw_data)[,-c(1,2)]
grid    = 10^seq(10, -5, length=250)

lasso.cv  = cv.glmnet(x=glmnetX[-test.set,], y=glmnetY[-test.set],
                      alpha=1, standardize=FALSE, family="binomial",
                      lambda=grid,
                      nfolds=10,parallel=TRUE)
#plot(lasso.cv)
best.lambda = lasso.cv$lambda.1se

lasso.fit = glmnet(x=glmnetX[-test.set,], y=glmnetY[-test.set],
                   alpha=1, standardize=FALSE, family="binomial",
                   lambda=grid)

lasso.pred = predict (lasso.fit, newx=glmnetX[test.set,], s=best.lambda,
                      type="response")

matrix = table(actual=raw_data$Survived[test.set], pred=lasso.pred>0.5)

TN = matrix[1]; FP = matrix[3]
FN = matrix[2]; TP = matrix[4]

model_accuracy = (TN + TP) / (TN + FP + FN + TP)

R              = TP / (FN + TP)
P              = TP / (TP + FP)
Fvalue         = 2 * ( (P * R) / (P + R) )

ROCRpred = prediction(lasso.pred, raw_data$Survived[test.set])
ROCRperf = performance(ROCRpred, "tpr", "fpr")
AUC = performance(ROCRpred,"auc")@y.values[[1]]

mse = mean((lasso.pred - Y[test.set])^2)

if (exists("comparison")) {
    comparison$lasso1se = c(model_accuracy,Fvalue,AUC,mse)
} else {
  comparison = data.frame(lasso1se=c(model_accuracy,Fvalue,AUC,mse))
  rownames(comparison) = c("model_accuracy","Fvalue","AUC","MSE")
}