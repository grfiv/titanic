# gam for titanic
library(gam)

train.errors = numeric(0)
test.errors  = numeric(0)
steps = c(seq(67,length(train.set),25),length(train.set))
for (i in steps) {
  train.subset = train.set[1:i]

  gam.fit = gam(Survived~.-PassengerId + 
                  lo(FamilySize, span=0.33, degree=1) +
                   lo(Age,  span=0.33, degree=1) + 
                   lo(Fare, span=0.33, degree=1) ,
                   #lo(Fare.pp, span=0.33, degree=1) +
                   #lo(AgeClass, span=0.33, degree=1) +
                   #lo(Fare.ppClass, span=0.33, degree=1), 
                data=raw_data[train.subset,],
                family=binomial(link="logit"))
  
  gam.pred = predict(gam.fit, newdata=raw_data[test.set,], type="response")
  
  matrix = table(actual=raw_data$Survived[test.set], gam.pred > 0.53)
  
  TN = matrix[1]; FP = matrix[3]
  FN = matrix[2]; TP = matrix[4]
  
  model_accuracy = (TN + TP) / (TN + FP + FN + TP)
  
  R              = TP / (FN + TP)
  P              = TP / (TP + FP)
  Fvalue         = 2 * ( (P * R) / (P + R) )
  
  MCC = ( TP*TN - FN*FP  ) / sqrt( (TN+FN)*(TN+FP)*(TP+FN)*(TP+FP) )
  
  mse = mean((gam.pred - Y[test.set])^2)
  test.errors  = c(test.errors, mse)
  
  # ------------ training errors -------------------
  gam.predT = predict (gam.fit, newdata=raw_data[train.subset,], type="response")
  mse.train = mean((gam.predT - Y[train.subset])^2)
  train.errors = c(train.errors, mse.train)
}
plot(steps, test.errors, 
     col="red", lty=1, type="l",
     main="Learning Curves",
     xlab="Size of Training Set",
     ylab="MSE",
     ylim=c(0,max(max(test.errors),max(train.errors))+0.05))
points(steps, train.errors, col="blue",lty=2,type="l")
legend("topright",c("Test","Train"),col=c("red","blue"),lty=c(1,2))

ROCRpred = prediction(gam.pred, raw_data$Survived[test.set])
ROCRperf = performance(ROCRpred, "tpr", "fpr")
AUC = performance(ROCRpred,"auc")@y.values[[1]]

plot_thresholds(gam.pred, full_chart=TRUE)

if (exists("comparison")) {
  comparison$gam = c(model_accuracy,Fvalue,AUC,mse,MCC)
} else {
  comparison = data.frame(gam=c(model_accuracy,Fvalue,AUC,mse,MCC))
  rownames(comparison) = c("model_accuracy","Fvalue","AUC","MSE","MCC")
}