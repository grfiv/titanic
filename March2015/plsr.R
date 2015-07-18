# plsr for Titanic
library ( pls )

train.errors = numeric(0)
test.errors  = numeric(0)
# steps = c(seq(50,length(train.set),25),length(train.set))
# for (i in steps) {
#   train.subset = train.set[1:i]
  train.subset = train.set
  plsr.fit = plsr(Survived~.-PassengerId, data=raw_data[train.subset,],
                scale=FALSE, validation="CV")
  
  best.comp = which.min(RMSEP(plsr.fit)$val[2,,])
  
  plsr.pred = predict (plsr.fit, newdata=raw_data[test.set,], ncomp=best.comp, type="response")
  plsr.pred = data.frame(plsr.pred)[,1]
  
  matrix = table(actual=raw_data$Survived[test.set], plsr.pred>0.5005)
  
  TN = matrix[1]; FP = matrix[3]
  FN = matrix[2]; TP = matrix[4]
  
  model_accuracy = (TN + TP) / (TN + FP + FN + TP)
  
  R              = TP / (FN + TP)
  P              = TP / (TP + FP)
  Fvalue         = 2 * ( (P * R) / (P + R) )
  
  MCC = ( TP*TN - FN*FP  ) / sqrt( (TN+FN)*(TN+FP)*(TP+FN)*(TP+FP) )
  
  mse = mean((plsr.pred - Y[test.set])^2)
  test.errors  = c(test.errors, mse)
  
  # ------------ training errors -------------------
  plsr.predT = predict (plsr.fit, newdata=raw_data[train.subset,], ncomp=best.comp, type="response")
  plsr.predT = data.frame(plsr.predT)[,1]
  mse.train = mean((plsr.predT - Y[train.subset])^2)
  train.errors = c(train.errors, mse.train)
# }
# plot(steps, test.errors, 
#      col="red", lty=1, type="l",
#      main="Learning Curves",
#      xlab="Size of Training Set",
#      ylab="MSE",
#      ylim=c(0,max(max(test.errors),max(train.errors))+0.05))
# points(steps, train.errors, col="blue",lty=2,type="l")
# legend("topright",c("Test","Train"),col=c("red","blue"),lty=c(1,2))

ROCRpred = prediction(plsr.pred, raw_data$Survived[test.set])
ROCRperf = performance(ROCRpred, "tpr", "fpr")
AUC = performance(ROCRpred,"auc")@y.values[[1]]

# plot_thresholds(plsr.pred)

if (exists("comparison")) {
    comparison$plsr = c(model_accuracy,Fvalue,AUC,mse,MCC)
} else {
  comparison = data.frame(plsr=c(model_accuracy,Fvalue,AUC,mse,MCC))
  rownames(comparison) = c("model_accuracy","Fvalue","AUC","MSE","MCC")
}