# pcr for Titanic

library(ROCR)
library ( pls )

# train.errors = numeric(0)
# test.errors  = numeric(0)
# steps = c(seq(100,length(train.set),150),length(train.set))
# steps = c(100,length(train.set))
# for (i in steps) {
  train.subset = train.set
  pcr.fit = pcr(Survived~.-PassengerId, data=raw_data[train.subset,],
                  scale=FALSE, validation="CV")
  
  best.comp = which.min(RMSEP(pcr.fit)$val[2,,])
  
  pcr.pred = predict (pcr.fit, newdata=raw_data[test.set,], ncomp=best.comp, type="response")
  pcr.pred = data.frame(pcr.pred)[,1]

  
  matrix = table(actual=raw_data$Survived[test.set], pcr.pred>0.5533)
  
  TN = matrix[1]; FP = matrix[3]
  FN = matrix[2]; TP = matrix[4]
  
  model_accuracy = (TN + TP) / (TN + FP + FN + TP)
  
  R              = TP / (FN + TP)
  P              = TP / (TP + FP)
  Fvalue         = 2 * ( (P * R) / (P + R) )
  
  mse = mean((pcr.pred - Y[test.set])^2)
  #test.errors  = c(test.errors, mse)
  
  # ------------ training errors -------------------
#   pcr.predT = predict (pcr.fit, newdata=raw_data[train.subset,], ncomp=best.comp, type="response")
#   pcr.predT = data.frame(pcr.predT)[,1]
#   mse.train = mean((pcr.predT - Y[train.subset])^2)
#   train.errors = c(train.errors, mse.train)

#}
# plot(steps, test.errors, 
#      col="red", lty=1, type="l",
#      main="Learning Curves",
#      xlab="Size of Training Set",
#      ylab="MSE",
#      ylim=c(0,max(max(test.errors),max(train.errors))+0.05))
# points(steps, train.errors, col="blue",lty=2,type="l")
# legend("topright",c("Test","Train"),col=c("red","blue"),lty=c(1,2))

ROCRpred = prediction(pcr.pred, raw_data$Survived[test.set])
ROCRperf = performance(ROCRpred, "tpr", "fpr")
AUC = performance(ROCRpred,"auc")@y.values[[1]]

#plot_thresholds(pcr.pred)

if (exists("comparison")) {
  comparison$pcr = c(model_accuracy,Fvalue,AUC,mse)
} else {
  comparison = data.frame(pcr=c(model_accuracy,Fvalue,AUC,mse))
  rownames(comparison) = c("model_accuracy","Fvalue","AUC","MSE")
}