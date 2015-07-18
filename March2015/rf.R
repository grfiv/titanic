# random forest for Titanic
library(randomForest)

# this function searches for the best mtry; 3 or 4 seem to be the answer
# ncol(X)/3 = 4
#bestmtry <- tuneRF(x=X,y=factor(Y), mtryStart=2, ntreeTry=100, 
#                   stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, 
#                   dobest=FALSE)

rf.fit=randomForest(x=X[-test.set,],    y=factor(Y[-test.set]),  
                    xtest=X[test.set,], ytest=factor(Y[test.set]), 
                    ntree=500, mtry=ncol(X)/3,
                    importance=TRUE,proximity=TRUE,
                    keep.forest=TRUE)

# to see the tree
#getTree(rf.fit,labelVar=TRUE)

matrix = table(actual=raw_data$Survived[test.set],pred=rf.fit$test$predicted)

TN = matrix[1]; FP = matrix[3]
FN = matrix[2]; TP = matrix[4]

model_accuracy = (TN + TP) / (TN + FP + FN + TP)

R              = TP / (FN + TP)
P              = TP / (TP + FP)
Fvalue         = 2 * ( (P * R) / (P + R) )

ROCRpred = prediction(as.numeric(rf.fit$test$predicted)-1, raw_data$Survived[test.set])
ROCRperf = performance(ROCRpred, "tpr", "fpr")
AUC = performance(ROCRpred,"auc")@y.values[[1]]

mse = mean((as.numeric(rf.fit$test$predicted)-1 - Y[test.set])^2)

if (exists("comparison")) {
    comparison$rf = c(model_accuracy,Fvalue,AUC,mse)
} else {
    print("comparison data.frame does not exist")
}