# forward subset selection
library(leaps)
library(boot)
# forward subset selection
subsets = regsubsets(Survived ~ ., data=YX[-test.set,], 
                     method="forward", nbest=1, nvmax=ncol(X)) 

# logical matrix of variables chosen for each model
models  = summary(subsets,matrix.logical=TRUE)$which[,-1] # drop the intercept

# a vector of the best adjusted CV errors
glm.error = rep(NA, ncol(X))

# loop through each model
# find the best adjusted CV error for each one on the training data
for (i in 1:ncol(X)) {
    glm.fit      = glm(Survived ~ ., data=YX[,c(TRUE, models[i,])],family=binomial(link = "logit")) 
    glm.error[i] = cv.glm(YX[,c(TRUE, models[i,])], glm.fit, K=10)$delta[2]
}

# out of 21 models selected, this one had the lowest adjusted CV error
best.model = which.min(glm.error)

# run glm with the variables specified by the best model
glm.best.fit = glm(Survived ~ ., data=YX[,c(TRUE, models[best.model,] )] )

glm.pred = predict(glm.best.fit, newdata=X[test.set,])

mse = mean((glm.pred - Y[test.set])^2)

matrix = table(actual=raw_data$Survived[test.set],pred=glm.pred>threshold)

TN = matrix[1]; FP = matrix[3]
FN = matrix[2]; TP = matrix[4]

model_accuracy = (TN + TP) / (TN + FP + FN + TP)

R              = TP / (FN + TP)
P              = TP / (TP + FP)
Fvalue         = 2 * ( (P * R) / (P + R) )

ROCRpred = prediction(glm.pred, raw_data$Survived[test.set])
ROCRperf = performance(ROCRpred, "tpr", "fpr")
AUC = performance(ROCRpred,"auc")@y.values[[1]]

if (exists("comparison")) {
    comparison$forwardss = c(model_accuracy,Fvalue,AUC,mse)
} else {
    print("comparison data.frame does not exist")
}