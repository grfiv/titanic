
library(e1071)
library(formula.tools)
formla.init = formula(factor(Survived)~.-PassengerId
                 + I(as.numeric(Deck)*FamilySize)
                 + poly(I(as.numeric(Deck)*FamilySize),4)
                 + poly(I(as.numeric(Deck)*as.numeric(Side)),3)
                 + poly(I(as.numeric(Embarked)*FamilySize),2)
                 + poly(I(as.numeric(Embarked)*as.numeric(Side)),2)
                 + poly(I(as.numeric(Embarked)*as.numeric(Deck)),2)
                 + poly(I(as.numeric(Embarked)*as.numeric(Fare2)),4)
                 + I(as.numeric(Embarked)*FamilySize)
                 + I(Farepp*as.numeric(Pclass)) 
                 
                 + I(as.numeric(Sex)*Parch)
                 + I(as.numeric(Sex)*Fare)
                 + I(as.numeric(Sex)*as.numeric(Pclass))
                 + I(as.numeric(Sex)*Age)
                 + I(as.numeric(Sex)*SibSp)
                 + I(as.numeric(Sex)*as.numeric(Embarked))
                 + I(as.numeric(Sex)*as.numeric(title))
                 + I(as.numeric(Sex)*FamilySize)
                 + I(as.numeric(Sex)*Farepp)
                 + I(as.numeric(Sex)*as.numeric(Deck))
                 + I(as.numeric(Sex)*as.numeric(Side))
                
                 
                 + poly(I(as.numeric(Sex)*Parch),2)
                 + poly(I(as.numeric(Sex)*Fare),2)
                 + poly(I(as.numeric(Sex)*as.numeric(Pclass)),2)
                 + poly(I(as.numeric(Sex)*Age),2)
                 + poly(I(as.numeric(Sex)*SibSp),2)
                 + poly(I(as.numeric(Sex)*as.numeric(Embarked)),2)
                 + poly(I(as.numeric(Sex)*as.numeric(title)),2)
                 + poly(I(as.numeric(Sex)*FamilySize),2)
                 + poly(I(as.numeric(Sex)*Farepp),2)
                 + poly(I(as.numeric(Sex)*as.numeric(Deck)),2)
                 + poly(I(as.numeric(Sex)*as.numeric(Side)),2)
                 
                 
                 + poly(I(as.numeric(Sex)*Fare),2)
                 + poly(I(Age*as.numeric(Deck)),4)
                 + poly(SibSp,2)
                 + poly(Parch,2)
                 + poly(Fare,2)
                 + poly(Farepp,2)
                 + poly(FamilySize,2)
                 + poly(Age,3)   )
form.vars = get.vars(formla.init,data=raw_data)
old.vars  = character()
accuracy  = 0 
meansqe   = 1

# go through each feature in the formula and test it
for (i in 2:length(form.vars)) {
  new.vars = paste(old.vars,"+",form.vars[[i]])
  formla   = formula(paste("factor(Survived)~-PassengerId",new.vars))
  svm.fit  = svm(formla, data=raw_data[train.set,],
                  kernel="radial",
                  scale=FALSE, probability=TRUE,decision.values=TRUE,
                  gamma=0.0078125,cost=1)
  
  svm.pred = predict(svm.fit, newdata=raw_data[test.set,], 
                     decision.values=TRUE, probability=TRUE)
  
  matrix = table(actual    = raw_data$Survived[test.set], 
                 predicted = attributes(svm.pred)$probabilities[,1]>=0.5)
  TN = matrix[1]; FP = matrix[3]
  FN = matrix[2]; TP = matrix[4]
  model_accuracy = (TN + TP) / (TN + FP + FN + TP)
  
  mse = mean((attributes(svm.pred)$probabilities[,1] - Y[test.set])^2)
  
  # if accuracy improves or MSE goes down, keep the feature
  if ((model_accuracy > accuracy) || (mse < meansqe)) {
    old.vars = new.vars
    accuracy = model_accuracy
    meansqe  = mse
    formla   = formula(paste("factor(Survived)~-PassengerId",new.vars))
  } 
}
print(formla)
print(paste("the formula starts with ",length(form.vars),
            " features and ends with ",length(get.vars(formla)),
            sep=""))


# # 10-fold CV to find the best gamma and cost parameters
# svm.tune <- tune(svm, formla, data = raw_data[train.set,], 
#             ranges = list(gamma = 2^(-12:-4), cost = 2^(0:5)),
#             best.model=FALSE,performances=FALSE,
#             tunecontrol = tune.control(sampling="cross",cross=10))
# best.gamma = svm.tune$best.parameters$gamma
# best.cost  = svm.tune$best.parameters$cost
# print(paste("gamma",best.gamma))
# print(paste("cost ",best.cost))
      
best.gamma = 0.0078125
best.cost  = 1

svm.fit = svm(formla, data=raw_data[train.set,],
              kernel="radial",
              scale=FALSE, probability=TRUE,decision.values=TRUE,
              gamma=best.gamma,
              cost=best.cost)

svm.pred = predict(svm.fit, newdata=raw_data[test.set,], 
                   decision.values=TRUE, probability=TRUE)


matrix = table(actual    = raw_data$Survived[test.set], 
               predicted = attributes(svm.pred)$probabilities[,1]>=0.5)
print(matrix)
TN = matrix[1]; FP = matrix[3]
FN = matrix[2]; TP = matrix[4]

model_accuracy = (TN + TP) / (TN + FP + FN + TP)
print(paste("acc",round(model_accuracy,digits=4)))

mse = mean((attributes(svm.pred)$probabilities[,1] - Y[test.set])^2)
print(paste("mse",round(mse,digits=4)))
