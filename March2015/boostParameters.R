# find optimal threshold/interaction.depth/lambda combination
# for boosting 

# per Ridgeway vignette, lambda=shrinkage=0.001 always has the best predictive power
# but requires proportionally (x10) more iterations than 0.01

formla = formula(Survived~.-PassengerId)

threshold = 0.5000

# Step 1: optimal interaction depth
# for formla = formula(Survived~.-PassengerId)
#        1      4      2      3      5
# acc 0.8649 0.8468 0.8423 0.8423 0.8378
# mse 0.1304 0.1200 0.1230 0.1210 0.1200
# F   0.8370 0.8172 0.8168 0.8148 0.8085
# AUC 0.8929 0.8883 0.8905 0.8887 0.8888
# [1] "best.interaction.depth: 1; best.n.trees: 55324"
# ================ step 1 ============================================

boost.test = data.frame()

  
    for (depth in seq(from=1,to=5,by=1)) {
      
      
      boost.fit = gbm(formla, 
                      data=raw_data[train.set,],
                      distribution="bernoulli", n.trees=60000, 
                      interaction.depth=depth,
                      shrinkage=0.0001)
      
      boost.pred = predict (boost.fit, newdata=raw_data[test.set,],n.trees=60000,
                            type="response")
      
      matrix = table(actual=raw_data$Survived[test.set], pred=boost.pred>=threshold)
      
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
      
      boost.test[1,c(paste(depth,sep=""))] = round(model_accuracy,digits=4)
      boost.test[2,c(paste(depth,sep=""))] = round(mse,digits=4)
      boost.test[3,c(paste(depth,sep=""))] = round(Fvalue,digits=4)
      boost.test[4,c(paste(depth,sep=""))] = round(AUC,digits=4)
      
    
    }
  
rownames(boost.test)=c("acc","mse","F","AUC")
print("columns are depth")
print(boost.test[,order(boost.test[1,],decreasing=TRUE)])


# ================ step 2 ============================================

# Step 2: fit a model with the optimal values from Step 1
#         using the entire sample
#         note: cv.folds=nn gives "Error in object$var.levels[[i]] : subscript out of bounds"
#               stackoverflow:  error occurs when you have variable levels that 
#               exist in the holdout fold which don't exist in the training set
#               could try different seeds to see if that prevents the problem

best.interaction.depth = 1


best.boost.fit = gbm(formla, 
                     data=raw_data,
                     distribution="bernoulli", n.trees=75000, 
                     interaction.depth=best.interaction.depth,
                     shrinkage=0.0001)

# ================ step 3 ============================================

# Step 3: find the optimal number of trees to build

best.boost.perf = gbm.perf(best.boost.fit,
                           method="OOB",   # would prefer "cv", but see above
                           plot.it=TRUE, oobag.curve=TRUE,overlay=TRUE)

best.n.trees = best.boost.perf
best.n.trees = best.n.trees # given the OOB warning; times 5%??

print(paste("best.interaction.depth: ",best.interaction.depth,
            "; best.n.trees: ",best.n.trees,
            sep=""))
# ================ step 4 ============================================

# Step 4: fit a model with all the best values
#         look at the variable importance

best.boost.fit = gbm(formla, 
                     data=raw_data,
                     distribution="bernoulli", 
                     n.trees=best.n.trees, 
                     interaction.depth=best.interaction.depth,
                     shrinkage=0.0001)

summary(best.boost.fit)
