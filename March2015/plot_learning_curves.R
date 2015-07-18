# plot learning curves

plot_learning_curves = function(train.set.sizes,
                                train.errors,
                                test.errors,
                                optimal=0.12) {
  
  plot(train.set.sizes, test.errors, 
       col="red", lty=1, type="l",
       main="Learning Curves",
       xlab="Size of Training Set",
       ylab="MSE",
       ylim=c(0,max(max(test.errors),max(train.errors))+0.05),
       sub=paste("final training mse:",round(train.errors[length(train.errors)],digits=4),
                 "; test:",round(test.errors[length(test.errors)],digits=4)))
  points(train.set.sizes, train.errors, col="blue",lty=2,type="l")
  legend("topright",c("Test","Train"),col=c("red","blue"),lty=c(1,2))
  abline(h=optimal, lty=2)
}