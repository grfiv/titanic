# print the section of the ROC curve
# between the thresholds of 0.40 and 0.60

plot_thresholds = function(pred,threshold) {
  ROCRpred = prediction(pred, raw_data$Survived[test.set])
  AUC      = performance(ROCRpred,"auc")@y.values[[1]]
  
  # True/False Positive Rates
  ROCRperf = performance(ROCRpred, "tpr", "fpr")
  cutoffs <- data.frame(cut=ROCRperf@alpha.values[[1]], 
                        fpr=ROCRperf@x.values[[1]], 
                        tpr=ROCRperf@y.values[[1]])
  cutseg=cutoffs[cutoffs$cut>=0.4 & cutoffs$cut<=0.6,]
  plot(cutseg[,2:3],
       main="True/False Positive threshold values between 0.6 & 0.4",
       sub=paste("0.5 in black, threshold in red",threshold),
       ylim=c(min(cutseg[,3]),max(cutseg[,3])+0.01))
  for (i in seq(2,dim(cutseg)[1],1)){
    text(cutseg[i,2],cutseg[i,3], pos=3, srt=45,
         toString(round(cutseg[i,1],digits=4)),
         cex=0.70)
  }
  abline(h=cutoffs[which(cutoffs$cut<=0.5)[1],3],lty=2)
  abline(h=cutoffs[which(cutoffs$cut<=threshold)[1],3],lty=2,col="red")
  
    plot(cutoffs[,2:3],pch=20,main="True/False Positive")
      abline(h=cutoffs[which(cutoffs$cut<=0.6)[1],3])
      text(0,cutoffs[which(cutoffs$cut<=0.6)[1],3],pos=1,"0.60")
      abline(h=cutoffs[which(cutoffs$cut<=0.5)[1],3],lty=2)
      text(0,cutoffs[which(cutoffs$cut<=0.5)[1],3],"0.50")
      abline(h=cutoffs[which(cutoffs$cut<=0.4)[1],3])
      text(0,cutoffs[which(cutoffs$cut<=0.4)[1],3],pos=3,"0.40")
      abline(h=cutoffs[which(cutoffs$cut<=threshold)[1],3],lty=2,col="red")
      
      text(0.4,0.4,paste("AUC",round(AUC,digits=4)))
    
  # True/False Negative Rates
  
    neg.perf = performance(ROCRpred, "tnr", "fnr")
    cut.neg = data.frame(cut=neg.perf@alpha.values[[1]],
                         fnr=neg.perf@x.values[[1]],
                         tnr=neg.perf@y.values[[1]])
  
  
  
  cutseg.neg=cut.neg[cut.neg$cut>=0.4 & cut.neg$cut<=0.6,]
  plot(cutseg.neg[,2:3],
       main="True/False Negative threshold values between 0.4 & 0.6",
       sub=paste("0.5 in black, threshold in red",threshold),
       ylim=c(min(cutseg.neg[,3]),max(cutseg.neg[,3])+0.01))
  for (i in seq(2,dim(cutseg.neg)[1],1)){
    text(cutseg.neg[i,2],cutseg.neg[i,3], pos=3, srt=45,
         toString(round(cutseg.neg[i,1],digits=4)),
         cex=0.70)
  }
  abline(h=cut.neg[which(cut.neg$cut<=0.5)[1],3],lty=2)
  abline(h=cut.neg[which(cut.neg$cut<=threshold)[1],3],lty=2,col="red")
  
  
  
    plot(cut.neg[,2:3],pch=20,
         main="True/False Negative")
      abline(h=cut.neg[which(cut.neg$cut<=0.6)[1],3])
      text(0,cut.neg[which(cut.neg$cut<=0.6)[1],3],pos=3,"0.60")
      abline(h=cut.neg[which(cut.neg$cut<=0.5)[1],3],lty=2)
      text(0,cut.neg[which(cut.neg$cut<=0.5)[1],3],"0.50")
      abline(h=cut.neg[which(cut.neg$cut<=0.4)[1],3])
      text(0,cut.neg[which(cut.neg$cut<=0.4)[1],3],pos=1,"0.40")
      abline(h=cut.neg[which(cut.neg$cut<=threshold)[1],3],lty=2,col="red")
  
      text(0.4,0.4,paste("AUC",round(AUC,digits=4)))
  
}