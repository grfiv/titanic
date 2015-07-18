# clean up the Titanic dataset
clean_the_dataset = function(input) {
    
    output   = input
    
    # dummy variables (remember to leave one out)
    output$Sex     = ifelse(output$Sex=="male",0,1)
    output$Pclass1 = ifelse(output$Pclass==1,1,0)
    output$Pclass2 = ifelse(output$Pclass==2,1,0)
   #output$Pclass3 = ifelse(output$Pclass==3,1,0)
    
    # Create a column with the titles of each passenger

    for (i in 1:length(input$Name)){
        matches = regexpr(" (\\w+)\\.", input$Name[i], perl=TRUE, ignore.case=TRUE);
        result  = attr(matches, "capture.start")[,1]
        attr(result, "match.length") = attr(matches, "capture.length")[,1]
        output$title[i] = regmatches(input$Name[i], result)
    }
    
    # Replace missing ages with the average for the title

    for (i in 1:length(output$Age)){
        if (is.na(output$Age[i])) {
            output$Age[i] = mean(output$Age[output$title==output$title[1]],na.rm=TRUE)
        }
    }
   
    # replace NAs in Fare with column mean for the Pclass
   for (Fi in seq(length(output$Fare))) {
       if (is.na(output$Fare[Fi])) {
           output$Fare[Fi] = mean(output[output$Pclass==output$Pclass[Fi],]$Fare,na.rm=TRUE)
       }
   }
   
    # Segment the fares
    
    output$Fare2 <- '30+'
    output$Fare2[output$Fare < 30 & output$Fare >= 20] <- '20-30'
    output$Fare2[output$Fare < 20 & output$Fare >= 10] <- '10-20'
    output$Fare2[output$Fare < 10] <- '<10'
    #output$Fare2 = factor(output$Fare2)
   
    output$Fare20_30 = ifelse(output$Fare2=='20-30',1,0)
    output$Fare10_20 = ifelse(output$Fare2=='10-20',1,0)
    output$FareLT10  = ifelse(output$Fare2=='<10',1,0)
    
    # replace NA Embarked with the mode
    if (sum(output$Embarked=="") > 0) {
        output[output$Embarked=="",]$Embarked = "S"
    }
    
    output$EmbarkedS = ifelse(output$Embarked=="S",1,0)
    output$EmbarkedC = ifelse(output$Embarked=="C",1,0)
   #output$EmbarkedQ = ifelse(output$Embarked=="Q",1,0)
        
    # drop the columns we won't use in the analysis
    # we need PassengerId to create the output datasets
    output = subset(output, select=-c(Name, Pclass, Fare2, Ticket, Cabin, Embarked, title))
   
   if ("Survived" %in% names(output)) { # training
        X = subset(output, select=-c(PassengerId,Survived))
        Y = output$Survived
        
        # return four objects
        list("all" = output, "X" = X, "Y" = Y, "YX" = data.frame(Survived=output$Survived,X))
   } else { # test
       list("Xtest" = output)
   }
}