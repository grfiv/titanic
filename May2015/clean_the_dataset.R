# clean up the Titanic dataset
clean_the_dataset = function(output) {

  require(plyr)
  require(stringr)
  library(nnet)

    # Create a column with the last name of each passenger
    # ###################################################
    name_parts = unlist(strsplit(output$Name,","))
    output$last_name = name_parts[seq(1,length(name_parts),2)]

    # Create a column with the titles of each passenger
    # ################################################

    for (i in 1:length(output$Name)){
        matches = regexpr(" (\\w+)\\.", output$Name[i], perl=TRUE, ignore.case=TRUE);
        result  = attr(matches, "capture.start")[,1]
        attr(result, "match.length") = attr(matches, "capture.length")[,1]
        output$title[i] = regmatches(output$Name[i], result)
    }
    # consolidate the titles
    # ----------------------
    if (sum((output$title=='Dr') & (output$Sex=='female')) > 0) {
      output$title[(output$title=='Dr') & (output$Sex=='female')] = 'Mrs'
    }
    if (sum((output$title=="Dr") & (output$Sex=="male")) > 0) {
      output$title[(output$title=="Dr") & (output$Sex=="male")] = 'Mr'
    }

    output$title[output$title %in% c('Capt','Col','Don','Jonkheer','Major','Rev','Sir')] <- 'Mr'
    output$title[output$title %in% c('Countess','Dona','Lady','Mme')] <- 'Mrs'
    output$title[output$title %in% c('Mlle','Ms')] <- 'Mrs'

    # Replace missing ages with the average for the title
    # ###################################################

    for (i in 1:length(output$Age)){
        if (is.na(output$Age[i])) {
            output$Age[i] = mean(output$Age[output$title==output$title[1]],na.rm=TRUE)
        }
    }

    # replace NAs in Fare with column mean for the Pclass
    # ###################################################
    for (Fi in seq(length(output$Fare))) {
        if (is.na(output$Fare[Fi])) {
            output$Fare[Fi] = mean(output[output$Pclass==output$Pclass[Fi],]$Fare,na.rm=TRUE)
        }
    }

    # Segment the fares
    # -----------------
    library(Hmisc)
    output$Fare2 = cut2(output$Fare,cuts=c(0,10,20,30,60))

    # replace NA Embarked with the modal value
    # ########################################
    if (sum(output$Embarked=="") > 0) {
        output[output$Embarked=="",]$Embarked = "S"
    }

    # Several additional features
    # ###########################

    # see http://trevorstephens.com/post/73461351896/titanic-getting-started-with-r-part-4-feature
    output$FamilySize <- output$SibSp + output$Parch + 1

    # see https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md
    isEven <- function(x) x %in% c("0","2","4","6","8")
    isOdd <- function(x) x %in% c("1","3","5","7","9")

    output$Farepp <- output$Fare/output$FamilySize

    output$Deck <- substring(output$Cabin, 1, 1)
    if (sum(output$Deck=="T") > 0) {
      output$Deck[output$Deck=="T"] <- "UNK"
    }
    output$Deck[ which( is.na(output$Deck ))] <- "UNK"
    output$Deck[output$Deck==""] = 'UNK'

    output$cabin.last.digit <- str_sub(output$Cabin, -1)

    output$Side <- "UNK"
    output$Side[which(isEven(output$cabin.last.digit))] <- "port"
    output$Side[which(isOdd(output$cabin.last.digit))] <- "starboard"
    output$Side <- as.factor(output$Side)
    output$cabin.last.digit <- NULL

    # Convert categorical variables to factors with labels
    # ####################################################
    output$Deck <- as.factor(output$Deck)
#     output$FamilySize = factor(output$FamilySize,
#                                levels=seq(min(output$FamilySize),max(output$FamilySize)),
#                                labels=paste("FamilySize",seq(min(output$FamilySize),max(output$FamilySize)),sep=""))
    output$title     = factor(output$title)
    output$last_name = factor(output$last_name)
    output$Sex       = factor(output$Sex)
    output$Pclass    = factor(output$Pclass,
                              levels=c(1,2,3),
                              labels=c("FirstClass","SecondClass","ThirdClass"))
    output$Embarked =  factor(output$Embarked)


    # drop the columns we won't use in the analysis
    # #############################################
    # we need PassengerId to create the output datasets
    output = subset(output, select=-c(Name, Ticket, Cabin))

  # #############################################################
  # --------------- decide whether to return
  #                   factors    (dummy.vars=FALSE)
  #                   dummy vars (dummy.vars=TRUE)

#   if (dummy.vars) {
#     len.orig = length(output)
#     deletions = numeric(0)
#
#     # 1) convert factors to indicators (class.ind())
#     # 2) add the column index of the factor to a list (deletions)
#     # 3) add all but the last indicator to the end of output
#     for (i in seq(len.orig)) {
#       if (class(output[,i]) == "factor") {
#         #print(names(output)[i])
#         temp.df = with(output, data.frame(class.ind(output[,i])))
#         deletions = c(deletions,i)
#         output = data.frame(output,temp.df[1:length(temp.df)-1])
#       }
#     }
#     # remove the factors in reverse order of their column index
#     for (i in sort(deletions,decreasing=TRUE)){
#       output = output[,-i]
#     }
#   }
  # #############################################################

  # return different data.frames for training and test
  # ##################################################
   if ("Survived" %in% names(output)) { # TRAINING
        X = subset(output, select=-c(PassengerId,Survived))
        Y = output$Survived

        # output files structured for use in MATLAB
        # note: MATLAB requires indicators not factors
#         write.csv(X,        file="trainXCLEAN.csv",row.names=FALSE)
#         write.csv(Y,        file="trainYCLEAN.csv",row.names=FALSE)
#         write.csv(output[1],file="trainPIDCLEAN.csv",row.names=FALSE)
        write.csv(output,file="train_titanic.csv",row.names=FALSE)

        # return four objects
        list("all" = output, "X" = X, "Y" = Y, "YX" = data.frame(Survived=output$Survived,X))

   } else {                           # TEST
#        write.csv(output[-1],file="testXCLEAN.csv",row.names=FALSE)
#        write.csv(output[1], file="testPIDCLEAN.csv",row.names=FALSE)
       write.csv(output, file="test_titanic.csv",row.names=FALSE)

       list("Xtest" = output)
   }
}