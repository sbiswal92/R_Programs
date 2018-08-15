install.packages("rpart")
install.packages("party")
install.packages("e1071")
install.packages("ipred")
install.packages("ROCR")

mydata <- read.csv("~/Downloads/blood.csv")

## Divide the data into training and testing dataset
head(mydata)
set.seed(56)
ind <- sample(1:2, nrow(mydata), replace = TRUE, prob=c(0.8, 0.2))

trainData<-mydata[ind==1,]
testData<-mydata[ind==2,]


##
#This dataset has a binary response (outcome, dependent) variable called admit. 
#There are three predictor variables: gre, gpa and rank. We will treat the variables gre and gpa as continuous. 
#The variable rank takes on the values 1 through 4. 
##

# to convert rank into factor data-type 
# (allows the column rank to be considered as categorical variable)
trainData$Made.Donation.in.March.2007 <- factor(trainData$Made.Donation.in.March.2007)
testData$Made.Donation.in.March.2007 <- factor(testData$Made.Donation.in.March.2007)
mydata$Made.Donation.in.March.2007 <- factor(mydata$Made.Donation.in.March.2007)


require(rpart)
x.rp <- rpart(Made.Donation.in.March.2007 ~ ., data=trainData, control=rpart.control(minsplit=1, minbucket=1, cp=0.01)) 
# predict classes for the evaluation data set
x.rp.pred <- predict(x.rp, type="class", newdata=testData)
# score the evaluation data set (extract the probabilities)
x.rp.prob <- predict(x.rp, type="prob", newdata=testData)
table(x.rp.pred,testData$Made.Donation.in.March.2007)

#######Add these lines###########

# create a new table containing testData and append the predictions obtained from the model
x.rp.compare <- testData
x.rp.compare$predict_result <- x.rp.pred
x.rp.compare$predict_result_prob <- x.rp.prob
# add a new column to this table containing yes for correct predictions, else no
x.rp.compare$correct_pred <- ifelse(x.rp.compare$predict_result != x.rp.compare$Made.Donation.in.March.2007,"no", "yes")

x.rp.compare[x.rp.compare$correct_pred == "no",5:8] # view the data
nrow(x.rp.compare) # total number of rows in testData
nrow(x.rp.compare[x.rp.compare$correct_pred == "yes",])  # total number of rows with correct predictions

##################

# To view the decision tree, uncomment this line.
plot(x.rp, main="Decision tree created using rpart")
text(x.rp, use.n=TRUE, all=TRUE, cex=.5)


# create model using conditional inference trees
library(party)
x.ct <- ctree(Made.Donation.in.March.2007 ~ ., data=trainData)
x.ct.pred <- predict(x.ct, newdata=testData)
x.ct.prob <-  1 - unlist(treeresponse(x.ct, testData), use.names=F)[seq(1,nrow(testData)*2,2)]
table(x.ct.pred,testData$Made.Donation.in.March.2007)

x.ct.pred
# To view the decision tree, uncomment this line.
plot(x.ct, main="Decision tree created using condition inference trees")


x.ct.compare <- mytestdata
x.ct.compare$Made.Donation.in.March.2007 <- x.ct.prob
x.ct.compare <- x.ct.compare[,c('X','Made.Donation.in.March.2007')]
x.ct.compare


# create model using random forest and bagging ensemble using conditional inference trees
x.cf <- cforest(Made.Donation.in.March.2007 ~ ., data=trainData, control = cforest_unbiased(mtry=ncol(testData)-2))
x.cf.pred <- predict(x.cf, newdata=testData)
x.cf.prob <-  1- unlist(treeresponse(x.cf, testData), use.names=F)[seq(1,nrow(testData)*2,2)]
table(testData$Made.Donation.in.March.2007,x.cf.pred)

x.ct.compare <- mytestdata
x.ct.compare$Made.Donation.in.March.2007 <- x.cf.prob
x.ct.compare <- x.ct.compare[,c('X','Made.Donation.in.March.2007')]
x.ct.compare

write.csv(x.ct.compare, file = "Result_Day3.csv")


# create model using bagging (bootstrap aggregating)
library(ipred)
x.ip <- bagging(Made.Donation.in.March.2007 ~ ., data=trainData)
x.ip.pred <- predict(x.ip, newdata=testData)
x.ip.prob <- predict(x.ip, type="prob", newdata=testData)

# create model using svm (support vector machine)

require(e1071)
# svm requires tuning
x.svm.tune <- tune(svm, Made.Donation.in.March.2007~., data = trainData,
                   ranges = list(gamma = 2^(-8:1), cost = 2^(0:4)),
                   tunecontrol = tune.control(sampling = "fix"))
# display the tuning results (in text format)
x.svm.tune
# If the tuning results are on the margin of the parameters (e.g., gamma = 2^-8), 
# then widen the parameters.
# I manually copied the cost and gamma from console messages above to parameters below.
x.svm <- svm(Made.Donation.in.March.2007~., data = trainData, cost=2, gamma=0.25, probability = TRUE)
x.svm.prob <- predict(x.svm, type="prob", newdata=testData, probability = TRUE)



########################################################################################
##
## plot ROC curves to compare the performance of the individual classifiers
##
# load the ROCR package which draws the ROC curves

require(ROCR)

# create an ROCR prediction object from rpart() probabilities
x.rp.prob.rocr <- prediction(x.rp.prob[,2], testData[,'Made.Donation.in.March.2007'])
# prepare an ROCR performance object for ROC curve (tpr=true positive rate, fpr=false positive rate)
x.rp.perf <- performance(x.rp.prob.rocr, "tpr","fpr")
# plot it
plot(x.rp.perf, col=2, main="ROC curves comparing classification performance of five machine learning models")
x.rp.perf1 <- performance(x.rp.prob.rocr, "auc", "tpr","fpr")
auc.rp <- as.numeric(x.rp.perf1@y.values)

# Draw a legend.
legend(0.6, 0.6, c('rpart', 'ctree', 'cforest','bagging','svm'), 2:6)

# ctree
x.ct.prob.rocr <- prediction(x.ct.prob, testData[,'Made.Donation.in.March.2007'])
x.ct.perf <- performance(x.ct.prob.rocr, "tpr","fpr")
# add=TRUE draws on the existing chart 
plot(x.ct.perf, col=3, add=TRUE)
x.ct.perf1 <- performance(x.ct.prob.rocr, "auc", "tpr","fpr")
auc.ct <- as.numeric(x.ct.perf1@y.values)

# cforest
x.cf.prob.rocr <- prediction(x.cf.prob, testData[,'Made.Donation.in.March.2007'])
x.cf.perf <- performance(x.cf.prob.rocr, "tpr","fpr")
plot(x.cf.perf, col=4, add=TRUE)
x.cf.perf1 <- performance(x.cf.prob.rocr, "auc", "tpr","fpr")
auc.cf <- as.numeric(x.cf.perf1@y.values)


# bagging
x.ip.prob.rocr <- prediction(x.ip.prob[,2], testData[,'Made.Donation.in.March.2007'])
x.ip.perf <- performance(x.ip.prob.rocr, "tpr","fpr")
plot(x.ip.perf, col=5, add=TRUE)
x.ip.perf1 <- performance(x.ip.prob.rocr, "auc", "tpr","fpr")
auc.ip <- as.numeric(x.ip.perf1@y.values)

# svm
x.svm.prob.rocr <- prediction(attr(x.svm.prob, "probabilities")[,2], testData[,'Made.Donation.in.March.2007'])
x.svm.perf <- performance(x.svm.prob.rocr,"tpr","fpr")
plot(x.svm.perf, col=6, add=TRUE)
x.svm.perf1 <- performance(x.svm.prob.rocr, "auc", "tpr","fpr")
auc.svm <- as.numeric(x.svm.perf1@y.values)


auc.rp
auc.ct
auc.cf
auc.ip
auc.svm

