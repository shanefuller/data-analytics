#############################################
# Shane Fuller                              #
# Professor Read                            #
# CS376B: Applied Data Science              #
# The Gang Steals the Script Data Analysis  #
# Applied Data Analysis Final Project       #
#############################################


############################################################
# A script to demonstrate Bayes Theorem as well as the use #
# of 1R and Naive Bayes classifiers in R                   #
############################################################


####################################
## Install packages - if necessary #
####################################

# Set a default repo 
# Uses the cloud URL which should pick a "close by" repo server
local({r <- getOption("repos")
r["CRAN"] <- "https://cloud.r-project.org" 
options(repos=r)
})

install.packages("OneR")
install.packages("mlbench")
install.packages("e1071")
install.packages("caret")
install.packages("car")
install.packages("lattice")
install.packages("Hmisc")
install.packages("RWeka")
install.packages("rpart")
install.packages("e1071")
install.packages("rparty")


###################
## Load libraries #
###################

library(OneR)
library(mlbench)
library(e1071)
library(caret)
library(car)
library(lattice)
library(Hmisc)
library(RWeka)
library(rpart)
library(e1071)

#############################################################
## Demonstrate basic model->predict cycle with linear model #
#############################################################

# Build linear model
# The entries with NA values had to be taken out
# damagePerOpeningRatio, openingsPerKillRatio, counterHitRatio


linear = lm(gangScriptData$killCount ~ wavelandCount + wavedashCount + 
              airDodgeCount + neutralWinRatioCount +
              dashDanceCount + rollCount  +
              spotDodgeCount + conversionCount +
              opponentId + stageId + totalDamage + characterId + 
              inputsPerMinuteRatio, gangScriptData[-11])



print(linear)

# Get predictions
p = round(predict(linear, gangScriptData[-11]))
print(p)

# Get count of correct predictions
cp = gangScriptData$killCount == p
print(cp)

# get ratio of correct predictions
sum(cp) / nrow(gangScriptData)

########################
## Classifying with 1R #
########################

##
## Use OneR with the gangScriptData
##

# Apply supervised binning to the continuous data
# Note that 1R can use continuous DFs but often
# the risk of overfitting can be reduced by
# using supervised binning
gangScriptData.binned <- optbin(gangScriptData)
gangScriptData.binned

# Create separate training and test sets
# Use a 60:40 split of data for train:test
set.seed(0)
gangScriptTrainSet <- sample(seq_len(nrow(gangScriptData.binned)), nrow(gangScriptData.binned) * .6)
gangScriptData.binned.train <-gangScriptData.binned[gangScriptTrainSet,]
gangScriptData.binned.test <-gangScriptData.binned[-gangScriptTrainSet,]


# Create a 1R classification formula
gangScriptData.cont.oner.formula <- as.formula(paste("killCount ~ ", 
                                    paste(names(gangScriptData.binned.train[!names(gangScriptData.binned.train) %in% 'killCount']), 
                                               collapse = " + "), sep=""))

gangScriptData.cont.oner.formula

# Create a 1R classification model
gangScriptData.oner.model <- OneR(gangScriptData.cont.oner.formula, 
                                  data = gangScriptData.binned.train) 

# Look at the raw model (e.g. the tree's decisions)
print(gangScriptData.oner.model)

# Show the structure of the model
str(gangScriptData.oner.model)

# Show details regarding the model
summary(gangScriptData.oner.model)

# Create predictions based on the model
gangScriptData.oner.pred <- predict(gangScriptData.oner.model, 
                                    gangScriptData.binned.test)

# Evaluate the model
eval_model(gangScriptData.oner.pred, gangScriptData.binned.test)

#################################
## Classifying with Naive Bayes #
#################################

##
## Use Naive Bayes with the gangScriptData
#

# Create separate training and test sets
# Use a 60:40 split of data for train:test
set.seed(6)
gangScriptTrainSet <- sample(seq_len(nrow(gangScriptData)), nrow(gangScriptData) * .6)
gangScriptData.train <-gangScriptData[gangScriptTrainSet,]
gangScriptData.test <-gangScriptData[-gangScriptTrainSet,]

# Verify the class splits in the training set
# to explain the priors in the model
# Instead of raw counts, lets look at proportions
tbl.data <- table(gangScriptData$killCount) / nrow(gangScriptData)
tbl.train <- table(gangScriptData.train$killCount) / nrow(gangScriptData.train)
print(tbl.data)
print(tbl.train)

# Plot the original and training set class proportions
op <- par(mfrow=c(1,2))
bp <- barplot(tbl.data, 
              ylim=c(0, .5),
              main = "Class Proportions in the\nGang Script Dataset (Original)",
              xlab="Class (killCount)",
              ylab="Proportion")
text(x = bp, y = tbl.data, label = round(tbl.data, digits=2), 
     pos = 3, cex = 0.8, col = "red")

bp <- barplot(tbl.train, 
              ylim=c(0, .5),
              main = "Class Proportions in the\nGang Script Dataset (Train)",
              xlab="Class (killCount)",
              ylab="Proportion")
text(x = bp, y = tbl.train, label = round(tbl.train, digits=2), 
     pos = 3, cex = 0.8, col = "red")
par(op)

# Create a Naive Bayes classification model
 
gangScriptData.cont.nb.formula
gangScriptData.nb.model <- naiveBayes(gangScriptData.cont.nb.formula, data = gangScriptData.train)

# Look at the raw model 
# Note the priors for the classes and the conditional 
# probabilities for the DFs
print(gangScriptData.nb.model)

# Show the structure of the model
str(gangScriptData.nb.model)

# Show an overview of the model
summary(gangScriptData.nb.model)

# Create predictions based on the model
gangScriptData.nb.pred <- predict(gangScriptData.nb.model, gangScriptData.test)

# Evaluate the model
eval_model(gangScriptData.nb.pred, gangScriptData.test)



################################################
## Decision Tree, Rule Set and Regression Tree #
################################################

# Create separate training and test sets
# Use a 60:40 split of data for train:test
set.seed(6)
gangScriptTrainSet <- sample(seq_len(nrow(gangScriptData)), nrow(gangScriptData) * .6)
gangScriptData.train <-gangScriptData[gangScriptTrainSet,]
gangScriptData.test <-gangScriptData[-gangScriptTrainSet,]

gangScriptData.cont.nn.formula <- as.formula(paste("killCount ~ ", 
                                                  paste(names(gangScriptData.train[!names(gangScriptData.train) %in% 'killCount']), 
                                                        collapse = " + "), sep=""))

# Build a decision tree for species using C4.5 (Weka's J48 implementation)
gangScriptData.model.nom <- J48(gangScriptData.cont.nn.formula, data=gangScriptData.train)

Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-11.0.2")
install.packages("rJava")
install.packages("RWeka")
library(rJava)
library(RWeka)
# View details of the constructed tree
# Be sure you understand each of the reported measures
# (how it is calculated and what it means)
summary(gangScriptData.model.nom)


# Plot the decision tree
plot(gangScriptData.model.nom)

# Create a regression tree for petal.length using rpart (CART implementation)
gangScriptData.model.reg <- rpart(gangScriptData.cont.nn.formula, data=gangScriptTrainSet)

# View details of the constructed tree
# Be sure you understand what the output means
summary(iris.model.reg)

# Plot the regression tree
plot(iris.model.reg, uniform=TRUE,
     main="Regression Tree for Iris Petal Length")
text(iris.model.reg, use.n=TRUE, all=TRUE, cex=.8)

# Attempt post-pruning of the regression tree to see if a better
# classifier can be created. The approach repeatedly prunes the tree
# generating a set of iteratively pruned trees and displays the cost
# complexity (CP) along with error rates of each
# Note that rpart will calculate the error rate 
# using 10-fold cross validation
printcp(iris.model.reg)

# Obtain one of the pruned trees using the cp value (round up)
# This will retrieve the 2nd tree (cp:0.082403)
iris.model.reg.prune <- prune(iris.model.reg, cp=0.09)

# Verify it is the correct pruned tree (will be last listed)
printcp(iris.model.reg.prune)

# Interactively (manually) prune a tree
# Click once with the left mouse button to shows the impact
# of removing the node. Click the left mouse button a second
# time on the same node to actually remove it.
# Click with the right mouse button to end the process
plot(iris.model.reg, uniform=TRUE,
     main="Regression Tree for Iris Petal Length")
text(iris.model.reg, use.n=TRUE, all=TRUE, cex=.8)
iris.model.reg.manualprune <- snip.rpart(iris.model.reg)

# Display the pruned tree (assuming the user chose to remove any nodes)
plot(iris.model.reg.manualprune, uniform=TRUE,
     main="Regression Tree for Pruned Iris Petal Length")
text(iris.model.reg.manualprune, use.n=TRUE, all=TRUE, cex=.8)

#
# Build a Rule Set Using RIPPER
# Remember that RIPPER creates a default rule for the majority class
# and then creates rules to cover the other classes
#

# Build the rule set
iris.model.rules <- JRip(species ~ ., data=iris.train)

# Display the rule set
print(iris.model.rules)


###############
## Evaluation #
###############

# Create predictions from the decision tree model using the test set
iris.predict.nom <- predict(iris.model.nom, iris.test)

# Calculation of performance for nominal values uses a confusion matrix
# and related measures. 
iris.eval.nom <- confusionMatrix(iris.predict.nom, iris.test$species)

# Display the evaluation results for the decision tree
# You should understand all of these measures (calculation and meaning)
print(iris.eval.nom)

# Create predictions from the rule set using the test set
iris.predict.rules <- predict(iris.model.rules, iris.test)

# Calculation of performance for nominal values uses a confusion matrix
# and related measures.
iris.eval.rules <- confusionMatrix(iris.predict.rules, iris.test$species)

# Display the evaluation results for the rule set
# You should understand all of these measures (calculation and meaning)
# Notes: sensitivity is a synonym for recall;
#        positive predictive value is a synonym for precision
print(iris.eval.rules)

# Create predictions from the regression tree model using the test set
iris.predict.reg <- predict(iris.model.reg, iris.test)

# Need to use a numeric measures for accuracy of a regression tree
# MSE
iris.predict.reg.mse <- mean((iris.predict.reg - iris.test$petal.length)^2)
print(paste("Mean Squared Error (MSE):", iris.predict.reg.mse))

# RMSE
iris.predict.reg.rmse <- sqrt(iris.predict.reg.mse)
print(paste("Root Mean Squared Error (RMSE)", iris.predict.reg.rmse))

# MAE
iris.predict.reg.mae <- mean(abs(iris.predict.reg - iris.test$petal.length))
print(paste("Mean Absolute Error (MAE):", iris.predict.reg.mae))

# Plot the predictions vs. the actuals and add a line showing 
# The location for a "perfect" model where each prediction
# would equal the actual value (e.g. line y=x; a.k.a. 0-intercept, slope 1)
plot(iris.predict.reg, iris.test$petal.length,
     main="Regression Tree Preditions vs. Actual",
     xlab="Predicted", ylab="Actual")
abline(0, 1, lty=2)
legend("topleft", c("Data", "Perfect Model"), pch=c(1, NA), lty=c(NA, 2))
