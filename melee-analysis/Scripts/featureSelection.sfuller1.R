#############################################
# Shane Fuller                              #
# Professor Read                            #
# CS376B: Applied Data Science              #
# The Gang Steals the Script Data Analysis  #
# Applied Data Analysis Final Project       #
#############################################


##########################################################
# A script to demonstrate a variety of feature selection #
# capabilities in R, shown on the gangScriptData table.  #
##########################################################


###################################################################
## NOTE: FSelector REQUIRES Java to be installed and R configured #
##       to use Java                                              #
## To enable Java support in R, open a terminal and type the      #
## following command (as root): R CMD javareconf                  #
###################################################################

install.packages("FSelector")
install.packages("rpart")
install.packages("entropy")

###################
## Load libraries #
###################

library(FSelector)
library(rpart)
library(entropy)

#########################
## Load the data set    #
#########################
gangScriptData <- gangScriptDataABT

############################
## Find the entropy values #
############################
# The entropy values all seem to revolve around 8.0-8.6
entropy(gangScriptData$wavelandCount)
entropy(gangScriptData$wavedashCount)
entropy(gangScriptData$airDodgeCount)
entropy(gangScriptData$neutralWinRatioCount)
entropy(gangScriptData$dashDanceCount)
entropy(gangScriptData$openingsPerKillRatio)
entropy(gangScriptData$rollCount)
entropy(gangScriptData$counterHitRatio)
entropy(gangScriptData$spotDodgeCount)
entropy(gangScriptData$damagePerOpeningRatio)
entropy(gangScriptData$killCount)
entropy(gangScriptData$conversionCount)
entropy(gangScriptData$opponentId)
entropy(gangScriptData$stageId)
entropy(gangScriptData$totalDamage)
entropy(gangScriptData$characterId)
entropy(gangScriptData$inputsPerMinuteRatio)

###############################################################
## Demonstrate dimensionality reduction and feature selection #
## using random forest (a tree-type classifier)               #
###############################################################


# Use random forest to choose the most important attributes (e.g. decision tree)
# Inclding sort_group which has a 1.0 correspondence 
gangScriptData.rf.scores <- random.forest.importance(killCount ~ ., gangScriptData)

# Display the feature scores
print(gangScriptData.rf.scores)

# Show the features with significantly higher importance
cutoff.biggest.diff(gangScripData.rf.scores)

# Show the top k important features
cutoff.k(gangScriptData.rf.scores, k = 3)

# Show the top k% of import features
cutoff.k.percent(gangScriptData.rf.scores, 0.4)

#######################################################################
## Demonstrate feature selection with greedy, hill-climb, exhaustive, #
## and correlation/entropy searches                                   #
#######################################################################

############################################################
## Demonstrate basic model->predict cycle using tree model #
############################################################

# Create the model surrounding killCount
gangScriptData.cont.formula <- as.formula(paste("gangScriptData$killCount ~ ", 
                                                     paste(names(gangScriptData[!names(gangScriptData) %in% 'killCount']), 
                                                           collapse = " + "), sep=""))
# Show the model
print(gangScriptData.cont.formula)

# Build decision tree
tree <- rpart(gangScriptData.cont.formula, data = gangScriptData[,-11])

# Show the resulting tree
print(tree)

# Get predictions
p = predict(tree, gangScriptData[,-11], type="c")

# Get count of correct predictions
cp = gangScriptData$killCount == p

# get ratio of correct predictions
sum(cp) / nrow(gangScriptData)

#############################################################
## Demonstrate basic model->predict cycle with linear model #
#############################################################

# Build linear model
# The entries with NA values had to be taken out
# damagePerOpeningRatio, openingsPerKillRatio, counterHitRatio
linear = lm(gangScriptData$killCount ~ wavelandCount + wavedashCount + airDodgeCount + neutralWinRatioCount +
              dashDanceCount + rollCount  +
              spotDodgeCount + conversionCount +
              opponentId + stageId + totalDamage + characterId + inputsPerMinuteRatio, gangScriptData[-11])
print(linear)

# Get predictions
p = round(predict(linear, gangScriptData[-11]))
print(p)

# Get count of correct predictions
cp = gangScriptData$killCount == p
print(cp)

# get ratio of correct predictions
sum(cp) / nrow(gangScriptData)

################################################
## Use FSelector library for feature selection #
################################################

############################################################################
## Define an evaluation function  for the gangScriptData using a tree      #
## (e.g. a way to score a the feature subset)                              #
## In this case we'll build a tree and return the mean of the success rate #
## This is from the FSelector package documentation)                       #
############################################################################
evaluator.gangScriptData.tree <- function(subset) {
  # Use k-fold cross validation
  k <- 15
  splits <- runif(nrow(gangScriptData))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- gangScriptData[test.idx, , drop=FALSE]
    train <- gangScriptData[train.idx, , drop=FALSE]
    tree <- rpart(as.simple.formula(subset, "killCount"), train)
    error.rate = sum(test$killCount != predict(tree, test, type="c")) / nrow(test)
    return(1 - error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}

###################################################################################
# Define an evaluation function for the gangScriptData using linear regression    #
# (e.g. a way to score a the feature subset)                                      #
# In this case we'll build a linear model and return the mean of the success rate #
# This is from the FSelector package documentation)                               #
###################################################################################
evaluator.gangScriptData.lm <- function(subset) {
  # Use k-fold cross validation
  k <- 15
  splits <- runif(nrow(gangScriptData))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- gangScriptData[test.idx, , drop=FALSE]
    train <- gangScriptData[train.idx, , drop=FALSE]
    linear <- lm(as.simple.formula(subset, "killCount"), train)
    error.rate = sum(test$killCount != round(predict(linear, test))) / nrow(test)
    return(1 - error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}

# Build the linear model
linear = lm(gangScriptData$killCount ~ ., gangScriptData[,-11])

# Obtain the predictions from the model
p = round(predict(linear, gangScriptData[,-11]))

############################################################
## Use forward greedy search on gangScriptData - tree eval #
############################################################
subset <- forward.search(names(gangScriptData)[-11], evaluator.gangScriptData.tree())

# Obtain the selected subset of features
ft <- as.simple.formula(subset, "killCount")

# Display the selected subset of features
print(ft)

##############################################################
## Use forward greedy search on gangScriptData - linear eval #
##############################################################
subset <- forward.search(names(gangScriptData)[-11], evaluator.gangScriptData.lm())

# Obtain the selected subset of features
fl <- as.simple.formula(subset, "killCount")

# Display the selected subset of features
print(fl)

#####################################################################
## Select features for gangScriptData using correlation and entropy #
#####################################################################
result <- cfs(killCount ~ ., gangScriptData)

# Obtain the selected subset of features
ce <- as.simple.formula(result, "killCount")

# Display the selected subset of features
print(ce)