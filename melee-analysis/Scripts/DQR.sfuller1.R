#############################################
# Shane Fuller                              #
# Professor Read                            #
# CS376B: Applied Data Science              #
# The Gang Steals the Script Data Analysis  #
# Applied Data Analysis Final Project       #
#############################################

#############################################################
# A script to demonstrate the work completed for the Data   #
# Quality Report. This includes work to look at correlation #
# plots in the data, as well as summary statistics for each #
# individual feature.                                       #
#############################################################

####################################
## Install packages - if necessary #
####################################
install.packages("corrplot")
install.packages("aplpack")
install.packages("modes")
install.packages("googleVis")
install.packages("ggplot2")


####################################
## Loading the libraries           #
####################################
library(corrplot)
library(ggplot2)
library(aplpack)
library(modes)
library(googleVis)

#######################################
## Work Done for the DQP/ABT Creation #
#######################################

# A majority of the DQP work was done away from the work that was done in R.
# As the plan highlights, most of the work in creating the ABT was in pruning
# the original JSON dump, reformatting the data, renaming the values, and
# converting it into a readable CSV file that could then be imported into R for
# further analysis. The steps below are the final measures that were necessary
# for the final preparation of the data.

# First see what the data looks like
summary(gangScriptData)

# Get an understanding of the data types and dimensions
str(gangScriptData)

# Change the data from character type to numeric type
gangScriptData <- lapply(gangScriptData, function(x) as.numeric(as.character(x)))

# Delete row 3067, as it is only filled with N/A values
gangScriptData <- gangScriptData[-c(3067),]

# Delete row 3067, as it has a killCount of 5
gangScriptData <- gangScriptData[-c(1673),]

# Delete column inputCount
gangScriptData <- gangScriptData[-4]

# Delete column openingsPerKillCount
gangScriptData <- gangScriptData[-4]

# Delete column inputsPerMinuteTotal
gangScriptData <- gangScriptData[-3]

# Delete column inputsPerMinuteCount
gangScriptData <- gangScriptData[-5]

# Delete column beneficalTradeRatioCount
gangScriptData <- gangScriptData[-15]

# Delete column id
gangScriptData <- gangScriptData[-1]

# Change the data into a data frame
gangScriptData <- data.frame(gangScriptData)

# Save final version of dataset (ABT)
write.csv(gangScriptData, "gangScriptDataABT.csv")


####################################
## Work Done for the DQR           #
####################################

# Below is the work done for the DQR portion of the project.
# Unfortunately, the data provided did not have much variation in terms of
# type. All but the id's where numeric, and of the numeric data, the information
# was either a series of simple integers in the form of counts, or they were
# doubles in the form of ratios. There ended up being 4 ids, 9 integers, and
# 5 doubles. Of the doubles, only toalDamage is not a ratio, otherwise the other
# doubles that are features are ratios.

####################################
## Correlation plots               #
####################################

# The entries with NA values had to be taken out
# damagePerOpeningRatio, openingsPerKillRatio, counterHitRatio
correlation.gangScriptData <- gangScriptData
correlation.gangScriptData <- correlation.gangScriptData[-10]
correlation.gangScriptData <- correlation.gangScriptData[-8]
correlation.gangScriptData <- correlation.gangScriptData[-6]

# Create the correlation plot
correlation <- cor(correlation.gangScriptData, method = c("pearson", "kendall", "spearman"))

# Show the correlation plot
print(correlation)

# Create correlation plot of type number
corrplot(correlation, method = "number")

# Create correlation plot of type circle
corrplot(correlation, method = "circle")

# Create correlation plot of type pie
corrplot(correlation, method = "pie")

# Create correlation plot of type color
corrplot(correlation, method = "color")


####################################
## Analysis of wavelandCount       #
####################################
summary(gangScriptData$wavelandCount)
head(gangScriptData$wavelandCount)
hist(gangScriptData$wavelandCount, main = "Histogram of WavelandCount", xlab = "Number of Wavelands")

####################################
## Analysis of wavedashCount       #
####################################
summary(gangScriptData$wavedashCount)
hist(gangScriptData$wavedashCount, main = "Histogram of WavedashCount", xlab = "Number of Wavedashes")

####################################
## Analysis of airDodgeCount       #
####################################
summary(gangScriptData$airDodgeCount)
hist(gangScriptData$airDodgeCount, main = "Histogram of AirDodgeCount", xlab = "Number of AirDodges")

###########################################
## Analysis of neutralWinRatioCount       #
###########################################
summary(gangScriptData$neutralWinRatioCount)
hist(gangScriptData$neutralWinRatioCount, main = "Histogram of NeutralWinRatioCount", xlab = "Number of Neutral Win Ratios")

#####################################
## Analysis of dashDanceCount       #
#####################################
summary(gangScriptData$dashDanceCount)
hist(gangScriptData$dashDanceCount, main = "Histogram of DashDanceCount", xlab = "Number of Dashdances")

###########################################
## Analysis of openingsPerKillRatio       #
###########################################
summary(gangScriptData$openingsPerKillRatio)
hist(gangScriptData$openingsPerKillRatio, main = "Histogram of OpeningsPerKillRatio", xlab = "Number of OpeningsPerKillRatio")

###############################
## Analysis of rollCount      #
###############################
summary(gangScriptData$rollCount)
hist(gangScriptData$rollCount, main = "Histogram of RollCount", xlab = "Number of Rolls")

######################################
## Analysis of counterHitCount       #
######################################
summary(gangScriptData$counterHitRatio)
hist(gangScriptData$counterHitRatio, main = "Histogram of CounterHitCount", xlab = "Number of counter Hits")

#####################################
## Analysis of spotDodgeCount       #
#####################################
summary(gangScriptData$spotDodgeCount)
hist(gangScriptData$spotDodgeCount, main = "Histogram of SpotDodgeCount", xlab = "Number of Spot Dodges")

###########################################
## Analysis of damagePerOpeningRatio      #
###########################################
summary(gangScriptData$damagePerOpeningRatio)
hist(gangScriptData$damagePerOpeningRatio, main = "Histogram of DamagePerOpeningRatio", xlab = "Damage Per Opening Ratios")

################################
## Analysis of killCount       #
################################
summary(gangScriptData$killCount)
(gangScriptData$killCount)

######################################
## Analysis of conversionCount       #
######################################
summary(gangScriptData$conversionCount)
hist(gangScriptData$conversionCount, main = "Histogram of ConversionCount", xlab = "Number of Conversions")

##################################
## Analysis of opponentId        #
##################################
summary(gangScriptData$opponentId)
barplot(gangScriptData$opponentId)

##############################
## Analysis of stageId       #
##############################
summary(gangScriptData$stageId)
hist(gangScriptData$stageId, main = "Histogram of AirDodgeCount", xlab = "Number of AirDodges")

##################################
## Analysis of totalDamage       #
##################################
summary(gangScriptData$totalDamage)
hist(gangScriptData$totalDamage, main = "Histogram of totalDamage", xlab = "Number of Total Damages")

#################################
## Analysis of characterId      #
#################################
summary(gangScriptData$characterId)
factor(gangScriptData$characterId)
w = table(gangScriptData$characterId)
w

t = as.data.frame(w)

library(plyr)
count(gangScriptData, 'characterId')

barplot(w, main = "Number of Games Played with Specific Character", xlab = "Character ID", ylab = "Frequency")

hist(gangScriptData$characterId, main = "Histogram of totalDamage", xlab = "Number of Total Damages")

###########################################
## Analysis of inputsPerMinuteRatio       #
###########################################
summary(gangScriptData$inputsPerMinuteRatio)
hist(gangScriptData$inputsPerMinuteRatio, main = "Histogram of inputsPerMinuteRatio", xlab = "Number of Inputs Per Minute Ratios")