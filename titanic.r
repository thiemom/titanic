# Titanic kaggle competion R script
# https://www.kaggle.com/c/titanic-gettingStarted
# Derived from the tutorial by Trevor Stephens
# trevorstephens.com/post/72916401642/titanic-getting-started-with-r
# Thiemo Meeuwissen, 2014

# first set the working directory
# setwd("C:/Users/Thiemo Meeuwissen/Documents/Developing Data Products/Shiny/titanic")

# read and show the data from my github page (I copied the data there)

# first read and inspect the training data
train <- read.csv(url(paste('https://raw.github.com/thiemom/titanic/master','train.csv', sep='/')))
summary(train)

# now read and inspect the test data
test <- read.csv(url(paste('https://raw.github.com/thiemom/titanic/master','test.csv', sep='/')))
summary(test)

# count number of samples in the training and test data
ntrain <- nrow(train)
ntest <- nrow(test)

# show the number of samples
cat('training samples:', ntrain, 'and test samples:', ntest)

# join together the test and train data sets for easier data mungling and feature engineering later
# the test data set does not have the Survived feature, so let's create it first and initialize with NA
test$Survived <- NA
combi <- rbind(train, test)
ncombi <- nrow(combi)

# convert the Name feature data type to a string for feature extraction
combi$Name <- as.character(combi$Name)

# find the indexes for the title part of the name string
# actually all entries are of the format "Surname, Title. Firstname..."
# so if the string is split by ., the title can be extracted
strsplit(combi$Name[1], split='[,.]') # not quite right
strsplit(combi$Name[1], split='[,.]')[[1]] # still not quite right
strsplit(combi$Name[1], split='[,.]')[[1]][2] # that's it

# Engineered variable: Title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- gsub(' ', '', combi$Title) # get rid of whitespaces
# Inspect new feature
table(combi$Title)

# Combine small title groups
# This seems sensible because the Titanic was famous for the "women and children first" evacuation policy
# Thinking about it: it just might make sense to group titles in Military (Mil), Miss and Mrs for women, Mr for mean,
# Lady and Sir for nobility and keeping the Reverend and Master titles
# most of the title interpretations are taken from wikipedia entries
# The grouping like this deviates slightly from the tutorial by Trevor Stephens which it was based on
combi$Title[combi$Title %in% c('Mme', 'Mrs')] <- 'Mrs'
combi$Title[combi$Title %in% c('Miss', 'Mlle')] <- 'Miss'
combi$Title[combi$Title %in% c('Capt', 'Col', 'Major')] <-' Mil'
combi$Title[combi$Title %in% c('Sir', 'Don', 'Jonkheer')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
# Convert to a factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
# Reading the data feature description page, the SibSp feature counts Siblings and Spouces
# and the Parch feature countsParents and Children
# The total family size is then SibSp + Parch + 1 for the person
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: Family
# Again following Trevor Stephens tutorial, it makes sense to engineer a feature that describes
# the family size. The idea being that large family have a harder (or is it easier?) time keeping track
# of family members to get into a lifeboat
# To create the unique family IDs, the surname and family size features are combined
# First lets extract the Surnames, similar to the Title approach
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
# Combine the Surname with the family size
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
# Group individuals together
combi$FamilyID[combi$FamilySize < 2] <- 'Single'
# Finally group small families together as well
combi$FamilyID[combi$FamilySize > 1 & combi$FamilySize < 3] <- 'Small'
# Inspect new feature
table(combi$FamilyID)

# Somehow there are some small families that got the wrong FamilyID
# The assumption that Surname and Familysize makes a unique feature when combined was not completely true
# So let's delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
# Check for individuals
SinglefamIDs <- famIDs[famIDs$Freq <= 1,]
combi$FamilyID[combi$FamilyID %in% SinglefamIDs$Var1] <- 'Single'
# Check for small families
SmallfamIDs <- famIDs[famIDs$Freq >1 & famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% SmallfamIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)
# Inspect cleaned feature
table(combi$FamilyID)

# Now continue with missing data treatment
# Check what else might be missing
summary(combi)

# Ok, so we are missing quite some Age data which sounds like it could be useful
# I will use rpart to impute the missing data based on a data model
require(rpart)

# Fill in Age NAs
summary(combi$Age)
# There seems to be 263 NAs
# Age is likely related to Pclass, Sex, Familysize, Title and maybew also where passengers embarked
# Let's try this, following Trevor Stephens tutorial again
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
summary(combi$Age)

# Fill in Embarked blanks
summary(combi$Embarked)
# Find which entries are NA
ix <- which(combi$Embarked == '')
# replace the 2 missing entries with the most common value ("S")
combi$Embarked[c(ix)] = names(table(combi$Embarked)[which.max(table(combi$Embarked))])
combi$Embarked <- factor(combi$Embarked)
summary(combi$Embarked)

## Fill in missing Fare data
summary(combi$Fare)
# Fare likely depends on Pclass, Embarked, and maybe Title and FamilySize
Farefit <- rpart(Fare ~ Pclass + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Fare),], method="anova")
combi$Fare[is.na(combi$Fare)] <- predict(Farefit, combi[is.na(combi$Fare),])
summary(combi$Fare)

# Add category Child
combi$Child <- 0
combi$Child[combi$Age <= 16] <- 1 # let's call passengers with age 16 or less children
combi$Child <- factor(combi$Child)
summary(combi$Child)

# Split back into test and train sets
rm(train, test)
train <- combi[1:ntrain,]
test <- combi[(ntrain+1):ncombi,]

# save cleaned data
save(train, file='titanictrain.RData')
save(test, file='titanictest.RData')

# Install and load required packages
require(rattle)
require(rpart.plot)
require(RColorBrewer)

# Build a new tree with our new features
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID + Child, data=train, method="class")
fancyRpartPlot(fit)

# Build condition inference tree Random Forest
library(party)
set.seed(013)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID + Child, data = train, controls=cforest_unbiased(ntree=3000, mtry=3))

# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "finalciforest.csv", row.names = FALSE)

