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

# custom function to extract the engineered feature Title from the dataset
extractTitle <- function(df, simplify=1) {
  names <- as.character(df$Name)
  titles <- sapply(names, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]}) # split string
  titles <- gsub('^ ', '', titles) # get rid of leading whitespaces
  # convert french title to equivalent english title
  titles[titles %in% c('Mme')] <- 'Mrs'
  titles[titles %in% c('Mlle')] <- 'Miss'
  if (simplify==1) {
    # simplify royalty titles
    titles[titles %in% c('the Countess', 'Dona')] <- 'Lady'
    titles[titles %in% c('Jonkheer', 'Don')] <- 'Sir'
    # simplify Ms: Wikipedia says it is a general title, so lets check for Siblings and Spouces
    ind <- which(titles=='Ms' & df$SibSp==0)
    titles[ind] <- 'Miss'
    ind <- which(titles=='Ms' & df$SibSp>0)
    titles[ind] <- 'Mrs'
    # simplify Dr: could be male or female
    # I assume female Dr are married here as I have no further information and there are not may Dr anyway
    ind <- which(titles=='Dr' & df$Sex=='male')
    titles[ind] <- 'Mr'
    ind <- which(titles=='Dr' & df$Sex=='female')
    titles[ind] <- 'Mrs'
    # simplify military titles
    titles[titles %in% c('Capt', 'Col', 'Major')] <- 'Mr'
    # finally the Titanic disaster was famous for the application of Women and Children first
    # thinking about it a bit, I will assume that this rule is applied due to the customs
    # of the time, and likely also to maintain honour (royalty / crew)
    # women with families are perhaps less concerned with their personal safety, so I keep them seperate
    # reverends are also less likely to be concerner with their personal safety and more with the ones staying on the ship
    titles[titles %in% c('Lady', 'Miss')] <- 'First'
    titles[titles %in% c('Sir', 'Rev')] <- 'Last'
  }
  # Convert to a factor
  titles <- factor(titles)
  return(titles)
}

combi$Title <- extractTitle(combi, simplify=1)
table(combi$Title)

# Engineered variable: Family size
# Reading the data feature description page, the SibSp feature counts Siblings and Spouces
# and the Parch feature countsParents and Children
# The total family size is then SibSp + Parch + 1 for the person
combi$FamSize <- combi$SibSp + combi$Parch + 1

# Extract the engineered feature FamId from the dataset
# Again following Trevor Stephens tutorial, it makes sense to engineer a feature that describes
# the family size. The idea being that large family have a harder (or is it easier?) time keeping track
# of family members to get into a lifeboat
# To create the unique family IDs, the surname and family size features are combined

# First lets extract the Surnames, similar to the Title approach
combi$Surname <- sapply(as.character(combi$Name), FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$Surname <- factor(combi$Surname)

# Combine the Surname with the family size
combi$FamId <- paste(as.character(combi$FamSize), as.character(combi$Surname), sep="-")
# Group individuals and couples together
combi$FamId[combi$FamSize == 1] <- 'Singles'
combi$FamId[combi$FamSize == 2] <- 'Couples'
# Somehow there are some families that don't match the FamSize
# So let's delete erroneous family Ids
tmp <- data.frame(table(combi$FamId))
Singles <- tmp[tmp$Freq <= 1,]
Couples <- tmp[tmp$Freq == 2,]
combi$FamId[combi$FamId %in% Singles$Var1] <- 'Singles'
combi$FamId[combi$FamId %in% Couples$Var1] <- 'Couples'
combi$FamId <- factor(combi$FamId)
# Inspect new feature
table(combi$FamId)

# Now continue with missing data treatment
# Check what else might be missing
summary(combi)

# Ok, so we are missing quite some Age data which sounds like it could be useful
# I will use rpart to impute the missing data based on a data model
require(rpart)

# Fill in Age NAs
summary(combi$Age)
# There seems to be 263 NAs
# Age is likely related to Pclass, Sex, FamSize, Title and maybe also where passengers embarked
# Let's try this, following Trevor Stephens tutorial again
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamSize,
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

# Fill in missing Fare data
summary(combi$Fare)
# Fare likely depends on Pclass, Embarked, and maybe Title and FamSize
Farefit <- rpart(Fare ~ Pclass + Embarked + Title + FamSize,
                 data=combi[!is.na(combi$Fare),], method="anova")
combi$Fare[is.na(combi$Fare)] <- predict(Farefit, combi[is.na(combi$Fare),])
summary(combi$Fare)

# try to calculate Fare per person
combi$Fare.pp <- combi$Fare/combi$FamSize

# Add category Child
combi$Child <- 0
combi$Child[combi$Age < 15] <- 1 # let's call passengers with age 15 or less children
combi$Child <- factor(combi$Child)
summary(combi$Child)

# Add categories boy and girl for male and female Children
combi$Boy <- combi$Child
combi$Boy[combi$Sex == 'female'] <- 0
combi$Girl <- combi$Child
combi$Girl[combi$Sex == 'male'] <- 0

# Extract deck information where available
# The 1-st character in Cabin number represents the Deck
combi$Deck <- substring(combi$Cabin, 1, 1)
combi$Deck[which(is.na(combi$Deck))] <- "Missing"
combi$Deck <- factor(combi$Deck)
summary(combi$Deck)

# add new factor fate
combi$Fate <- 'Perished'
combi$Fate[which(is.na(combi$Survived))] <- NA
combi$Fate[which(combi$Survived==1)] <- "Survived"
combi$Fate <- factor(combi$Fate)
summary(combi$Fate)

# make the feature Survived a factor
combi$Survived <- factor(combi$Survived)

# Split back into test and train sets
rm(train, test)
train <- combi[1:ntrain,]
test <- combi[(ntrain+1):ncombi,]

save(train, file='titanic-train.RData')
save(test, file='titanic-test.RData')
