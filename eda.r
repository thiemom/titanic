# Titanic kaggle competion R script
# https://www.kaggle.com/c/titanic-gettingStarted
# Derived from the tutorial by Trevor Stephens
# trevorstephens.com/post/72916401642/titanic-getting-started-with-r
# Thiemo Meeuwissen, 2014

# first set the working directory
# setwd("C:/Users/Thiemo Meeuwissen/Documents/Developing Data Products/Shiny/titanic")
setwd("D:/Documents and Settings/tmeeuwis/Desktop/Sandbox/DataProducts/titanic")

# read and show the data from my github page (I copied the data there)

# # first read and inspect the training data
# train <- read.csv(url(paste('https://raw.github.com/thiemom/titanic/master','train.csv', sep='/')))
# summary(train)
#
# # now read and inspect the test data
# test <- read.csv(url(paste('https://raw.github.com/thiemom/titanic/master','test.csv', sep='/')))
# summary(test)

train <- read.csv('./train.csv')
test <- read.csv('./test.csv')

# count number of samples in the training and test data
ntrain <- nrow(train)
ntest <- nrow(test)

# join together the test and train data sets for easier data mungling and feature engineering later
# the test data set does not have the Survived feature, so let's create it first and initialize with NA
test$Survived <- NA
combi <- rbind(train, test)
ncombi <- nrow(combi)
summary(combi)

foo <- function(x) {
  return(1)
}

foo(2)

extractTitle <- function(df) {

  # convert the input df$Name type to string
  df$Name <- as.character(df$Name)

  # All entries are of the format "Surname, Title. Firstname..."
  # So if the string is split by ., the title can be extracted
  #   strsplit(combi$Name[1], split='[,.]') # not quite right
  #   strsplit(combi$Name[1], split='[,.]')[[1]] # still not quite right
  #   strsplit(combi$Name[1], split='[,.]')[[1]][2] # that's it

#   df$Surname <- sapply(df$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

  df$Title <- sapply(df$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
  df$Title <- gsub('^ ', '', df$Title) # get rid of leading whitespace

  # Now reduce uncommon Titles

  # Let's assume military and doctors are like Mr
  df$Title[df$Title %in% c('Col','Capt','Major','Dr')] <- 'Mr'
  # Checking sex reveals that there is a female Doctor around
  # Although this is probably not very important, I'll clean it up anyway
  ix <- which(df$Title == 'Mr' & df$Sex == 'female')
  # Because she doesn't have SibSp or Parch entries, lets assume that she is not married
  # So cleaned Title will be Miss
  df$Title[ix] <- 'Miss'

  # Mme is really just Mrs
  df$Title[df$Title %in% c('Mme')] <- 'Mrs'

  # Now apply some rescue priority grouping
  # idea: Titanic was famous for women and children first
  # assumption: probably honour dictated that royalty / reverends / crew needed to go last
  # so lets group titles accordingly to allow the algorithm to learn easier and generalize better if the assumption is correct

  df$Title[df$Title %in% c('Sir','Don','Jonkheer','Rev')] <- 'Last'
  df$Title[df$Title %in% c('Lady','Dona','the Countess', 'Ms', 'Mlle')] <- 'First'

  df$Title <- factor(df$Title)

  return(df)
}

combi <- extractTitle(combi)

table(combi$Title)

which(combi$Title == 'Mr' & combi$Sex == 'female')

