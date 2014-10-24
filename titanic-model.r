# variable selecttion

load('titanic-train.RData')
load('titanic-test.RData')

# select features for variable selection
Keep <- c("Fate","Pclass","Sex","Age","SibSp",
          "Parch","Fare","Embarked","Title",
          "FamSize","FamId","Fare.pp",
          "Child","Boy","Girl","Deck")
train <- train[Keep]

require(party)

# mtry will be set to sqrt of number of features
sqrtn = floor(sqrt(length(names(train))))
cat(sqrtn)

# train condition interference forest
cf <- cforest(Fate ~ ., data=train,
              control=cforest_unbiased(mtry=sqrtn,
                                       ntree=100))
# get variable importance
vImp <- varimp(cf)
svImp <- -sort(-vImp) # sort descending
svImp <- svImp/sum(svImp) # normalize importance to unity
barplot(svImp, las=2)

vRank = rank(-vImp) # rank descending
svRank = sort(vRank) # sort rank
names(svRank[1:5]) # show top 5
q <- quantile(svImp)
ModelVars <- names(svRank[svImp>q['50%']]) # drop botom 50% importance
# ModelVars <- names(svRank[svImp>0.1]) # user limit

# select vars
train.selected <- train[c('Fate', ModelVars)]

# inspect selected
head(train.selected)

require(caret)

## split training data into train batch and test batch
set.seed(013)
train.rows <- createDataPartition(
  train.selected$Fate, p=0.8, list = FALSE)
train.batch <- train.selected[train.rows, ]
test.batch <- train.selected[-train.rows, ]

# Define control function to handle optional arguments for train function
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3, summaryFunction = twoClassSummary, classProbs = TRUE)

# logistic regression
glm <- train(Fate ~ ., data = train.batch,
  method = "glm", metric = "ROC", trControl = cv.ctrl)
summary(glm)

glm.pred <- predict(glm, test.batch)
confusionMatrix(glm.pred, test.batch$Fate)
glm.probs <- predict(glm, test.batch, type = "prob")


# # boosting with ada
# set.seed(013)
# ada.grid <- expand.grid(.iter = c(50, 100),
#                         .maxdepth = c(4, 8),
#                         .nu = c(0.1, 1))
#
# ada <- train(Fate ~ ., data = train.batch,
#              method = "ada", metric = "ROC",
#              tuneGrid = ada.grid, trControl = cv.ctrl)


# random forest
set.seed(013)
rf.grid <- data.frame(.mtry = c(2, 3))
rf <- train(Fate ~ ., data = train.batch,
            method = "rf", metric = "ROC",
            tuneGrid = rf.grid, trControl = cv.ctrl)

# svm (svmLinear, svmRadial)
set.seed(013)
svm <- train(Fate ~ ., data = train.batch,
             method = "svmLinear", tuneLength = 9,
             preProcess = c("center", "scale"),
             metric = "ROC", trControl = cv.ctrl)


# Models to be assessed based on largest absolute area under ROC curve

# Logistic regression model (BLACK curve)
glm.probs <- predict(glm, test.batch, type = "prob")
glm.ROC <- roc(response = test.batch$Fate,
               predictor = glm.probs$Survived,
               levels = levels(test.batch$Fate))
plot(glm.ROC, type="S", col="black")
# Area under the curve: 0.898

# Boosted model (GREEN curve)
# ada.probs <- predict(ada.tune, test.batch, type = "prob")
# ada.ROC <- roc(response = test.batch$Fate,
#                predictor = ada.probs$Survived,
#                levels = levels(test.batch$Fate))
# plot(ada.ROC, add=TRUE, col="green")
# # Area under the curve:

# Random Forest model (RED curve)
rf.probs <- predict(rf, test.batch, type = "prob")
rf.ROC <- roc(response = test.batch$Fate,
              predictor = rf.probs$Survived,
              levels = levels(test.batch$Fate))
plot(rf.ROC, add=TRUE, col="red")
# Area under the curve: 0.9019

# SVM model (BLUE curve)
svm.probs <- predict(svm, test.batch, type = "prob")
svm.ROC <- roc(response = test.batch$Fate,
               predictor = svm.probs$Survived,
               levels = levels(test.batch$Fate))
plot(svm.ROC, add=TRUE, col="blue")
# Area under the curve: 0.8813

###################################################

#
# # Build condition inference tree Random Forest
# library(party)
# set.seed(013)
# fit <- cforest(Survived ~ Pclass + Sex + Age + Fare.pp + Embarked + Title + FamilySize + Boy + Girl, data = train, controls=cforest_unbiased(ntree=100, mtry=3))
#
# # Now let's make a prediction and write a submission file
# Prediction <- predict(fit, test, OOB=TRUE, type = "response")
# submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
# write.csv(submit, file = "finalciforest.csv", row.names = FALSE)
#

#
#
# # use the logistic regression model to generate predictions
# Survived <- predict(glm.tune.5, newdata = pred.these)
#
# # reformat predictions to 0 or 1 and link to PassengerId in a data frame
# Survived <- revalue(Survived, c("Survived" = 1, "Perished" = 0))
# predictions <- as.data.frame(Survived)
# predictions$PassengerId <- df.infer$PassengerId
#
# # write predictions to csv file for submission to Kaggle
# write.csv(predictions[,c("PassengerId", "Survived")],
#           file="Titanic_predictions.csv", row.names=FALSE, quote=FALSE)