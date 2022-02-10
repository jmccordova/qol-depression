# Pre-processing

## Install important libraries
install.packages(c("foreign", "car", "caret","class", "gmodels", "rpart", "rpart.plot", "e1071", "Hmisc", "ggcorrplot", "GGally", "randomForest"), dependencies = TRUE, lib = "./R")

## Declare mode function
## This is important as we need to impute categorical variables that are missing
mode <- function(v){
  v = v[nchar(as.character(v))>0]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## These are the variables that represent Quality of Life
test_cols <- c(
  "GENHLTH", "PHYSHLTH", "MENTHLTH", "POORHLTH",
  "HLTHPLN1", "PERSDOC2", "MEDCOST", "CHECKUP1", "HLTHCVR1", 
  "EXERANY2",
  "SLEPTIM1",
  "RENTHOM1", "CPDEMO1B", "EMPLOY1", "INCOME2", "EDUCA", "CHILDREN")

## Parse the dataset and store it in a variable
library("foreign")
llcp2020 <-  read.xport("./LLCP2020.XPT")
## Remove columns that will not be tested
llcp2020 <- subset(llcp2020, select = c(GENHLTH, PHYSHLTH, MENTHLTH, POORHLTH, HLTHPLN1, PERSDOC2, MEDCOST, CHECKUP1, HLTHCVR1, RENTHOM1, CPDEMO1B, EMPLOY1, INCOME2, EDUCA, CHILDREN, EXERANY2, SLEPTIM1, ADDEPEV3))
## Show the structure of the dataset
str(llcp2020)
head(llcp2020)
## Change "unsure" and "refused" to NA, especially to numerical values. Impute NA values.
## Rather than setting them to factor, we will put them as numeric. Always use the codebook as reference.
library("car")
llcp2020$GENHLTH <- as.numeric(llcp2020$GENHLTH)
llcp2020$GENHLTH[is.na(llcp2020$GENHLTH)] <- mode(llcp2020$GENHLTH) # Impute missing data
llcp2020$PHYSHLTH <- as.numeric(recode(llcp2020$PHYSHLTH, "88=0; 77=NA; 99=NA"))
llcp2020$PHYSHLTH[is.na(llcp2020$PHYSHLTH)] <- median(llcp2020$PHYSHLTH, na.rm = TRUE) # Impute missing data
llcp2020$MENTHLTH <- as.numeric(recode(llcp2020$MENTHLTH, "88=0; 77=NA; 99=NA"))
llcp2020$MENTHLTH[is.na(llcp2020$MENTHLTH)] <- median(llcp2020$MENTHLTH, na.rm = TRUE) # Impute missing data
llcp2020$POORHLTH <- as.numeric(recode(llcp2020$POORHLTH, "88=0; 77=NA; 99=NA"))
llcp2020$POORHLTH[is.na(llcp2020$POORHLTH)] <- median(llcp2020$POORHLTH, na.rm = TRUE) # Impute missing data
llcp2020$HLTHPLN1 <- as.numeric(llcp2020$HLTHPLN1)
llcp2020$HLTHPLN1[is.na(llcp2020$HLTHPLN1)] <- mode(llcp2020$HLTHPLN1) # Impute missing data
llcp2020$PERSDOC2 <- as.numeric(recode(llcp2020$PERSDOC2, "1=2; 2=2; 3=1; 7=NA; 9=NA")) # Transform this to only Yes or No
llcp2020$PERSDOC2[is.na(llcp2020$PERSDOC2)] <- mode(llcp2020$PERSDOC2) # Impute missing data
llcp2020$MEDCOST <- as.numeric(recode(llcp2020$MEDCOST, "7=NA; 9=NA"))
llcp2020$MEDCOST[is.na(llcp2020$MEDCOST)] <- mode(llcp2020$MEDCOST) # Impute missing data
### Factor never as 0
llcp2020$CHECKUP1 <- as.numeric(recode(llcp2020$CHECKUP1, "8=0; 7=NA; 9=NA")) 
llcp2020$CHECKUP1[is.na(llcp2020$CHECKUP1)] <- mode(llcp2020$CHECKUP1) # Impute missing data
### Regardless of type of insurance, as long as there is an insurance, it will be 1 (Yes)
llcp2020$HLTHCVR1 <- as.numeric(recode(llcp2020$HLTHCVR1, "2=1; 3=1; 4=1; 5=1; 6=1; 7=1; 8=2; 77=NA; 99=NA"))
llcp2020$HLTHCVR1[is.na(llcp2020$HLTHCVR1)] <- ifelse(is.na(mode(llcp2020$HLTHCVR1)), 0, mode(llcp2020$HLTHCVR1)) # Impute missing data
llcp2020$EXERANY2 <- as.numeric(recode(llcp2020$EXERANY2, "7=NA; 9=NA"))
llcp2020$EXERANY2[is.na(llcp2020$EXERANY2)] <- mode(llcp2020$EXERANY2) # Impute missing data
llcp2020$SLEPTIM1 <- as.numeric(recode(llcp2020$SLEPTIM1, "77=NA; 99=NA"))
llcp2020$SLEPTIM1[is.na(llcp2020$SLEPTIM1)] <- median(llcp2020$SLEPTIM1, na.rm = TRUE) # Impute missing data
### 1 means Own while 2 means Renting and Other arrangement arrange
llcp2020$RENTHOM1 <- as.numeric(recode(llcp2020$RENTHOM1, "3=2; 7=NA; 9=NA"))
llcp2020$RENTHOM1[is.na(llcp2020$RENTHOM1)] <- mode(llcp2020$RENTHOM1) # Impute missing data
llcp2020$CPDEMO1B <- as.numeric(recode(llcp2020$CPDEMO1B, "7=NA; 8=0; 9=NA"))
llcp2020$CPDEMO1B[is.na(llcp2020$CPDEMO1B)] <- mode(llcp2020$CPDEMO1B) # Impute missing data
### 1 means working, 2 otherwise
llcp2020$EMPLOY1 <- as.numeric(recode(llcp2020$EMPLOY1, "2=1; 3=2; 4=2; 5=2; 6=2; 7=2; 8=2; 9=NA"))
llcp2020$EMPLOY1[is.na(llcp2020$EMPLOY1)] <- mode(llcp2020$EMPLOY1) # Impute missing data
llcp2020$INCOME2 <- as.numeric(recode(llcp2020$INCOME2, "77=NA; 99=NA"))
llcp2020$INCOME2[is.na(llcp2020$INCOME2)] <- mode(llcp2020$INCOME2) # Impute missing data
llcp2020$EDUCA <- as.numeric(recode(llcp2020$EDUCA, "9=NA"))
llcp2020$EDUCA[is.na(llcp2020$EDUCA)] <- mode(llcp2020$EDUCA) # Impute missing data
llcp2020$CHILDREN <- as.numeric(recode(llcp2020$CHILDREN, "88=0; 99=NA"))
llcp2020$CHILDREN[is.na(llcp2020$CHILDREN)] <- median(llcp2020$CHILDREN, na.rm = TRUE) # Impute missing data
### Dependendent variable
llcp2020$ADDEPEV3 <- as.factor(recode(llcp2020$ADDEPEV3, "1='YES'; 2='NO'; 7='UNSURE'; 9='REFUSED'"))

barplot(table(llcp2020$ADDEPEV3))
## Show frequency of mental health risk (dependent variable)
print(paste("Before imputation: ", nrow(llcp2020))); table(llcp2020$ADDEPEV3)
llcp2020$ADDEPEV3 <- as.factor(recode(llcp2020$ADDEPEV3, "'YES'=1; 'NO'=2; 'UNSURE'=NA; 'REFUSED'=NA"))
llcp2020 <- llcp2020[!(is.na(llcp2020$ADDEPEV3)), ] # Drop NA
llcp2020$ADDEPEV3 <- as.factor(recode(llcp2020$ADDEPEV3, "1='YES'; 2='NO'"))
print(paste("After imputation: ", nrow(llcp2020))); table(llcp2020$ADDEPEV3)
## Show and plot correlation matrix
library(Hmisc); library(ggcorrplot)
varcorr.matrix <- rcorr(as.matrix(llcp2020))
ggcorrplot(varcorr.matrix$r, method = "square", type = "lower", lab = TRUE, p.mat = varcorr.matrix$P)

## Rename the factors
# llcp2020$ADDEPEV3 <- as.factor(recode(llcp2020$ADDEPEV3, "1='YES'; 2='NO';"))
### Create training and test llcp2020 dataset
library(caret)
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.
set.seed(100)
index <- createDataPartition(llcp2020$ADDEPEV3, p=0.75, list = F)
trainset <- llcp2020[index, ]
testset <- llcp2020[-index, ]

### Perform downsampling to balance the dataset
### Upsampling takes too much memory and time for processing
set.seed(100)
print("Original trainset: "); table(trainset$ADDEPEV3)
trainset <- downSample(x = trainset[, colnames(trainset) %ni% "ADDEPEV3"], yname = "ADDEPEV3", y = trainset$ADDEPEV3)
print("Downsampled: "); table(trainset$ADDEPEV3)

# Main methodology
## Metrics for logistic regression and KNN
err_metric <- function(tp, tn, fn, fp) {
  print(paste("Accuracy: ", accuracy <- (tp + tn)/(tp + tn + fp + fn)))
  print(paste("Sensitivity: ", sensitivity <- (tp) / (tp + fn)))
  print(paste("Specificity: ", specificity <- (tn) / (tn + fp)))
  print(paste("Precision: ", precision <- (tp) / (tp + fp)))
  print(paste("F-score: ", fscore <- 2 * ((precision * sensitivity) / (precision + sensitivity))))
}

### Naive Bayes
set.seed(100)
modelnb <- train(form = ADDEPEV3 ~ ., data = trainset, method = "naive_bayes", trControl = trainControl(method='cv', number=10))
pred.modelnb <- predict(modelnb, newdata = testset)
confMatrix.modelnb <- confusionMatrix(pred.modelnb, testset$ADDEPEV3)
err_metric(confMatrix.modelnb$table[1,1], confMatrix.modelnb$table[2,2], confMatrix.modelnb$table[1,2], confMatrix.modelnb$table[2,1])
varImp(modelnb, useModel = TRUE, nonpara = TRUE, scale = TRUE)
### K Nearest Neighbors
library(class); library(gmodels)
set.seed(100)
# Parameter tuning: run 10 times
trctrl <- trainControl(method='repeatedcv', number=10, repeats=15)
paramtune <- train(ADDEPEV3 ~ ., data = trainset.cut, method = "knn", trControl=trctrl, preProcess = c("center", "scale"), tuneLength = 10)
trainset$ADDEPEV3 <- as.factor(recode(trainset$ADDEPEV3, "'YES'=1; 'NO'=2"))
testset$ADDEPEV3 <- as.factor(recode(testset$ADDEPEV3, "'YES'=1; 'NO'=2"))
modelknn <- knn(train = trainset, test = testset, cl = trainset$ADDEPEV3, k = 5)
confMatrix.modelknn <- CrossTable(x = testset$ADDEPEV3, y = modelknn, prop.chisq = FALSE)
err_metric(confMatrix.modelknn$t[1,1], confMatrix.modelknn$t[2,2], confMatrix.modelknn$t[1,2], confMatrix.modelknn$t[2,1])
### Decision Tree
library(rpart)
modeldt <- rpart(formula = ADDEPEV3 ~ ., data = trainset, method = "class", control = rpart.control(minsplit=2, minbucket = 1, cp = 0.001))
summary(modeldt)
varImp(modeldt)
library(rpart.plot)
rpart.plot(modeldt)
pred.modeldt <- predict(modeldt, newdata = testset, type = "class")
confMatrix.modeldt <- confusionMatrix(testset$ADDEPEV3, pred.modeldt)
err_metric(confMatrix.modeldt$table[1,1], confMatrix.modeldt$table[2,2], confMatrix.modeldt$table[1,2], confMatrix.modeldt$table[2,1])
### Support Vector Machine
library(e1071)
set.seed(100)
# For SVM and random forest, cut the dataset to 10% of the dataset to make processing quicker
trainset.cut <- trainset[sample(x = 1:nrow(trainset), size = nrow(trainset) * .10, replace = TRUE), colnames(trainset)] 
trainset.cut <- upSample(x = trainset.cut[, colnames(trainset.cut) %ni% "ADDEPEV3"], yname = "ADDEPEV3", y = trainset.cut$ADDEPEV3)
modelsvm <- c()
for (kernel in c("linear", "polynomial", "radial", "sigmoid")) {
  for (cost in c(0.001, 0.01, 0.1, 1, 5, 10, 100)) {
    print(paste(kernel," @ ", cost))
    modelsvm <- svm(ADDEPEV3 ~ ., data = trainset.cut, kernel = kernel, cost = cost, scale = TRUE)
    pred.modelsvm <- predict(modelsvm, testset)
    print(table(predict = pred.modelsvm, truth = testset$ADDEPEV3))
    confMatrix.modelsvm <- confusionMatrix(testset$ADDEPEV3, pred.modelsvm)
    err_metric(confMatrix.modelsvm$table[1,1], confMatrix.modelsvm$table[2,2], confMatrix.modelsvm$table[1,2], confMatrix.modelsvm$table[2,1])
    # modelsvm <- cbind(modelsvm, c(modelsvm))    
  }
}
### Logistic Regression
library(e1071)
modellogit <- glm(ADDEPEV3 ~ ., family = binomial(link = "logit"), data = trainset)
summary(modellogit)
print(cbind("coeff" = modellogit$coefficients, "odds ratio" = (exp(modellogit$coefficients) - 1) * 100)) # Odds ratio
pred.modellogit <- predict(modellogit, newdata = testset[, colnames(testset) %ni% "ADDEPEV3"], type = "response") > 0.5
confMatrix.modellogit <- table(testset$ADDEPEV3, pred.modellogit)
print(confMatrix.modellogit)
err_metric(confMatrix.modellogit[1,1], confMatrix.modellogit[2,2], confMatrix.modellogit[1,2], confMatrix.modellogit[2,1])
### Random Forest
library(randomForest)
set.seed(100)
print(paste("mtry: ", mtries <- sort.int(sample(ncol(trainset)-1, 5))))
randomForestFunction <- function(train, test, n, m, varImp) {
  model <- randomForest(ADDEPEV3 ~ ., data = train, ntree = n, mtry = m)
  pred <- predict(model, newdata = test, type="class")
  confMatrix <- table(test$ADDEPEV3, pred)
  print(model)
  err_metric(confMatrix[1,1], confMatrix[2,2], confMatrix[1,2], confMatrix[2,1])
  if(varImp) {
    print(importance(model))
    varImpPlot(model)
  }
}
# Parameter tuning
for(ntree in c(500, 600, 700, 800)) {
  for(mtry in mtries) {
    print(paste("ntree = ", ntree, " mtry = ", mtry))
    randomForestFunction(trainset.cut, testset, ntree, mtry, FALSE)
  }
}
# Choose the best parameters for the model
ntree <- 800; mtry <- 3
randomForestFunction(trainset.cut, testset, ntree, mtry, TRUE)
