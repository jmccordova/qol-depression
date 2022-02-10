# Pre-processing

## Install important libraries
install.packages(c("foreign", "car", "caret","class", "gmodels", "rpart", "rpart.plot", "e1071", "MASS", "ggplot2", "reshape2", "GGally", "randomForest"), dependencies = TRUE, lib = "./R")

## Declare mode function
## This is important as we need to impute categorical variables that are missing
mode <- function(v){
  v = v[nchar(as.character(v))>0]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## These are the variables that represent Quality of Life
test_cols <- c("GENHLTH", "HLTHPLN1", "PERSDOC2", "MEDCOST", "CHECKUP1", "HLTHCVR1", "RENTHOM1", "CPDEMO1B", "EMPLOY1", "INCOME2", "EDUCA", "CHILDREN", "EXERANY2", "X_PHYS14D")

## Parse the dataset and store it in a variable
library("foreign")
llcp2020 <-  read.xport("./LLCP2020.XPT")
## Show the structure of the dataset
str(llcp2020)
head(llcp2020)
## Remove columns that will not be tested
llcp2020 <- subset(llcp2020, select = c(GENHLTH, HLTHPLN1, PERSDOC2, MEDCOST, CHECKUP1, HLTHCVR1, RENTHOM1, CPDEMO1B, EMPLOY1, INCOME2, EDUCA, CHILDREN, EXERANY2, X_PHYS14D, X_MENT14D))
## Set factors that means "unsure" and "refused" to NA.
## Rather than setting them to factor, we will put them as numeric. Always use the codebook as reference.
library("car")
llcp2020$GENHLTH <- as.numeric(recode(llcp2020$GENHLTH, "7=NA; 9=NA"))
llcp2020$GENHLTH[is.na(llcp2020$GENHLTH)] <- mode(llcp2020$GENHLTH) # Impute missing data
llcp2020$HLTHPLN1 <- as.numeric(recode(llcp2020$HLTHPLN1, "7=NA; 9=NA"))
llcp2020$HLTHPLN1[is.na(llcp2020$HLTHPLN1)] <- mode(llcp2020$HLTHPLN1) # Impute missing data
llcp2020$PERSDOC2 <- as.numeric(recode(llcp2020$PERSDOC2, "1=2; 2=2; 3=1; 7=NA; 9=NA"))
llcp2020$PERSDOC2[is.na(llcp2020$PERSDOC2)] <- mode(llcp2020$PERSDOC2) # Impute missing data
### Combine 1 (Yes, only one) and 2 (Yes, more than one) as 1 (Yes); change 3 (No) to 2
llcp2020$MEDCOST <- as.numeric(recode(llcp2020$MEDCOST, "7=NA; 9=NA"))
llcp2020$MEDCOST[is.na(llcp2020$MEDCOST)] <- mode(llcp2020$MEDCOST) # Impute missing data
### Factor never as 0
llcp2020$CHECKUP1 <- as.numeric(recode(llcp2020$CHECKUP1, "8=0; 7=NA; 9=NA")) 
llcp2020$CHECKUP1[is.na(llcp2020$CHECKUP1)] <- mode(llcp2020$CHECKUP1) # Impute missing data
### Regardless of type of insurance, as long as there is an insurance, it will be 1 (Yes)
llcp2020$HLTHCVR1 <- as.numeric(recode(llcp2020$HLTHCVR1, "2=1; 3=1; 4=1; 5=1; 6=1; 7=1; 8=2; 77=NA; 99=NA"))
llcp2020$HLTHCVR1[is.na(llcp2020$HLTHCVR1)] <- ifelse(is.na(mode(llcp2020$HLTHCVR1)), 0, mode(llcp2020$HLTHCVR1)) # Impute missing data
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
llcp2020$EXERANY2 <- as.numeric(recode(llcp2020$EXERANY2, "7=NA; 9=NA"))
llcp2020$EXERANY2[is.na(llcp2020$EXERANY2)] <- mode(llcp2020$EXERANY2) # Impute missing data
### Do a binomial factor
## 1 means GOOD while 2 means NOT GOOD
llcp2020$X_PHYS14D <- as.numeric(recode(llcp2020$X_PHYS14D, "1=1; 2=1; 3=2; 9=NA"))
llcp2020$X_PHYS14D[is.na(llcp2020$X_PHYS14D)] <- mode(llcp2020$X_PHYS14D) # Impute missing data
## Create two separate instances of dataset: binomial factors and multinomial factors
### 1 means 0-13 day when mental health not good -> NORMAL
### 3 means 14+ days when mental health not good -> RISK (based on DSM V)
ment14d.binomial <- as.factor(recode(llcp2020$X_MENT14D, "1='NORMAL'; 2='NORMAL'; 3='RISK'; 9=NA"))
ment14d.binomial.ord <- ordered(ment14d.binomial)
llcp2020.binomial <- llcp2020
llcp2020.binomial$X_MENT14D <- ment14d.binomial
barplot(table(llcp2020.binomial$X_MENT14D))
## Show frequency of mental health risk (dependent variable)
print(paste("Before imputation: ", nrow(llcp2020.binomial))); table(llcp2020.binomial$X_MENT14D)
llcp2020.binomial <- llcp2020.binomial[!(is.na(llcp2020.binomial$X_MENT14D)), ]  # Impute missing values
## Show frequency of mental health risk (dependent variable)
print(paste("After imputation: ", nrow(llcp2020.binomial))); table(llcp2020.binomial$X_MENT14D)
### 1 means 0 day when mental health not good -> GOOD
### 2 means 1-13 days when mental health not good -> NORMAL
### 3 means 14+ days when mental health not good -> RISK (based on DSM V)
ment14d.multinomial <- as.factor(recode(llcp2020$X_MENT14D, "1='GOOD'; 2='NORMAL'; 3='RISK'; 9=NA"))
ment14d.multinomial.ord <- ordered(ment14d.multinomial)
llcp2020.multinomial <- llcp2020
llcp2020.multinomial$X_MENT14D <- ment14d.multinomial
barplot(table(llcp2020.multinomial$X_MENT14D))
## Show frequency of mental health risk (dependent variable)
print(paste("Before imputation: ", nrow(llcp2020.multinomial))); table(llcp2020.multinomial$X_MENT14D)
llcp2020.multinomial <- llcp2020.multinomial[!(is.na(llcp2020.multinomial$X_MENT14D)), ]  # Impute missing values
## Show frequency of mental health risk (dependent variable)
print(paste("After imputation: ", nrow(llcp2020.multinomial))); table(llcp2020.multinomial$X_MENT14D)

## Show and plot correlation matrix
library(ggplot2); library(reshape2)
d <-ggplot(data = melt(cor(llcp2020)), aes(x=Var1, y=Var2, fill=value))
d +geom_tile()
## Check for multicollinearity
library(GGally)
ggpairs(llcp2020[, test_cols])

## For binomial
### Create training and test llcp2020 dataset
library(caret)
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.
set.seed(100)
index.binomial <- createDataPartition(llcp2020.binomial$X_MENT14D, p=0.75, list = F)
trainset.binomial <- llcp2020.binomial[index.binomial, ]
testset.binomial <- llcp2020.binomial[-index.binomial, ]

### Perform downsampling to balance the dataset
### Upsampling takes too much memory and time for processing
set.seed(100)
trainset.binomial <- downSample(x = trainset.binomial[, colnames(trainset.binomial) %ni% "X_MENT14D"], yname = "X_MENT14D", y = trainset.binomial$X_MENT14D)
print("Downsampled: "); table(trainset.binomial$X_MENT14D)

# Main methodology
## For binomial
# Metrics for logistic regression and KNN
err_metric <- function(tp, tn, fn, fp) {
  print(paste("Accuracy: ", accuracy <- (tp + tn)/(tp + tn + fp + fn)))
  print(paste("Sensitivity: ", sensitivity <- (tp) / (tp + fn)))
  print(paste("Specificity: ", specificity <- (tn) / (tn + fp)))
  print(paste("Precision: ", precision <- (tp) / (tp + fp)))
  print(paste("F-score: ", fscore <- 2 * ((precision * sensitivity) / (precision + sensitivity))))
}
### Naive Bayes
set.seed(100)
modelnb.binomial <- train(form = X_MENT14D ~ ., data = trainset.binomial, method = "naive_bayes", trControl = trainControl(method='cv', number=10))
pred.modelnb.binomial <- predict(modelnb.binomial, newdata = testset.binomial)
confusionMatrix(pred.modelnb.binomial, testset.binomial$X_MENT14D)
varImp(modelnb.binomial, useModel = TRUE, nonpara = TRUE, scale = TRUE)
### K Nearest Neighbors
library(class); library(gmodels)
set.seed(100)
#trctrl <- 
#modelknn.binomial <- train(X_MENT14D ~ ., data = trainset.binomial, method = "knn", trControl=trctrl, preProcess = c("center", "scale"), tuneLength = 10)
modelknn.binomial <- knn(train = trainset.binomial, test = testset.binomial, cl = trainset.binomial$X_MENT14D, k=23)
CrossTable(x = testset.binomial$X_MENT14D, y = modelknn.binomial, prop.chisq = FALSE)
### Decision Tree
library(rpart)
modeldt.binomial <- rpart(formula = X_MENT14D ~ ., data = trainset.binomial, method = "class")
summary(modeldt.binomial)
varImp(modeldt.binomial)
library(rpart.plot)
rpart.plot(modeldt.binomial)
pred.modeldt.binomial <- predict(modeldt.binomial, newdata = testset.binomial, type = "class")
confusionMatrix(testset.binomial$X_MENT14D, pred.modeldt.binomial)
### Support Vector Machine
library(e1071)
set.seed(100)
# For SVM, cut the dataset to 100 of the dataset to make processing quicker
svmdataset <- trainset.binomial[sample(x = 1:nrow(trainset.binomial), size = nrow(trainset.binomial) * .10, replace = TRUE), colnames(trainset.binomial)] 
svmdataset <- upSample(x = svmdataset[, colnames(svmdataset) %ni% "X_MENT14D"], yname = "X_MENT14D", y = svmdataset$X_MENT14D)
modelsvm.binomial <- c()
for (kernel in c("linear", "polynomial", "radial", "sigmoid")) {
  for (cost in c(0.001, 0.01, 0.1, 1, 5, 10, 100)) {
    print(paste(kernel," @ ", cost))
    modelsvm <- svm(X_MENT14D ~ ., data = svmdataset, kernel = kernel, cost = cost, scale = TRUE)
    pred.modelsvm <- predict(modelsvm, testset.binomial)
    print(table(predict = pred.modelsvm, truth = testset.binomial$X_MENT14D))
    print(confusionMatrix(testset.binomial$X_MENT14D, pred.modelsvm))
    # modelsvm.binomial <- cbind(modelsvm.binomial, c(modelsvm))    
  }
}
### Logistic Regression
library(e1071)
trainset.binomial$X_MENT14D <- ordered(trainset.binomial$X_MENT14D)
testset.binomial$X_MENT14D <- ment14d.binomial.ord
modellogit.binomial <- glm(X_MENT14D ~ ., family = binomial(link = "logit"), data = trainset.binomial)
summary(modellogit.binomial)
print(cbind("coeff" = modellogit.binomial$coefficients, "odds ratio" = (exp(modellogit.binomial$coefficients) - 1) * 100)) # Odds ratio
pred.modellogit.binomial <- predict(modellogit.binomial, newdata = testset.binomial[, colnames(svmdataset) %ni% "X_MENT14D"], type = "response") > 0.5
confMatrix.modellogit.binomial <- table(testset.binomial$X_MENT14D, pred.modellogit.binomial)
print(confMatrix.modellogit.binomial)
err_metric(confMatrix.modellogit.binomial[1,1], confMatrix.modellogit.binomial[2,2], confMatrix.modellogit.binomial[1,2], confMatrix.modellogit.binomial[2,1])
### Random Forest
library(randomForest)
set.seed(100)
print(paste("mtry: ", mtries <- sort.int(sample(ncol(trainset.binomial)-1, 5))))
randomForestFunction <- function(train, test, n, m, varImp) {
  model <- randomForest(X_MENT14D ~ ., data = train, ntree = n, mtry = m)
  print(model)
  pred <- predict(model, newdata = test, type="class")
  table(test$X_MENT14D, pred)
  print(paste("Accuracy: ", mean(pred == test$X_MENT14D)))
  if(varImp) {
    print(importance(model))
    varImpPlot(model)
  }
}
# Parameter tuning
for(ntree in c(500, 600, 700, 800)) {
  for(mtry in mtries) {
    print(paste("ntree = ", ntree, " mtry = ", mtry))
    randomForestFunction(trainset.binomial, testset.binomial, ntree, mtry, FALSE)
  }
}
# Choose the best parameters for the model
ntree <- 600; mtry <- 2
randomForestFunction(trainset.binomial, testset.binomial, ntree, mtry, TRUE)


## For multinomial
### Create training and test llcp2020 dataset
library(caret)
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.
set.seed(100)
index.multinomial <- createDataPartition(llcp2020.multinomial$X_MENT14D, p=0.75, list = F)
trainset.multinomial <- llcp2020.multinomial[index.multinomial, ]
testset.multinomial <- llcp2020.multinomial[-index.multinomial, ]

### Perform downsampling to balance the dataset
### Upsampling takes too much memory and time for processing
set.seed(100)
trainset.multinomial <- downSample(x = trainset.multinomial[, colnames(trainset.multinomial) %ni% "X_MENT14D"], yname = "X_MENT14D", y = trainset.multinomial$X_MENT14D)
print("Downsampled: "); table(trainset.multinomial$X_MENT14D)
## For multinomial
### Naive Bayes
set.seed(100)
modelnb.multinomial <- train(form = X_MENT14D ~ ., data = trainset.multinomial, method = "naive_bayes", trControl = trainControl(method='cv', number=10))
pred.modelnb.multinomial <- predict(modelnb.multinomial, newdata = testset.multinomial)
confusionMatrix(pred.modelnb.multinomial, testset.multinomial$X_MENT14D)
varImp(modelnb.multinomial, useModel = TRUE, nonpara = TRUE, scale = TRUE)
### K Nearest Neighbors
library(class); library(gmodels)
set.seed(100)
# Split the dataset to smaller chunks
knndataset <- trainset.multinomial[sample(x = 1:nrow(trainset.multinomial), size = nrow(trainset.multinomial) * .10, replace = TRUE), colnames(trainset.multinomial)] 
knndataset <- upSample(x = knndataset[, colnames(knndataset) %ni% "X_MENT14D"], yname = "X_MENT14D", y = knndataset$X_MENT14D)
modelknn.multinomial <- knn(train = knndataset, test = testset.multinomial, cl = knndataset$X_MENT14D, k = 23, use.all = TRUE)
CrossTable(x = testset.multinomial$X_MENT14D, y = modelknn.multinomial, prop.chisq = FALSE)
### Decision Tree
library(rpart)
modeldt.multinomial <- rpart(formula = X_MENT14D ~ ., data = trainset.multinomial, method = "class")
summary(modeldt.multinomial)
varImp(modeldt.multinomial)
library(rpart.plot)
rpart.plot(modeldt.multinomial)
pred.modeldt.multinomial <- predict(modeldt.multinomial, newdata = testset.multinomial, type = "class")
confusionMatrix(testset.multinomial$X_MENT14D, pred.modeldt.multinomial)
### Support Vector Machine
library(e1071)
set.seed(100)
# For SVM, cut the dataset to 100 of the dataset to make processing quicker
svmdataset <- trainset.multinomial[sample(x = 1:nrow(trainset.multinomial), size = nrow(trainset.multinomial) * .10, replace = TRUE), colnames(trainset.multinomial)] 
svmdataset <- upSample(x = svmdataset[, colnames(svmdataset) %ni% "X_MENT14D"], yname = "X_MENT14D", y = svmdataset$X_MENT14D)
modelsvm.multinomial <- c()
for (kernel in c("linear", "polynomial", "radial", "sigmoid")) {
  for (cost in c(0.001, 0.01, 0.1, 1, 5, 10, 100)) {
    print(paste(kernel," @ ", cost))
    modelsvm <- svm(X_MENT14D ~ ., data = svmdataset, kernel = kernel, cost = cost, scale = TRUE)
    pred.modelsvm <- predict(modelsvm, testset.multinomial)
    print(table(predict = pred.modelsvm, truth = testset.multinomial$X_MENT14D))
    print(confusionMatrix(testset.multinomial$X_MENT14D, pred.modelsvm))
    # modelsvm.multinomial <- cbind(modelsvm.multinomial, c(modelsvm))    
  }
}
### Logistic Regression
library(MASS)
trainset.multinomial$X_MENT14D <- ordered(trainset.multinomial$X_MENT14D)
testset.multinomial$X_MENT14D <- ment14d.multinomial.ord
modellogit.multinomial <- polr(X_MENT14D ~ ., data = trainset.multinomial, Hess = TRUE)
print(summary(modellogit.multinomial))
ctable <- coef(summary(modellogit.multinomial))
p <- round(pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2, digits = 4)
ctable <- cbind(ctable, "odds ratio" = ((exp(ctable[, "Value"]) - 1) * 100), "p value" = p)
print(ctable)
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

print(s <- with(trainset.multinomial, summary(as.numeric(X_MENT14D) ~ ., fun=sf)))
pred.modellogit.multinomial <- predict(modellogit.multinomial, newdata = testset.multinomial[, colnames(svmdataset) %ni% "X_MENT14D"])
# Metrics for logistic regression
confMatrix.modellogit.multinomial <- table(testset.multinomial$X_MENT14D, pred.modellogit.multinomial)
print(confMatrix.modellogit.multinomial)
print(paste("Misclassification error: ", mean(as.character(testset.multinomial$X_MENT14D) != as.character(pred.modellogit.multinomial))))
### Random Forest
library(randomForest)
set.seed(100)
# Parameter tuning
rfdataset <- trainset.multinomial[sample(x = 1:nrow(trainset.multinomial), size = nrow(trainset.multinomial) * .10, replace = TRUE), colnames(trainset.multinomial)] 
rfdataset <- upSample(x = rfdataset[, colnames(rfdataset) %ni% "X_MENT14D"], yname = "X_MENT14D", y = rfdataset$X_MENT14D)
for(ntree in c(500, 600, 700, 800)) {
  for(mtry in mtries) {
    print(paste("ntree = ", ntree, " mtry = ", mtry))
    randomForestFunction(rfdataset, testset.multinomial, ntree, mtry, FALSE)
  }
}
# Choose the best parameters for the model
ntree <- 500; mtry <- 2
randomForestFunction(rfdataset, testset.multinomial, ntree, mtry, TRUE)
