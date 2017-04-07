project_data <- read.csv("~/data.train.csv")

#### Exploratory Analysis ####

head(project_data)
tail(project_data)

str(project_data)

summary(project_data)

library(corrplot)

corrplot(cor(project_data[,-21]), method = "number", number.cex = 0.5)

#### Logistic Regresion ####

library(boot)
library(mgcv)

randomized_data <- project_data[sample(nrow(project_data),nrow(project_data)),]

randomized_data$label <- as.factor(randomized_data$label)

feats <- names(randomized_data[,-c(1,21)])
f <- paste(feats,collapse=' + ')
f <- paste('label ~',f,' + s(meanfreq)')
f <- as.formula(f)

lrfit.0 <- glm(label~. + log(mindom) + log(maxdom) +  log(centroid) ,data=randomized_data, family = binomial)

cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
cv.glm(randomized_data,lrfit.0,K=nrow(randomized_data),cost=cost)$delta[1]

#### NEURAL NETWORk ####

library(neuralnet)

maxs <- apply(X = project_data[,1:20], MARGIN = 2, FUN = max)
mins <- apply(X = project_data[,1:20], MARGIN = 2, FUN = min)

scaled.project_data <- as.data.frame(scale(project_data[,1:20], center = mins, scale = maxs - mins))

gender <- as.integer(as.factor(project_data$label))-1

scaled.project_data <- cbind(gender, scaled.project_data)

feats <- names(scaled.project_data[,-1])
f <- paste(feats,collapse=' + ')
f <- paste('gender ~',f)
f <- as.formula(f)

# Loop to check the best number of neurons (~15 minutes)

randomized_data <- scaled.project_data[sample(nrow(scaled.project_data),nrow(scaled.project_data)),]

error_mat <- matrix(0, nrow = 10, ncol = 8)
inicio <- 1
fin <- 268
for(i in 1:10){
  index_test <- seq(from = inicio, to = fin, by = 1)
  temp_train <- randomized_data[-index_test,]
  temp_test <- randomized_data[index_test,]
  for (j in 8:15) {
  nn <- neuralnet(f, temp_train, hidden = c(j), linear.output = FALSE)
  predicted.nn.values <- ifelse(compute(nn, temp_test[,2:21])$net.result > 0.5, 1, 0)
  error_mat[i,j-7] <- sum(abs(predicted.nn.values - temp_test$gender))
  }
 inicio <- inicio + 268
 fin <- fin + 268
}

apply(error_mat,2,mean)

# The best number of neurons appears to be 12 with an error of 6.8 (2.53%)

# Loop to see the best number of hidden layers (~6 min)

randomized_data <- scaled.project_data[sample(nrow(scaled.project_data),nrow(scaled.project_data)),]

error_mat <- matrix(0, nrow = 10, ncol = 4)
inicio <- 1
fin <- 268
for(i in 1:10){
  index_test <- seq(from = inicio, to = fin, by = 1)
  temp_train <- randomized_data[-index_test,]
  temp_test <- randomized_data[index_test,]
  
  nn0 <- neuralnet(f, temp_train, hidden = 0, linear.output = FALSE)
  nn1 <- neuralnet(f, temp_train, hidden = c(12), linear.output = FALSE)
  nn2 <- neuralnet(f, temp_train, hidden = c(12,12), linear.output = FALSE)
  nn3 <- neuralnet(f, temp_train, hidden = c(12,12,12), linear.output = FALSE)
  
  predicted.nn0.values <- ifelse(compute(nn0, temp_test[,2:21])$net.result > 0.5, 1, 0)
  predicted.nn1.values <- ifelse(compute(nn1, temp_test[,2:21])$net.result > 0.5, 1, 0)
  predicted.nn2.values <- ifelse(compute(nn2, temp_test[,2:21])$net.result > 0.5, 1, 0)
  predicted.nn3.values <- ifelse(compute(nn3, temp_test[,2:21])$net.result > 0.5, 1, 0)
  
  error_mat[i,1] <- sum(abs(predicted.nn0.values - temp_test$gender))
  error_mat[i,2] <- sum(abs(predicted.nn1.values - temp_test$gender))
  error_mat[i,3] <- sum(abs(predicted.nn2.values - temp_test$gender))
  error_mat[i,4] <- sum(abs(predicted.nn3.values - temp_test$gender))
  
  inicio <- inicio + 268
  fin <- fin + 268
}

apply(error_mat,2,mean)

# The best NN has one hidden layer wiht 12 neruons (error of 7 2.61%)

final_nn_error_vec <- rep(0,20)
inicio <- 1
fin <- 139
for(i in 1:20){
  index_test <- seq(from = inicio, to = fin, by = 1)
  temp_train <- randomized_data[-index_test,]
  temp_test <- randomized_data[index_test,]
  nn <- neuralnet(f, temp_train, hidden = c(12), linear.output = FALSE)
  predicted.nn.values <- ifelse(compute(nn, temp_test[,2:21])$net.result > 0.5, 1, 0)
  final_nn_error_vec[i] <- sum(abs(predicted.nn.values - temp_test$gender))
  inicio <- inicio + 139
  fin <- fin + 139
}

summary(final_nn_error_vec)

#### SVM ####

library(e1071)

randomized_data <- project_data[sample(nrow(project_data),nrow(project_data)),]

randomized_data$label <- as.factor(randomized_data$label)

cost <- c(1,2,3,4,5,6,7,8,9,10)

final_svm_error_mat <- matrix(0,nrow=20, ncol=10)
inicio <- 1
fin <- 134
for(i in 1:20){
  index_test <- seq(from = inicio, to = fin, by = 1)
  temp_train <- randomized_data[-index_test,]
  temp_test <- randomized_data[index_test,]
  for (j in cost){
    SVM.rad <- svm(label ~ ., data = temp_train, kernel="radial", cost=j, scale = TRUE)
    predicted.svm.values <- predict(SVM.rad, newdata = temp_test)
    final_svm_error_mat[i,j] <- sum(abs(as.numeric(as.factor(predicted.svm.values)) - as.numeric(temp_test$label)))
  }
  inicio <- inicio + 134
  fin <- fin + 134
}

apply(final_svm_error_mat,2,mean)

# the best is a radial kernel and a cost of 2-3

#### FOREST ####

library(tree)

project_data$label <- as.factor(project_data$label)

split = sample.split(project_data$label, SplitRatio = 0.70)
project_data_train = subset(project_data, split == TRUE)
project_data_test = subset(project_data, split == FALSE)

tree.0 <- tree(label ~ ., project_data_train)

summary(tree.0)
plot(tree.0)
text(tree.0, pretty = 0, cex=0.5)

tree.pred=predict(tree.0, project_data_test, type = "class")

table(tree.pred ,project_data_test$label)

cv_data <- cv.tree(tree.0, FUN=prune.misclass)

par(mfrow =c(1,2))
plot(cv_data$size ,cv_data$dev ,type="b")
plot(cv_data$k ,cv_data$dev ,type="b")

par(mfrow =c(1,1))
prune.tree.0 =prune.misclass (tree.0 ,best =3)
plot(prune.tree.0)
text(prune.tree.0 ,pretty =0, cex=0.5)

prune.tree.0.pred <- predict(prune.tree.0, project_data_test, type="class")
table(tree.pred ,project_data_test$label)

# The following code builds a Random Forest and uses bagging

library(randomForest)

bag.tree.0 <- randomForest(label ~ ., 
                           data = project_data_train,
                           mtry=4, 
                           importance=TRUE,
                           ntree =100)

bag.tree.0.pred <- predict(bag.tree.0, newdata = project_data_test)

table(bag.tree.0.pred ,project_data_test$label)

# the Random Forrest has an error of ~2.36%

randomized_data <- project_data[sample(nrow(project_data),nrow(project_data)),]

randomized_data$label <- as.factor(randomized_data$label)

final_nn_error_mat <- matrix(0,nrow=40, ncol=4)
inicio <- 1
fin <- 67
for(i in 1:40){
  index_test <- seq(from = inicio, to = fin, by = 1)
  temp_train <- randomized_data[-index_test,]
  temp_test <- randomized_data[index_test,]
  for (j in 1:4){
    bag.tree.0 <- randomForest(label ~ ., data = temp_train, mtry = j + 3, importance=TRUE, ntree =100)
    predicted.rf.values <- predict(bag.tree.0, newdata = temp_test)
    final_nn_error_mat[i,j] <- sum(abs(as.numeric(as.factor(predicted.rf.values)) - as.numeric(temp_test$label)))
    }
  inicio <- inicio + 67
  fin <- fin + 67
}

apply(final_nn_error_mat,2,mean)

#### Ensemble ####

library(caret)
library(caretEnsemble)
library(kernlab)
library(caTools)

data_test <- read_csv("~/data.test.csv")

randomized_data <- project_data[sample(nrow(project_data),nrow(project_data)),]

randomized_data$label <- as.factor(randomized_data$label)

my_control <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",
  classProbs=TRUE,
  index=createResample(randomized_data$label, 25),
  summaryFunction=twoClassSummary
)

model_list_big <- caretList(
  label~., data=randomized_data,
  trControl = my_control,
  metric="ROC",
  tuneList=list(
    rf=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=3),  preProcess = c('center', 'scale')),
    svmRadial=caretModelSpec(method="svmRadial", tuneGrid=data.frame(.sigma=0.05541684,.C=1), preProcess = c('center', 'scale')),
    nnet=caretModelSpec(method="nnet", tuneGrid=data.frame(.decay=0.1,.size=16), preProcess = c('center', 'scale'))
  )
)

linear.ensemble <- caretEnsemble(model_list_big, metric="ROC",
                                 trControl=trainControl(
                                   number=2,
                                   summaryFunction=twoClassSummary,
                                   classProbs=TRUE))

summary(linear.ensemble)

ens_preds <- as.character(predict(linear.ensemble, data_test, type="raw"))

write(ens_preds,file="output.txt")  
