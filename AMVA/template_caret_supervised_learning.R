# Caret - supervised learning ---------------------------------------------
# https://quantdev.ssri.psu.edu/tutorials/supervised-machine-learning-caret-package
#Load the Package
library("caret")

#Load the Package
library("mlbench")

# Iris  Database
data(iris)
dim(iris)
levels(iris$Species)

head(iris)

#Plot Iris dataset
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "ellipse",
            ## Add a key at the top
            auto.key = list(columns = 3))

#Plot Iris dataset
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "ellipse",
            ## Add a key at the top
            auto.key = list(columns = 3))


#Split data set           
inTrain<-createDataPartition(y=iris$Species, p=0.75, list=FALSE)

#Create Training  and Testing Sets
training.Iris<-iris[inTrain,]
testing.Iris<-iris[-inTrain,]

dim(training.Iris)
dim(testing.Iris)

boxplot(training.Iris[, -5], main="Raw Data")

#Center and Scale all varialbes (not class) == Standardizing
preObj<-preProcess(training.Iris[,-5], method = c("center", "scale"))
preObjData<-predict(preObj,training.Iris[,-5])
boxplot(preObjData, main="Normalized data" )

set.seed(123456)

#Linear Discriminant Analysis (LDA)
#preProcess option allows to  center and scale the data 
modelFit<-train(Species~., data=training.Iris, 
                preProcess=c("center", "scale"), method="lda")

modelFit$finalModel

#Predict new data with model fitted
predictions<-predict(modelFit, newdata=testing.Iris)

#Shows Confusion Matrix and performance metrics
confusionMatrix(predictions, testing.Iris$Species)

#K-folds
folds<-createFolds(y=training.Iris$Species, k=10, list=T)
sapply(folds, length)
folds[[1]] 

#Bootstrapping
Resamples<-createResample(y=training.Iris$Species, time=10, list=T)
sapply(Resamples, length)
Resamples[[1]][1:20]    

# Cross-Validation
kfoldcv <- trainControl(method="cv", number=10)
performance_metric <- "Accuracy"
set.seed(1234)

#Linear Discriminant Analysis (LDA)
lda.iris <- train(Species~., data=iris, method="lda", 
                  metric=performance_metric, 
                  trControl=kfoldcv,preProcess=c("center", "scale"))

#Classification and Regression Trees (CART)
cart.iris <- train(Species~., data=iris, method="rpart", 
                   metric=performance_metric, 
                   trControl=kfoldcv,preProcess=c("center", "scale"))

#Support Vector Machines (SVM)
svm.iris <- train(Species~., data=iris, method="svmRadial", 
                  metric=performance_metric, 
                  trControl=kfoldcv,preProcess=c("center", "scale"))

# Random Forest
rf.iris <- train(Species~., data=iris, method="rf", 
                 metric=performance_metric, 
                 trControl=kfoldcv,preProcess=c("center", "scale"))

# Accuracy Summary of Iris Dataset
results.iris <- resamples(list(lda=lda.iris, cart=cart.iris,  
                               svm=svm.iris, rf=rf.iris))


summary(results.iris)
dotplot(results.iris)

# Parameter Tuning

control <- trainControl(method="repeatedcv", number=10, repeats=3, 
                        search="grid")
tunegrid <- expand.grid(mtry=c(1:4))
rf_gridsearch <- train(Species~., data=iris, method="rf", 
                       metric=performance_metric, 
                       tuneGrid=tunegrid, 
                       trControl=control,preProcess=c("center", "scale"))
print(rf_gridsearch)

plot(rf_gridsearch)

