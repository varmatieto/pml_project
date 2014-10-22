# set local directory
setwd ("~/GitHub/pml_project/data")

library(caret, quietly=TRUE)

url_train <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
url_test <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'


if (!file.exists('~/GitHub/pml_project/data/data_train.csv')) {
  download.file(url = url_train, destfile = 'data_train.csv')
}

if (!file.exists('~/GitHub/pml_project/data/data_test.csv')) {
  download.file(url = url_test, destfile = 'data_test.csv')  
}

pml_train <- read.csv(file = '~/GitHub/pml_project/data/data_train.csv',
                      na.strings = c('NA','#DIV/0!',''))
pml_submit <- read.csv(file = '~/GitHub/pml_project/data/data_test.csv',
                     na.strings = c('NA','#DIV/0!',''))

# str(pml_submit)
# str(pml_train)

dim(pml_submit)
dim(pml_train) # 19622   160


# colnames are identical except "problem_id" and "classe"
ct<-colnames(pml_submit)
cd<-colnames(pml_train)
ct[!(ct%in%cd)]
cd[!(cd%in%ct)]



# there are several variables full of NA

colna<-colSums(is.na(pml_submit))
hist(colna)
colnat<-colSums(is.na(pml_train))
hist(colnat)
colna0<-colSums(is.na(pml_submit)) == 0
colna20<-colSums(is.na(pml_submit)) == 20
table(colna0)
table(colna20)
sum(!colna0)

# select significative variables only 

colnames(pml_submit)[colna0]
features <- colnames(pml_submit[colna0])[8:59]

data <- pml_train[,c(features,"classe")]
submit <- pml_submit[,c(features,"problem_id")]

dim(data)
dim(submit)

# summary (data); summary (submit)

plot(data$classe,col=rainbow(5),main = "`classe` frequency plot")

################################################################
set.seed(2204)
inTrain = createDataPartition(data$classe, p = 0.75, list = F)
training = data[inTrain,]
testing = data[-inTrain,]

dim(training)
dim(testing)

############################################à

# optiom zero : on all data

# the model
model_all <- train(classe ~ ., data = training, 
                   method = "rf", 
                   trControl = trainControl(method = "cv", 
                                            number = 4, 
                                            allowParallel = TRUE, 
                                            verboseIter = TRUE))
model_all$finalModel


#  out of sample error to be estimated
confusionMatrix(testing$classe,predict(model_all,testing))




# prediction model to predict 20 different test cases
submit_a<-predict(model_all,submit)
submit_a

submita<- as.character (submit_a)


# optiom one :identify key predictors 

outcome = which(names(training) == "classe")
highCorrCols = findCorrelation(abs(cor(training[,-outcome])),0.90)
highCorrFeatures = names(training)[highCorrCols]
highCorrFeatures
trainingh = training[,-highCorrCols]
outcome = which(names(trainingh) == "classe")

dim(trainingh)

library (randomForest) 

fsRF = randomForest(trainingh[,-outcome], trainingh[,outcome], importance = T)
rfImp = data.frame(fsRF$importance)
dim (rfImp)
str (rfImp)
impFeatures = order(-rfImp$MeanDecreaseGini)
inImp = createDataPartition(data$classe, p = 0.05, list = F)
dim (inImp)
featurePlot(trainingh[inImp,impFeatures[1:4]],trainingh$classe[inImp], plot = "pairs")
colnames (trainingh)[impFeatures[1:4]]


ctrlKNN = trainControl(method = "adaptive_cv")
modelKNN = train(classe ~ ., trainingh, method = "knn", trControl = ctrlKNN)
ctrlRF = trainControl(method = "oob")
modelRF = train(classe ~ ., trainingh, method = "rf", ntree = 200, trControl = ctrlRF)
resultsKNN = data.frame(modelKNN$results)
resultsRF = data.frame(modelRF$results)

fitKNN = predict(modelKNN, testing)
fitRF = predict(modelRF, testing)

confusionMatrix(testing$classe,fitKNN)
confusionMatrix(testing$classe,fitRF)


submit_h<-predict(modelRF,submit)
submit_h



# optiom two : PCA

preProc<-preProcess(training[,-53],method="pca")

trainPC<-predict(preProc,training[,-53])
dim(trainPC)

modelFit<-train(training$classe~.,
                method="rf",data=trainPC,trControl=trainControl(method="cv"))
modelFit$finalModel

validatePC<-predict(preProc,testing[,-53])
confusionMatrix(testing$classe,predict(modelFit,validatePC))

pre_submitPC<-predict(preProc,submit[,-53])
submit_PC<-predict(modelFit,pre_submitPC)
submit_PC


########################################
submitPC <- as.character (submit_PC)
submith <- as.character (submit_h)
str(submitPC)

c(1:20) [!submitPC==submith]
submitPC[11]
submith[11]


