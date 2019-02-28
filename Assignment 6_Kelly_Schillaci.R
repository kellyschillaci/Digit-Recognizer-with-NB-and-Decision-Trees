library(readr)
filename="Kaggle-digit-train-sample-small-1400.csv"
Kaggle_digit_train_sample_small_1400 <- read.csv(filename, header = TRUE, stringsAsFactors = TRUE)
dim(Kaggle_digit_train_sample_small_1400)
str(Kaggle_digit_train_sample_small_1400)
anyNA(Kaggle_digit_train_sample_small_1400)
sum(duplicated(Kaggle_digit_train_sample_small_1400))
table(Kaggle_digit_train_sample_small_1400$label)

Kaggle_digit_train_sample_small_1400$label=as.factor(Kaggle_digit_train_sample_small_1400$label)

(every4_rows<-seq(1,nrow(Kaggle_digit_train_sample_small_1400),4))

Digit_Test=Kaggle_digit_train_sample_small_1400[every4_rows, ]
Digit_Train=Kaggle_digit_train_sample_small_1400[-every4_rows, ]
## View the created Test and Train sets
(head(Digit_Train))
(table(Digit_Test$label))
(table(Digit_Train$label))

dim(Digit_Train)
plot(Digit_Train$label, col="purple")

dim(Digit_Test)
plot(Digit_Test$label, col="red")


(head(Digit_Test))
Digit_Test_noLabel<-Digit_Test[-c(1)]
Digit_Test_justLabel<-Digit_Test$label
(head(Digit_Test_noLabel))

library(e1071)
install.packages("naivebayes")
library(naivebayes)


NB_e1071<-naiveBayes(label~., data=Digit_Train, laplace = 3,na.action = na.pass, type="raw")
NB_e1071_Pred <- predict(NB_e1071, Digit_Test_noLabel)
plot(NB_e1071_Pred, col="blue")

table(NB_e1071_Pred,Digit_Test_justLabel)
confusionMatrix(NB_e1071_Pred,Digit_Test_justLabel)

install.packages("mlr")
#Loading the library
library(mlr)
#Create a classification task for learning on Dataset and specify the target feature to try to make the model better
task = makeClassifTask(data = Digit_Train, target = "label")
#Initialize the Naive Bayes classifier
selected_model = makeLearner("classif.naiveBayes")
#Train the model
NB_mlr = train(selected_model, task)

NB_mlr$learner.model
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = Digit_Test_noLabel))
table(predictions_mlr[,1],Digit_Test_justLabel)



NB_object<- naive_bayes(label~., data=Digit_Train)
NB_prediction<-predict(NB_object, Digit_Test_noLabel, type = c("class"))
head(predict(NB_object, Digit_Test_noLabel, type = "prob"))
table(NB_prediction,Digit_Test_justLabel)
confusionMatrix(NB_prediction,Digit_Test_justLabel)
plot(NB_prediction, col='Orange')

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(Cairo)

fit <- rpart(Digit_Train$label ~ ., data = Digit_Train, method="class")
summary(fit)
predicted= predict(fit,Digit_Test_noLabel, type="class")
(head(predicted,n=10))
(head(CleanTest, n=10))
plot(fit)
text(fit)
fancyRpartPlot(fit)
table(predicted, Digit_Test_justLabel)
confusionMatrix(predicted, Digit_Test_justLabel)
plot(predicted, col="green")
