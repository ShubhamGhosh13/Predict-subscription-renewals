
#Read data.
data <- read.csv("C:\\Users\\Rahul\\Desktop\\redhat\\Problem 1\\dataset.csv")
str(data)
data$Renewal <- as.factor(data$Renewal)
table(data$Renewal)
str(data)

#Data Partition
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7,0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Converting factor variables with factors more than 52 to integer variables.
# Reason why it is done is because Random forest model can not work with variables having factors greater than 52.
train$C2 <- as.integer(train$C2)
train$C14 <- as.integer(train$C14)
test$C2 <- as.integer(test$C2)
test$C14 <- as.integer(test$C14)

#Building Random Forest model.
library(randomForest)
set.seed(123)
rf <- randomForest(Renewal ~. , data = train)
rf
attributes(rf)

#Predictions and Confusion Matrix with train data
library(caret)
p1 <- predict(rf,train)
p1
confusionMatrix(p1,train$Renewal)  #As obvious it gives 100% accuracy beacause it is being tested on the very same data on which its trained.

#Predictions and confusion matrix with test data
p2 <- predict(rf, test)
cm1 <- confusionMatrix(p2, test$Renewal) #This gives accuracy of 84.8%
cm1

#Error Rate
plot(rf) #we can see as the no. of trees grows the error stays constant and is not reduced any further.

#Tuning Random forest
t <- tuneRF(train[,-16], train[,16],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 300,
            trace = TRUE,
            improve = 0.05)
# now we see that OOB value is lowest at mtry= 6 which earlier was 3 at OOB 14.2%

#Tuned RF Model
rft <- randomForest(Renewal~., data = train,
                   ntree = 300,   #this time ntrees chosen is 300 as seen in the graph, OOB is more or less constant after 300 trees.
                   mtry = 6,
                   importance = TRUE,
                   proximity = TRUE)
rft

p3 <- predict(rft, test)
confusionMatrix(p3, test$Renewal) #This gives accuracy of 84.8%

#As it is seen that even after tuning our model the accuracy still remains the same. So our first model was correct.

#Variable importance
varImpPlot(rf)  #this shows the most important variables for the prediction of the outcomes.
                #clearly C9 is the most important var, followed by C11,C6 and so on.

varUsed(rf) #tells us which predictor var is actualy used the most no. of times in RF model.

#Drawing confusion matrix for visualizing.
draw_confusion_matrix <- function(cm1) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm1$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm1$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm1$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm1$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm1$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm1$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm1$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm1$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm1$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm1$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm1$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm1$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm1$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm1$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm1$overall[2]), 3), cex=1.4)
}  
draw_confusion_matrix(cm1) 




