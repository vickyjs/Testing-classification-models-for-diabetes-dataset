diabetes <- read.csv ("C:/Users/Vignesh/Downloads/diabetes.csv")
summary(diabetes)
str(diabetes)

# Removing zero values from the data
clean_data <- diabetes[!(apply(diabetes[2:6], 1, function(row) any(row == 0))),]

# Converting everything into lower case
names(clean_data) <- tolower(names(clean_data))

# renaming for easy identification
names(clean_data)[7] <- "dpf"
names(clean_data)

#view the structure and summary of clean_data
str(clean_data)
summary(clean_data)

# For better understanding, sample plots ( histogram)
par(mfrow = c(2,2))
hist(clean_data$pregnancies)
hist(clean_data$glucose)
hist(clean_data$bloodpressure)
hist(clean_data$bmi)
hist(clean_data$insulin)
hist(clean_data$skinthickness)
hist(clean_data$bmi)
hist(clean_data$dpf)
hist(clean_data$age)
hist(clean_data$outcome)

#loading libraries
library(GGally)
library(caret)

#Finding the correlations between the variables
ggpairs (clean_data, axisLabels = "internal", 
        diag = list(continuous = "densityDiag", discrete = "barDiag"))
#From the plot it can be inferred that glucose (0.516), 
#insulin(0.30)and age(0.35) is somewhat correlated
#but it wont be enough to try linear regression using only this factors.

ones <- (clean_data$outcome == 1)
str(clean_data[ones,])

zeros <- (clean_data$outcome == 0)
str(clean_data[zeros,"outcome"])

prop.table(table(clean_data$outcome))
# From the above code, we get there are 262 zeros ("0") and 130 ones ("1")
# and the percentage of "0"= 0.668 and "1" = 0.331

#SVM

library(caret)
library(e1071)

set.seed(2)

split <- sample(nrow(clean_data), nrow(clean_data)*0.85)
train <- clean_data[split,]
test <- clean_data[-split,]

set.seed(7)
fit <- svm(factor(outcome)~., data= train, probability= T)
pre <- predict(fit, test, decision.value= T, probability= T)

#creating confusion matrix to get accuracy 
confusionMatrix(factor(test$outcome), factor(pre))

plot_ly(clean_data, x= ~clean_data$outcome, y= ~clean_data$age, 
        mode= "markers", color = clean_data$outcome, size = clean_data$outcome)

#Neural Net
set.seed(6)
library("neuralnet")
neural_pre <- neuralnet(outcome~ pregnancies+glucose+bloodpressure+skinthickness+insulin+
                        bmi+dpf+age,
                        train,hidden = 4,lifesign = "minimal",linear.output = FALSE,
                        threshold = 0.1)
plot(neural_pre, rep = "best")


test_temp <- subset(test, select = c("pregnancies","glucose","bloodpressure",
                                     "skinthickness","insulin","bmi","dpf","age"))
predict_set <- compute(neural_pre,test_temp)

 

#naivebayes
set.seed(5)
train$outcome <- as.factor(train$outcome)
naive_model <- naiveBayes(outcome ~pregnancies+glucose+bloodpressure+skinthickness+insulin+
                        bmi+dpf+age, data = train)

naive_pre <- predict(naive_model,test)

confusionMatrix(test$outcome, naive_pre)

        














