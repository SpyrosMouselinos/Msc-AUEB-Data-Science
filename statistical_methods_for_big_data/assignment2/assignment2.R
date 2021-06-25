library("readxl")
library("dplyr")
library("fastDummies")
library("caret")
library("splitTools")
library("tibble")
#### Set Seed for Reproducability ####
set.seed(2020)

#### Load Data  ####
data_path <-  "C:\\Users\\Guldan\\Desktop\\statistics4bigdata\\assignment2_data.xls"
data <- read_excel(data_path)

#### EDA ####
data <- data %>% select(job, marital, education, previous, month, cons.price.idx, cons.conf.idx, euribor3m, nr.employed, SUBSCRIBED)

### Create binary target value ###
data <- data %>% mutate(SUBSCRIBED=case_when( SUBSCRIBED=="no"~ 0,SUBSCRIBED=="yes"~ 1))

### Find all job categories ###
job_columns <- unique(data$job)
job_columns
### Find all marital categories ###
marital_columns <- unique(data$marital)
marital_columns
### Find all education categories ###
edu_columns <- unique(data$education)
edu_columns
### Find all months categories ###
month_columns <- unique(data$month)
month_columns
### Columns to be converted ###
conversion_columns <- c("job","marital","education","month")



### Convert to Dummy Variable ###
data <- dummy_cols(data, select_columns = conversion_columns,  remove_selected_columns = TRUE, remove_first_dummy=TRUE)

### Get non-duplicate rows ###
data <- distinct(data)

#### Create DataSet ####
### In order to measure the overall Accuracy/F1 score before
### and after we need to create a Train and a Test Set###
train.index <- createDataPartition(data$SUBSCRIBED, p = .7, list = FALSE)
train <- data[ train.index,]
test  <- data[-train.index,]



#### Fit Model as a Whole ####

logistic_regression_whole <- glm(SUBSCRIBED ~ previous + cons.price.idx + cons.conf.idx + euribor3m + nr.employed +
                             `job_blue-collar` + job_entrepreneur + job_housemaid + job_management +
                             job_retired + `job_self-employed` + job_services + job_student + job_technician +
                             job_unemployed + job_unknown + marital_married + marital_single +
                             marital_unknown + education_basic.6y + education_basic.9y +
                             education_high.school + education_illiterate + education_professional.course +
                             education_university.degree + education_unknown +month_aug +
                             month_dec + month_jul + month_jun + month_mar + month_may + month_nov +
                             month_oct + month_sep,data=train,family='binomial')

### Get a Summary of the Model ###
summary(logistic_regression_whole)

### loglik ###
logLik(logistic_regression_whole)

dd<-coef(logistic_regression_whole)

### Predict on Test Set ###
test_probas <- predict(logistic_regression_whole, test, type = "response")

sub.pred = rep("No", dim(test)[1])
sub.pred[test_probas > .5] = "Yes"

### Show Test Confusion Matrix ###
conf <- table(sub.pred, test$SUBSCRIBED)

### Accuracy ###
precision <- conf[2,2] / (conf[2,2] + conf[1,2])
precision
recall <- conf[2,2] / (conf[2,2] + conf[2,1])
recall
acc <- 100 *(conf[1,1] + conf[2,2]) / sum(conf)
acc

f1 <- 100* (2*precision*recall / (precision + recall))
f1


conf
#### Break into 10 parts and fit each one alone combining results ####
folds <- create_folds(train$SUBSCRIBED, k = 10)
coef_list <- data.frame()
for (fold in folds) {
  fit <- glm(SUBSCRIBED ~ previous + cons.price.idx + cons.conf.idx + euribor3m + nr.employed +
               `job_blue-collar` + job_entrepreneur + job_housemaid + job_management +
               job_retired + `job_self-employed` + job_services + job_student + job_technician +
               job_unemployed + job_unknown + marital_married + marital_single +
               marital_unknown + education_basic.6y + education_basic.9y +
               education_high.school + education_illiterate + education_professional.course +
               education_university.degree + education_unknown +month_aug +
               month_dec + month_jul + month_jun + month_mar + month_may + month_nov +
               month_oct + month_sep, data=train[fold,], family='binomial')
  
  
  
  entry <- as.data.frame(t(as.matrix(data.frame(coef(fit)))))
  coef_list <- rbind(coef_list, entry)
}

average_coefficients_10 <- data.frame(sapply(coef_list, mean))
average_coefficients_10 <- cbind(rownames(average_coefficients_10), data.frame(average_coefficients_10, row.names=NULL))
average_coefficients_10 <- deframe(average_coefficients_10)

### Make model with constant weights ###

fit$coefficients <- average_coefficients_10

### Get a Summary of the Model ###
summary(fit)

### loglik ###
logLik(fit)

### After vs Before ###
difference10<-coef(fit) - dd
difference10
### Mean Absolute Difference ###
mean(abs(difference10))

### Predict on Test Set ###
test_probas <- predict(fit, test, type = "response")

sub.pred = rep("No", dim(test)[1])
sub.pred[test_probas > .5] = "Yes"

### Show Test Confusion Matrix ###
conf <- table(sub.pred, test$SUBSCRIBED)

### Accuracy ###
precision <- conf[2,2] / (conf[2,2] + conf[1,2])
precision

recall <- conf[2,2] / (conf[2,2] + conf[2,1])
recall

acc <- 100 *(conf[1,1] + conf[2,2]) / sum(conf)
acc

f1 <- 100* (2*precision*recall / (precision + recall))
f1

conf
#### Break into 20 parts and fit each one alone combining results ####
folds <- create_folds(train$SUBSCRIBED, k = 20)
coef_list <- data.frame()
for (fold in folds) {
  fit <- glm(SUBSCRIBED ~ previous + cons.price.idx + cons.conf.idx + euribor3m + nr.employed +
               `job_blue-collar` + job_entrepreneur + job_housemaid + job_management +
               job_retired + `job_self-employed` + job_services + job_student + job_technician +
               job_unemployed + job_unknown + marital_married + marital_single +
               marital_unknown + education_basic.6y + education_basic.9y +
               education_high.school + education_illiterate + education_professional.course +
               education_university.degree + education_unknown +month_aug +
               month_dec + month_jul + month_jun + month_mar + month_may + month_nov +
               month_oct + month_sep, data=train[fold,], family='binomial')
  
  
  
  entry <- as.data.frame(t(as.matrix(data.frame(coef(fit)))))
  coef_list <- rbind(coef_list, entry)
}

average_coefficients_20 <- data.frame(sapply(coef_list, mean))
average_coefficients_20 <- cbind(rownames(average_coefficients_20), data.frame(average_coefficients_20, row.names=NULL))
average_coefficients_20 <- deframe(average_coefficients_20)

### Make model with constant weights ###

fit$coefficients <- average_coefficients_20

### Get a Summary of the Model ###
summary(fit)

### loglik ###
logLik(fit)

### After vs Before ###
difference20<-coef(fit) - dd
difference20
### Mean Absolute Difference ###
mean(abs(difference20))

### Predict on Test Set ###
test_probas <- predict(fit, test, type = "response")

sub.pred = rep("No", dim(test)[1])
sub.pred[test_probas > .5] = "Yes"

### Show Test Confusion Matrix ###
conf <- table(sub.pred, test$SUBSCRIBED)

### Accuracy ###
precision <- conf[2,2] / (conf[2,2] + conf[1,2])
precision

recall <- conf[2,2] / (conf[2,2] + conf[2,1])
recall

acc <- 100 *(conf[1,1] + conf[2,2]) / sum(conf)
acc

f1 <- 100* (2*precision*recall / (precision + recall))
f1

conf

