#get data
data = read.csv("train.csv", na.strings = c('', 'NA'))
data = data[, -c(1,4,9,11)]
data = data[, c(2:8,1)]
summary(data)

#handle blanks
#find blanks
for (i in 1:length(data)){
  if (length(which(is.na(data[, i]))) != 0)
    print(names(data[i]))
}

#impute
library(mice)
md.pattern(data)
imputation = mice(data, m=5, maxit=10, seed=42)
data = complete(imputation, runif(1, min = 1, max = 6))
summary(data)

#Encoding
train[1] = as.integer(factor(train[, 1],
                             levels = sort(unique(train[, 1])),
                             labels = c(1:length(unique(train[, 1]))),
                             ordered = TRUE))
data[, c(2,7)] = lapply(data[, c(2,7)],
                        FUN = function(x){
                          (as.integer(factor(x,
                                             levels = sort(unique(x)),
                                             labels = c(1:length(unique(x))))))
                        })
data[8] = factor(data[, 8],
                  levels = c(0,1),
                  labels = c('dead','alive'))

str(data)
data$Age = as.integer(data$Age)

#data preprocessing
library(tidyverse)
data = data %>% mutate(family = SibSp + Parch)
# data %>% ggplot(aes(Fare, y = after_stat(count), fill = factor(Survived))) +
#   geom_density(alpha = 0.2)+
#   scale_x_continuous(n.breaks = 10, trans = 'log2')
data = data %>% mutate(Age = case_when(
  .$Age <= 10 ~ 1,
  .$Age > 10 & .$Age <= 20 ~ 2,
  .$Age > 20 & .$Age <= 25 ~ 3,
  .$Age > 25 & .$Age <= 30 ~ 4,
  .$Age > 30 & .$Age <= 40 ~ 5,
  .$Age > 40 & .$Age <= 50 ~ 6,
  .$Age > 50 ~ 7
))
q = quantile(data$Fare, seq(0.125,0.875,0.125))
data = data %>% mutate(Fare = case_when(
  .$Fare == 0 ~ 1,
  .$Fare >= 0 & .$Fare <= q[1] ~ 2,
  .$Fare > q[1] & .$Fare <= q[2] ~ 3,
  .$Fare > q[2] & .$Fare <= q[3] ~ 4,
  .$Fare > q[3] & .$Fare <= q[4] ~ 5,
  .$Fare > q[4] & .$Fare <= q[5] ~ 6,
  .$Fare > q[5] & .$Fare <= q[6] ~ 7,
  .$Fare > q[6] & .$Fare <= q[7] ~ 8,
  .$Fare > q[7] ~ 9
))
data = data %>% mutate(alone = ifelse(family == 0, 1, 0))
data = data %>% mutate(age_class = Age*Pclass)
data = data %>% mutate(fare_per_person = Fare/(family+1))
data = data[, c(1:7,9:12,8)]

#split data
library(caTools)
set.seed(7)
split = sample.split(data, SplitRatio = 1.0)
train = subset(data, split == TRUE)
blank = subset(data, split == FALSE)

####################Testing##################
lr = train(form = Survived ~ .,
           data = train,
           method = 'glm',
           metric = 'Accuracy',
           maximize = TRUE)
pred1 = predict(lr, newdata = test[-12], type = 'prob')
pred = apply(predict(lr, newdata = test[-12], type = 'prob'), MARGIN = 1, max)
head(pred1)

nb = train(form = Survived ~ .,
           data = train,
           method = 'naive_bayes',
           metric = 'Accuracy',
           maximize = TRUE)
pred2 = predict(nb, newdata = test[-12], type = 'prob')
head(pred2)

svm = train(form = Survived ~ .,
            data = train,
            method = 'svmRadial',
            metric = 'Accuracy',
            maximize = TRUE,
            trControl = trainControl(classProbs =  TRUE))
pred3 = predict(svm, newdata = test[-12], type = 'prob')
head(pred3)
table(pred3)
ifelse (names(which.max(table(pred3))) == 'dead', 1, 0)
names(which.max(table(pred3))) == 'dead'

knn = train(form = Survived ~ .,
            data = train,
            method = 'knn')
pred4 = predict(knn, newdata = test[-12], type = 'prob')
head(pred4)

rf = train(form = names(train[, length(train)]) ~ .,
           x = train[, -12],
           y = train[, 12],
           method = 'rf',
           metric = 'Accuracy',
           maximize = TRUE)
pred5 = predict(rf, newdata = test[-12], type = 'prob')
head(pred5)

pred = data.frame(dead = apply(data.frame(lr = pred1$dead, nb = pred2$dead, svm = pred3$dead, knn = pred4$dead, rf = pred5$dead),
                            MARGIN = 1,
                            mean),
                  survided = apply(data.frame(lr = pred1$alive, nb = pred2$alive, svm = pred3$alive, knn = pred4$alive, rf = pred5$alive),
                            MARGIN = 1,
                            mean))
head(pred)
pred = apply(pred, MARGIN = 1,
             FUN = function(x){
               ifelse(x[1]>x[2], 0, 1)
             })
class(pred)

cm = table(test[, 12], pred)
accuracy = (cm[1,1]+cm[2,2])/sum(cm)
accuracy
######################################
#create model
library(caret)
folds = createFolds(train$Survived, k=10)

# #logistic Regression
# cv = lapply(folds, function(x){
#   train_fold = train[-x, ]
#   test_fold = train[x, ]
#   lr = train(form = Survived ~ .,
#              data = train_fold,
#              method = 'glm',
#              metric = 'Accuracy',
#              maximize = TRUE)
#   pred = predict(lr, newdata = test_fold[-12])
#   cm = table(test_fold[, 12], pred)
#   accuracy = (cm[1,1]+cm[2,2])/sum(cm)
#   return(accuracy)
# })
# acc = c(acc, mean(as.numeric(cv)))
# 
# #naive bayes
# cv = lapply(folds, function(x){
#   train_fold = train[-x, ]
#   test_fold = train[x, ]
#   nb = train(form = Survived ~ .,
#              data = train_fold,
#              method = 'naive_bayes',
#              metric = 'Accuracy',
#              maximize = TRUE)
#   pred = predict(lr, newdata = test_fold[-12])
#   cm = table(test_fold[, 12], pred)
#   accuracy = (cm[1,1]+cm[2,2])/sum(cm)
#   return(accuracy)
# })
# acc = c(acc, mean(as.numeric(cv)))
# 
# #K nearest neighbour 
# cv = lapply(folds, function(x){
#   train_fold = train[-x, ]
#   test_fold = train[x, ]
#   knn = train(form = Survived ~ .,
#               data = train_fold,
#               method = 'knn')
#   pred = predict(lr, newdata = test_fold[-12])
#   cm = table(test_fold[, 12], pred)
#   accuracy = (cm[1,1]+cm[2,2])/sum(cm)
#   return(accuracy)
# })
# acc = c(acc, mean(as.numeric(cv)))
# 
# #kernel SVM
# cv = lapply(folds, function(x){
#   train_fold = train[-x, ]
#   test_fold = train[x, ]
#   svm = train(form = Survived ~ .,
#               data = train_fold,
#               method = 'svmRadial',
#               metric = 'Accuracy',
#               maximize = TRUE)
#   pred = predict(lr, newdata = test_fold[-12])
#   cm = table(test_fold[, 12], pred)
#   accuracy = (cm[1,1]+cm[2,2])/sum(cm)
#   return(accuracy)
# })
# acc = c(acc, mean(as.numeric(cv)))
# 
# #Random Forest
# cv = lapply(folds, function(x){
#   train_fold = train[-x, ]
#   test_fold = train[x, ]
#   rf = train(form = names(train[, length(train)]) ~ .,
#              x = train_fold[, -12],
#              y = train_fold[, 12],
#              method = 'rf',
#              metric = 'Accuracy',
#              maximize = TRUE)
#   pred = predict(lr, newdata = test_fold[-12])
#   cm = table(test_fold[, 12], pred)
#   accuracy = (cm[1,1]+cm[2,2])/sum(cm)
#   return(accuracy)
# })
# acc = c(acc, mean(as.numeric(cv)))

#ensemble
cv = lapply(folds, function(x){
  train_fold = train[-x, ]
  test_fold = train[x, ]
  lr = train(form = Survived ~ .,
             data = train_fold,
             method = 'glm',
             metric = 'Accuracy',
             maximize = TRUE)
  
  nb = train(form = Survived ~ .,
             data = train_fold,
             method = 'naive_bayes',
             metric = 'Accuracy',
             maximize = TRUE)
  
  knn = train(form = Survived ~ .,
              data = train_fold,
              method = 'knn')

  svm = train(form = Survived ~ .,
              data = train_fold,
              method = 'svmRadial',
              metric = 'Accuracy',
              maximize = TRUE,
              trControl = trainControl(classProbs =  TRUE))

  rf = train(form = names(train[, length(train)]) ~ .,
             x = train_fold[, -12],
             y = train_fold[, 12],
             method = 'rf',
             metric = 'Accuracy',
             maximize = TRUE)

  # pred_lr = predict(lr, newdata = test_fold[-12])
  # pred_nb = predict(nb, newdata = test_fold[-12])
  # pred_knn = predict(knn, newdata = test_fold[-12])
  # pred_svm = predict(svm, newdata = test_fold[-12])
  # pred_rf = predict(rf, newdata = test_fold[-12])
  # ensemble = data.frame(pred_lr, pred_nb, pred_knn, pred_svm, pred_rf)
  # pred = apply(ensemble,
  #              MARGIN = 1,
  #              FUN = function(x){
  #                as.numeric(names(which.max(table(x))))
  #              })
  
  pred_lr = predict(lr, newdata = test_fold[-12], type = 'prob')
  pred_nb = predict(nb, newdata = test_fold[-12], type = 'prob')
  pred_knn = predict(knn, newdata = test_fold[-12], type = 'prob')
  pred_svm = predict(svm, newdata = test_fold[-12], type = 'prob')
  pred_rf = predict(rf, newdata = test_fold[-12], type = 'prob')
  prob = data.frame(dead = apply(data.frame(lr = pred_lr$dead, nb = pred_nb$dead, knn = pred_knn$dead, svm = pred_svm$dead, rf = pred_rf$dead),
                                 MARGIN = 1, mean),
                    survived = apply(data.frame(lr = pred_lr$alive, nb = pred_nb$alive, knn = pred_knn$alive, svm = pred_svm$alive, rf = pred_rf$alive),
                                       MARGIN = 1, mean))
  pred = apply(prob, MARGIN = 1,
               FUN = function(x){
                 ifelse(x[1]>x[2], 0, 1)
               })
  cm = table(test_fold[, 12], pred)
  accuracy = (cm[1,1]+cm[2,2])/sum(cm)
  return(accuracy)
})
mean(as.numeric(cv))

################################
#test data
test = read.csv('test.csv', na.strings = c('', 'NA'))
ids = test[, 1]
test = test[, -c(1,3,8,10)]
#test = test[, c(2:7,1)]

#handle blanks
#find blanks
for (i in 1:length(test)){
  if (length(which(is.na(test[, i]))) != 0)
    print(names(test[i]))
}

#impute data
md.pattern(test)
imputation = mice(test, m=5, maxit = 10, seed = 41)
test = complete(imputation, floor(runif(1, min = 1, max = 6)))
summary(test)

#factorizing
test[1] = as.integer(factor(test[, 1],
                            levels = sort(unique(test[, 1])),
                            labels = c(1:length(unique(test[, 1]))),
                            ordered = TRUE))
test[, c(2,7)] = lapply(test[, c(2,7)],
                          function(x){
                            as.integer(factor(x,
                                              levels = sort(unique(x)),
                                              labels = c(1:length(unique(x)))))
                          })
test$Age = as.integer(test$Age)

#preprocessing
test = test %>% mutate(family = SibSp + Parch)
test = test %>% mutate(Age = case_when(
  .$Age <= 10 ~ 1,
  .$Age > 10 & .$Age <= 20 ~ 2,
  .$Age > 20 & .$Age <= 25 ~ 3,
  .$Age > 25 & .$Age <= 30 ~ 4,
  .$Age > 30 & .$Age <= 40 ~ 5,
  .$Age > 40 & .$Age <= 50 ~ 6,
  .$Age > 50 ~ 7
))
q = quantile(test$Fare, seq(0.125,0.875,0.125))
test = test %>% mutate(Fare = case_when(
  .$Fare == 0 ~ 1,
  .$Fare >= 0 & .$Fare <= q[1] ~ 2,
  .$Fare > q[1] & .$Fare <= q[2] ~ 3,
  .$Fare > q[2] & .$Fare <= q[3] ~ 4,
  .$Fare > q[3] & .$Fare <= q[4] ~ 5,
  .$Fare > q[4] & .$Fare <= q[5] ~ 6,
  .$Fare > q[5] & .$Fare <= q[6] ~ 7,
  .$Fare > q[6] & .$Fare <= q[7] ~ 8,
  .$Fare > q[7] ~ 9
))
test = test %>% mutate(alone = ifelse(family == 0, 1, 0))
test = test %>% mutate(age_class = Age*Pclass)
test = test %>% mutate(fare_per_person = Fare/(family+1))
test = test[, c(1:7,9:11,8)]

#predictions
pred_lr = predict(lr, newdata = test[-12], type = 'prob')
pred_nb = predict(nb, newdata = test[-12], type = 'prob')
pred_knn = predict(knn, newdata = test[-12], type = 'prob')
pred_svm = predict(svm, newdata = test[-12], type = 'prob')
pred_rf = predict(rf, newdata = test[-12], type = 'prob')
prob = data.frame(dead = apply(data.frame(lr = pred_lr$dead, nb = pred_nb$dead, knn = pred_knn$dead, svm = pred_svm$dead, rf = pred_rf$dead),
                               MARGIN = 1, mean),
                  survived = apply(data.frame(lr = pred_lr$alive, nb = pred_nb$alive, knn = pred_knn$alive, svm = pred_svm$alive, rf = pred_rf$alive),
                                     MARGIN = 1, mean))
pred = apply(prob, MARGIN = 1,
             FUN = function(x){
               ifelse(x[1]>x[2], 0, 1)
             })
head(pred)
submit = data.frame(ids, pred)
names(submit) = c('PassengerId', 'Survived')
summary(submit)
write.csv(submit, "titanic submission_ensemble.csv", row.names = FALSE)

