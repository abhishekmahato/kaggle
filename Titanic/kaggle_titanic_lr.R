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
train[1] = factor(train[, 1],
                  levels = sort(unique(train[, 1])),
                  labels = c(1:length(unique(train[, 1]))),
                  ordered = TRUE)
data[, c(2,7)] = lapply(data[, c(2,7)],
                         FUN = function(x){
                           as.integer(factor(x,
                                  levels = sort(unique(x)),
                                  labels = c(1:length(unique(x)))))
                         })
data[8] = factor(data[, 8],
                  levels = c(0,1),
                  labels = c(0,1))

str(data)
data$Age = as.integer(data$Age)

#data preprocessing
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
split = sample.split(data, SplitRatio = 0.75)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

#create model
library(caret)
folds = createFolds(train$Survived, k=10)

lr = train(form = Survived ~ .,
           data = train,
           method = 'glm',
           metric = 'Accuracy',
           maximize = TRUE)
pred = predict(lr, newdata = test[-12])
cm = table(test[, 12], pred)
accuracy = (cm[1,1]+cm[2,2])/sum(cm)
cv = lapply(folds, function(x){
  train_fold = train[-x, ]
  test_fold = train[x, ]
  lr = train(form = Survived ~ .,
            data = train_fold,
            method = 'glm',
            metric = 'Accuracy',
            maximize = TRUE)
  pred = predict(lr, newdata = test_fold[-12])
  cm = table(test_fold[, 12], pred)
  accuracy = (cm[1,1]+cm[2,2])/sum(cm)
  return(accuracy)
})

mean(as.numeric(cv))
#######################
#test data preprocessing
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
test[, c(1,2,7)] = lapply(test[, c(1,2,7)],
                          function(x){
                            factor(x,
                                   levels = sort(unique(x)),
                                   labels = c(1:length(unique(x))))
                          })

pred = predict(lr, newdata = test)
submit = data.frame(ids, pred)
names(submit) = c('PassengerId', 'Survived')
summary(submit)
write.csv(submit, "titanic submission_lr.csv")

names(which.max(table(pred)))
summary(fin)
data.frame(table(pred))
order(data.frame(table(pred))$Freq)

