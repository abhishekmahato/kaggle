#get data
train = read.csv("train.csv", na.strings = c('', 'NA'))
train = train[, -c(1,4,9,11)]
train = train[, c(2:8,1)]
summary(train)

#handle blanks
#find blanks
for (i in 1:length(train)){
  if (length(which(is.na(train[, i]))) != 0)
    print(names(train[i]))
}

#impute
library(mice)
md.pattern(train)
imputation = mice(train, m=5, maxit=10, seed=42)
train = complete(imputation, runif(1, min = 1, max = 6))
summary(train)

#Encoding
train[1] = factor(train[, 1],
                  levels = sort(unique(train[, 1])),
                  labels = c(1:length(unique(train[, 1]))),
                  ordered = TRUE)
train[, c(2,7)] = lapply(train[, c(2,7)],
                           FUN = function(x){
                             factor(x,
                                    levels = sort(unique(x)),
                                    labels = c(1:length(unique(x))))
                           })
train[8] = factor(train[, 8],
                  levels = c(0,1),
                  labels = c(0,1))


#create model
library(caret)
knn = train(form = Survived ~ .,
           data = train,
           method = 'knn')

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

pred = predict(knn, newdata = test)
submit = data.frame(ids, pred)
names(submit) = c('PassengerId', 'Survived')
summary(submit)
write.csv(submit, "titanic submission_knn.csv")


