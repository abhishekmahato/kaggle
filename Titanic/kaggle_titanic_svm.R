#get train
train = read.csv('train.csv', na.strings = c('', 'NA'))
head(train, 12)


#Remove uncessary Rows
train = train[, -c(1,4,9,11)]

#Rearrange to put dependent varaible in last column
train = train[c(2:8,1)]

which(c(FALSE, FALSE, FALSE, TRUE))


#find NA values
for (i in 1:length(train)){
  if (length(which(is.na(train[i]))) != 0){
    print(names(train[i]))
  }
}

#impute values
#install.packages('mice')
library(mice)
md.pattern(train)
imputation = mice(train, m=5, maxit = 5, seed = 7)
#random number between 1 to 5
train = complete(imputation, floor(runif(1, max = 6, min = 1))) 
summary(train)

#Factoring
train[, 1] = factor(train[, 1],
                   levels = c('1', '2', '3'),
                   labels = c(1:length(unique(train[,1]))),
                   ordered = TRUE)
train[, 2] = factor(train[, 2],
                   levels = c('male', 'female'),
                   labels = c(1, 2))
train[, 7] = factor(train[, 7],
                   levels = c('C', 'Q', 'S'),
                   labels = c(1, 2, 3))
train[, 8] = factor(train[, 8],
                   levels = c(0,1),
                   labels = c(0,1))

#Scaling 
train[, c(3,4,5,6)] = scale(train[, c(3,4,5,6)])

#Grid Search
library(caret)
svm = train(form = Survived ~ .,
            data = train,
            method = 'svmRadial')

##################
#Test Set
test = read.csv('test.csv', na.strings = c('', 'NA'))
ids = test[, 1]
summary(test)

#Remove uncessary Rows
test = test[, -c(1,3,8,10)]

#find NA values
for (i in 1:length(test)){
  if (length(which(is.na(test[i]))) != 0){
    print(names(test[i]))
  }
}

#impute values
#install.packages('mice')
library(mice)
md.pattern(test)
imputation = mice(test, m=5, maxit = 5, seed = 7)
#random number between 1 to 5
test = complete(imputation, floor(runif(1, max = 6, min = 1))) 
summary(test)

#Factoring
test[, 1] = factor(test[, 1],
                    levels = sort(unique(test[,1])),
                    labels = c(1:length(unique(test[,1]))),
                    ordered = TRUE)

test[, 2] = factor(test[, 2],
                    levels = c('male', 'female'),
                    labels = c(1, 2))
test[, 7] = factor(test[, 7],
                    levels = c('C', 'Q', 'S'),
                    labels = c(1, 2, 3))
#Scaling 
test[, c(3,4,5,6)] = scale(test[, c(3,4,5,6)])

pred = predict(svm, newdata = test)
summary(pred)


submit = data.frame(ids, pred)
names(submit) = c('PassengerId', 'Survived')
summary(submit)
write.csv(submit, "submission.csv")
