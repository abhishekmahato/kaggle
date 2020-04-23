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

#Gender
sr = table(train[, 2], train[, 8])
sr
surv_rate_f = sr[1,2]/sum(sr[1,]) #74%
surv_rate_m = sr[2,2]/sum(sr[2,]) #19%

#class
sr = table(train[, 1], train[, 8])
sr
surv_rate_1 = sr[1,2]/sum(sr[1,]) #63%
surv_rate_2 = sr[2,2]/sum(sr[2,]) #47%
surv_rate_3 = sr[3,2]/sum(sr[3,]) #24%

#Embarked
sr = table(train[, 7], train[, 8])
sr
surv_rate_1 = sr[1,2]/sum(sr[1,]) #56%
surv_rate_2 = sr[2,2]/sum(sr[2,]) #39%
surv_rate_3 = sr[3,2]/sum(sr[3,]) #34%

#Rest
cor(train[, 3:6], method = 'pearson')

ggplot()+
  geom_point(aes(x = train$SibSp, y = train$Survived),
             color = 'red')

sr = table(train[, 4], train[, 8])
sr


ggplot() +
  geom_point(aes(x = test$YearsExperience, y = test$Salary),
             color = 'red') +
  geom_line(aes(x = train$YearsExperience, y = predict(linreg, newdata = train)),
            color = 'green') +
  ggtitle ('Exp vs Salary (Test)') +
  xlab ('Years') +
  ylab ('Salary')




histogram(train[, 8], type='count', )
ggplot(train, aes(y = train[,8], x = train[, 1]))+
         geom_bar(stat = 'identity', position = 'dodge')

