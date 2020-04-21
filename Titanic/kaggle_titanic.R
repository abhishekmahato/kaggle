#get data
data = read.csv('train.csv')
head(data, 10)

#Remove uncessary Rows
data = data[, -c(1,4,9)]

#Rearrange to put dependent varaible in last column
data = data[c(2:9,1)]

#find NA values
for (i in 1:length(data)){
  if (length(which(is.na(data[i]))) != 0){
    print(names(data[i]))
  }
}

#impute values
#install.packages('mice')
library(mice)
md.pattern(data)
imputation = mice(data, m=5, maxit = 5, seed = 7)
data = complete(imputation, 3)
summary(data)
