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
install.packages('corrplot')
library(corrplot)
corrplot(cor(train[, 3:6], method = 'pearson'))
ggplot()+
  geom_point(aes(x = train$SibSp, y = train$Survived),
             color = 'red')

sr = table(train[, 4], train[, 8])
sr



histogram(train[, 3], type='count', main = 'Age distribution', xlab = 'Age')

plot(train[, c(3,5)], col=train[,8])
points(train[,8], pch = 21, bg = ifelse(train[,8] == 0, 'red', 'green'))
ggplot(train, aes(y = train[,8], x = train[, 3], fill=train[,2]))+
          geom_bar(stat = 'identity', position = 'dodge')

#install.packages('dslabs')
library(dslabs)
library(tidyverse)
data(heights)
summary(heights)
p = seq(0.01, 0.99, 0.01)
percentile = quantile(heights$height, p)
percentile
#install.packages('tidyverse')

data(murders)
names(murders)
p = murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y = total)) +
  geom_text(aes(x = population/10^6, y = total, label = abb))+
  geom_abline(aes(x = muders$population/10^6, y = murders$total),
              data = murders, 0, r)
  
r = summarize(murders, rate = sum(murders$total) / sum(murders$population) * 10^6)
data(starwars)

starwars
starwars %>%
  mutate(mass / mean(mass, na.rm = TRUE)) %>%
  pull()

starwars %>%
  group_by(gender) %>%
  mutate(mass / mean(mass, na.rm = TRUE)) %>%
  pull()

p = murders %>% group_by(region) %>%
  summarize(median = median(total))

murders %>% arrange(region, population) 


