#get data
options(digits = 3)
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

#Analysis
#gender
library(tidyverse)
train %>% ggplot(aes(Survived, fill = Sex)) +
  geom_bar(position = 'fill')

library(tidyverse)
train %>% ggplot(aes(Sex, fill = Survived)) +
  geom_bar(position = 'fill')

train %>% group_by(Sex) %>%
  summarize( survived = length(which(Survived == 1))/n(),
             died = length(which(Survived == 0))/n()) %>%
  knitr::kable()

train %>% group_by(Survived) %>%
  summarize( male = length(which(Sex == 'male'))/n(),
             female = length(which(Sex == 'female'))/n()) %>%
  knitr::kable()

train %>% ggplot(aes(Age, y = after_stat(count), fill = Survived)) +
  geom_density(alpha = 0.2) +
  facet_grid(. ~ Sex)
  
train %>% ggplot(aes(Age, fill = Survived)) +
  geom_histogram(binwidth = 10, position = 'fill') +
  facet_grid(. ~ Sex)+
  scale_x_continuous(n.breaks = 10)

train %>% filter(!is.na(Fare)) %>%
  ggplot(aes(x = Survived, y = Fare)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.2) +
  scale_y_continuous(trans = 'log2') 
  
train %>% filter(!is.na(Fare)) %>%
  ggplot(aes(Fare, fill = Survived)) +
  geom_density(alpha = 0.2, bw = 0.5) + 
  scale_x_continuous(trans = 'log2', n.breaks = 10) +
  facet_grid(Pclass ~ .)

train %>% ggplot(aes(Parch, fill = Survived)) +
  geom_bar(position = 'dodge') +
  scale_y_continuous(trans = 'log2') + 
  scale_x_continuous(n.breaks = 6)
train %>% ggplot(aes(SibSp, fill = Survived)) +
  geom_bar(position = 'dodge') +
  scale_y_continuous(trans = 'log2') + 
  scale_x_continuous(n.breaks = 6)


train %>% ggplot(aes(Survived, Age, fill = Survived)) +
  geom_boxplot()+
  facet_grid(. ~ Parch)
train %>% ggplot(aes(Survived, Age, fill = Survived)) +
  geom_boxplot()+
  facet_grid(. ~ SibSp)


train %>% ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar(position = 'fill') +
  facet_grid(Embarked ~ Sex) +
  scale_y_continuous()


train %>% apply(MARGIN = 2, unique)
summary(train)
head(train)

head(train$Name, 20)



