library(dslabs)
library(tidyverse)
data(heights)
data(mtcars)
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

data("gapminder")
head(gapminder)
gapminder %>% filter(year == 2005 & country %in% c('Sri Lanka', 'Turkey')) %>%
  select(country, infant_mortality)

#4.1.3 gapminder dataset
gapminder %>% filter(year == 1962) %>%
  ggplot(aes(x = fertility, y = life_expectancy)) +
  geom_point(aes(col = continent)) 
  
#4.2.1 facteing
gapminder %>% filter(year %in% c(1962, 2005)) %>%
  ggplot(aes(x = fertility, y = life_expectancy)) +
  geom_point(aes(col = continent)) +
  facet_grid(continent ~ year)

gapminder %>% filter(year %in% c(1962, 1979, 1985, 1995, 2000, 2005)) %>%
  ggplot(aes(x = fertility, y = life_expectancy)) +
  geom_point(aes(col = continent)) +
  facet_wrap(. ~ year)

#4.2.2 timeseries
countries = c('South Korea', 'Germany')
labels = data.frame(country = countries, x = c(1975, 1965), y=c(60,72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(x = year, y = life_expectancy, group = country, col = country)) +
  geom_line() +
  geom_text()+
  theme(legend.position = "none")

#4.2.3 histogram
gapminder = gapminder %>% mutate(dollars_per_day = gdp/(population*365)) 
past_year = 1970
gapminder %>% 
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day)))+
  geom_histogram(binwidth = 1)
  
gapminder %>% 
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes((dollars_per_day)))+
  geom_histogram(binwidth = 1)+
  scale_x_continuous(trans = 'log2')

#4.2.4 Boxplots
gapminder$region = with(gapminder, reorder(region, dollars_per_day, FUN = median))
gapminder %>%
  mutate(reorder(region, dollars_per_day, FUN = median)) %>% #can be used instead of the above line
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(reorder(region, dollars_per_day, FUN = median), dollars_per_day, group = region)) +
  geom_boxplot(aes(col = continent))+
  theme(axis.text =  element_text(angle = 60, hjust = 1))+
  scale_y_continuous(trans = 'log2')

#4.2.5 Comparing distribution
west = c('Northern Europe', 'Northern America', 'Australia and New Zealand', 'Western Europe', 'Western Europe', 'Southern Europe')
past_year = 1970
present_year = 2010
past_country = gapminder %>% filter(year == past_year) %>% .$country
present_country = gapminder %>% filter(year == present_year) %>% .$country
countries = intersect(past_country, present_country)

gapminder %>% 
  filter (year %in% c(past_year, present_year) & !is.na(gdp) & country %in% countries) %>%
  mutate (group = ifelse(region %in% west, 'West', 'Developing')) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, fill = 'grey')+
  scale_x_continuous(trans = 'log2')+
  facet_wrap(year ~ group)

gapminder %>%
  filter(year %in% c(past_year, present_year), !is.na(gdp), country %in% countries) %>%
  mutate (region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_y_continuous(trans = 'log2') +
  facet_grid(year ~ .)


gapminder %>%
  filter(year %in% c(past_year, present_year), !is.na(gdp), country %in% countries) %>%
  mutate (region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day)) +
  geom_boxplot(aes(fill = factor(year))) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_y_continuous(trans = 'log2')

head(gapminder)

#4.2.6 density plots
gapminder %>%
  filter(year %in% c(past_year, present_year) & !is.na(gdp) & country %in% countries) %>%
  mutate(group = ifelse(region %in% west, 'West', 'Developing')) %>%
  ggplot(aes(x = dollars_per_day, y = after_stat(count), fill = group)) +
  geom_density(alpha = 0.2, bw = 0.75) +
  scale_x_continuous(trans = 'log2')+
  facet_grid(year ~ .)

gapminder <- gapminder %>%
  mutate(group = case_when(
      .$region %in% west ~ "West",
      .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
      .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
      .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
      TRUE ~ "Others"))
  # reorder factor levels
  gapminder <- gapminder %>%
    mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))  
  
  
gapminder %>%
  filter(year %in% c(past_year, present_year) & !is.na(gdp) & country %in% countries) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  geom_density(alpha = 0.2, bw = 0.75, position = 'stack') +
  scale_x_continuous(trans = 'log2')+
  facet_grid(year ~ .)

gapminder %>%
  filter(year %in% c(past_year, present_year) & !is.na(gdp) & country %in% countries) %>%
  group_by(year) %>%
  mutate (weight = population/sum(population)) %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  geom_density(alpha = 0.2, bw = 0.75, position = 'stack') +
  scale_x_continuous(trans = 'log2')+
  facet_grid(year ~ .)

#5.2.1 Show data
heights %>% ggplot(aes(sex, height)) +
  geom_jitter(width = 0.1, alpha = 0.2)

#5.3.3 Show 3 variables
library(RColorBrewer)
data(us_contagious_diseases)
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))

dat %>% filter (state == 'California' & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() + 
  geom_vline(xintercept = 1963, colour = 'green')+
  ylab("cases per 10,000")

dat %>% filter(!is.na(rate)) %>% ggplot(aes(year, state, fill = rate)) +
  geom_tile(color = 'grey50')+
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, 'Reds'), trans = 'sqrt') +
  theme(panel.grid = element_blank())+
  geom_vline(xintercept = 1963)

#titanic Assignment
options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
#install.packages('titanic')
#Question 2
train = titanic_train
str(train)
for(i in 1:length(train)){
  if ( train[i] %>% is.na() %>% which() %>% length() != 0 )
    print(names(train[i]))
}
#train[i] %>% is.na() %>% which() %>% length() == 0
#length(which(is.na(train[, i]))) !=0
summary(train)
head(train)

train %>% ggplot(aes(x = Age, fill = Sex)) +
  geom_density(alpha = 0.2)+
  facet_grid(Sex ~ .)

train %>% ggplot(aes(x = Age, y = after_stat(count))) +
  geom_density(alpha = 0.2) +
  scale_x_continuous(trans = 'log2') +
  facet_grid(Sex ~ .)
  geom_vline(xintercept = 17) +
  geom_vline(xintercept = 35)

train %>% ggplot(aes(x = Age, y = after_stat(count), fill = Sex)) +
  geom_density(alpha = 0.2, bw = 0.75) +
  scale_x_continuous(trans = 'log2')+
  scale_y_continuous(trans = 'log2')

train %>% group_by(Sex) %>% filter(Age == 40) %>%
summarize(max = max(Age, na.rm = TRUE), n=n()) %>% 
  knitr::kable()

age_group = train %>% filter (Age <= 17) %>% group_by(Sex) %>%
  summarize(n = n())
total = train %>% group_by(Sex) %>% 
  summarize(n=n())
age_group$n/total$n

#Question 3
params <- train %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

p = seq(0,0.99,0.01)
observed = quantile(train$Age, p, na.rm = TRUE)
ggplot() +
  geom_qq(aes(sample = observed), dparams = params, na.rm = TRUE)+
  geom_abline()
  
#Question 4
train %>% ggplot(aes(Survived, fill = Sex)) +
  geom_bar(position = position_dodge())

train %>% ggplot(aes(Sex, fill = factor(Survived))) +
  geom_bar()



train %>% group_by(Survived) %>%
  summarize(prop = n()/(train %>% summarize(n =n()))$n) %>%
  knitr::kable()

#Question 5
train %>%
  ggplot(aes(Age, y = stat(count), fill = factor(Survived))) +
  geom_density(alpha = 0.2) +
  scale_x_continuous(breaks = c(0, 8, 18, 30, 50, 70, 80)) +
  geom_vline(xintercept = c(0, 8, 18, 30, 50, 70, 80))

#Question 6 
train %>% filter(Fare != 0) %>%
  ggplot(aes(x = Survived, y = Fare)) +
  geom_boxplot(aes(group = Survived)) +
  geom_jitter(alpha = 0.2, width = 0.2) + 
  scale_y_continuous(trans = 'log2')

#Question 7
train %>% ggplot(aes(Pclass, fill = factor(Survived))) +
  geom_bar()

train %>% ggplot(aes(Survived, fill = factor(Pclass))) +
  geom_bar(position = position_fill())

train %>% group_by(Pclass, Survived) %>% 
  summarize(prop = n()) %>%
  knitr::kable()

#Question 8
train %>% filter(!is.na(Age)) %>%
  ggplot(aes(Age, fill = factor(Survived))) +
  geom_density(aes(y = after_stat(count), alpha = 0.2)) +
  facet_grid(Sex ~ Pclass)

################################################
#Regex Code

#install.packages('stringr')
library(stringr)
data(words)
x = words
str_view(x, "\\b[\\w]{7}\\w*\\b", match = TRUE)
x = c("apple pie", "apple", "apple cake")
x


