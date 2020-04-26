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
